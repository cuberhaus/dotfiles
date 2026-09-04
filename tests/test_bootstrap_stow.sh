#!/usr/bin/env bash
# shellcheck disable=SC2317
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BASE_FUNCTIONS="$REPO_ROOT/.local/scripts/bootstrap/base_functions"
ORIGINAL_PATH="$PATH"

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

assert_file_contains() {
    local file="$1"
    local expected="$2"
    grep -Fq -- "$expected" "$file" || fail "Expected $file to contain: $expected"
}

assert_file_missing() {
    local file="$1"
    [ ! -e "$file" ] || fail "Expected $file to be absent"
}

write_fake_stow() {
    cat > "$FAKE_BIN/stow" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$STOW_LOG"
case " $* " in
    *" -n "*) printf '%s\n' "${FAKE_STOW_PREVIEW:-}" ;;
esac
EOF
    chmod +x "$FAKE_BIN/stow"
}

setup_case() {
    CASE_DIR="$(mktemp -d)"
    unset SKIP_STOW TEST_CONFIRM UNATTENDED
    export HOME="$CASE_DIR/home"
    export FAKE_BIN="$CASE_DIR/bin"
    export STOW_LOG="$CASE_DIR/stow.log"
    export DOTFILES_ROOT="$REPO_ROOT"
    mkdir -p "$HOME" "$FAKE_BIN"
    : > "$STOW_LOG"
    write_fake_stow
    export PATH="$FAKE_BIN:$ORIGINAL_PATH"
}

teardown_case() {
    rm -rf "$CASE_DIR"
    export PATH="$ORIGINAL_PATH"
}

run_stow_preflight() {
    (
        source "$BASE_FUNCTIONS"
        bootstrap_stdin_is_interactive() { return 0; }
        bootstrap_confirm_stow() { [ "${TEST_CONFIRM:-yes}" = yes ]; }
        bootstrap_stow_checkout "$1"
    )
}

test_bootstrap_logging_mirrors_output() {
    setup_case
    (
        export XDG_STATE_HOME="$CASE_DIR/state"
        source "$BASE_FUNCTIONS"
        bootstrap_enable_logging work
        printf 'bootstrap logging test\n'
        exec 1>&- 2>&-
        wait "$BOOTSTRAP_LOG_TEE_PID"
    )
    local log_file
    log_file="$(find "$CASE_DIR/state/cuberhaus/bootstrap" -type f -name 'work-*.log' -print -quit)"
    [ -n "$log_file" ] || fail 'Expected the work bootstrap to create a log file'
    assert_file_contains "$log_file" 'bootstrap logging test'
    teardown_case
}

test_logged_output_retains_interactive_state() {
    (
        source "$BASE_FUNCTIONS"
        BOOTSTRAP_STDOUT_WAS_TTY=true
        bootstrap_stdout_is_interactive
    ) || fail 'Logging must preserve the original interactive stdout state'
}

test_clean_install_applies_previewed_links() {
    setup_case
    export FAKE_STOW_PREVIEW='LINK: .zshenv => dotfiles/.zshenv'
    run_stow_preflight ubuntu
    grep -Fv -- ' -n ' "$STOW_LOG" | grep -Fq -- ' -t ' \
        || fail 'Expected Stow apply after confirmation'
    teardown_case
}

test_existing_links_are_idempotent() {
    setup_case
    export FAKE_STOW_PREVIEW=''
    run_stow_preflight ubuntu
    if grep -Fv -- ' -n ' "$STOW_LOG" | grep -Fq -- ' -t '; then
        fail 'Existing links should not trigger a Stow apply'
    fi
    teardown_case
}

test_conflicts_are_backed_up_after_confirmation() {
    setup_case
    export FAKE_STOW_PREVIEW='* existing target is neither a link nor a directory: .zshenv'
    printf 'existing configuration\n' > "$HOME/.zshenv"
    run_stow_preflight ubuntu
    assert_file_missing "$HOME/.zshenv"
    find "$HOME/.dotfiles-backup" -type f -name '.zshenv' -print -quit | grep -q . \
        || fail 'Expected the conflicting file to be backed up'
    teardown_case
}

test_decline_preserves_conflicts() {
    setup_case
    export FAKE_STOW_PREVIEW='* existing target is neither a link nor a directory: .zshenv'
    export TEST_CONFIRM=no
    printf 'existing configuration\n' > "$HOME/.zshenv"
    run_stow_preflight ubuntu
    [ -f "$HOME/.zshenv" ] || fail 'Declining must preserve the conflicting file'
    assert_file_missing "$HOME/.dotfiles-backup"
    teardown_case
}

test_no_stow_skips_all_stow_commands() {
    setup_case
    export SKIP_STOW=true
    export FAKE_STOW_PREVIEW='LINK: .zshenv => dotfiles/.zshenv'
    run_stow_preflight ubuntu
    [ ! -s "$STOW_LOG" ] || fail '--no-stow must not invoke Stow'
    teardown_case
}

test_no_stow_argument_sets_skip_flag() {
    (
        source "$BASE_FUNCTIONS"
        parse_bootstrap_args --no-stow
        [ "$SKIP_STOW" = true ] || fail '--no-stow must set SKIP_STOW=true'
    )
}

test_missing_stow_uses_selected_platform_installer() {
    setup_case
    rm "$FAKE_BIN/stow"
    export FAKE_STOW_PREVIEW='LINK: .zshenv => dotfiles/.zshenv'
    (
        source "$BASE_FUNCTIONS"
        bootstrap_stdin_is_interactive() { return 0; }
        bootstrap_confirm_stow() { return 0; }
        bootstrap_stow_is_available() { [ -x "$FAKE_BIN/stow" ]; }
        bootstrap_install_stow() {
            printf 'install:%s\n' "$1" >> "$STOW_LOG"
            write_fake_stow
        }
        bootstrap_stow_checkout ubuntu
    )
    assert_file_contains "$STOW_LOG" 'install:ubuntu'
    teardown_case
}

test_noninteractive_input_never_applies_stow() {
    setup_case
    export FAKE_STOW_PREVIEW='LINK: .zshenv => dotfiles/.zshenv'
    printf '' | (
        source "$BASE_FUNCTIONS"
        bootstrap_stow_checkout ubuntu
    )
    [ ! -s "$STOW_LOG" ] || fail 'Noninteractive input must not invoke Stow'
    teardown_case
}

test_entrypoints_use_checkout_relative_paths() {
    local entrypoint
    for entrypoint in arch manjaro ubuntu ubuntu_windows mac work; do
        assert_file_contains "$REPO_ROOT/.local/scripts/bootstrap/$entrypoint" 'DOTFILES_ROOT='
        assert_file_contains "$REPO_ROOT/.local/scripts/bootstrap/$entrypoint" 'bootstrap_stow_checkout'
    done
    assert_file_contains "$REPO_ROOT/.local/scripts/bootstrap/work" 'bootstrap_enable_logging work'
}

test_bootstrap_logging_mirrors_output
test_logged_output_retains_interactive_state
test_clean_install_applies_previewed_links
test_existing_links_are_idempotent
test_conflicts_are_backed_up_after_confirmation
test_decline_preserves_conflicts
test_no_stow_skips_all_stow_commands
test_no_stow_argument_sets_skip_flag
test_missing_stow_uses_selected_platform_installer
test_noninteractive_input_never_applies_stow
test_entrypoints_use_checkout_relative_paths

printf 'Bootstrap Stow tests passed.\n'