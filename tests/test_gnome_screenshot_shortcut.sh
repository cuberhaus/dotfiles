#!/usr/bin/env bash
# Hermetic tests for gnome_screenshot_shortcut_install and gnome_screenshot_shortcut_uninstall
# in bootstrap/base_functions: adding <Super><Shift>s to org.gnome.shell.keybindings
# show-screenshot-ui, preserving existing bindings, idempotence, non-persistent gsettings
# failure detection, skipping machines without gsettings or the GNOME schema, uninstalling,
# and how the Ubuntu/work profiles and uninstall functions wire them up.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BOOTSTRAP_DIR="$REPO_ROOT/.local/scripts/bootstrap"
SCRATCH="$(mktemp -d)"
RM_COMMAND="$(command -v rm)"
ORIGINAL_PATH="$PATH"
trap '"$RM_COMMAND" -rf "$SCRATCH"' EXIT

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

assert_equals() {
    [ "$1" = "$2" ] || fail "$3: expected [$1], got [$2]"
}

assert_contains() {
    case "$1" in
        *"$2"*) ;;
        *) fail "$3: [$2] not found in [$1]" ;;
    esac
}

# shellcheck source=/dev/null
source "$BOOTSTRAP_DIR/base_functions"

new_case() {
    CASE="$SCRATCH/$1"
    mkdir -p "$CASE/bin"
    STATE_FILE="$CASE/keybindings"
    CALLS_FILE="$CASE/calls"
    printf "['Print']\n" > "$STATE_FILE"
    : > "$CALLS_FILE"
    cat > "$CASE/bin/gsettings" <<EOF
#!/usr/bin/env bash
printf '%s\n' "\$*" >> "$CALLS_FILE"
if [ "\${FAKE_GSETTINGS_NO_SCHEMA:-false}" = true ]; then
    exit 1
fi
case "\$1" in
    get)
        [ "\$2" = "org.gnome.shell.keybindings" ] && [ "\$3" = "show-screenshot-ui" ] || exit 1
        cat "$STATE_FILE"
        ;;
    set)
        [ "\$2" = "org.gnome.shell.keybindings" ] && [ "\$3" = "show-screenshot-ui" ] || exit 1
        if [ "\${FAKE_GSETTINGS_DROP_WRITE:-false}" != true ]; then
            printf '%s\n' "\$4" > "$STATE_FILE"
        fi
        ;;
    *)
        exit 1
        ;;
esac
EOF
    chmod +x "$CASE/bin/gsettings"
    export PATH="$CASE/bin:$ORIGINAL_PATH"
}

test_adds_super_shift_s_to_default_print_binding() {
    new_case default

    gnome_screenshot_shortcut_install >/dev/null || fail 'the install failed'

    assert_equals "['Print', '<Super><Shift>s']" "$(cat "$STATE_FILE")" \
        'show-screenshot-ui after install'
}

test_adds_only_super_shift_s_when_empty() {
    new_case empty
    printf '@as []\n' > "$STATE_FILE"

    gnome_screenshot_shortcut_install >/dev/null || fail 'the install failed'
    assert_equals "['<Super><Shift>s']" "$(cat "$STATE_FILE")" \
        'show-screenshot-ui when starting from @as []'

    gnome_screenshot_shortcut_uninstall >/dev/null || fail 'the uninstall failed'
    assert_equals '@as []' "$(cat "$STATE_FILE")" \
        'show-screenshot-ui restored to @as [] after uninstall'
}

test_recognizes_gnome_settings_shift_super_order() {
    new_case shift-super
    local output
    printf "['Print', '<Shift><Super>s']\n" > "$STATE_FILE"

    output="$(gnome_screenshot_shortcut_install)" || fail 'install with <Shift><Super>s failed'
    assert_contains "$output" 'already set' 'message when <Shift><Super>s is present'
    if grep -Fq 'set org.gnome.shell.keybindings' "$CALLS_FILE"; then
        fail 'must not duplicate binding when <Shift><Super>s is already present'
    fi

    gnome_screenshot_shortcut_uninstall >/dev/null || fail 'uninstall of <Shift><Super>s failed'
    assert_equals "['Print']" "$(cat "$STATE_FILE")" \
        'show-screenshot-ui after removing <Shift><Super>s'
}

test_rejects_unexpected_gsettings_format() {
    new_case malformed
    local status=0
    printf 'not-an-array\n' > "$STATE_FILE"

    gnome_screenshot_shortcut_install >/dev/null 2>&1 || status=$?
    [ "$status" -ne 0 ] || fail 'must fail on unexpected gsettings format'
    assert_equals 'not-an-array' "$(cat "$STATE_FILE")" \
        'unexpected format must not be overwritten'
}

test_preserves_custom_bindings() {
    new_case custom
    printf "['Print', '<Control>Print']\n" > "$STATE_FILE"

    gnome_screenshot_shortcut_install >/dev/null || fail 'the install failed'

    assert_equals "['Print', '<Control>Print', '<Super><Shift>s']" "$(cat "$STATE_FILE")" \
        'show-screenshot-ui with existing custom binding preserved'
}

test_second_run_is_idempotent() {
    new_case idempotent
    local output

    gnome_screenshot_shortcut_install >/dev/null || fail 'first install failed'
    : > "$CALLS_FILE"
    output="$(gnome_screenshot_shortcut_install)" || fail 'second install failed'

    assert_contains "$output" 'already set' 'message on second run'
    if grep -Fq 'set org.gnome.shell.keybindings' "$CALLS_FILE"; then
        fail 'second run must not call gsettings set when shortcut is already present'
    fi
}

test_fails_when_gsettings_does_not_persist_write() {
    new_case memory-backend
    local status=0 output

    output="$(FAKE_GSETTINGS_DROP_WRITE=true gnome_screenshot_shortcut_install 2>&1)" || status=$?

    [ "$status" -ne 0 ] || fail 'must fail when gsettings set does not persist'
    assert_contains "$output" 'did not persist' 'warning when gsettings write is dropped'
}

test_skips_when_gsettings_or_schema_is_missing() {
    new_case missing
    local empty="$CASE/empty" status=0
    mkdir -p "$empty"

    PATH="$empty" gnome_screenshot_shortcut_install >/dev/null 2>&1 || status=$?
    assert_equals 0 "$status" 'exit status without gsettings'

    FAKE_GSETTINGS_NO_SCHEMA=true gnome_screenshot_shortcut_install >/dev/null 2>&1 || status=$?
    assert_equals 0 "$status" 'exit status without GNOME schema'
}

test_uninstall_removes_only_super_shift_s() {
    new_case uninstall

    gnome_screenshot_shortcut_install >/dev/null || fail 'install failed'
    gnome_screenshot_shortcut_uninstall >/dev/null || fail 'uninstall failed'

    assert_equals "['Print']" "$(cat "$STATE_FILE")" \
        'show-screenshot-ui restored to Print after uninstall'
}

test_profiles_and_uninstallers_use_it() {
    local entry_scripts=("$BOOTSTRAP_DIR/ubuntu" "$BOOTSTRAP_DIR/work")
    local uninstall_functions=("$BOOTSTRAP_DIR/uninstall_ubuntu_functions" "$BOOTSTRAP_DIR/uninstall_work_functions")
    local file

    for file in "${entry_scripts[@]}"; do
        grep -Eq '^[[:space:]]*gnome_screenshot_shortcut_install \|\|$' "$file" ||
            fail "$file must run gnome_screenshot_shortcut_install without aborting on failure"
    done
    for file in "${uninstall_functions[@]}"; do
        grep -Eq '^[[:space:]]*gnome_screenshot_shortcut_uninstall( \|\| true)?$' "$file" ||
            fail "$file must undo what gnome_screenshot_shortcut_install set"
    done
}

for test_name in \
    test_adds_super_shift_s_to_default_print_binding \
    test_adds_only_super_shift_s_when_empty \
    test_recognizes_gnome_settings_shift_super_order \
    test_rejects_unexpected_gsettings_format \
    test_preserves_custom_bindings \
    test_second_run_is_idempotent \
    test_fails_when_gsettings_does_not_persist_write \
    test_skips_when_gsettings_or_schema_is_missing \
    test_uninstall_removes_only_super_shift_s \
    test_profiles_and_uninstallers_use_it; do
    ( "$test_name" ) || {
        printf 'not ok - %s\n' "$test_name" >&2
        exit 1
    }
    printf 'ok - %s\n' "$test_name"
done

printf 'GNOME screenshot shortcut tests passed.\n'
