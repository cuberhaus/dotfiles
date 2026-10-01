#!/usr/bin/env bash
# Tests for the experimental Gentoo bootstrap profile (issue #20).
#
# Every test runs against a throw-away HOME and a fake sysroot. External
# commands (sudo, emerge, portageq, rc-update, ...) are PATH stubs that only
# record what they were asked to do, so no test can touch the host system.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BOOTSTRAP_DIR="$REPO_ROOT/.local/scripts/bootstrap"
ORIGINAL_PATH="$PATH"
REAL_ID="$(command -v id)"
export REAL_ID

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

assert_equals() {
    local expected="$1" actual="$2" message="$3"
    [ "$expected" = "$actual" ] ||
        fail "$message"$'\n'"--- expected ---"$'\n'"$expected"$'\n'"--- actual ---"$'\n'"$actual"
}

assert_contains() {
    local haystack="$1" needle="$2" message="$3"
    case "$haystack" in
        *"$needle"*) ;;
        *) fail "$message"$'\n'"--- missing: $needle ---"$'\n'"$haystack" ;;
    esac
}

setup_case() {
    CASE_DIR="$(mktemp -d)"
    export HOME="$CASE_DIR/home"
    # A developer shell usually exports these; pin them so no test can write outside CASE_DIR.
    export XDG_CONFIG_HOME="$HOME/.config"
    export XDG_DATA_HOME="$HOME/.local/share"
    export XDG_CACHE_HOME="$HOME/.cache"
    export XDG_STATE_HOME="$HOME/.local/state"
    export EVENT_LOG="$CASE_DIR/events.log"
    mkdir -p "$HOME"
    : > "$EVENT_LOG"
    unset GENTOO_INIT GENTOO_SECTIONS GENTOO_SYSROOT GENTOO_DRY_RUN GENTOO_EMERGE_OPTS
    unset UNATTENDED SKIP_STOW DOTFILES DOTFILES_ROOT SUDO_COMMAND
    unset FAKE_UID FAKE_USER FAKE_SUDO_FAIL FAKE_EMERGE_FAIL FAKE_LOGIN_SHELL FAKE_RC_DIR FAKE_STOW_PREVIEW
    export PATH="$ORIGINAL_PATH"
}

teardown_case() {
    export PATH="$ORIGINAL_PATH"
    rm -rf "$CASE_DIR"
}

## Write a manifest fixture and print its path.
write_manifest() {
    local path="$CASE_DIR/manifest.packages"
    cat > "$path"
    printf '%s\n' "$path"
}

# shellcheck source=/dev/null
source_gentoo_functions() {
    source "$BOOTSTRAP_DIR/base_functions"
    source "$BOOTSTRAP_DIR/gentoo_functions"
}

###############################################################################
# Seam: package-list parsing
###############################################################################

test_manifest_parsing_reads_sections_and_ignores_noise() {
    setup_case
    local manifest actual
    manifest="$(write_manifest <<'EOF'
# A comment before any section is fine.

[base]
app-admin/stow            # trailing comment
   dev-vcs/git

[shell]
app-shells/zsh
dev-vcs/git               # duplicate across sections is emitted once
EOF
)"
    (
        source_gentoo_functions
        gentoo_manifest_atoms "$manifest" openrc
    ) > "$CASE_DIR/atoms" || fail 'a well-formed manifest must parse'
    actual="$(cat "$CASE_DIR/atoms")"
    assert_equals $'app-admin/stow\ndev-vcs/git\napp-shells/zsh' "$actual" \
        'atoms must be emitted in file order, trimmed, without comments or duplicates'
    teardown_case
}

## Print the atoms selected for an init system on one space-separated line.
selected_atoms() {
    local manifest="$1" init="$2"
    (
        source_gentoo_functions
        gentoo_manifest_atoms "$manifest" "$init" | tr '\n' ' ' | sed 's/ $//'
    )
}

test_manifest_selection_follows_init_system_and_optional_sections() {
    setup_case
    local manifest error_log
    error_log="$CASE_DIR/error.log"
    manifest="$(write_manifest <<'EOF'
[base]
app-admin/stow
[extra:optional]
x11-wm/xmonad
[session-openrc:openrc]
sys-auth/elogind
[session-systemd:systemd]
sys-apps/systemd-utils
EOF
)"
    assert_equals 'app-admin/stow sys-auth/elogind' "$(selected_atoms "$manifest" openrc)" \
        'openrc default must include openrc sections and skip optional and systemd sections'
    assert_equals 'app-admin/stow sys-apps/systemd-utils' "$(selected_atoms "$manifest" systemd)" \
        'systemd default must include systemd sections and skip optional and openrc sections'

    assert_equals 'app-admin/stow x11-wm/xmonad' \
        "$(GENTOO_SECTIONS='extra base' selected_atoms "$manifest" openrc)" \
        'an explicit selection must install exactly the named sections in manifest order'
    assert_equals 'app-admin/stow x11-wm/xmonad' \
        "$(GENTOO_SECTIONS='base,extra' selected_atoms "$manifest" systemd)" \
        'an explicit selection may be comma separated'

    if GENTOO_SECTIONS='session-systemd' selected_atoms "$manifest" openrc 2> "$error_log"; then
        fail 'selecting a systemd-only section on an openrc target must be refused'
    fi
    assert_contains "$(cat "$error_log")" 'session-systemd' \
        'the refusal must name the offending section'
    assert_contains "$(cat "$error_log")" 'systemd' \
        'the refusal must say which init system the section needs'

    if GENTOO_SECTIONS='base nope' selected_atoms "$manifest" openrc 2> "$error_log"; then
        fail 'selecting an unknown section must be refused'
    fi
    assert_contains "$(cat "$error_log")" 'nope' \
        'the refusal must name the unknown section'
    teardown_case
}

## Assert that a manifest is refused with a message at the given line.
## Usage: assert_manifest_rejected <line> <expected message fragment> <manifest line>...
assert_manifest_rejected() {
    local line="$1" fragment="$2" manifest status=0
    shift 2
    manifest="$CASE_DIR/manifest.packages"
    printf '%s\n' "$@" > "$manifest"
    (
        source_gentoo_functions
        gentoo_manifest_atoms "$manifest" openrc > "$CASE_DIR/stdout" 2> "$CASE_DIR/stderr"
    ) || status=$?
    [ "$status" -ne 0 ] || fail "manifest must be refused: $*"
    [ ! -s "$CASE_DIR/stdout" ] || fail "a refused manifest must not emit partial atoms: $*"
    assert_contains "$(cat "$CASE_DIR/stderr")" "manifest.packages:$line:" \
        "the refusal must point at line $line for: $*"
    assert_contains "$(cat "$CASE_DIR/stderr")" "$fragment" \
        "the refusal must explain the problem for: $*"
}

test_manifest_parsing_rejects_anything_that_is_not_a_plain_atom() {
    setup_case
    assert_manifest_rejected 1 'outside a section' 'app-admin/stow'
    # Option-, set-, version-, and slot-shaped entries would change what emerge does.
    assert_manifest_rejected 2 'plain category/package atom' '[base]' '--unmerge'
    assert_manifest_rejected 2 'plain category/package atom' '[base]' '-C'
    assert_manifest_rejected 2 'plain category/package atom' '[base]' '@world'
    assert_manifest_rejected 2 'plain category/package atom' '[base]' '>=app-admin/stow-2.0'
    assert_manifest_rejected 2 'plain category/package atom' '[base]' 'app-admin/stow:0'
    assert_manifest_rejected 2 'plain category/package atom' '[base]' 'stow'
    assert_manifest_rejected 1 'invalid section header' '[Bad Name]'
    assert_manifest_rejected 1 'unknown section attribute' '[base:bogus]'
    assert_manifest_rejected 2 'duplicate section' '[base]' '[base]'
    assert_manifest_rejected 1 'two init systems' '[base:openrc,systemd]'
    teardown_case
}

## Create a fake Gentoo root and a PATH containing a portageq stub.
## Usage: make_target [openrc|systemd|both|none] [arch]
make_target() {
    local init="${1:-openrc}" arch="${2:-amd64}"
    export GENTOO_SYSROOT="$CASE_DIR/root"
    mkdir -p "$GENTOO_SYSROOT/etc" "$CASE_DIR/bin"
    printf 'Gentoo Base System release 2.17\n' > "$GENTOO_SYSROOT/etc/gentoo-release"
    case "$init" in
        openrc) mkdir -p "$GENTOO_SYSROOT/run/openrc" ;;
        systemd) mkdir -p "$GENTOO_SYSROOT/run/systemd/system" ;;
        both) mkdir -p "$GENTOO_SYSROOT/run/openrc" "$GENTOO_SYSROOT/run/systemd/system" ;;
        none) ;;
    esac
    cat > "$CASE_DIR/bin/portageq" <<EOF
#!/usr/bin/env bash
if [ "\$*" = 'envvar ARCH' ]; then
    printf '%s\n' '$arch'
else
    printf 'unexpected portageq call: %s\n' "\$*" >&2
    exit 64
fi
EOF
    chmod +x "$CASE_DIR/bin/portageq"
    export PATH="$CASE_DIR/bin:$ORIGINAL_PATH"
}

## Run target detection in a subshell; leave stdout, stderr, and the init in $CASE_DIR.
run_detect_target() {
    (
        source_gentoo_functions
        gentoo_detect_target > "$CASE_DIR/stdout" 2> "$CASE_DIR/stderr" || exit $?
        printf '%s\n' "$GENTOO_INIT" > "$CASE_DIR/init"
    )
}

assert_target_refused() {
    local fragment="$1" status=0
    run_detect_target || status=$?
    [ "$status" -ne 0 ] || fail "target must be refused (expected: $fragment)"
    assert_contains "$(cat "$CASE_DIR/stderr")" "$fragment" \
        'the refusal must explain what is unsupported'
}

test_target_detection_selects_the_running_init_system() {
    setup_case
    unset GENTOO_INIT

    make_target openrc
    run_detect_target || fail 'an amd64 OpenRC system must be accepted'
    assert_equals 'openrc' "$(cat "$CASE_DIR/init")" 'the OpenRC marker must select openrc'
    assert_contains "$(cat "$CASE_DIR/stdout")" 'init=openrc' 'the detected target must be reported'

    rm -rf "$CASE_DIR/root"
    make_target systemd
    run_detect_target || fail 'an amd64 systemd system must be accepted'
    assert_equals 'systemd' "$(cat "$CASE_DIR/init")" 'the systemd marker must select systemd'

    # A chroot (for example during installation) runs neither init system, so the
    # operator must name it; a matching override on a running system is accepted too.
    rm -rf "$CASE_DIR/root"
    make_target none
    GENTOO_INIT=openrc run_detect_target || fail 'an explicit init system must be accepted in a chroot'
    assert_equals 'openrc' "$(cat "$CASE_DIR/init")" 'the override must select the init system'
    rm -rf "$CASE_DIR/root"
    make_target systemd
    GENTOO_INIT=systemd run_detect_target || fail 'an override matching the running init must be accepted'
    teardown_case
}

test_unsupported_targets_are_refused_with_an_explanation() {
    setup_case
    unset GENTOO_INIT

    make_target openrc
    rm "$GENTOO_SYSROOT/etc/gentoo-release"
    assert_target_refused 'not a Gentoo system'

    make_target openrc arm64
    assert_target_refused 'arm64'
    assert_contains "$(cat "$CASE_DIR/stderr")" 'amd64' 'the refusal must name the supported architecture'

    rm -rf "$CASE_DIR/root"
    make_target none
    assert_target_refused 'GENTOO_INIT'

    rm -rf "$CASE_DIR/root"
    make_target both
    assert_target_refused 'GENTOO_INIT'

    rm -rf "$CASE_DIR/root"
    make_target openrc
    GENTOO_INIT=runit assert_target_refused 'runit'
    GENTOO_INIT=systemd assert_target_refused 'openrc'

    # A target that is not Gentoo must be refused before any package manager is consulted.
    rm -rf "$CASE_DIR/root"
    make_target openrc
    rm "$GENTOO_SYSROOT/etc/gentoo-release"
    printf '#!/usr/bin/env bash\ntouch "%s"\n' "$CASE_DIR/portageq-called" > "$CASE_DIR/bin/portageq"
    assert_target_refused 'not a Gentoo system'
    [ ! -e "$CASE_DIR/portageq-called" ] || fail 'a non-Gentoo target must be refused before portageq runs'
    teardown_case
}

## Install a PATH stub whose body is read from stdin.
stub_command() {
    mkdir -p "$CASE_DIR/bin"
    cat > "$CASE_DIR/bin/$1"
    chmod +x "$CASE_DIR/bin/$1"
    export PATH="$CASE_DIR/bin:$ORIGINAL_PATH"
}

## Stub sudo and id; every sudo call is appended to the event log.
stub_privileges() {
    stub_command sudo <<'EOF'
#!/usr/bin/env bash
printf 'sudo %s\n' "$*" >> "$EVENT_LOG"
[ "${FAKE_SUDO_FAIL:-false}" != true ]
EOF
    stub_command id <<'EOF'
#!/usr/bin/env bash
case "$*" in
    -u) printf '%s\n' "${FAKE_UID:-1000}" ;;
    -un) printf '%s\n' "${FAKE_USER:-tester}" ;;
    *) exec "$REAL_ID" "$@" ;;
esac
EOF
}

## Run a snippet with the Gentoo functions loaded in a subshell.
## Usage: in_gentoo_shell '<commands>'
in_gentoo_shell() {
    (
        source_gentoo_functions
        eval "$1"
    )
}

test_privileges_are_checked_before_anything_is_changed() {
    setup_case
    local status=0 error_log="$CASE_DIR/error.log"
    stub_privileges

    FAKE_UID=0 in_gentoo_shell 'gentoo_prepare_privileges' 2> "$error_log" || status=$?
    [ "$status" -ne 0 ] || fail 'running as root must be refused'
    assert_contains "$(cat "$error_log")" 'root' 'the refusal must say why root is refused'
    [ ! -s "$EVENT_LOG" ] || fail 'no privileged command may run when the user is root'

    # Without sudo installed the profile cannot elevate; say how to get it.
    status=0
    mkdir -p "$CASE_DIR/no-sudo"
    cp "$CASE_DIR/bin/id" "$CASE_DIR/no-sudo/id"
    ln -s "$(command -v bash)" "$CASE_DIR/no-sudo/bash" # the id stub's env shebang needs bash on PATH
    PATH="$CASE_DIR/no-sudo" in_gentoo_shell 'gentoo_prepare_privileges' 2> "$error_log" || status=$?
    [ "$status" -ne 0 ] || fail 'a missing sudo must be refused'
    assert_contains "$(cat "$error_log")" 'app-admin/sudo' 'the refusal must name the package that provides sudo'
    teardown_case
}

test_interactive_privileges_authenticate_up_front() {
    setup_case
    stub_privileges
    in_gentoo_shell 'gentoo_prepare_privileges; gentoo_stop_sudo_keepalive' ||
        fail 'interactive privilege preparation must succeed with a working sudo'
    assert_contains "$(head -n 1 "$EVENT_LOG")" 'sudo -v' \
        'credentials must be requested before the first change so a long run cannot stall on a prompt'
    teardown_case
}

test_unattended_privileges_never_prompt() {
    setup_case
    local status=0 error_log="$CASE_DIR/error.log"
    stub_privileges

    FAKE_SUDO_FAIL=true UNATTENDED=true in_gentoo_shell 'gentoo_prepare_privileges' 2> "$error_log" || status=$?
    [ "$status" -ne 0 ] || fail 'unattended mode must refuse when sudo needs a password'
    assert_contains "$(cat "$error_log")" 'sudo -v' 'the refusal must explain how to cache credentials'

    : > "$EVENT_LOG"
    UNATTENDED=true in_gentoo_shell 'gentoo_prepare_privileges; sudo example-command; gentoo_stop_sudo_keepalive' ||
        fail 'unattended mode must accept cached credentials'
    assert_contains "$(cat "$EVENT_LOG")" 'sudo -n example-command' \
        'every unattended privileged command must be non-interactive'
    teardown_case
}

## Start the sudo keepalive, wait until it has refreshed twice, then stop it.
## The keepalive's PID is left in $CASE_DIR/keepalive.pid.
run_keepalive_probe() {
    (
        source_gentoo_functions
        GENTOO_SUDO_KEEPALIVE_SECONDS=0.1
        gentoo_start_sudo_keepalive
        printf '%s\n' "$GENTOO_SUDO_KEEPALIVE_PID" > "$CASE_DIR/keepalive.pid"
        # The loop must keep refreshing on its own, without further calls from us.
        for _ in $(seq 1 50); do
            if [ "$(grep -c '^sudo -n true$' "$EVENT_LOG")" -ge 2 ]; then
                break
            fi
            sleep 0.1
        done
        gentoo_stop_sudo_keepalive
    )
}

test_sudo_keepalive_refreshes_credentials_until_stopped() {
    setup_case
    stub_privileges
    local pid attempts=0

    run_keepalive_probe || fail 'the sudo keepalive must start and stop cleanly'
    [ "$(grep -c '^sudo -n true$' "$EVENT_LOG")" -ge 2 ] ||
        fail 'the keepalive must refresh sudo credentials repeatedly while the bootstrap runs'

    pid="$(cat "$CASE_DIR/keepalive.pid")"
    while kill -0 "$pid" 2>/dev/null && [ "$attempts" -lt 20 ]; do
        sleep 0.1
        attempts=$((attempts + 1))
    done
    ! kill -0 "$pid" 2>/dev/null || fail 'stopping the keepalive must terminate its process'
    teardown_case
}

## Stub emerge; every call is appended to the event log.
stub_emerge() {
    stub_command emerge <<'EOF'
#!/usr/bin/env bash
printf 'emerge %s\n' "$*" >> "$EVENT_LOG"
[ "${FAKE_EMERGE_FAIL:-false}" != true ]
EOF
}

## Put atoms into the fake @world file of the current fake root.
set_world() {
    export GENTOO_SYSROOT="$CASE_DIR/root"
    mkdir -p "$GENTOO_SYSROOT/var/lib/portage"
    printf '%s\n' "$@" > "$GENTOO_SYSROOT/var/lib/portage/world"
}

install_manifest() {
    write_manifest <<'EOF'
[base]
app-admin/stow
dev-vcs/git
app-shells/zsh
EOF
}

test_install_emerges_only_packages_missing_from_world() {
    setup_case
    stub_privileges
    stub_emerge
    local manifest output
    manifest="$(install_manifest)"
    set_world dev-vcs/git app-misc/unrelated

    output="$(in_gentoo_shell "gentoo_install_packages '$manifest' openrc")" ||
        fail 'installing the missing packages must succeed'
    assert_equals 'sudo emerge --ask=n --verbose --noreplace --select app-admin/stow app-shells/zsh' \
        "$(cat "$EVENT_LOG")" \
        'only atoms missing from @world may be emerged, recorded in @world, and never replaced'
    assert_contains "$output" '3 selected, 1 already in @world, 2 to install' \
        'the report must say what is already satisfied'

    # Binary-package and parallelism policy belongs to the operator.
    : > "$EVENT_LOG"
    GENTOO_EMERGE_OPTS='--getbinpkg --usepkg --jobs=2' in_gentoo_shell "gentoo_install_packages '$manifest' openrc" >/dev/null ||
        fail 'extra emerge options must be accepted'
    assert_equals 'sudo emerge --ask=n --verbose --noreplace --select --getbinpkg --usepkg --jobs=2 app-admin/stow app-shells/zsh' \
        "$(cat "$EVENT_LOG")" 'GENTOO_EMERGE_OPTS must be passed through before the atoms'
    teardown_case
}

test_install_is_idempotent_when_everything_is_in_world() {
    setup_case
    stub_privileges
    stub_emerge
    local manifest output
    manifest="$(install_manifest)"
    set_world app-admin/stow dev-vcs/git app-shells/zsh

    output="$(in_gentoo_shell "gentoo_install_packages '$manifest' openrc")" ||
        fail 'a satisfied manifest must succeed'
    [ ! -s "$EVENT_LOG" ] || fail "a satisfied manifest must not invoke sudo or emerge: $(cat "$EVENT_LOG")"
    assert_contains "$output" 'nothing to install' 'a satisfied manifest must say so'

    # A missing @world file (fresh stage3) means nothing is satisfied yet.
    rm "$GENTOO_SYSROOT/var/lib/portage/world"
    in_gentoo_shell "gentoo_install_packages '$manifest' openrc" >/dev/null ||
        fail 'a missing @world file must be treated as empty'
    assert_contains "$(cat "$EVENT_LOG")" 'app-admin/stow dev-vcs/git app-shells/zsh' \
        'with no @world file every atom must be installed'
    teardown_case
}

test_install_dry_run_only_previews_with_pretend() {
    setup_case
    stub_privileges
    stub_emerge
    local manifest
    manifest="$(install_manifest)"
    set_world dev-vcs/git

    GENTOO_DRY_RUN=true in_gentoo_shell "gentoo_install_packages '$manifest' openrc" >/dev/null ||
        fail 'a dry run must succeed'
    assert_equals 'emerge --pretend --verbose --noreplace --select app-admin/stow app-shells/zsh' \
        "$(cat "$EVENT_LOG")" \
        'a dry run may only run emerge --pretend, without sudo'
    teardown_case
}

test_install_failures_are_reported_with_a_recovery_path() {
    setup_case
    stub_privileges
    stub_emerge
    local manifest status=0 error_log="$CASE_DIR/error.log"
    manifest="$(install_manifest)"
    set_world

    FAKE_SUDO_FAIL=true in_gentoo_shell "gentoo_install_packages '$manifest' openrc" 2> "$error_log" >/dev/null || status=$?
    [ "$status" -ne 0 ] || fail 'a failed emerge must fail the bootstrap'
    assert_contains "$(cat "$error_log")" 'rerun' 'a failed emerge must tell the operator how to recover'

    # A malformed manifest is refused before anything is installed.
    : > "$EVENT_LOG"
    status=0
    printf '[base]\n--unmerge\n' > "$CASE_DIR/bad.packages"
    in_gentoo_shell "gentoo_install_packages '$CASE_DIR/bad.packages' openrc" 2> "$error_log" >/dev/null || status=$?
    [ "$status" -ne 0 ] || fail 'a malformed manifest must be refused'
    [ ! -s "$EVENT_LOG" ] || fail 'a malformed manifest must not reach sudo or emerge'
    teardown_case
}

## Stub rc-update: "show <runlevel>" prints $FAKE_RC_DIR/<runlevel>; anything else is logged.
stub_rc_update() {
    export FAKE_RC_DIR="$CASE_DIR/rc"
    mkdir -p "$FAKE_RC_DIR"
    stub_command rc-update <<'EOF'
#!/usr/bin/env bash
if [ "$1" = show ]; then
    cat "$FAKE_RC_DIR/$2" 2>/dev/null || true
else
    printf 'rc-update %s\n' "$*" >> "$EVENT_LOG"
fi
EOF
}

test_openrc_services_are_enabled_once() {
    setup_case
    stub_privileges
    stub_rc_update
    local output

    in_gentoo_shell 'gentoo_enable_services openrc' >/dev/null ||
        fail 'enabling the OpenRC session services must succeed'
    assert_equals $'sudo rc-update add elogind boot\nsudo rc-update add dbus default' "$(cat "$EVENT_LOG")" \
        'elogind belongs in the boot runlevel and dbus in default'

    printf '            elogind |      boot\n' > "$FAKE_RC_DIR/boot"
    printf '               dbus |      default\n' > "$FAKE_RC_DIR/default"
    : > "$EVENT_LOG"
    output="$(in_gentoo_shell 'gentoo_enable_services openrc')" ||
        fail 'enabling already-enabled services must succeed'
    [ ! -s "$EVENT_LOG" ] || fail "enabled services must not be added again: $(cat "$EVENT_LOG")"
    assert_contains "$output" 'elogind is already enabled' 'already-satisfied services must be reported'

    # A similarly named service must not count as the one we need.
    printf '          elogind-extra |      boot\n' > "$FAKE_RC_DIR/boot"
    in_gentoo_shell 'gentoo_enable_services openrc' >/dev/null || fail 'enabling must succeed'
    assert_contains "$(cat "$EVENT_LOG")" 'sudo rc-update add elogind boot' \
        'a service with a longer name must not satisfy the check'
    teardown_case
}

test_systemd_needs_no_extra_services() {
    setup_case
    stub_privileges
    stub_rc_update
    in_gentoo_shell 'gentoo_enable_services systemd' >/dev/null ||
        fail 'the systemd path must succeed'
    [ ! -s "$EVENT_LOG" ] || fail "the systemd path must not touch OpenRC or sudo: $(cat "$EVENT_LOG")"
    teardown_case
}

test_dry_run_describes_mutations_without_running_them() {
    setup_case
    stub_privileges
    stub_rc_update
    local output
    output="$(GENTOO_DRY_RUN=true in_gentoo_shell 'gentoo_enable_services openrc')" ||
        fail 'a dry run must succeed'
    [ ! -s "$EVENT_LOG" ] || fail "a dry run must not run privileged commands: $(cat "$EVENT_LOG")"
    assert_contains "$output" '[dry-run] would run: sudo rc-update add elogind boot' \
        'a dry run must describe what it would change'
    teardown_case
}

test_user_environment_is_prepared_without_privileges() {
    setup_case
    stub_privileges
    local output

    in_gentoo_shell 'gentoo_prepare_user_environment' >/dev/null ||
        fail 'preparing the user environment must succeed'
    [ ! -s "$EVENT_LOG" ] || fail "the user environment must not need sudo: $(cat "$EVENT_LOG")"
    local dir
    for dir in .local/share/vim/undo .local/share/vim/swap .local/share/vim/backup \
        .cache/zsh .cache/vlc .vim/cache; do
        [ -d "$HOME/$dir" ] || fail "missing $dir"
    done
    [ -f "$HOME/.vim/theme" ] || fail 'the vim theme file must be created'

    # The operator's edits survive a rerun.
    printf 'colorscheme custom\n' > "$HOME/.vim/theme"
    output="$(in_gentoo_shell 'gentoo_prepare_user_environment')" || fail 'a rerun must succeed'
    assert_equals 'colorscheme custom' "$(cat "$HOME/.vim/theme")" 'an existing vim theme must not be overwritten'
    assert_contains "$output" 'already' 'a rerun must report already-satisfied steps'
    teardown_case
}

test_distro_marker_is_written_once() {
    setup_case
    local output
    in_gentoo_shell 'gentoo_write_distro_marker' >/dev/null || fail 'writing the distro marker must succeed'
    assert_equals $'#! /bin/sh\nexport DISTRO=gentoo' "$(cat "$XDG_CONFIG_HOME/distro")" \
        'the marker must follow the other profiles'
    output="$(in_gentoo_shell 'gentoo_write_distro_marker')" || fail 'a rerun must succeed'
    assert_contains "$output" 'already' 'a rerun must report the marker as already written'
    teardown_case
}

## Stub getent so the login shell of the fake user is $FAKE_LOGIN_SHELL.
stub_getent() {
    stub_command getent <<'EOF'
#!/usr/bin/env bash
if [ "$1" = passwd ]; then
    printf '%s:x:1000:1000::/home/%s:%s\n' "$2" "$2" "${FAKE_LOGIN_SHELL:-/bin/bash}"
fi
EOF
}

test_default_shell_becomes_zsh_only_when_it_safely_can() {
    setup_case
    stub_privileges
    stub_getent
    stub_command zsh <<'EOF'
#!/usr/bin/env bash
exit 0
EOF
    export GENTOO_SYSROOT="$CASE_DIR/root"
    mkdir -p "$GENTOO_SYSROOT/etc"
    printf '/bin/bash\n%s/bin/zsh\n' "$CASE_DIR" > "$GENTOO_SYSROOT/etc/shells"
    local status=0 error_log="$CASE_DIR/error.log" output

    in_gentoo_shell 'gentoo_configure_default_shell' >/dev/null || fail 'changing the login shell must succeed'
    assert_equals "sudo chsh -s $CASE_DIR/bin/zsh tester" "$(cat "$EVENT_LOG")" \
        'the login shell of the invoking user must be set to the listed zsh'

    # Idempotent: judged by the account's login shell, not the stale $SHELL of this session.
    : > "$EVENT_LOG"
    output="$(FAKE_LOGIN_SHELL=/bin/zsh in_gentoo_shell 'gentoo_configure_default_shell')" ||
        fail 'a rerun must succeed'
    [ ! -s "$EVENT_LOG" ] || fail 'a login shell that is already zsh must not be changed again'
    assert_contains "$output" 'already' 'a rerun must report that zsh is already the login shell'

    # chsh refuses shells missing from /etc/shells, and we do not edit /etc/shells ourselves.
    printf '/bin/bash\n' > "$GENTOO_SYSROOT/etc/shells"
    in_gentoo_shell 'gentoo_configure_default_shell' 2> "$error_log" >/dev/null || status=$?
    [ "$status" -ne 0 ] || fail 'a zsh missing from /etc/shells must be refused'
    assert_contains "$(cat "$error_log")" '/etc/shells' 'the refusal must name /etc/shells'
    [ ! -s "$EVENT_LOG" ] || fail 'no chsh may run when zsh is not a listed shell'

    # A listed path that no longer exists would leave the account with an unusable login shell.
    status=0
    printf '/bin/bash\n%s/gone/zsh\n' "$CASE_DIR" > "$GENTOO_SYSROOT/etc/shells"
    in_gentoo_shell 'gentoo_configure_default_shell' 2> "$error_log" >/dev/null || status=$?
    [ "$status" -ne 0 ] || fail 'a stale /etc/shells entry must not be trusted'
    [ ! -s "$EVENT_LOG" ] || fail "no chsh may run for a shell that does not exist: $(cat "$EVENT_LOG")"

    # Without zsh installed there is nothing to switch to.
    status=0
    mkdir -p "$CASE_DIR/no-zsh"
    cp "$CASE_DIR/bin/id" "$CASE_DIR/bin/getent" "$CASE_DIR/no-zsh/"
    ln -s "$(command -v bash)" "$CASE_DIR/no-zsh/bash"
    PATH="$CASE_DIR/no-zsh" in_gentoo_shell 'gentoo_configure_default_shell' 2> "$error_log" >/dev/null || status=$?
    [ "$status" -ne 0 ] || fail 'a missing zsh must be refused'
    assert_contains "$(cat "$error_log")" 'not installed' 'the refusal must say that zsh is not installed'
    teardown_case
}

###############################################################################
# Seam: Gentoo uses Portage and never another distribution's package manager
###############################################################################

## Stub the package managers of other platforms; any use is a bug on Gentoo.
stub_foreign_package_managers() {
    local name
    for name in apt apt-get dpkg pacman yay brew snap flatpak; do
        stub_command "$name" <<'EOF'
#!/usr/bin/env bash
printf 'FOREIGN %s %s\n' "$(basename "$0")" "$*" >> "$EVENT_LOG"
exit 1
EOF
    done
}

## Fail if the event log shows any foreign package manager, even behind sudo.
assert_no_foreign_package_manager() {
    if grep -Eq '(^|[ :])(apt|apt-get|dpkg|pacman|yay|brew|snap|flatpak)( |$)' "$EVENT_LOG"; then
        fail "a foreign package manager was used on Gentoo:"$'\n'"$(cat "$EVENT_LOG")"
    fi
}

test_stow_is_installed_through_portage() {
    setup_case
    stub_privileges
    stub_foreign_package_managers
    stub_command stow <<'EOF'
#!/usr/bin/env bash
exit 0
EOF
    in_gentoo_shell 'bootstrap_install_stow gentoo' >/dev/null ||
        fail 'installing Stow on Gentoo must succeed'
    assert_equals 'sudo emerge --ask=n --noreplace --select app-admin/stow' "$(cat "$EVENT_LOG")" \
        'Stow must be installed with emerge, recorded in @world, and never reinstalled'
    assert_no_foreign_package_manager
    teardown_case
}

###############################################################################
# Seam: profile selection through the Makefile
###############################################################################

## Print a make target's recipe as written, without running anything.
##
## `make -n` is not safe for this: it still runs every recipe line that mentions
## $(MAKE), and bootstrap-workspace has such a line that can clone a private
## repository and restore skills. `make -p` prints the parsed rules before any
## goal is built, and the unknown goal makes make stop right after.
make_recipe() {
    {
        make --no-print-directory -C "$REPO_ROOT" -pnrR __no_such_target__ 2>/dev/null || true
    } | awk -v target="$1" '
        $0 == target ":" { in_rule = 1; next }
        in_rule && /^\t/ { sub(/^\t/, ""); print; next }
        in_rule && !/^#/ { exit }'
}

test_make_selects_the_gentoo_profile_explicitly() {
    setup_case
    local recipe other output status=0

    recipe="$(make_recipe bootstrap-gentoo)"
    [ -n "$recipe" ] || fail 'bootstrap-gentoo must be a make target with a recipe'
    assert_contains "$recipe" "bash .local/scripts/bootstrap/gentoo \$(BOOTSTRAP_ARGS)" \
        'the Gentoo target must run the Gentoo entrypoint with the shared arguments'
    assert_contains "$recipe" 'bootstrap-workspace' \
        'the Gentoo target must deploy the workspace files like the other profiles'
    for other in arch manjaro ubuntu mac work; do
        case "$recipe" in
            *"bootstrap/$other "*) fail "the Gentoo target must not run the $other bootstrap" ;;
        esac
    done
    # Native automation needs systemd timers plus apt or pacman; app restores target
    # applications the Gentoo manifest omits. Neither is chained until proven usable.
    case "$recipe" in
        *install-automations*) fail 'the Gentoo target must not install native automation' ;;
        *restore-apps*) fail 'the Gentoo target must not restore application data' ;;
    esac

    assert_equals 'bash .local/scripts/bootstrap/gentoo --dry-run' "$(make_recipe bootstrap-gentoo-dry-run)" \
        'a dedicated target must preview the Gentoo bootstrap and nothing else'

    # The unattended entry point dispatches to bootstrap-<profile>.
    assert_contains "$(make_recipe bootstrap-unattended)" '|gentoo) ;;' \
        'the unattended entry point must accept an explicit gentoo profile'

    # Profiles are chosen explicitly; nothing is detected or guessed. Both refusals stop
    # at the first recipe line, before any $(MAKE) line; the overrides are a safety net.
    output="$(make --no-print-directory -C "$REPO_ROOT" \
        CUBERHAUS_WORKSPACE_DIR="$CASE_DIR/workspace" RESTORE_WORKSPACE_SKILLS=0 \
        bootstrap-unattended PROFILE=bogus 2>&1)" || status=$?
    [ "$status" -ne 0 ] || fail 'an unknown profile must be refused'
    assert_contains "$output" 'gentoo' 'the refusal must list gentoo among the profiles'
    status=0
    make --no-print-directory -C "$REPO_ROOT" \
        CUBERHAUS_WORKSPACE_DIR="$CASE_DIR/workspace" RESTORE_WORKSPACE_SKILLS=0 \
        bootstrap-unattended > /dev/null 2>&1 || status=$?
    [ "$status" -ne 0 ] || fail 'the unattended entry point must refuse to guess a profile'
    teardown_case
}

###############################################################################
# Seam: the Stow preview in a dry run
###############################################################################

## Stub stow: record every call and print $FAKE_STOW_PREVIEW for simulations.
stub_stow() {
    stub_command stow <<'EOF'
#!/usr/bin/env bash
printf 'stow %s\n' "$*" >> "$EVENT_LOG"
case " $* " in
    *" -n "*) printf '%s\n' "${FAKE_STOW_PREVIEW:-}" ;;
esac
EOF
}

## Print how Stow reports a plain file standing in the way of a link. Stow 2.4.1
## is the only version in the Gentoo tree; Stow 2.3 and earlier word this
## differently, and the shared backup plan must understand both.
stow_conflict_line() {
    printf '  * cannot stow ../../../home/user/cuberhaus/dotfiles/%s over existing target %s since neither a link nor a directory and --adopt not specified\n' "$1" "$1"
}

## Run the dry-run Stow preview as if started from a terminal.
run_stow_preview() {
    in_gentoo_shell 'bootstrap_stdin_is_interactive() { return 0; }; gentoo_preview_stow' 2>&1
}

## Run the real, shared Stow step as a terminal user who declines the prompt.
run_shared_stow_step() {
    in_gentoo_shell 'bootstrap_stdin_is_interactive() { return 0; }
        bootstrap_confirm_stow() { return 1; }
        bootstrap_stow_checkout gentoo' 2>&1
}

## The conflict plan names a timestamped backup folder; blank the timestamp.
blank_backup_timestamp() {
    sed -E 's/[0-9]{4}-[0-9]{2}-[0-9]{2}_[0-9]{6}/TIMESTAMP/g'
}

test_the_dry_run_shows_stow_links_and_conflicts_without_applying_them() {
    setup_case
    stub_stow
    local output unapplied

    printf 'existing configuration\n' > "$HOME/.zshenv"
    FAKE_STOW_PREVIEW="WARNING! stowing dotfiles would cause conflicts:"$'\n'"$(stow_conflict_line .zshenv)"$'\nAll operations aborted.'
    export FAKE_STOW_PREVIEW
    output="$(run_stow_preview)" || fail 'the preview must succeed'

    assert_contains "$output" 'Stow dry run for' 'the preview must show the simulated links'
    assert_contains "$output" 'over existing target .zshenv' 'the preview must show the conflict'
    assert_contains "$output" 'Conflict backup plan:' 'the preview must show the backup plan'
    assert_contains "$output" "Would move $HOME/.zshenv" 'the preview must name the file that would be backed up'
    [ -f "$HOME/.zshenv" ] || fail 'the preview must not move the conflicting file'
    [ ! -e "$HOME/.dotfiles-backup" ] || fail 'the preview must not create a backup directory'
    [ -s "$EVENT_LOG" ] || fail 'the preview must consult stow'
    unapplied="$(grep -v -- ' -n ' "$EVENT_LOG" || true)"
    [ -z "$unapplied" ] || fail "every stow call in a preview must be a simulation:"$'\n'"$unapplied"
    teardown_case
}

test_the_stow_preview_says_why_it_is_skipped() {
    setup_case
    stub_stow
    local output

    export FAKE_STOW_PREVIEW='LINK: .zshenv => dotfiles/.zshenv'

    output="$(SKIP_STOW=true run_stow_preview)" || fail 'the preview must succeed with --no-stow'
    assert_contains "$output" '--no-stow' 'the preview must explain that linking is skipped'

    output="$(UNATTENDED=true run_stow_preview)" || fail 'the preview must succeed when unattended'
    assert_contains "$output" 'confirmation is unavailable' 'the preview must explain that unattended runs do not link'

    output="$(in_gentoo_shell 'gentoo_preview_stow' < /dev/null 2>&1)" ||
        fail 'the preview must succeed without a terminal'
    assert_contains "$output" 'confirmation is unavailable' 'the preview must explain that linking needs a terminal'

    output="$(in_gentoo_shell "bootstrap_stdin_is_interactive() { return 0; }
        bootstrap_stow_is_available() { return 1; }
        bootstrap_install_stow() { printf 'install\\n' >> \"\$EVENT_LOG\"; }
        gentoo_preview_stow" 2>&1)" || fail 'the preview must succeed without Stow'
    assert_contains "$output" 'app-admin/stow' 'the preview must say that a real run installs Stow'

    [ ! -s "$EVENT_LOG" ] || fail "a skipped preview must neither run nor install Stow: $(cat "$EVENT_LOG")"
    teardown_case
}

test_the_stow_preview_matches_what_the_real_step_shows() {
    setup_case
    stub_stow
    local shared preview

    # With changes to make, the real step shows the same two views and then asks.
    printf 'existing configuration\n' > "$HOME/.vimrc"
    FAKE_STOW_PREVIEW=$'LINK: .zshenv => dotfiles/.zshenv\n'"$(stow_conflict_line .vimrc)"
    export FAKE_STOW_PREVIEW
    shared="$(run_shared_stow_step)"
    preview="$(run_stow_preview)"
    assert_contains "$(printf '%s\n' "$shared" | tail -n 1)" 'declined' \
        'the real step must end at the declined prompt once it has shown its views'
    assert_equals "$(printf '%s\n' "$shared" | sed '$d' | blank_backup_timestamp)" \
        "$(printf '%s\n' "$preview" | blank_backup_timestamp)" \
        'the preview must show exactly what the real step shows before it asks'

    # With nothing to link, the real step stops after saying so.
    export FAKE_STOW_PREVIEW=''
    shared="$(run_shared_stow_step)"
    preview="$(run_stow_preview)"
    assert_contains "$preview" 'No Stow changes are needed.' 'the preview must say when everything is already linked'
    assert_equals "$shared" "$preview" 'the preview must report a clean tree like the real step'
    teardown_case
}

###############################################################################
# Seam: entrypoint orchestration and profile flags
###############################################################################

## Source the entrypoint and replace every stage with a recorder.
## The caller must run inside a subshell, because functions are redefined.
# The recorders are invoked indirectly, by the sourced entrypoint's gentoo_main,
# which ShellCheck cannot see.
# shellcheck disable=SC2317
record_stages() {
    source "$BOOTSTRAP_DIR/gentoo"
    record() { printf '%s\n' "$1" >> "$EVENT_LOG"; }
    bootstrap_enable_logging() { record 'logging'; }
    gentoo_detect_target() { GENTOO_INIT="${GENTOO_INIT:-openrc}"; record "detect:dry=${GENTOO_DRY_RUN}"; }
    gentoo_prepare_privileges() { record 'privileges'; }
    gentoo_stop_sudo_keepalive() { :; }
    configure_dual_boot_utc_rtc() { record 'dual-boot'; }
    gentoo_preview_stow() { record 'stow-preview'; }
    bootstrap_stow_checkout() { record "stow:$1:$SKIP_STOW"; }
    gentoo_prepare_user_environment() { record 'user-environment'; }
    gentoo_write_distro_marker() { record 'distro-marker'; }
    configure_inotify_watches() { record 'inotify'; }
    gentoo_install_packages() { record "packages:$2"; }
    sops_install() { record 'sops'; }
    gentoo_enable_services() { record "services:$1"; }
    configure_brightness_access() { record 'brightness'; }
    gentoo_configure_default_shell() { record 'default-shell'; }
    obsidian_vault_install() { record 'vault'; }
    apply_skip_worktree() { record 'skip-worktree'; }
    sudo() { record "sudo $*"; }
    info() { :; }
}

test_main_runs_the_stages_in_order() {
    setup_case
    (
        record_stages
        gentoo_main --unattended --no-stow
    ) || fail 'a normal run must succeed'
    assert_equals $'logging\ndetect:dry=false\nprivileges\nstow:gentoo:true\nuser-environment\ndistro-marker\ninotify\npackages:openrc\nsops\nservices:openrc\nbrightness\ndefault-shell\nvault\nskip-worktree' \
        "$(cat "$EVENT_LOG")" \
        'stages must run: validate target, elevate, link dotfiles, prepare, install packages, services, shell'

    : > "$EVENT_LOG"
    (
        record_stages
        gentoo_main --sync --update-world --no-stow
    ) || fail 'syncing and updating must succeed'
    assert_contains "$(cat "$EVENT_LOG")" $'sudo emerge --sync\nsudo emerge --ask=n --verbose --update --deep --newuse @world' \
        'the tree is synced before the world is updated, and neither happens by default'
    teardown_case
}

test_tree_sync_and_world_update_are_opt_in() {
    setup_case
    (
        record_stages
        gentoo_main --no-stow
    ) || fail 'a default run must succeed'
    case "$(cat "$EVENT_LOG")" in
        *'emerge'*) fail "a default run must not sync the tree or update @world: $(cat "$EVENT_LOG")" ;;
    esac
    teardown_case
}

test_dry_run_changes_nothing() {
    setup_case
    (
        record_stages
        gentoo_main --dry-run
    ) || fail 'a dry run must succeed'
    assert_equals $'detect:dry=true\nstow-preview\npackages:openrc\nservices:openrc\ndefault-shell' "$(cat "$EVENT_LOG")" \
        'a dry run may only validate the target and run the self-guarding preview stages'
    teardown_case
}

test_unsupported_options_are_refused_before_any_change() {
    setup_case
    local status=0 error_log="$CASE_DIR/error.log"

    (
        record_stages
        gentoo_main --bogus
    ) 2> "$error_log" || status=$?
    [ "$status" -ne 0 ] || fail 'an unknown option must be refused'
    [ ! -s "$EVENT_LOG" ] || fail "no stage may run for an unknown option: $(cat "$EVENT_LOG")"

    # The shared flag exists for systemd only; OpenRC keeps the hardware clock in conf.d.
    : > "$EVENT_LOG"
    status=0
    (
        record_stages
        gentoo_main --dual-boot-utc
    ) 2> "$error_log" || status=$?
    [ "$status" -ne 0 ] || fail '--dual-boot-utc must be refused on OpenRC'
    assert_contains "$(cat "$error_log")" 'systemd' 'the refusal must explain the systemd requirement'
    assert_contains "$(cat "$error_log")" 'hwclock' 'the refusal must point OpenRC users at their clock configuration'
    case "$(cat "$EVENT_LOG")" in
        *privileges*|*dual-boot*|*packages*) fail "nothing may run before --dual-boot-utc is refused: $(cat "$EVENT_LOG")" ;;
    esac

    : > "$EVENT_LOG"
    (
        record_stages
        GENTOO_INIT=systemd gentoo_main --dual-boot-utc --no-stow
    ) || fail '--dual-boot-utc must be accepted on systemd'
    assert_contains "$(cat "$EVENT_LOG")" 'dual-boot' '--dual-boot-utc must configure the clock on systemd'
    teardown_case
}

test_help_prints_usage_without_doing_anything() {
    setup_case
    local output
    output="$(
        record_stages
        gentoo_main --help
    )" || fail '--help must succeed'
    assert_contains "$output" '--dry-run' 'the usage must document the dry run'
    assert_contains "$output" 'GENTOO_INIT' 'the usage must document the init override'
    [ ! -s "$EVENT_LOG" ] || fail "--help must not run any stage: $(cat "$EVENT_LOG")"
    teardown_case
}

###############################################################################
# Seam: the real entrypoint, run as a subprocess against a simulated machine
###############################################################################

## Simulate a Gentoo machine: a fake root plus stateful stubs for every command
## the profile can run. sudo only forwards to those stubs (or to tee for one
## case-local file), so nothing here can reach the host.
stub_machine() {
    export CASE_DIR FAKE_MACHINE="$CASE_DIR/machine"
    export INOTIFY_SYSCTL_CONF="$CASE_DIR/sysctl-inotify.conf"
    export OBSIDIAN_VAULT_ROOT="$CASE_DIR/vault"
    make_target openrc
    mkdir -p "$FAKE_MACHINE/rc" "$GENTOO_SYSROOT/var/lib/portage"
    printf '%s\n' /bin/bash "$CASE_DIR/bin/zsh" > "$GENTOO_SYSROOT/etc/shells"
    printf '%s\n' /bin/bash > "$FAKE_MACHINE/login_shell"
    : > "$FAKE_MACHINE/groups"

    stub_privileges # supplies id; sudo is replaced below
    stub_foreign_package_managers

    stub_command sudo <<'EOF'
#!/usr/bin/env bash
# Forward only the commands the Gentoo profile is known to issue, and only to the
# simulated machine. Anything else is refused and recorded.
printf 'sudo %s\n' "$*" >> "$EVENT_LOG"
if [ "${1:-}" = -n ]; then shift; fi
if [ "$#" -eq 0 ] || [ "$1" = true ]; then exit 0; fi
case "$1" in
    emerge|rc-update|chsh|usermod|sysctl)
        case "$(command -v "$1")" in
            "$CASE_DIR"/bin/*) exec "$@" ;;
        esac
        ;;
    tee)
        for last_argument in "$@"; do :; done
        case "$last_argument" in
            "$CASE_DIR"/*) exec "$@" ;;
        esac
        ;;
esac
printf 'REFUSED sudo %s\n' "$*" >> "$EVENT_LOG"
exit 99
EOF
    stub_command emerge <<'EOF'
#!/usr/bin/env bash
printf 'emerge %s\n' "$*" >> "$EVENT_LOG"
if [ "${FAKE_EMERGE_FAIL:-false}" = true ]; then exit 1; fi
world="$GENTOO_SYSROOT/var/lib/portage/world"
pretend=false
atoms=()
for argument in "$@"; do
    case "$argument" in
        --pretend) pretend=true ;;
        -*|@*) ;;
        */*) atoms+=("$argument") ;;
    esac
done
if [ "$pretend" = true ]; then exit 0; fi
mkdir -p "$(dirname "$world")"
touch "$world"
for atom in "${atoms[@]}"; do
    grep -Fxq -- "$atom" "$world" || printf '%s\n' "$atom" >> "$world"
done
EOF
    stub_command rc-update <<'EOF'
#!/usr/bin/env bash
case "$1" in
    show)
        if [ -f "$FAKE_MACHINE/rc/$2" ]; then
            while IFS= read -r service; do
                printf '%20s | %s\n' "$service" "$2"
            done < "$FAKE_MACHINE/rc/$2"
        fi
        ;;
    add)
        printf 'rc-update %s\n' "$*" >> "$EVENT_LOG"
        printf '%s\n' "$2" >> "$FAKE_MACHINE/rc/$3"
        ;;
esac
EOF
    stub_command getent <<'EOF'
#!/usr/bin/env bash
case "$1" in
    passwd) printf '%s:x:1000:1000::/home/%s:%s\n' "$2" "$2" "$(cat "$FAKE_MACHINE/login_shell")" ;;
    *) exit 2 ;;
esac
EOF
    stub_command chsh <<'EOF'
#!/usr/bin/env bash
printf 'chsh %s\n' "$*" >> "$EVENT_LOG"
printf '%s\n' "$2" > "$FAKE_MACHINE/login_shell"
EOF
    stub_command usermod <<'EOF'
#!/usr/bin/env bash
printf 'usermod %s\n' "$*" >> "$EVENT_LOG"
grep -Fxq -- "$3 $4" "$FAKE_MACHINE/groups" || printf '%s %s\n' "$3" "$4" >> "$FAKE_MACHINE/groups"
EOF
    stub_command sysctl <<'EOF'
#!/usr/bin/env bash
printf 'sysctl %s\n' "$*" >> "$EVENT_LOG"
EOF
    stub_command sops <<'EOF'
#!/usr/bin/env bash
printf 'sops 3.13.2\n'
EOF
    stub_command git <<'EOF'
#!/usr/bin/env bash
printf 'git %s\n' "$*" >> "$EVENT_LOG"
if [ "$1" = clone ]; then mkdir -p "$3/.git"; fi
EOF
    stub_command make <<'EOF'
#!/usr/bin/env bash
printf 'make %s\n' "$*" >> "$EVENT_LOG"
EOF
    stub_command stow <<'EOF'
#!/usr/bin/env bash
printf 'stow %s\n' "$*" >> "$EVENT_LOG"
EOF
    stub_command curl <<'EOF'
#!/usr/bin/env bash
printf 'REFUSED curl %s\n' "$*" >> "$EVENT_LOG"
exit 97
EOF
    stub_command zsh <<'EOF'
#!/usr/bin/env bash
exit 0
EOF
}

## Run the real entrypoint in a subprocess; its combined output lands in $1.
## Reading through a pipe also waits for the bootstrap log's tee process, so the
## file is complete when this returns.
run_entrypoint() {
    local output_file="$1"
    shift
    bash "$BOOTSTRAP_DIR/gentoo" "$@" 2>&1 < /dev/null | cat > "$output_file"
}

## Describe everything a bootstrap could change: the user's home (minus the
## per-run logs), the fake root, the simulated machine, and the side files.
machine_fingerprint() {
    (
        cd "$CASE_DIR"
        paths=()
        for path in home root machine vault sysctl-inotify.conf; do
            if [ -e "$path" ]; then
                paths+=("$path")
            fi
        done
        {
            find "${paths[@]}" -path home/.local/state -prune -o -type d -print
            find "${paths[@]}" -path home/.local/state -prune -o -type f -exec cksum {} +
        } | sort
    )
}

## Fail if the event log shows a sudo call that could stop to ask for a password.
assert_only_noninteractive_sudo() {
    local interactive
    interactive="$(grep '^sudo ' "$EVENT_LOG" | grep -v '^sudo -n ' || true)"
    [ -z "$interactive" ] ||
        fail "an unattended run must only use non-interactive sudo:"$'\n'"$interactive"
}

test_dry_run_leaves_the_simulated_machine_untouched() {
    setup_case
    stub_machine
    local before expected_atoms output step

    before="$(machine_fingerprint)"
    run_entrypoint "$CASE_DIR/dry-run.out" --dry-run ||
        fail "a dry run must succeed: $(cat "$CASE_DIR/dry-run.out")"
    output="$(cat "$CASE_DIR/dry-run.out")"

    assert_equals "$before" "$(machine_fingerprint)" \
        'a dry run must not change the home directory, the system, or the machine state'
    expected_atoms="$(selected_atoms "$BOOTSTRAP_DIR/gentoo.packages" openrc)"
    assert_equals "emerge --pretend --verbose --noreplace --select $expected_atoms" "$(cat "$EVENT_LOG")" \
        'the only command a dry run may execute is an unprivileged emerge --pretend of the shipped manifest'

    for step in \
        'bootstrap_stow_checkout gentoo' \
        'gentoo_prepare_user_environment' \
        'gentoo_write_distro_marker' \
        'configure_inotify_watches' \
        'sops_install' \
        'sudo rc-update add elogind boot' \
        'sudo rc-update add dbus default' \
        'configure_brightness_access' \
        "sudo chsh -s $CASE_DIR/bin/zsh tester" \
        'obsidian_vault_install' \
        'apply_skip_worktree'; do
        assert_contains "$output" "[dry-run] would run: $step" "a dry run must describe the step: $step"
    done
    assert_contains "$output" 'Stow preview skipped: confirmation is unavailable' \
        'a dry run without a terminal must say why the Stow step is not previewed'
    assert_contains "$output" 'Dry run complete; nothing was changed.' 'a dry run must say that nothing changed'
    assert_no_foreign_package_manager
    teardown_case
}

test_a_second_run_finds_every_step_already_satisfied() {
    setup_case
    stub_machine
    local first second expected_atoms atom_count after_first events

    run_entrypoint "$CASE_DIR/first.out" --unattended ||
        fail "the first run must succeed: $(cat "$CASE_DIR/first.out")"
    first="$(cat "$CASE_DIR/first.out")"
    events="$(cat "$EVENT_LOG")"
    expected_atoms="$(selected_atoms "$BOOTSTRAP_DIR/gentoo.packages" openrc)"
    atom_count="$(wc -w <<< "$expected_atoms")"

    assert_contains "$first" 'Bootstrap complete!' 'the first run must finish'
    assert_contains "$events" "emerge --ask=n --verbose --noreplace --select $expected_atoms" \
        'the first run must merge the shipped manifest in one transaction'
    assert_contains "$events" 'rc-update add elogind boot' 'the first run must enable elogind'
    assert_contains "$events" "chsh -s $CASE_DIR/bin/zsh tester" 'the first run must switch the login shell'
    assert_equals "$expected_atoms" "$(tr '\n' ' ' < "$GENTOO_SYSROOT/var/lib/portage/world" | sed 's/ $//')" \
        'every merged package must be recorded in @world'
    case "$events" in
        *REFUSED*) fail "the profile issued a command outside the simulated machine: $events" ;;
    esac
    assert_no_foreign_package_manager
    assert_only_noninteractive_sudo
    assert_equals 'export DISTRO=gentoo' "$(tail -n 1 "$XDG_CONFIG_HOME/distro")" \
        'the first run must record the distribution like the other profiles'
    after_first="$(machine_fingerprint)"

    : > "$EVENT_LOG"
    run_entrypoint "$CASE_DIR/second.out" --unattended ||
        fail "a rerun must succeed: $(cat "$CASE_DIR/second.out")"
    second="$(cat "$CASE_DIR/second.out")"
    events="$(cat "$EVENT_LOG")"

    assert_contains "$second" "Packages: $atom_count selected, $atom_count already in @world, 0 to install." \
        'a rerun must report that every package is already installed'
    assert_contains "$second" 'nothing to install' 'a rerun must not merge anything'
    assert_contains "$second" 'elogind is already enabled' 'a rerun must report elogind as enabled'
    assert_contains "$second" 'dbus is already enabled' 'a rerun must report dbus as enabled'
    assert_contains "$second" 'zsh is already the login shell' 'a rerun must report the login shell as set'
    assert_contains "$second" 'is already written' 'a rerun must report the distro marker as written'
    assert_contains "$second" 'User environment is already prepared' 'a rerun must report the user environment as prepared'
    assert_contains "$second" 'inotify watch limit is already persisted' 'a rerun must report the inotify limit as persisted'
    assert_contains "$second" 'Obsidian vault already exists' 'a rerun must not clone the vault again'
    assert_contains "$second" 'Bootstrap complete!' 'a rerun must finish'

    # A here-string, not a pipeline: under pipefail an early-exiting grep -q would
    # make the producer fail and silently turn a match into "no match".
    if grep -Eq '^(emerge|rc-update|chsh|stow) |^sudo -n (emerge|rc-update|chsh|tee)|^git clone|^REFUSED' <<< "$events"; then
        fail "a rerun must not repeat changes that are already in place:"$'\n'"$events"
    fi
    assert_no_foreign_package_manager
    assert_only_noninteractive_sudo
    assert_equals "$after_first" "$(machine_fingerprint)" \
        'a rerun must leave the machine exactly as the first run left it'
    teardown_case
}

test_a_failed_build_stops_the_run_and_a_rerun_recovers() {
    setup_case
    stub_machine
    local failed rerun expected_atoms

    expected_atoms="$(selected_atoms "$BOOTSTRAP_DIR/gentoo.packages" openrc)"
    if FAKE_EMERGE_FAIL=true run_entrypoint "$CASE_DIR/failed.out" --unattended; then
        fail 'a failed build must fail the bootstrap'
    fi
    failed="$(cat "$CASE_DIR/failed.out")"
    assert_contains "$failed" 'emerge failed' 'the failure must be reported'
    assert_contains "$failed" 'docs/GENTOO-BOOTSTRAP.md' 'the failure must point at the recovery steps'
    case "$failed" in
        *'Bootstrap complete!'*) fail 'a failed run must not claim success' ;;
    esac
    # The run stops at the failing stage: nothing after the package install may run.
    if grep -Eq '^(rc-update|chsh|git|make) ' "$EVENT_LOG"; then
        fail "the run must stop at the failed build:"$'\n'"$(cat "$EVENT_LOG")"
    fi
    [ ! -s "$GENTOO_SYSROOT/var/lib/portage/world" ] || fail 'a failed build must not record packages in @world'

    # The operator fixes the problem and reruns: finished work is skipped, the rest completes.
    : > "$EVENT_LOG"
    run_entrypoint "$CASE_DIR/recovered.out" --unattended ||
        fail "a rerun after fixing the build must succeed: $(cat "$CASE_DIR/recovered.out")"
    rerun="$(cat "$CASE_DIR/recovered.out")"
    assert_contains "$rerun" 'is already written' 'steps finished before the failure must be reported as satisfied'
    assert_contains "$rerun" 'Bootstrap complete!' 'the rerun must finish'
    assert_contains "$(cat "$EVENT_LOG")" "emerge --ask=n --verbose --noreplace --select $expected_atoms" \
        'the rerun must merge the packages the failed run did not'
    assert_equals "$expected_atoms" "$(tr '\n' ' ' < "$GENTOO_SYSROOT/var/lib/portage/world" | sed 's/ $//')" \
        'the rerun must record every package in @world'
    assert_equals "$CASE_DIR/bin/zsh" "$(cat "$FAKE_MACHINE/login_shell")" \
        'the rerun must complete the stages after the packages'
    assert_only_noninteractive_sudo
    teardown_case
}

###############################################################################
# Seam: the shipped package manifest
###############################################################################

test_shipped_manifest_is_valid_and_keeps_init_systems_apart() {
    setup_case
    local manifest="$BOOTSTRAP_DIR/gentoo.packages" openrc systemd section sections

    [ -r "$manifest" ] || fail 'the Gentoo package manifest must ship with the profile'
    openrc="$(selected_atoms "$manifest" openrc)" || fail 'the manifest must parse for openrc'
    systemd="$(selected_atoms "$manifest" systemd)" || fail 'the manifest must parse for systemd'

    for atom in app-admin/stow app-shells/zsh dev-vcs/git; do
        assert_contains " $openrc " " $atom " "the default selection must include $atom (openrc)"
        assert_contains " $systemd " " $atom " "the default selection must include $atom (systemd)"
    done

    # Init-specific packages must never leak across, and keyword-masked ones are opt-in.
    assert_contains " $openrc " ' sys-auth/elogind ' 'OpenRC sessions need elogind'
    case " $systemd " in
        *' sys-auth/elogind '*) fail 'elogind is the OpenRC session provider and must not be selected for systemd' ;;
    esac
    case " $openrc $systemd " in
        *' x11-wm/xmonad '*) fail 'keyword-masked packages must stay out of the default selection' ;;
    esac

    # Every declared section must be selectable on at least one init system.
    sections="$(sed -n 's/^\[\([a-z0-9-]*\).*\]$/\1/p' "$manifest")"
    [ -n "$sections" ] || fail 'the manifest must declare sections'
    for section in $sections; do
        GENTOO_SECTIONS="$section" selected_atoms "$manifest" openrc >/dev/null 2>&1 ||
            GENTOO_SECTIONS="$section" selected_atoms "$manifest" systemd >/dev/null 2>&1 ||
            fail "section [$section] cannot be selected on any init system"
    done
    teardown_case
}

test_manifest_parsing_reads_sections_and_ignores_noise
test_manifest_selection_follows_init_system_and_optional_sections
test_manifest_parsing_rejects_anything_that_is_not_a_plain_atom
test_target_detection_selects_the_running_init_system
test_unsupported_targets_are_refused_with_an_explanation
test_privileges_are_checked_before_anything_is_changed
test_interactive_privileges_authenticate_up_front
test_unattended_privileges_never_prompt
test_sudo_keepalive_refreshes_credentials_until_stopped
test_install_emerges_only_packages_missing_from_world
test_install_is_idempotent_when_everything_is_in_world
test_install_dry_run_only_previews_with_pretend
test_install_failures_are_reported_with_a_recovery_path
test_openrc_services_are_enabled_once
test_systemd_needs_no_extra_services
test_dry_run_describes_mutations_without_running_them
test_user_environment_is_prepared_without_privileges
test_distro_marker_is_written_once
test_default_shell_becomes_zsh_only_when_it_safely_can
test_shipped_manifest_is_valid_and_keeps_init_systems_apart
test_stow_is_installed_through_portage
test_make_selects_the_gentoo_profile_explicitly
test_the_dry_run_shows_stow_links_and_conflicts_without_applying_them
test_the_stow_preview_says_why_it_is_skipped
test_the_stow_preview_matches_what_the_real_step_shows
test_main_runs_the_stages_in_order
test_tree_sync_and_world_update_are_opt_in
test_dry_run_changes_nothing
test_unsupported_options_are_refused_before_any_change
test_help_prints_usage_without_doing_anything
test_dry_run_leaves_the_simulated_machine_untouched
test_a_second_run_finds_every_step_already_satisfied
test_a_failed_build_stops_the_run_and_a_rerun_recovers

printf 'Gentoo bootstrap tests passed.\n'
