#!/usr/bin/env bash
# Hermetic tests for caffeine_indicator_autostart_install in bootstrap/base_functions: the
# autostart entry it writes, that a second run changes nothing, that an entry it did not write
# is kept as a backup, the failure path, and how the profiles and the uninstall functions use
# it.  Everything happens inside a temporary home folder, so the real ~/.config/autostart is
# never read or written.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BOOTSTRAP_DIR="$REPO_ROOT/.local/scripts/bootstrap"
SCRATCH="$(mktemp -d)"
RM_COMMAND="$(command -v rm)"
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

# A sandbox home whose XDG_CONFIG_HOME is pinned too: one inherited from the real session
# would send the helper's writes to the real ~/.config.
new_case() {
    CASE="$SCRATCH/$1"
    mkdir -p "$CASE/home"
    export HOME="$CASE/home"
    export XDG_CONFIG_HOME="$CASE/home/.config"
    ENTRY="$XDG_CONFIG_HOME/autostart/caffeine-indicator.desktop"
}

test_the_entry_starts_the_indicator_not_the_daemon() {
    new_case entry

    caffeine_indicator_autostart_install >/dev/null || fail 'the install failed'

    [ -f "$ENTRY" ] || fail "no entry written to $ENTRY"
    grep -Fxq 'Type=Application' "$ENTRY" || fail 'the entry must be an application'
    grep -Fxq 'Exec=/usr/bin/caffeine-indicator' "$ENTRY" ||
        fail 'the entry must start the indicator; the package already autostarts the daemon'
    grep -Fxq 'X-GNOME-Autostart-enabled=true' "$ENTRY" || fail 'the entry must be enabled'
    if grep -Eq '^(Hidden=true|NoDisplay=true|X-GNOME-Autostart-enabled=false)$' "$ENTRY"; then
        fail 'the entry must not disable itself'
    fi
}

test_the_entry_is_skipped_when_the_package_is_gone() {
    new_case try-exec

    caffeine_indicator_autostart_install >/dev/null || fail 'the install failed'

    grep -Fxq 'TryExec=/usr/bin/caffeine-indicator' "$ENTRY" ||
        fail 'TryExec lets the session skip the entry after the package is removed'
}

test_the_entry_is_a_valid_desktop_file() {
    new_case validate
    command -v desktop-file-validate >/dev/null 2>&1 || {
        printf 'skip - desktop-file-validate is not installed\n'
        return 0
    }

    caffeine_indicator_autostart_install >/dev/null || fail 'the install failed'

    desktop-file-validate "$ENTRY" || fail 'desktop-file-validate rejected the entry'
}

test_a_second_run_changes_nothing() {
    new_case idempotent
    local first second output

    caffeine_indicator_autostart_install >/dev/null || fail 'the first install failed'
    first="$(cat "$ENTRY")"
    output="$(caffeine_indicator_autostart_install)" || fail 'the second install failed'
    second="$(cat "$ENTRY")"

    assert_equals "$first" "$second" 'the entry after a second run'
    assert_contains "$output" 'already starts at login' 'the message of a run with nothing to do'
    [ ! -e "$ENTRY.bak" ] || fail 'an unchanged entry must not be backed up'
}

test_an_entry_it_did_not_write_is_backed_up_before_replacement() {
    new_case backup
    local previous='[Desktop Entry]
Type=Application
Name=Mine
Exec=/usr/bin/true'

    mkdir -p "$XDG_CONFIG_HOME/autostart"
    printf '%s\n' "$previous" > "$ENTRY"

    caffeine_indicator_autostart_install >/dev/null || fail 'the install failed'

    assert_equals "$previous" "$(cat "$ENTRY.bak")" 'the backup of the previous entry'
    grep -Fxq 'Exec=/usr/bin/caffeine-indicator' "$ENTRY" || fail 'the entry was not replaced'
}

test_xdg_config_home_falls_back_to_the_home_folder() {
    new_case fallback
    unset XDG_CONFIG_HOME

    caffeine_indicator_autostart_install >/dev/null || fail 'the install failed'

    [ -f "$HOME/.config/autostart/caffeine-indicator.desktop" ] ||
        fail 'without XDG_CONFIG_HOME the entry belongs in ~/.config/autostart'
}

test_it_fails_when_the_autostart_folder_cannot_be_made() {
    new_case blocked
    local status=0

    # A plain file where the folder must go.
    mkdir -p "$XDG_CONFIG_HOME"
    : > "$XDG_CONFIG_HOME/autostart"

    caffeine_indicator_autostart_install >/dev/null 2>&1 || status=$?

    [ "$status" -ne 0 ] || fail 'a blocked autostart folder must fail'
}

test_the_profiles_with_the_apt_package_use_it_and_undo_it() {
    local entry_scripts=("$BOOTSTRAP_DIR/ubuntu" "$BOOTSTRAP_DIR/work")
    local uninstall_functions=("$BOOTSTRAP_DIR/uninstall_ubuntu_functions" "$BOOTSTRAP_DIR/uninstall_work_functions")
    local file

    for file in "${entry_scripts[@]}"; do
        grep -Eq '^[[:space:]]*caffeine_indicator_autostart_install \|\|$' "$file" ||
            fail "$file must run caffeine_indicator_autostart_install without aborting on failure"
    done
    for file in "${uninstall_functions[@]}"; do
        grep -Fq 'autostart/caffeine-indicator.desktop' "$file" ||
            fail "$file must remove the autostart entry it uninstalls the package for"
    done
}

for test_name in \
    test_the_entry_starts_the_indicator_not_the_daemon \
    test_the_entry_is_skipped_when_the_package_is_gone \
    test_the_entry_is_a_valid_desktop_file \
    test_a_second_run_changes_nothing \
    test_an_entry_it_did_not_write_is_backed_up_before_replacement \
    test_xdg_config_home_falls_back_to_the_home_folder \
    test_it_fails_when_the_autostart_folder_cannot_be_made \
    test_the_profiles_with_the_apt_package_use_it_and_undo_it; do
    # A subshell keeps each test's HOME and XDG_CONFIG_HOME to itself.
    ( "$test_name" ) || {
        printf 'not ok - %s\n' "$test_name" >&2
        exit 1
    }
    printf 'ok - %s\n' "$test_name"
done

printf 'Caffeine autostart tests passed.\n'
