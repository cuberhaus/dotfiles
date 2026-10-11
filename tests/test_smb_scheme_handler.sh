#!/usr/bin/env bash
# Hermetic tests for smb_scheme_handler_install and smb_scheme_handler_uninstall in
# bootstrap/base_functions: the hidden desktop entry that lets a browser click on an smb:// link
# open GNOME Files, the registration GIO needs to list it, the default, that a second run changes
# nothing, that earlier files are kept as backups, and how the profiles and the uninstall
# functions use them.  Everything happens inside a temporary home folder with a stub nautilus, so
# the real ~/.local/share/applications and ~/.config/mimeapps.list are never read or written.
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

# A sandbox home with every XDG folder pinned: one inherited from the real session would send
# the helper's writes, or xdg-mime's, to the real home.  The stub nautilus records how it was
# started instead of opening a window.
new_case() {
    CASE="$SCRATCH/$1"
    mkdir -p "$CASE/home" "$CASE/bin"
    export HOME="$CASE/home"
    export XDG_CONFIG_HOME="$CASE/home/.config"
    export XDG_DATA_HOME="$CASE/home/.local/share"
    unset XDG_CONFIG_DIRS
    cat > "$CASE/bin/nautilus" <<EOF
#!/usr/bin/env bash
printf '%s|%s\n' "\${GIO_USE_VFS-unset}" "\$1" > "$CASE/launched"
EOF
    chmod +x "$CASE/bin/nautilus"
    export PATH="$CASE/bin:$ORIGINAL_PATH"
    APPS_DIR="$XDG_DATA_HOME/applications"
    ENTRY="$APPS_DIR/smb-nautilus.desktop"
    MIME_CONFIG="$XDG_CONFIG_HOME/mimeapps.list"
}

# The registration and the default are the part the desktop decides, so they are checked with
# the real tools; a machine without them skips those cases instead of faking the answer.
need_desktop_tools() {
    if command -v xdg-mime >/dev/null 2>&1 && command -v update-desktop-database >/dev/null 2>&1; then
        return 0
    fi
    printf 'skip - xdg-mime or update-desktop-database is not installed\n'
    return 1
}

install_quietly() {
    smb_scheme_handler_install >/dev/null || fail 'the install failed'
}

test_the_entry_names_the_scheme() {
    new_case scheme
    need_desktop_tools || return 0

    install_quietly

    [ -f "$ENTRY" ] || fail "no entry written to $ENTRY"
    grep -Fxq 'Type=Application' "$ENTRY" || fail 'the entry must be an application'
    grep -Fxq 'MimeType=x-scheme-handler/smb;' "$ENTRY" ||
        fail 'the entry must name the smb scheme: GNOME lists only applications that do'
    grep -Fxq 'NoDisplay=true' "$ENTRY" || fail 'the entry is a handler, not a menu item'
}

test_the_entry_clears_gio_use_vfs_for_the_files_it_starts() {
    new_case environment
    need_desktop_tools || return 0
    local exec_line command

    install_quietly
    exec_line="$(sed -n 's/^Exec=//p' "$ENTRY")"
    [ -n "$exec_line" ] || fail 'the entry has no Exec line'
    command="${exec_line//%U/smb://nas/shared}"

    # A browser starts its handlers with GIO_USE_VFS=local, which makes GIO ignore GVFS, so
    # Nautilus refuses smb locations.  Run the Exec line the way the browser would.
    GIO_USE_VFS=local bash -c "$command" || fail "the Exec line failed: $exec_line"

    assert_equals 'unset|smb://nas/shared' "$(cat "$CASE/launched")" \
        'GIO_USE_VFS and the address Nautilus was started with'
}

test_the_entry_is_a_valid_desktop_file() {
    new_case validate
    need_desktop_tools || return 0
    command -v desktop-file-validate >/dev/null 2>&1 || {
        printf 'skip - desktop-file-validate is not installed\n'
        return 0
    }

    install_quietly

    desktop-file-validate "$ENTRY" || fail 'desktop-file-validate rejected the entry'
}

test_the_scheme_is_registered_and_the_entry_is_the_default() {
    new_case registered
    need_desktop_tools || return 0

    install_quietly

    # The cache is what GIO reads to list the applications for a scheme; without it the chooser
    # says "No Apps Available" even when a default is set.
    grep -Fxq 'x-scheme-handler/smb=smb-nautilus.desktop;' "$APPS_DIR/mimeinfo.cache" ||
        fail 'the desktop database does not list the entry for the smb scheme'
    assert_equals 'smb-nautilus.desktop' "$(xdg-mime query default x-scheme-handler/smb)" \
        'the default application for smb links'
    # xdg-mime already answers with the only registered handler, so the answer above proves
    # nothing about the setting: it must be written, or another app that registers smb later
    # could take the links over.
    grep -Fxq 'x-scheme-handler/smb=smb-nautilus.desktop' "$MIME_CONFIG" ||
        fail 'the default must be written to mimeapps.list, not left to registration order'
}

test_it_replaces_a_default_that_cannot_open_smb_and_keeps_the_other_lines() {
    new_case replace
    need_desktop_tools || return 0
    local previous='[Default Applications]
text/html=google-chrome.desktop
x-scheme-handler/smb=org.gnome.Nautilus.desktop'

    mkdir -p "$XDG_CONFIG_HOME"
    printf '%s\n' "$previous" > "$MIME_CONFIG"

    install_quietly

    assert_equals 'smb-nautilus.desktop' "$(xdg-mime query default x-scheme-handler/smb)" \
        'the default application for smb links'
    grep -Fxq 'text/html=google-chrome.desktop' "$MIME_CONFIG" ||
        fail 'the other default applications must survive'
    assert_equals "$previous" "$(cat "$MIME_CONFIG.bak")" 'the backup of the previous mimeapps.list'
}

test_a_second_run_changes_nothing() {
    new_case idempotent
    need_desktop_tools || return 0
    local entry_before config_before cache_before output

    install_quietly
    entry_before="$(cat "$ENTRY")"
    config_before="$(cat "$MIME_CONFIG")"
    cache_before="$(cat "$APPS_DIR/mimeinfo.cache")"
    rm -f "$MIME_CONFIG.bak"

    output="$(smb_scheme_handler_install)" || fail 'the second install failed'

    assert_equals "$entry_before" "$(cat "$ENTRY")" 'the entry after a second run'
    assert_equals "$config_before" "$(cat "$MIME_CONFIG")" 'mimeapps.list after a second run'
    assert_equals "$cache_before" "$(cat "$APPS_DIR/mimeinfo.cache")" 'the desktop database after a second run'
    assert_contains "$output" 'already' 'the message of a run with nothing to do'
    [ ! -e "$ENTRY.bak" ] || fail 'an unchanged entry must not be backed up'
    [ ! -e "$MIME_CONFIG.bak" ] || fail 'an unchanged default must not back up mimeapps.list'
}

test_an_entry_it_did_not_write_is_backed_up_before_replacement() {
    new_case backup
    need_desktop_tools || return 0
    local previous='[Desktop Entry]
Type=Application
Name=Mine
Exec=/usr/bin/true'

    mkdir -p "$APPS_DIR"
    printf '%s\n' "$previous" > "$ENTRY"

    install_quietly

    assert_equals "$previous" "$(cat "$ENTRY.bak")" 'the backup of the previous entry'
    grep -Fxq 'MimeType=x-scheme-handler/smb;' "$ENTRY" || fail 'the entry was not replaced'
}

test_xdg_folders_fall_back_to_the_home_folder() {
    new_case fallback
    need_desktop_tools || return 0
    unset XDG_CONFIG_HOME XDG_DATA_HOME

    install_quietly

    [ -f "$HOME/.local/share/applications/smb-nautilus.desktop" ] ||
        fail 'without XDG_DATA_HOME the entry belongs in ~/.local/share/applications'
    [ -f "$HOME/.config/mimeapps.list" ] ||
        fail 'without XDG_CONFIG_HOME the default belongs in ~/.config/mimeapps.list'
}

test_it_skips_a_machine_without_nautilus() {
    new_case no-nautilus
    local empty="$CASE/empty-path" status=0
    mkdir -p "$empty"

    # Only an empty PATH: the helper must decide before it needs any tool.
    PATH="$empty" smb_scheme_handler_install >/dev/null 2>&1 || status=$?

    assert_equals 0 "$status" 'the exit status without Nautilus'
    [ ! -e "$ENTRY" ] || fail 'nothing may be written without Nautilus'
    [ ! -e "$MIME_CONFIG" ] || fail 'no default may be set without Nautilus'
}

test_it_fails_loudly_without_the_desktop_database_tool() {
    new_case no-database
    local bin="$CASE/bin" tool status=0 output

    # Nautilus is there but update-desktop-database is not: the entry would exist without being
    # registered, which is the half-fix that ends in "No Apps Available".
    for tool in mkdir cat cp rm grep sed; do
        ln -sf "$(command -v "$tool")" "$bin/$tool"
    done

    output="$(PATH="$bin" smb_scheme_handler_install 2>&1)" || status=$?

    [ "$status" -ne 0 ] || fail 'a missing desktop database tool must fail'
    assert_contains "$output" 'update-desktop-database' 'the message must name the missing tool'
}

test_uninstall_removes_the_entry_and_only_its_own_default() {
    new_case uninstall
    need_desktop_tools || return 0

    install_quietly
    printf 'text/html=google-chrome.desktop\n' >> "$MIME_CONFIG"

    smb_scheme_handler_uninstall >/dev/null || fail 'the uninstall failed'

    [ ! -e "$ENTRY" ] || fail 'the entry must be removed'
    if grep -Fq 'smb-nautilus.desktop' "$MIME_CONFIG" "$APPS_DIR/mimeinfo.cache"; then
        fail 'no default or registration of the entry may remain'
    fi
    grep -Fxq 'text/html=google-chrome.desktop' "$MIME_CONFIG" ||
        fail 'the other default applications must survive'
}

test_uninstall_leaves_a_default_that_is_not_its_own() {
    new_case uninstall-foreign
    mkdir -p "$XDG_CONFIG_HOME"
    printf '[Default Applications]\nx-scheme-handler/smb=other.desktop\n' > "$MIME_CONFIG"

    smb_scheme_handler_uninstall >/dev/null || fail 'the uninstall failed'

    grep -Fxq 'x-scheme-handler/smb=other.desktop' "$MIME_CONFIG" ||
        fail 'a default the helper did not set must stay'
}

test_uninstall_with_nothing_installed_succeeds() {
    new_case uninstall-empty

    smb_scheme_handler_uninstall >/dev/null || fail 'an uninstall with nothing to do must succeed'
}

test_the_profiles_with_nautilus_use_it_and_undo_it() {
    local entry_scripts=("$BOOTSTRAP_DIR/ubuntu" "$BOOTSTRAP_DIR/work")
    local uninstall_functions=("$BOOTSTRAP_DIR/uninstall_ubuntu_functions" "$BOOTSTRAP_DIR/uninstall_work_functions")
    local file

    for file in "${entry_scripts[@]}"; do
        grep -Eq '^[[:space:]]*smb_scheme_handler_install \|\|$' "$file" ||
            fail "$file must run smb_scheme_handler_install without aborting on failure"
    done
    for file in "${uninstall_functions[@]}"; do
        grep -Eq '^[[:space:]]*smb_scheme_handler_uninstall( \|\| true)?$' "$file" ||
            fail "$file must undo what smb_scheme_handler_install wrote"
    done
}

for test_name in \
    test_the_entry_names_the_scheme \
    test_the_entry_clears_gio_use_vfs_for_the_files_it_starts \
    test_the_entry_is_a_valid_desktop_file \
    test_the_scheme_is_registered_and_the_entry_is_the_default \
    test_it_replaces_a_default_that_cannot_open_smb_and_keeps_the_other_lines \
    test_a_second_run_changes_nothing \
    test_an_entry_it_did_not_write_is_backed_up_before_replacement \
    test_xdg_folders_fall_back_to_the_home_folder \
    test_it_skips_a_machine_without_nautilus \
    test_it_fails_loudly_without_the_desktop_database_tool \
    test_uninstall_removes_the_entry_and_only_its_own_default \
    test_uninstall_leaves_a_default_that_is_not_its_own \
    test_uninstall_with_nothing_installed_succeeds \
    test_the_profiles_with_nautilus_use_it_and_undo_it; do
    # A subshell keeps each test's HOME, XDG folders and PATH to itself.
    ( "$test_name" ) || {
        printf 'not ok - %s\n' "$test_name" >&2
        exit 1
    }
    printf 'ok - %s\n' "$test_name"
done

printf 'SMB scheme handler tests passed.\n'
