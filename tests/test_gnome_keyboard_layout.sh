#!/usr/bin/env bash
# Hermetic tests for gnome_keyboard_layout_install and gnome_keyboard_layout_uninstall
# in bootstrap/base_functions, plus the setxkbmap calls in shell startup files.
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
    SOURCES_FILE="$CASE/sources"
    OPTIONS_FILE="$CASE/options"
    CALLS_FILE="$CASE/calls"
    SETXKBMAP_FILE="$CASE/setxkbmap"
    printf "[('xkb', 'gb')]\n" > "$SOURCES_FILE"
    printf '@as []\n' > "$OPTIONS_FILE"
    : > "$CALLS_FILE"
    : > "$SETXKBMAP_FILE"
    cat > "$CASE/bin/gsettings" <<EOF
#!/usr/bin/env bash
printf '%s\n' "\$*" >> "$CALLS_FILE"
if [ "\${FAKE_GSETTINGS_NO_SCHEMA:-false}" = true ]; then
    exit 1
fi
[ "\$2" = "org.gnome.desktop.input-sources" ] || exit 1
case "\$1:\$3" in
    get:sources)
        cat "$SOURCES_FILE"
        ;;
    get:xkb-options)
        cat "$OPTIONS_FILE"
        ;;
    set:sources)
        if [ "\${FAKE_GSETTINGS_DROP_WRITE:-false}" != true ]; then
            printf '%s\n' "\$4" > "$SOURCES_FILE"
        fi
        ;;
    set:xkb-options)
        if [ "\${FAKE_GSETTINGS_DROP_WRITE:-false}" != true ]; then
            printf '%s\n' "\$4" > "$OPTIONS_FILE"
        fi
        ;;
    *)
        exit 1
        ;;
esac
EOF
    cat > "$CASE/bin/setxkbmap" <<EOF
#!/usr/bin/env bash
printf '%s\n' "\$*" >> "$SETXKBMAP_FILE"
EOF
    chmod +x "$CASE/bin/gsettings" "$CASE/bin/setxkbmap"
    export PATH="$CASE/bin:$ORIGINAL_PATH"
}

test_sets_spanish_layout_and_swapescape_and_runs_setxkbmap() {
    new_case default

    DISPLAY=:0 gnome_keyboard_layout_install >/dev/null || fail 'the install failed'

    assert_equals "[('xkb', 'es'), ('xkb', 'gb')]" "$(cat "$SOURCES_FILE")" \
        'Spanish must be prepended as primary input source'
    assert_equals "['caps:swapescape']" "$(cat "$OPTIONS_FILE")" \
        'caps:swapescape must be added to xkb-options'
    assert_equals '-layout es -option caps:swapescape' "$(cat "$SETXKBMAP_FILE")" \
        'setxkbmap must be called with -layout es -option caps:swapescape when DISPLAY is set'
}

test_populates_empty_sources_and_moves_existing_es_first() {
    new_case empty-and-reorder
    printf '@a(ss) []\n' > "$SOURCES_FILE"

    DISPLAY='' gnome_keyboard_layout_install >/dev/null || fail 'install on empty sources failed'
    assert_equals "[('xkb', 'es')]" "$(cat "$SOURCES_FILE")" \
        'sources when starting from @a(ss) []'

    printf "[('xkb', 'us'), ('xkb', 'es')]\n" > "$SOURCES_FILE"
    DISPLAY='' gnome_keyboard_layout_install >/dev/null || fail 'install when reordering es failed'
    assert_equals "[('xkb', 'es'), ('xkb', 'us')]" "$(cat "$SOURCES_FILE")" \
        'Spanish moved to primary position without duplicating it'
}

test_preserves_custom_xkb_options_and_is_idempotent() {
    new_case custom-and-idempotent
    local output
    printf "[('xkb', 'es')]\n" > "$SOURCES_FILE"
    printf "['compose:ralt']\n" > "$OPTIONS_FILE"

    DISPLAY='' gnome_keyboard_layout_install >/dev/null || fail 'first install failed'
    assert_equals "['compose:ralt', 'caps:swapescape']" "$(cat "$OPTIONS_FILE")" \
        'existing xkb-options preserved'

    : > "$CALLS_FILE"
    output="$(DISPLAY='' gnome_keyboard_layout_install)" || fail 'second install failed'
    assert_contains "$output" 'already set' 'message on second run'
    if grep -Fq 'set org.gnome.desktop.input-sources' "$CALLS_FILE"; then
        fail 'second run must not call gsettings set when already configured'
    fi
}

test_fails_when_gsettings_does_not_persist_write() {
    new_case memory-backend
    local status=0 output

    output="$(DISPLAY='' FAKE_GSETTINGS_DROP_WRITE=true gnome_keyboard_layout_install 2>&1)" || status=$?

    [ "$status" -ne 0 ] || fail 'must fail when gsettings set does not persist'
    assert_contains "$output" 'did not persist' 'warning when gsettings write is dropped'
}

test_rejects_unexpected_gsettings_format() {
    new_case malformed
    local status=0
    printf 'not-an-array\n' > "$SOURCES_FILE"

    DISPLAY='' gnome_keyboard_layout_install >/dev/null 2>&1 || status=$?
    [ "$status" -ne 0 ] || fail 'must fail on unexpected gsettings format'
    assert_equals 'not-an-array' "$(cat "$SOURCES_FILE")" \
        'unexpected format must not be overwritten'
}

test_skips_gsettings_without_schema_and_still_updates_x11() {
    new_case no-schema
    local status=0

    DISPLAY=:0 FAKE_GSETTINGS_NO_SCHEMA=true gnome_keyboard_layout_install >/dev/null 2>&1 || status=$?
    assert_equals 0 "$status" 'exit status without GNOME schema'
    assert_equals '-layout es -option caps:swapescape' "$(cat "$SETXKBMAP_FILE")" \
        'setxkbmap still runs when DISPLAY is set'
}

test_uninstall_removes_only_swapescape_and_keeps_spanish_layout() {
    new_case uninstall
    printf "[('xkb', 'es')]\n" > "$SOURCES_FILE"
    printf "['compose:ralt', 'caps:swapescape']\n" > "$OPTIONS_FILE"

    gnome_keyboard_layout_uninstall >/dev/null || fail 'uninstall with other options failed'
    assert_equals "['compose:ralt']" "$(cat "$OPTIONS_FILE")" \
        'other xkb-options preserved after uninstall'
    assert_equals "[('xkb', 'es')]" "$(cat "$SOURCES_FILE")" \
        'Spanish input source must remain untouched after uninstall'

    printf "['caps:swapescape']\n" > "$OPTIONS_FILE"
    gnome_keyboard_layout_uninstall >/dev/null || fail 'uninstall to empty failed'
    assert_equals '@as []' "$(cat "$OPTIONS_FILE")" \
        'xkb-options restored to @as [] when empty'
}

test_profiles_uninstallers_and_shell_files_keep_spanish_layout() {
    local entry_scripts=("$BOOTSTRAP_DIR/ubuntu" "$BOOTSTRAP_DIR/work")
    local uninstall_functions=("$BOOTSTRAP_DIR/uninstall_ubuntu_functions" "$BOOTSTRAP_DIR/uninstall_work_functions")
    local shell_files=("$REPO_ROOT/.config/zsh/.zprofile" "$REPO_ROOT/.xprofile" "$REPO_ROOT/.zshenv")
    local file

    for file in "${entry_scripts[@]}"; do
        grep -Eq '^[[:space:]]*gnome_keyboard_layout_install \|\|$' "$file" ||
            fail "$file must run gnome_keyboard_layout_install without aborting on failure"
    done
    for file in "${uninstall_functions[@]}"; do
        grep -Eq '^[[:space:]]*gnome_keyboard_layout_uninstall( \|\| true)?$' "$file" ||
            fail "$file must undo what gnome_keyboard_layout_install set"
    done
    grep -Fq 'gnome_keyboard_layout_install' "$REPO_ROOT/.local/scripts/repair-installation" ||
        fail 'repair-installation keyboard step must run gnome_keyboard_layout_install'
    for file in "${shell_files[@]}"; do
        grep -Fq 'setxkbmap -layout es -option caps:swapescape' "$file" ||
            fail "$file must pass both -layout es and -option caps:swapescape to setxkbmap"
        if grep -Eq '^[[:space:]]*setxkbmap[[:space:]]+-option' "$file"; then
            fail "$file must not call setxkbmap -option without -layout es (resets XWayland to us)"
        fi
    done
}

for test_name in \
    test_sets_spanish_layout_and_swapescape_and_runs_setxkbmap \
    test_populates_empty_sources_and_moves_existing_es_first \
    test_preserves_custom_xkb_options_and_is_idempotent \
    test_fails_when_gsettings_does_not_persist_write \
    test_rejects_unexpected_gsettings_format \
    test_skips_gsettings_without_schema_and_still_updates_x11 \
    test_uninstall_removes_only_swapescape_and_keeps_spanish_layout \
    test_profiles_uninstallers_and_shell_files_keep_spanish_layout; do
    ( "$test_name" ) || {
        printf 'not ok - %s\n' "$test_name" >&2
        exit 1
    }
    printf 'ok - %s\n' "$test_name"
done

printf 'GNOME and XWayland keyboard layout tests passed.\n'
