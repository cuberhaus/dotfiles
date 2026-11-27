#!/usr/bin/env bash
# Hermetic tests for gnome_automatic_timezone_install and gnome_automatic_timezone_uninstall
# in bootstrap/base_functions: enabling org.gnome.system.location enabled and
# org.gnome.desktop.datetime automatic-timezone in order, partial-state updates,
# idempotence, malformed-value rejection, non-persistent gsettings failure detection,
# skipping machines without gsettings or either GNOME schema, uninstalling, and how the
# Ubuntu/work profiles and uninstall functions wire them up.
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
    LOCATION_FILE="$CASE/location"
    DATETIME_FILE="$CASE/datetime"
    CALLS_FILE="$CASE/calls"
    printf 'false\n' > "$LOCATION_FILE"
    printf 'false\n' > "$DATETIME_FILE"
    : > "$CALLS_FILE"
    cat > "$CASE/bin/gsettings" <<EOF
#!/usr/bin/env bash
printf '%s\n' "\$*" >> "$CALLS_FILE"
if [ "\${FAKE_GSETTINGS_NO_SCHEMA:-false}" = true ]; then
    exit 1
fi
if [ "\${FAKE_GSETTINGS_MISSING_SCHEMA:-}" = "\$2" ]; then
    exit 1
fi
case "\$1:\$2:\$3" in
    get:org.gnome.system.location:enabled)
        cat "$LOCATION_FILE"
        ;;
    set:org.gnome.system.location:enabled)
        if [ "\${FAKE_GSETTINGS_DROP_WRITE:-false}" != true ] &&
            [ "\${FAKE_GSETTINGS_DROP_SCHEMA:-}" != "\$2" ]; then
            printf '%s\n' "\$4" > "$LOCATION_FILE"
        fi
        ;;
    get:org.gnome.desktop.datetime:automatic-timezone)
        cat "$DATETIME_FILE"
        ;;
    set:org.gnome.desktop.datetime:automatic-timezone)
        if [ "\${FAKE_GSETTINGS_DROP_WRITE:-false}" != true ] &&
            [ "\${FAKE_GSETTINGS_DROP_SCHEMA:-}" != "\$2" ]; then
            printf '%s\n' "\$4" > "$DATETIME_FILE"
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

test_enables_location_then_automatic_timezone() {
    new_case default
    local set_calls

    gnome_automatic_timezone_install >/dev/null || fail 'the install failed'

    assert_equals 'true' "$(cat "$LOCATION_FILE")" 'location enabled after install'
    assert_equals 'true' "$(cat "$DATETIME_FILE")" 'automatic-timezone after install'

    set_calls="$(grep '^set ' "$CALLS_FILE")"
    assert_equals "$(printf '%s\n%s' \
        'set org.gnome.system.location enabled true' \
        'set org.gnome.desktop.datetime automatic-timezone true')" \
        "$set_calls" 'gsettings set order (location before automatic-timezone)'
}

test_updates_only_false_key_when_partially_enabled() {
    new_case partial-location-on
    printf 'true\n' > "$LOCATION_FILE"
    printf 'false\n' > "$DATETIME_FILE"

    gnome_automatic_timezone_install >/dev/null || fail 'install with location already on failed'
    assert_equals 'true' "$(cat "$LOCATION_FILE")" 'location stays true'
    assert_equals 'true' "$(cat "$DATETIME_FILE")" 'automatic-timezone becomes true'
    if grep -Fq 'set org.gnome.system.location' "$CALLS_FILE"; then
        fail 'must not rewrite org.gnome.system.location when already true'
    fi

    new_case partial-datetime-on
    printf 'false\n' > "$LOCATION_FILE"
    printf 'true\n' > "$DATETIME_FILE"

    gnome_automatic_timezone_install >/dev/null || fail 'install with automatic-timezone already on failed'
    assert_equals 'true' "$(cat "$LOCATION_FILE")" 'location becomes true'
    assert_equals 'true' "$(cat "$DATETIME_FILE")" 'automatic-timezone stays true'
    if grep -Fq 'set org.gnome.desktop.datetime' "$CALLS_FILE"; then
        fail 'must not rewrite org.gnome.desktop.datetime when already true'
    fi
}

test_second_run_is_idempotent() {
    new_case idempotent
    local output

    gnome_automatic_timezone_install >/dev/null || fail 'first install failed'
    : > "$CALLS_FILE"
    output="$(gnome_automatic_timezone_install)" || fail 'second install failed'

    assert_contains "$output" 'already enabled' 'message on second run'
    if grep -Eq '^set ' "$CALLS_FILE"; then
        fail 'second run must not call gsettings set when already enabled'
    fi
}

test_rejects_unexpected_gsettings_values() {
    new_case malformed-location
    local status=0
    printf 'maybe\n' > "$LOCATION_FILE"

    gnome_automatic_timezone_install >/dev/null 2>&1 || status=$?
    [ "$status" -ne 0 ] || fail 'must fail on unexpected location value'
    assert_equals 'maybe' "$(cat "$LOCATION_FILE")" 'unexpected location value must not be overwritten'
    assert_equals 'false' "$(cat "$DATETIME_FILE")" 'datetime must not change when location is malformed'

    new_case malformed-datetime
    status=0
    printf 'not-a-bool\n' > "$DATETIME_FILE"

    gnome_automatic_timezone_install >/dev/null 2>&1 || status=$?
    [ "$status" -ne 0 ] || fail 'must fail on unexpected automatic-timezone value'
    assert_equals 'false' "$(cat "$LOCATION_FILE")" 'location must not change when datetime is malformed'
    assert_equals 'not-a-bool' "$(cat "$DATETIME_FILE")" 'unexpected datetime value must not be overwritten'
}

test_fails_when_gsettings_does_not_persist_write() {
    new_case memory-backend-location
    local status=0 output

    output="$(FAKE_GSETTINGS_DROP_WRITE=true gnome_automatic_timezone_install 2>&1)" || status=$?
    [ "$status" -ne 0 ] || fail 'must fail when location gsettings write does not persist'
    assert_contains "$output" 'did not persist' 'warning when location write is dropped'

    new_case memory-backend-datetime
    status=0
    output="$(FAKE_GSETTINGS_DROP_SCHEMA=org.gnome.desktop.datetime gnome_automatic_timezone_install 2>&1)" || status=$?
    [ "$status" -ne 0 ] || fail 'must fail when automatic-timezone gsettings write does not persist'
    assert_contains "$output" 'did not persist' 'warning when automatic-timezone write is dropped'
}

test_skips_when_gsettings_or_either_schema_is_missing() {
    new_case missing
    local empty="$CASE/empty" status=0
    mkdir -p "$empty"

    PATH="$empty" gnome_automatic_timezone_install >/dev/null 2>&1 || status=$?
    assert_equals 0 "$status" 'exit status without gsettings'

    FAKE_GSETTINGS_NO_SCHEMA=true gnome_automatic_timezone_install >/dev/null 2>&1 || status=$?
    assert_equals 0 "$status" 'exit status without GNOME schemas'

    FAKE_GSETTINGS_MISSING_SCHEMA=org.gnome.desktop.datetime gnome_automatic_timezone_install >/dev/null 2>&1 || status=$?
    assert_equals 0 "$status" 'exit status when datetime schema is missing'
    assert_equals 'false' "$(cat "$LOCATION_FILE")" 'location must not change when datetime schema is missing'
}

test_uninstall_disables_only_automatic_timezone() {
    new_case uninstall

    gnome_automatic_timezone_install >/dev/null || fail 'install failed'
    gnome_automatic_timezone_uninstall >/dev/null || fail 'uninstall failed'

    assert_equals 'false' "$(cat "$DATETIME_FILE")" 'automatic-timezone disabled after uninstall'
    assert_equals 'true' "$(cat "$LOCATION_FILE")" 'location left untouched by uninstall'

    : > "$CALLS_FILE"
    gnome_automatic_timezone_uninstall >/dev/null || fail 'second uninstall failed'
    if grep -Eq '^set ' "$CALLS_FILE"; then
        fail 'second uninstall must not call gsettings set when already false'
    fi

    FAKE_GSETTINGS_NO_SCHEMA=true gnome_automatic_timezone_uninstall >/dev/null ||
        fail 'uninstall without schema must succeed'
}

test_profiles_and_uninstallers_use_it() {
    local entry_scripts=("$BOOTSTRAP_DIR/ubuntu" "$BOOTSTRAP_DIR/work")
    local uninstall_functions=("$BOOTSTRAP_DIR/uninstall_ubuntu_functions" "$BOOTSTRAP_DIR/uninstall_work_functions")
    local file

    declare -f ubuntu_gnome_desktop_install | grep -Eq 'gnome_automatic_timezone_install \|\|' ||
        fail 'ubuntu_gnome_desktop_install must run gnome_automatic_timezone_install without aborting on failure'
    declare -f ubuntu_gnome_desktop_uninstall | grep -Eq 'gnome_automatic_timezone_uninstall( \|\| true)?' ||
        fail 'ubuntu_gnome_desktop_uninstall must undo what gnome_automatic_timezone_install set'
    for file in "${entry_scripts[@]}"; do
        grep -Eq '^[[:space:]]*ubuntu_gnome_desktop_install$' "$file" ||
            fail "$file must call ubuntu_gnome_desktop_install"
    done
    for file in "${uninstall_functions[@]}"; do
        grep -Eq '^[[:space:]]*ubuntu_gnome_desktop_uninstall$' "$file" ||
            fail "$file must call ubuntu_gnome_desktop_uninstall"
    done
}

for test_name in \
    test_enables_location_then_automatic_timezone \
    test_updates_only_false_key_when_partially_enabled \
    test_second_run_is_idempotent \
    test_rejects_unexpected_gsettings_values \
    test_fails_when_gsettings_does_not_persist_write \
    test_skips_when_gsettings_or_either_schema_is_missing \
    test_uninstall_disables_only_automatic_timezone \
    test_profiles_and_uninstallers_use_it; do
    ( "$test_name" ) || {
        printf 'not ok - %s\n' "$test_name" >&2
        exit 1
    }
    printf 'ok - %s\n' "$test_name"
done

printf 'GNOME automatic time zone tests passed.\n'
