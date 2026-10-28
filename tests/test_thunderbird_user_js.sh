#!/usr/bin/env bash
# Hermetic tests for thunderbird_user_js_install in bootstrap/base_functions: it writes the
# curated calendar.week.start preference to the default profile's user.js, resolves the
# profile from profiles.ini, preserves other preferences, backs up before replacing a
# conflicting value, is idempotent, and self-skips without a profile.  Everything happens
# inside a temporary home folder, so the real ~/.thunderbird is never read or written.
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

WANTED='user_pref("calendar.week.start", 1);'

# A sandbox home with a two-profile profiles.ini: the second profile is the default.
new_case() {
    CASE="$SCRATCH/$1"
    mkdir -p "$CASE/home/.thunderbird/first.default" "$CASE/home/.thunderbird/c9qk6br6.pol"
    export HOME="$CASE/home"
    cat > "$HOME/.thunderbird/profiles.ini" <<'EOF'
[Profile1]
Name=first
IsRelative=1
Path=first.default

[Profile0]
Name=pol
IsRelative=1
Path=c9qk6br6.pol
Default=1

[InstallD78BF5DD33499EC2]
Default=c9qk6br6.pol
Locked=1
EOF
    USER_JS="$HOME/.thunderbird/c9qk6br6.pol/user.js"
}

test_it_writes_monday_to_the_default_profile_only() {
    new_case default

    thunderbird_user_js_install >/dev/null || fail 'the install failed'

    [ -f "$USER_JS" ] || fail "no user.js written to $USER_JS"
    grep -Fxq "$WANTED" "$USER_JS" || fail 'user.js must start the calendar week on Monday'
    [ ! -e "$HOME/.thunderbird/first.default/user.js" ] ||
        fail 'a profile that is not the default must not be touched'
}

test_a_second_run_changes_nothing() {
    new_case idempotent
    local first second output

    thunderbird_user_js_install >/dev/null || fail 'the first install failed'
    first="$(cat "$USER_JS")"
    output="$(thunderbird_user_js_install)" || fail 'the second install failed'
    second="$(cat "$USER_JS")"

    assert_equals "$first" "$second" 'user.js after a second run'
    assert_contains "$output" 'already starts on Monday' 'the message of a run with nothing to do'
    [ ! -e "$USER_JS.bak" ] || fail 'an unchanged user.js must not be backed up'
}

test_it_preserves_other_preferences() {
    new_case preserve
    local other='user_pref("calendar.alarms.show", false);'

    printf '%s\n' "$other" > "$USER_JS"

    thunderbird_user_js_install >/dev/null || fail 'the install failed'

    grep -Fxq "$other" "$USER_JS" || fail 'an unrelated preference must be kept'
    grep -Fxq "$WANTED" "$USER_JS" || fail 'the Monday preference must be appended'
    [ ! -e "$USER_JS.bak" ] || fail 'appending must not need a backup'
}

test_a_conflicting_value_is_replaced_and_backed_up() {
    new_case replace
    local previous='user_pref("calendar.week.start", 0);'

    printf '%s\n' "$previous" > "$USER_JS"

    thunderbird_user_js_install >/dev/null || fail 'the install failed'

    assert_equals "$previous" "$(cat "$USER_JS.bak")" 'the backup of the previous user.js'
    grep -Fxq "$WANTED" "$USER_JS" || fail 'the Monday preference must replace Sunday'
    if grep -Fxq "$previous" "$USER_JS"; then
        fail 'the Sunday preference must not survive alongside Monday'
    fi
}

test_it_supports_an_absolute_profile_path() {
    new_case absolute
    local profile="$CASE/elsewhere/profile.default"

    mkdir -p "$profile"
    cat > "$HOME/.thunderbird/profiles.ini" <<EOF
[Profile0]
Name=pol
IsRelative=0
Path=$profile
Default=1
EOF

    thunderbird_user_js_install >/dev/null || fail 'the install failed'

    grep -Fxq "$WANTED" "$profile/user.js" || fail 'an absolute profile path must be honoured'
}

test_it_falls_back_to_the_install_default() {
    new_case install-fallback

    cat > "$HOME/.thunderbird/profiles.ini" <<'EOF'
[Profile0]
Name=pol
IsRelative=1
Path=c9qk6br6.pol

[InstallD78BF5DD33499EC2]
Default=c9qk6br6.pol
Locked=1
EOF

    thunderbird_user_js_install >/dev/null || fail 'the install failed'

    grep -Fxq "$WANTED" "$USER_JS" || fail 'the Install Default profile must be used'
}

test_it_skips_when_there_is_no_profile() {
    CASE="$SCRATCH/no-profile"
    mkdir -p "$CASE/home"
    export HOME="$CASE/home"
    local output

    output="$(thunderbird_user_js_install)" || fail 'a missing profile must not fail'

    assert_contains "$output" 'No Thunderbird profile' 'the skip message'
    [ ! -e "$HOME/.thunderbird" ] || fail 'no Thunderbird folder must be created'
}

test_it_fails_when_user_js_cannot_be_written() {
    new_case blocked
    local status=0

    mkdir -p "$USER_JS"

    thunderbird_user_js_install >/dev/null 2>&1 || status=$?

    [ "$status" -ne 0 ] || fail 'an unwritable user.js must fail'
}

test_the_profiles_that_restore_thunderbird_apply_it() {
    local file

    for file in "$BOOTSTRAP_DIR/ubuntu" "$BOOTSTRAP_DIR/work"; do
        grep -Eq '^[[:space:]]*thunderbird_user_js_install \|\|$' "$file" ||
            fail "$file must run thunderbird_user_js_install without aborting on failure"
    done
}

for test_name in \
    test_it_writes_monday_to_the_default_profile_only \
    test_a_second_run_changes_nothing \
    test_it_preserves_other_preferences \
    test_a_conflicting_value_is_replaced_and_backed_up \
    test_it_supports_an_absolute_profile_path \
    test_it_falls_back_to_the_install_default \
    test_it_skips_when_there_is_no_profile \
    test_it_fails_when_user_js_cannot_be_written \
    test_the_profiles_that_restore_thunderbird_apply_it; do
    # A subshell keeps each test's HOME to itself.
    ( "$test_name" ) || {
        printf 'not ok - %s\n' "$test_name" >&2
        exit 1
    }
    printf 'ok - %s\n' "$test_name"
done

printf 'Thunderbird user.js tests passed.\n'
