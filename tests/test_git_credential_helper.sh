#!/usr/bin/env bash
# Tests for git_credential_helper_configure in .local/scripts/bootstrap/base_functions.
#
# Git must not save passwords unencrypted. The 'store' helper writes them to
# ~/.git-credentials, where anyone who can read the home folder, root included, can use them.
# The bootstrap replaces it with the in-memory 'cache' helper, and it must:
#   - replace every spelling of 'store' and nothing else: a better helper (libsecret,
#     osxkeychain, a cache with its own timeout) is never downgraded, and the helpers that
#     are set for one host, such as the gh ones, are never touched;
#   - change the config file as little as possible, in place;
#   - report a leftover ~/.git-credentials, never delete it and never print what it holds;
#   - never fail the bootstrap that calls it, whatever state the config is in.
# It also guards the sources: no bootstrap or template may set 'store' again, and the profiles
# that provision Git call the function (the work profile leaves Git credentials alone).
#
# Every case runs against a throwaway HOME and GIT_CONFIG_GLOBAL, so the real Git config is
# never read or written.

# The messages the assertions look for name ~/.git-credentials with a literal tilde (SC2088).
# shellcheck disable=SC2088

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
case_dir="$(mktemp -d)"
trap 'chmod -R u+rwX "$case_dir" 2>/dev/null || true; rm -rf "$case_dir"' EXIT

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

assert_equals() {
    local expected=$1 actual=$2 message=$3
    if [[ $expected != "$actual" ]]; then
        printf 'FAIL: %s\n--- expected\n%s\n--- actual\n%s\n' "$message" "$expected" "$actual" >&2
        exit 1
    fi
}

assert_contains() {
    [[ $1 == *"$2"* ]] || fail "$3 (missing: $2). Output was:
$1"
}

assert_not_contains() {
    [[ $1 != *"$2"* ]] || fail "$3 (unexpected: $2). Output was:
$1"
}

export HOME="$case_dir/home"
export GIT_CONFIG_GLOBAL="$HOME/.gitconfig"
export GIT_CONFIG_NOSYSTEM=1
unset XDG_CONFIG_HOME GIT_CONFIG_COUNT
mkdir -p "$HOME"

# shellcheck source=../.local/scripts/bootstrap/base_functions
source "$repo_root/.local/scripts/bootstrap/base_functions"

secure='cache --timeout=28800'
credentials="$HOME/.git-credentials"

# write_config TEXT: the global config of the next case, and no leftover credentials file.
# TEXT empty means no config file at all.
write_config() {
    rm -f "$GIT_CONFIG_GLOBAL" "$credentials"
    if [[ -n ${1:-} ]]; then
        printf '%s' "$1" > "$GIT_CONFIG_GLOBAL"
    fi
}

# configure: run the function. Sets `status` and `output` (its stdout and stderr).
configure() {
    status=0
    output="$(git_credential_helper_configure 2>&1)" || status=$?
}

# helpers: the global credential.helper values, one per line.
helpers() {
    git config --global --get-all credential.helper 2>/dev/null || true
}

# assert_config_is EXPECTED MESSAGE: the whole config file must be exactly EXPECTED.
assert_config_is() {
    local expected_file="$case_dir/expected"
    printf '%s' "$1" > "$expected_file"
    if ! cmp -s "$expected_file" "$GIT_CONFIG_GLOBAL"; then
        printf 'FAIL: %s\n--- expected\n%s\n--- actual\n%s\n' "$2" "$1" "$(cat "$GIT_CONFIG_GLOBAL")" >&2
        exit 1
    fi
}

# The shape of a real ~/.gitconfig: the generic helper, then the gh helpers for GitHub, whose
# empty value resets the helper list for that host.
gh_sections=$'[credential "https://github.com"]\n\thelper = \n\thelper = !/usr/bin/gh auth git-credential\n[credential "https://gist.github.com"]\n\thelper = \n\thelper = !/usr/bin/gh auth git-credential\n'

###############################################################
# => A helper that saves passwords in plain text is replaced
###############################################################

before=$'[credential]\n\thelper = store\n[user]\n\temail = me@example.com\n\tname = me\n'"$gh_sections"
write_config "$before"
configure
assert_equals 0 "$status" 'replacing store must succeed'
assert_config_is "${before/helper = store/helper = $secure}" \
    'only the store line may change, in place; the gh helpers must stay as they are'
assert_contains "$output" "replaced it with '$secure'" 'it must say what it did'

# A second run has nothing left to do.
snapshot="$(cat "$GIT_CONFIG_GLOBAL")"
configure
assert_equals 0 "$status" 'a second run must succeed'
assert_equals "$snapshot" "$(cat "$GIT_CONFIG_GLOBAL")" 'a second run must change nothing'
assert_contains "$output" 'left as is' 'a second run must say there was nothing to do'

# Every spelling of the plain-text helper goes.
for plain in 'store' 'store --file=/tmp/credentials' 'store --file /tmp/credentials' \
    '!git credential-store' '!/usr/lib/git-core/git-credential-store --file x' \
    '/usr/lib/git-core/git-credential-store' \
    'store --file=/tmp/(a)[b]*.txt'; do # the last one has regular-expression characters in it
    write_config $'[credential]\n\thelper = '"$plain"$'\n'
    configure
    assert_equals 0 "$status" "replacing '$plain' must succeed"
    assert_equals "$secure" "$(helpers)" "'$plain' saves passwords in plain text and must be replaced"
done

###############################################################
# => Nothing configured yet
###############################################################

write_config ''
configure
assert_equals 0 "$status" 'no config must not be an error'
assert_equals "$secure" "$(helpers)" 'no helper must become the in-memory cache'
assert_contains "$output" "set to '$secure'" 'it must say what it set'

# Helpers for one host do not count as the generic helper, and they stay as they are.
write_config "$gh_sections"
configure
assert_equals "$secure" "$(helpers)" 'host helpers alone leave the generic helper unset, so it must be set'
assert_equals $'\n!/usr/bin/gh auth git-credential' "$(git config --global --get-all 'credential.https://github.com.helper')" \
    'the gh helper for github.com must stay, with its reset'
assert_equals $'\n!/usr/bin/gh auth git-credential' "$(git config --global --get-all 'credential.https://gist.github.com.helper')" \
    'the gh helper for gist.github.com must stay, with its reset'

###############################################################
# => A helper that is not plain text is left alone
###############################################################

for safe in libsecret osxkeychain 'cache --timeout=60' manager store-more '!/usr/bin/gh auth git-credential'; do
    config=$'[credential]\n\thelper = '"$safe"$'\n'
    write_config "$config"
    configure
    assert_equals 0 "$status" "'$safe' must not be an error"
    assert_config_is "$config" "'$safe' does not save passwords in plain text and must stay"
    assert_contains "$output" 'left as is' "'$safe' must be reported as fine"
done

# Beside another helper, the plain-text one is removed and nothing is added.
write_config $'[credential]\n\thelper = cache --timeout=60\n\thelper = store\n'
configure
assert_equals 'cache --timeout=60' "$(helpers)" "the user's own cache must stay and store must go"

write_config $'[credential]\n\thelper = store\n\thelper = libsecret\n'
configure
assert_equals 'libsecret' "$(helpers)" 'libsecret must stay and store must go, without a cache added'

# Two plain-text spellings, or one listed twice, become exactly one cache.
write_config $'[credential]\n\thelper = store\n\thelper = store --file=/tmp/credentials\n'
configure
assert_equals "$secure" "$(helpers)" 'two plain-text spellings must become one cache'

write_config $'[credential]\n\thelper = store\n\thelper = store\n'
configure
assert_equals "$secure" "$(helpers)" 'a repeated store must become one cache'

write_config $'[credential]\n\thelper = store\n\thelper = libsecret\n\thelper = store\n'
configure
assert_equals 'libsecret' "$(helpers)" 'a repeated store beside libsecret must go, without a cache added'

# What git then runs per host: GitHub keeps only its own helper, every other host gets the cache.
# The gh helper is a harmless stand-in here, so no real gh runs.
write_config $'[credential]\n\thelper = store\n[credential "https://github.com"]\n\thelper = \n\thelper = !true\n'
configure
trace_for() {
    # The trace goes to stderr; stdout would be the credential, which is not wanted.
    printf 'protocol=https\nhost=%s\n\n' "$1" |
        { GIT_TERMINAL_PROMPT=0 GIT_ASKPASS=/bin/false GIT_TRACE=1 git credential fill > /dev/null; } 2>&1 || true
}
github_trace="$(trace_for github.com)"
other_trace="$(trace_for gitlab.example)"
assert_contains "$github_trace" "'true get'" 'GitHub must still use its own helper'
assert_not_contains "$github_trace" 'credential-cache' 'GitHub must not use the cache'
assert_not_contains "$github_trace" 'credential-store' 'GitHub must not use the plain-text store'
assert_contains "$other_trace" "credential-cache --timeout=28800 get" 'every other host must use the in-memory cache'
assert_not_contains "$other_trace" 'credential-store' 'no host may use the plain-text store'

###############################################################
# => A leftover ~/.git-credentials
###############################################################

write_config $'[credential]\n\thelper = store\n'
printf 'https://alice:s3cretpassw0rd@example.com\n' > "$credentials"
chmod 600 "$credentials"
configure
assert_contains "$output" '~/.git-credentials still holds saved passwords in plain text' 'a leftover file must be reported'
assert_not_contains "$output" 's3cretpassw0rd' 'the saved password must never be printed'
assert_not_contains "$output" 'alice' 'the saved user must never be printed'
assert_equals 'https://alice:s3cretpassw0rd@example.com' "$(< "$credentials")" 'the file must not be touched'
assert_equals 600 "$(stat -c %a "$credentials")" 'its mode must not change'

# Already safe, but the file is still there: still reported.
configure
assert_contains "$output" '~/.git-credentials still holds saved passwords in plain text' 'the file must be reported on every run'

write_config $'[credential]\n\thelper = store\n'
configure
assert_not_contains "$output" '.git-credentials' 'without the file there is nothing to report'

###############################################################
# => It never fails the bootstrap
###############################################################

# A config that git cannot read: say so, leave it alone, and succeed.
broken=$'[credential\n\thelper = store\n'
write_config "$broken"
configure
assert_equals 0 "$status" 'an unreadable config must not fail the bootstrap'
assert_contains "$output" 'could not be read' 'an unreadable config must be reported'
assert_config_is "$broken" 'an unreadable config must not be touched'
assert_equals continued "$(
    set -e
    git_credential_helper_configure > /dev/null 2>&1
    echo continued
)" 'a caller under set -e must carry on'

# No git at all.
mkdir -p "$case_dir/empty"
no_git_output="$(
    hash -r
    # An empty search path is the point: git is not found. It is set in this subshell only.
    # shellcheck disable=SC2123
    PATH="$case_dir/empty"
    git_credential_helper_configure 2>&1
)"
assert_contains "$no_git_output" 'git is not installed' 'a missing git must be reported'

# A removal that fails without saying so (an old git that does not know --fixed-value, say)
# leaves the plain-text helper in place. The function must judge by the config as it is
# afterwards, not by the commands it ran, and report it.
git() {
    if [[ ${FAKE_GIT_REFUSES_UNSET:-} == true && " $* " == *' --unset-all '* ]]; then
        return 129
    fi
    command git "$@"
}
write_config $'[credential]\n\thelper = store\n\thelper = libsecret\n'
FAKE_GIT_REFUSES_UNSET=true configure
assert_equals 0 "$status" 'a removal that failed must not fail the bootstrap'
assert_contains "$output" 'still has a credential helper that saves passwords in plain text' \
    'a plain-text helper that is still there afterwards must be reported'
assert_equals $'store\nlibsecret' "$(helpers)" 'a failed removal must leave the config as it was'
unset -f git

# A config that cannot be written: the plain-text helper stays, and it says so. A root user
# can write anywhere, so the case needs another user.
if [[ $(id -u) -ne 0 ]]; then
    config=$'[credential]\n\thelper = store\n'
    write_config "$config"
    chmod 555 "$HOME"
    configure
    chmod 755 "$HOME"
    assert_equals 0 "$status" 'a config that cannot be written must not fail the bootstrap'
    assert_contains "$output" 'still has a credential helper that saves passwords in plain text' \
        'a plain-text helper that could not be replaced must be reported'
    assert_config_is "$config" 'a config that cannot be written must stay as it was'
else
    printf 'SKIP: running as root, so a read-only home cannot be simulated.\n'
fi

###############################################################
# => The sources
###############################################################

bootstrap_dir="$repo_root/.local/scripts/bootstrap"
if grep -rnE 'credential\.helper[[:space:]]+store|helper[[:space:]]*=[[:space:]]*store([[:space:]]|$)' \
    "$bootstrap_dir" "$repo_root/.local/Mini"; then
    fail 'no bootstrap or template may configure the plain-text store helper'
fi

calls_in() {
    grep -c '^[[:space:]]*git_credential_helper_configure$' "$1" || true
}
assert_equals 1 "$(calls_in "$bootstrap_dir/ubuntu_functions")" 'the ubuntu profiles must configure the Git credential helper'
assert_equals 2 "$(calls_in "$bootstrap_dir/arch_functions")" 'the arch and manjaro profiles must configure the Git credential helper'
assert_equals 0 "$(calls_in "$bootstrap_dir/work")" 'the work profile must leave Git credentials alone'

printf 'Git credential helper tests passed.\n'
