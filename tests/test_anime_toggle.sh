#!/usr/bin/env bash
# Unit tests for the `anime-toggle` function in .config/zsh/aliases, the toggle for the
# AniMe lid display of an ASUS laptop.
#
# They guard what a toggle for that display must get right:
#   - It is a function: a toggle reads the current state first, which an alias cannot do.
#   - It exists only on Linux where asusctl and busctl are installed, so shells on other
#     machines never offer a command that cannot work.
#   - The lid counts as lit only when it is enabled AND brighter than Off. The lid was first
#     switched off by hand with `--enable-display false --brightness off`, and
#     `--brightness off` alone leaves EnableDisplay true, so the first toggle after either
#     must switch the lid ON, and switching on must raise an Off brightness: enabling a
#     display whose brightness is Off shows nothing and would look like a broken toggle.
#   - A laptop without the display (asusd exports no /xyz/ljones/aura/anime), a stopped
#     daemon, or a reply it cannot read changes nothing: an answer the function cannot
#     read is never taken for a state.
#   - It takes no arguments, so `anime-toggle off` cannot turn a dark lid on.
#
# A fake busctl serves the two properties in the text the real busctl printed on a ROG Strix
# SCAR 16 (G635LX: "b false" and "u 0") and refuses any other request, and a fake asusctl
# changes that state the way the real CLI does, so a toggle can be repeated and a wrong flag
# fails. Every case runs in a clean `env -i` shell with a throwaway HOME and a PATH that
# holds only the stubs, so nothing touches the real daemon or the real lid.
# ALIASES=path runs the cases against another copy of the aliases file.

# The scripts handed to the child shells below are single-quoted on purpose: their
# variables must be expanded by the child shell, not by this one.
# shellcheck disable=SC2016

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
aliases="${ALIASES:-$repo_root/.config/zsh/aliases}"
case_dir="$(mktemp -d)"
trap 'rm -rf "$case_dir"' EXIT

home="$case_dir/home"
stubs="$case_dir/stubs" # every stub command; a case links the ones it installs into $bin
bin="$case_dir/bin"
state_dir="$case_dir/state"
calls_log="$case_dir/calls.log"
bash_bin="$(command -v bash)"
mkdir -p "$home" "$stubs" "$bin" "$state_dir"

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
    [[ $1 == *"$2"* ]] || fail "$3 (missing: $2; got: $1)"
}

assert_not_contains() {
    [[ $1 != *"$2"* ]] || fail "$3 (unexpected: $2; got: $1)"
}

###############################################################
# => Stub commands
###############################################################

# write_stub NAME: the script body comes from stdin. The stubs use shell builtins only,
# because the PATH of a case holds nothing else.
write_stub() {
    {
        printf '#!%s\n' "$bash_bin"
        cat
    } > "$stubs/$1"
    chmod +x "$stubs/$1"
}

# STUB_BUSCTL_FAIL makes the request fail with that reason, as when asusd is not on the bus
# or exports no such object. STUB_BUSCTL_USE_REPLY=1 replaces the reply with STUB_BUSCTL_REPLY
# (which may be empty; `\n` in it is a line break).
write_stub busctl <<'EOF'
printf 'busctl %s\n' "$*" >> "$STUB_LOG"
if [[ -n ${STUB_BUSCTL_FAIL:-} ]]; then
    echo "Failed to get property EnableDisplay on interface xyz.ljones.Anime: $STUB_BUSCTL_FAIL" >&2
    exit 1
fi
if [[ -n ${STUB_BUSCTL_USE_REPLY:-} ]]; then
    printf '%b' "$STUB_BUSCTL_REPLY"
    exit 0
fi
if [[ $* != '--system get-property xyz.ljones.Asusd /xyz/ljones/aura/anime xyz.ljones.Anime EnableDisplay Brightness' ]]; then
    echo "Failed to get property: unexpected request: $*" >&2
    exit 1
fi
read -r enabled < "$STUB_STATE/enabled"
read -r level < "$STUB_STATE/level"
printf 'b %s\nu %s\n' "$enabled" "$level"
EOF

# Only `asusctl anime` with --enable-display true|false and --brightness off|low|med|high
# exists; anything else fails, so a typo in a flag fails the case. STUB_ASUSCTL_FAIL makes
# every call fail with that message.
write_stub asusctl <<'EOF'
printf 'asusctl %s\n' "$*" >> "$STUB_LOG"
if [[ -n ${STUB_ASUSCTL_FAIL:-} ]]; then
    echo "Error: $STUB_ASUSCTL_FAIL" >&2
    exit 1
fi
if [[ ${1:-} != anime ]]; then
    echo "unexpected asusctl command: $*" >&2
    exit 64
fi
shift
while [[ $# -gt 0 ]]; do
    case $1 in
        --enable-display)
            if [[ ${2:-} != true && ${2:-} != false ]]; then
                echo "bad value for --enable-display: ${2:-}" >&2
                exit 64
            fi
            printf '%s\n' "$2" > "$STUB_STATE/enabled"
            shift 2
            ;;
        --brightness)
            case ${2:-} in
                off) level=0 ;;
                low) level=1 ;;
                med) level=2 ;;
                high) level=3 ;;
                *)
                    echo "bad value for --brightness: ${2:-}" >&2
                    exit 64
                    ;;
            esac
            printf '%s\n' "$level" > "$STUB_STATE/level"
            shift 2
            ;;
        *)
            echo "unexpected argument: $1" >&2
            exit 64
            ;;
    esac
done
EOF

###############################################################
# => Harness
###############################################################

# pick_shell SHELL: set shell_bin, flags and setup to start SHELL without any startup file.
pick_shell() {
    shell_bin="$(command -v "$1")"
    case "$1" in
        bash)
            flags=(--noprofile --norc)
            setup='shopt -s expand_aliases'
            ;;
        zsh)
            flags=(-f)
            setup=':'
            ;;
    esac
}

# set_state ENABLED LEVEL: the lid as asusd reports it (ENABLED true|false; LEVEL 0 Off to 3 High).
set_state() {
    printf '%s\n' "$1" > "$state_dir/enabled"
    printf '%s\n' "$2" > "$state_dir/level"
}

# lid_state: print "ENABLED LEVEL" as asusd holds them now.
lid_state() {
    local enabled level
    read -r enabled < "$state_dir/enabled"
    read -r level < "$state_dir/level"
    printf '%s %s\n' "$enabled" "$level"
}

# set_tools TOOL...: the stubs that are "installed" in the next runs; PATH holds nothing else.
set_tools() {
    local tool
    rm -rf "$bin"
    mkdir -p "$bin"
    for tool in "$@"; do
        ln -s "$stubs/$tool" "$bin/$tool"
    done
}

# run_in SHELL OSTYPE COMMAND [ARGS...]
# Load the aliases into a clean SHELL that believes it runs on OSTYPE and run
# `COMMAND ARGS`. Sets `status` (its exit status), `output` (stdout and stderr) and
# `calls` (what the stubs saw). Extra environment for the stubs goes in case_env.
run_in() {
    local shell_name=$1 ostype=$2
    shift 2
    pick_shell "$shell_name"
    : > "$calls_log"
    status=0
    # The shell sets OSTYPE itself at startup and ignores the environment, so the script
    # assigns it before the aliases are loaded.
    output="$(
        env -i HOME="$home" PATH="$bin" TERM=dumb DISTRO=ubuntu \
            STUB_LOG="$calls_log" STUB_STATE="$state_dir" \
            "${case_env[@]}" \
            "$shell_bin" "${flags[@]}" -c "$setup"$'\n''
                aliases_file=$1
                OSTYPE=$2
                shift 2
                source "$aliases_file"
                "$@"
            ' "$shell_name" "$aliases" "$ostype" "$@" 2>&1
    )" || status=$?
    calls="$(< "$calls_log")"
}

# toggle SHELL STATE... : the function on Linux; the state and extra environment are set by the caller.
toggle() {
    local shell_name=$1
    shift
    run_in "$shell_name" linux-gnu anime-toggle "$@"
}

# kind_of SHELL OSTYPE: print what the aliases file leaves anime-toggle as in a clean SHELL.
kind_of() {
    pick_shell "$1"
    env -i HOME="$home" PATH="$bin" TERM=dumb DISTRO=ubuntu \
        "$shell_bin" "${flags[@]}" -c "$setup"$'\n''
            OSTYPE=$2
            source "$1"
            if [ -n "${ZSH_VERSION:-}" ]; then
                kind=$(whence -w anime-toggle)
                printf "%s\n" "${kind##* }"
            else
                type -t anime-toggle || echo none
            fi
        ' "$1" "$aliases" "$2"
}

reset_case() {
    case_env=(STUB_CASE=1)
}

busctl_request='busctl --system get-property xyz.ljones.Asusd /xyz/ljones/aura/anime xyz.ljones.Anime EnableDisplay Brightness'

shells=(bash)
if command -v zsh > /dev/null 2>&1; then
    shells+=(zsh)
else
    printf 'SKIP: zsh is not installed; testing Bash only.\n'
fi

###############################################################
# => The ## description above the definition (what `commands` lists)
###############################################################

previous_line="$(grep -B1 -E '^[[:space:]]*anime-toggle\(\)' "$aliases" | sed -n '1p')"
[[ $previous_line =~ ^[[:space:]]*\#\#[[:space:]]+[A-Z] ]] \
    || fail "anime-toggle needs a '## Description' line directly above it for the commands catalog (got: $previous_line)"

###############################################################
# => A function, defined only on Linux with asusctl and busctl
###############################################################

for shell_name in "${shells[@]}"; do
    set_tools asusctl busctl
    assert_equals function "$(kind_of "$shell_name" linux-gnu)" \
        "$shell_name: anime-toggle must be a function; a toggle has to read the lid's state first"

    set_tools asusctl
    assert_equals none "$(kind_of "$shell_name" linux-gnu)" \
        "$shell_name: without busctl the state cannot be read, so the function must not exist"

    set_tools busctl
    assert_equals none "$(kind_of "$shell_name" linux-gnu)" \
        "$shell_name: without asusctl nothing can be switched, so the function must not exist"

    set_tools
    assert_equals none "$(kind_of "$shell_name" linux-gnu)" \
        "$shell_name: a machine without ASUS tools must not get the function"

    set_tools asusctl busctl
    assert_equals none "$(kind_of "$shell_name" darwin23)" \
        "$shell_name: the function belongs to the Linux block"
done

###############################################################
# => Toggling: every state of the lid, in both shells
###############################################################

# check_toggle SHELL LABEL ENABLED LEVEL RESULT_STATE WORD ASUSCTL_ARGS
# From the lid state ENABLED/LEVEL, anime-toggle must make exactly one asusctl call with
# ASUSCTL_ARGS, report WORD, and leave the lid in RESULT_STATE.
check_toggle() {
    local shell_name=$1 label=$2 enabled=$3 level=$4 result=$5 word=$6 args=$7
    reset_case
    set_state "$enabled" "$level"
    toggle "$shell_name"
    assert_equals 0 "$status" "$shell_name: $label must succeed ($output)"
    assert_equals "$busctl_request"$'\n'"asusctl $args" "$calls" \
        "$shell_name: $label must read both properties once, then make one asusctl call"
    assert_equals "AniMe lid display $word" "$output" "$shell_name: $label must say what it did"
    assert_equals "$result" "$(lid_state)" "$shell_name: $label left the lid in the wrong state"
}

for shell_name in "${shells[@]}"; do
    set_tools asusctl busctl

    check_toggle "$shell_name" 'a lit lid (med)' true 2 'false 0' off \
        'anime --enable-display false --brightness off'
    check_toggle "$shell_name" 'a lit lid (high)' true 3 'false 0' off \
        'anime --enable-display false --brightness off'
    check_toggle "$shell_name" 'the lid as it was first switched off (disabled, brightness off)' false 0 'true 2' on \
        'anime --enable-display true --brightness med'
    check_toggle "$shell_name" 'an enabled display with brightness off (dark)' true 0 'true 2' on \
        'anime --enable-display true --brightness med'
    check_toggle "$shell_name" 'a disabled display that keeps a chosen brightness' false 3 'true 3' on \
        'anime --enable-display true'

    # Two toggles bring the lid back to where it started.
    reset_case
    set_state false 0
    toggle "$shell_name"
    toggle "$shell_name"
    assert_equals 'false 0' "$(lid_state)" "$shell_name: a second toggle must undo the first"
    assert_equals "$busctl_request"$'\n'"asusctl anime --enable-display false --brightness off" \
        "$(tail -n 2 "$calls_log")" "$shell_name: the second toggle must switch the lid off"
done

###############################################################
# => Nothing changes without a readable lid
###############################################################

for shell_name in "${shells[@]}"; do
    set_tools asusctl busctl

    # No AniMe object (a laptop without the display) and no daemon on the bus: the same
    # message, and asusctl is never called.
    for reason in "Unknown object '/xyz/ljones/aura/anime'" \
        'The name xyz.ljones.Asusd was not provided by any .service files'; do
        reset_case
        case_env+=("STUB_BUSCTL_FAIL=$reason")
        set_state false 0
        toggle "$shell_name"
        assert_equals 1 "$status" "$shell_name: a laptop without the display must fail ($reason)"
        assert_contains "$output" 'No AniMe lid display' "$shell_name: the failure must say why ($reason)"
        assert_equals "$busctl_request" "$calls" "$shell_name: asusctl must not run without a display ($reason)"
        assert_equals 'false 0' "$(lid_state)" "$shell_name: nothing may change without a display"
    done

    # A reply that is not "b true|false" then "u N" is never taken for a state.
    for reply in 'b maybe\nu 1\n' 'b true\ns Med\n' 'b true\n' 'b false\nu x\n' ''; do
        reset_case
        case_env+=("STUB_BUSCTL_REPLY=$reply" STUB_BUSCTL_USE_REPLY=1)
        set_state true 2
        toggle "$shell_name"
        assert_equals 1 "$status" "$shell_name: an unreadable reply must fail (reply: $reply)"
        assert_contains "$output" 'Unexpected reply from asusd' "$shell_name: the failure must say why (reply: $reply)"
        assert_equals "$busctl_request" "$calls" "$shell_name: asusctl must not run on an unreadable reply (reply: $reply)"
        assert_equals 'true 2' "$(lid_state)" "$shell_name: an unreadable reply must change nothing (reply: $reply)"
    done

    # A failing asusctl fails the toggle, in both directions, and nothing claims success.
    for initial in 'true 2' 'false 0'; do
        reset_case
        case_env+=(STUB_ASUSCTL_FAIL='daemon refused')
        # shellcheck disable=SC2086
        set_state $initial
        toggle "$shell_name"
        assert_equals 1 "$status" "$shell_name: a failing asusctl must fail the toggle ($initial)"
        assert_contains "$output" 'daemon refused' "$shell_name: asusctl's own error must reach the user ($initial)"
        assert_not_contains "$output" 'AniMe lid display' "$shell_name: a failed toggle must not report success ($initial)"
        assert_equals "$initial" "$(lid_state)" "$shell_name: a failed toggle must change nothing ($initial)"
    done
done

###############################################################
# => No arguments
###############################################################

for shell_name in "${shells[@]}"; do
    set_tools asusctl busctl

    # `anime-toggle off` on a dark lid would otherwise turn it on.
    for argument in off on --status; do
        reset_case
        set_state false 0
        toggle "$shell_name" "$argument"
        assert_equals 2 "$status" "$shell_name: an argument ($argument) must be a usage error"
        assert_contains "$output" "unexpected argument: $argument" "$shell_name: the error must name the argument ($argument)"
        assert_equals '' "$calls" "$shell_name: an argument ($argument) must not reach busctl or asusctl"
        assert_equals 'false 0' "$(lid_state)" "$shell_name: an argument ($argument) must not toggle the lid"
    done

    for help_flag in -h --help; do
        reset_case
        set_state false 0
        toggle "$shell_name" "$help_flag"
        assert_equals 0 "$status" "$shell_name: $help_flag must succeed"
        assert_contains "$output" 'Usage: anime-toggle' "$shell_name: $help_flag must print the usage"
        assert_equals '' "$calls" "$shell_name: $help_flag must not reach busctl or asusctl"
        assert_equals 'false 0' "$(lid_state)" "$shell_name: $help_flag must not toggle the lid"
    done
done

printf 'anime-toggle tests passed.\n'
