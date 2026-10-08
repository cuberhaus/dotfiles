#!/usr/bin/env bash
# Tests for .local/scripts/automation/desktop-shortcut-cleanup.
#
# They guard what the Desktop must keep: only launchers (.desktop, .url) move, into Shortcuts, and
# nothing else on the Desktop is touched. A launcher an installer recreates replaces its older copy
# instead of piling up as "(2)". --dry-run changes nothing. With --notify a run that moved something
# says so, and a Desktop that was already tidy stays silent.
#
# Every case runs against a throwaway HOME and a stub notification daemon.

# The cases write single-quoted $HOME on purpose: user-dirs.dirs holds the text.
# shellcheck disable=SC2016

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
script="${SCRIPT:-$repo_root/.local/scripts/automation/desktop-shortcut-cleanup}"
case_dir="$(mktemp -d)"
trap 'rm -rf "$case_dir"' EXIT

home="$case_dir/home"
stubs="$case_dir/stubs"
state="$case_dir/state"
calls_log="$case_dir/calls.log"
real_id="$(command -v id)"
mkdir -p "$stubs"

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
    [[ $1 == *"$2"* ]] || fail "$3 (missing: $2) in:"$'\n'"$1"
}

assert_exists() { [[ -e $1 || -L $1 ]] || fail "$2 (missing: $1)"; }
assert_absent() { [[ ! -e $1 && ! -L $1 ]] || fail "$2 (still there: $1)"; }

cat >"$stubs/gdbus" <<'EOF'
#!/usr/bin/env bash
printf 'gdbus' >> "$STUB_LOG"
printf ' [%s]' "$@" >> "$STUB_LOG"
printf '\n' >> "$STUB_LOG"
[[ "$*" == *.Notify* ]] && printf '(uint32 5,)\n'
exit 0
EOF
cat >"$stubs/id" <<EOF
#!/usr/bin/env bash
case \$1 in
-u) echo 1000 ;;
-un) echo lara ;;
*) exec "$real_id" "\$@" ;;
esac
EOF
chmod +x "$stubs/gdbus" "$stubs/id"

desktop="$home/Desktop"

reset_world() {
    rm -rf "$home" "$state"
    mkdir -p "$desktop" "$state" "$home/.config"
    : >"$calls_log"
}

# run_cleanup ARGS...: sets `status`, `output`, `calls`.
run_cleanup() {
    status=0
    output="$(
        env -i HOME="$home" PATH="$stubs:$PATH" XDG_CONFIG_HOME="$home/.config" XDG_STATE_HOME="$state" \
            DBUS_SESSION_BUS_ADDRESS=unix:path=/test/bus STUB_LOG="$calls_log" \
            bash "$script" "$@" 2>&1
    )" || status=$?
    calls="$(<"$calls_log")"
}

###############################################################
# => Launchers move, the rest of the Desktop stays
###############################################################

reset_world
printf 'a' >"$desktop/Firefox.desktop"
printf 'b' >"$desktop/Steam.DESKTOP"
printf 'c' >"$desktop/Site.url"
printf 'd' >"$desktop/.hidden.desktop"
printf 'e' >"$desktop/notes.txt"
mkdir "$desktop/1 Projects" "$desktop/Folder.desktop.d"
printf 'f' >"$desktop/1 Projects/inner.desktop"
ln -s /usr/share/applications/gimp.desktop "$desktop/gimp.desktop"

run_cleanup --desktop "$desktop"
assert_equals 0 "$status" 'a cleanup must succeed'
for moved in Firefox.desktop Steam.DESKTOP Site.url gimp.desktop; do
    assert_exists "$desktop/Shortcuts/$moved" "$moved must move into Shortcuts"
    assert_absent "$desktop/$moved" "$moved must leave the Desktop"
done
for stayed in .hidden.desktop notes.txt "1 Projects/inner.desktop" Folder.desktop.d; do
    assert_exists "$desktop/$stayed" "$stayed must stay where it is"
done
assert_contains "$output" "Moved 4 Desktop launcher item(s) to $desktop/Shortcuts." 'the output must count what moved'
assert_exists "$state/cuberhaus-automations/desktop-shortcut-cleanup.last-success" 'a run must record its time for the digest'

# The same launcher recreated replaces the older copy.
printf 'new' >"$desktop/Firefox.desktop"
run_cleanup --desktop "$desktop"
assert_equals new "$(<"$desktop/Shortcuts/Firefox.desktop")" 'a recreated launcher must replace the old one'
assert_absent "$desktop/Shortcuts/Firefox (2).desktop" 'a recreated launcher must not pile up'

###############################################################
# => Already tidy, and a Desktop that follows XDG
###############################################################

reset_world
printf 'e' >"$desktop/notes.txt"
run_cleanup --desktop "$desktop"
assert_contains "$output" 'Moved 0 Desktop launcher item(s)' 'a tidy Desktop must say nothing moved'
assert_absent "$desktop/Shortcuts" 'a tidy Desktop must not get an empty Shortcuts folder'

reset_world
mkdir -p "$home/Escritorio"
printf 'XDG_DESKTOP_DIR="$HOME/Escritorio"\n' >"$home/.config/user-dirs.dirs"
printf 'a' >"$home/Escritorio/App.desktop"
run_cleanup
assert_exists "$home/Escritorio/Shortcuts/App.desktop" 'the Desktop must be the one user-dirs.dirs names'

###############################################################
# => --dry-run
###############################################################

reset_world
printf 'a' >"$desktop/Firefox.desktop"
run_cleanup --desktop "$desktop" --dry-run
assert_equals 0 "$status" 'a preview must succeed'
assert_exists "$desktop/Firefox.desktop" 'a preview must not move anything'
assert_absent "$desktop/Shortcuts" 'a preview must not create Shortcuts'
assert_contains "$output" 'Would move 1 Desktop launcher item(s)' 'a preview must say what it would do'
assert_absent "$state/cuberhaus-automations/desktop-shortcut-cleanup.last-success" 'a preview is not a success'

###############################################################
# => Arguments and errors
###############################################################

reset_world
run_cleanup --bogus
assert_equals 2 "$status" 'an unknown option must exit with 2'
run_cleanup --help
assert_equals 0 "$status" '--help must succeed'
assert_contains "$output" 'Usage: desktop-shortcut-cleanup' '--help must show the usage'
run_cleanup --desktop "$home/nowhere"
assert_equals 1 "$status" 'a missing Desktop must fail'
assert_contains "$output" 'Desktop directory does not exist' 'a missing Desktop must say so'

###############################################################
# => Notifications
###############################################################

reset_world
printf 'a' >"$desktop/A.desktop"
printf 'b' >"$desktop/B.desktop"
run_cleanup --desktop "$desktop" --notify
assert_equals 1 "$(printf '%s\n' "$calls" | grep -cF 'Notify')" 'a run that moved something must send one summary'
assert_contains "$calls" "['Desktop shortcut cleanup finished']" 'the summary must carry the title'
assert_contains "$calls" "Moved 2 shortcuts to $desktop/Shortcuts." 'the summary must say what moved'

: >"$calls_log"
run_cleanup --desktop "$desktop" --notify
assert_equals '' "$calls" 'a Desktop that was already tidy must stay silent'

reset_world
printf 'a' >"$desktop/A.desktop"
run_cleanup --desktop "$desktop"
assert_equals '' "$calls" 'a run without --notify must never notify'

reset_world
run_cleanup --desktop "$home/nowhere" --notify
assert_contains "$calls" "['Desktop shortcut cleanup failed']" 'a missing Desktop must show as a failure'

printf 'desktop-shortcut-cleanup tests passed.\n'
