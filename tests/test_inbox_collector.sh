#!/usr/bin/env bash
# Tests for .local/scripts/automation/inbox-collector.
#
# They guard what makes a collector safe to run unattended every evening:
#   - only stable items move: nothing hidden, linked, half downloaded, or touched in the last
#     minutes, and a folder stays whole or stays put;
#   - nothing is overwritten, and --undo puts back exactly what the last run moved, leaving alone
#     whatever has taken the original place;
#   - --dry-run changes nothing;
#   - the PARA folders and the inbox itself are never collected, even when the Desktop is a source;
#   - the source list understands ~, $HOME and the XDG folders of the user (Descargas, not Downloads);
#   - with --notify a run that moved something says so, a run with nothing to do stays silent, and
#     a failure always shows.
#
# Every case runs against a throwaway HOME. The notification daemon is a stub, so nothing is shown.

# The cases write single-quoted $HOME on purpose: the source list holds the text.
# shellcheck disable=SC2016

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
script="${SCRIPT:-$repo_root/.local/scripts/automation/inbox-collector}"
case_dir="$(mktemp -d)"
trap 'rm -rf "$case_dir"' EXIT

home="$case_dir/home"
stubs="$case_dir/stubs"
calls_log="$case_dir/calls.log"
real_id="$(command -v id)"
real_mv="$(command -v mv)"
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

assert_not_contains() {
    [[ $1 != *"$2"* ]] || fail "$3 (unexpected: $2) in:"$'\n'"$1"
}

assert_exists() { [[ -e $1 || -L $1 ]] || fail "$2 (missing: $1)"; }
assert_absent() { [[ ! -e $1 && ! -L $1 ]] || fail "$2 (still there: $1)"; }

###############################################################
# => Stubs: the notification daemon, and a uid that is not root
###############################################################

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

# A mv that fails on a file named FAIL, for the error path.
cat >"$case_dir/mv-that-fails" <<EOF
#!/usr/bin/env bash
for argument in "\$@"; do
    [[ \$argument == *FAIL* ]] && exit 1
done
exec "$real_mv" "\$@"
EOF
chmod +x "$case_dir/mv-that-fails"

###############################################################
# => World
###############################################################

desktop="$home/Desktop"
downloads="$home/Downloads"
shots="$home/Pictures/Screenshots"
state="$case_dir/state"
inbox="$desktop/0 Inbox"

reset_world() {
    rm -rf "$home" "$state"
    mkdir -p "$desktop" "$downloads" "$shots" "$state" "$home/.config"
    : >"$calls_log"
    extra_path=''
}

# old PATH...: make files, folders and links look two hours old, so the ten minute gate lets them go.
old() {
    local path
    for path in "$@"; do
        find "$path" -exec touch -h -d '2 hours ago' {} +
    done
}

# run_collector ARGS...: run the script on the world above. Sets `status`, `output`, `calls`.
run_collector() {
    status=0
    output="$(
        env -i HOME="$home" PATH="$extra_path$stubs:$PATH" XDG_CONFIG_HOME="$home/.config" \
            XDG_STATE_HOME="$state" DBUS_SESSION_BUS_ADDRESS=unix:path=/test/bus STUB_LOG="$calls_log" \
            bash "$script" --desktop "$desktop" --state-dir "$state" "$@" 2>&1
    )" || status=$?
    calls="$(<"$calls_log")"
}

###############################################################
# => Stable items move; the rest stay
###############################################################

reset_world
printf 'a' >"$downloads/report.pdf"
printf 'b' >"$downloads/.hidden"
printf 'c' >"$downloads/movie.mp4.crdownload"
printf 'd' >"$downloads/iso.part"
printf 'e' >"$downloads/"'~$lock.docx'
printf 'f' >"$downloads/fresh.txt"
ln -s "$downloads/report.pdf" "$downloads/link-to-report"
mkdir "$downloads/Photos" "$downloads/Busy" "$downloads/Partial" "$downloads/Linked"
printf 'p' >"$downloads/Photos/one.jpg"
printf 'x' >"$downloads/Busy/old.txt"
printf 'x' >"$downloads/Partial/data.bin"
printf 'x' >"$downloads/Partial/other.tmp"
printf 'x' >"$downloads/Linked/real.txt"
ln -s "$downloads/Linked/real.txt" "$downloads/Linked/alias"
old "$downloads"
printf 'g' >"$downloads/Busy/just-now.txt" # a fresh file inside holds the whole folder back
printf 'h' >"$downloads/fresh.txt"

run_collector --source "$downloads"
assert_equals 0 "$status" 'a collection must succeed'
assert_exists "$inbox/Downloads/report.pdf" 'a stable file must move into the inbox folder named after its source'
assert_exists "$inbox/Downloads/Photos/one.jpg" 'a stable folder must move whole'
for stayed in .hidden movie.mp4.crdownload iso.part '~$lock.docx' fresh.txt link-to-report Busy Partial Linked; do
    assert_exists "$downloads/$stayed" "$stayed must stay in the source"
    assert_absent "$inbox/Downloads/$stayed" "$stayed must not be collected"
done
assert_contains "$output" 'Moved 2 item(s); skipped 9 item(s); errors 0.' 'the summary must count what moved and what stayed'
assert_exists "$state/inbox-collector.last-success" 'a successful run must record its time for the digest'
assert_contains "$(<"$state/inbox-collector.log")" "[move] $downloads/report.pdf -> $inbox/Downloads/report.pdf" \
    'every move must be logged'

# A second run has nothing left to move.
run_collector --source "$downloads"
assert_contains "$output" 'Moved 0 item(s); skipped 9 item(s)' 'a second run must find nothing new'

###############################################################
# => The age gate
###############################################################

reset_world
printf 'a' >"$downloads/new.txt"
run_collector --source "$downloads"
assert_contains "$output" 'Moved 0 item(s); skipped 1 item(s)' 'a file changed a moment ago must wait'
run_collector --source "$downloads" --minimum-age-minutes 0
assert_exists "$inbox/Downloads/new.txt" '--minimum-age-minutes 0 must collect a fresh file'
run_collector --minimum-age-minutes 1441
assert_equals 2 "$status" 'an age past a day must be refused'
run_collector --minimum-age-minutes soon
assert_equals 2 "$status" 'a non-number age must be refused'

###############################################################
# => Nothing is overwritten
###############################################################

reset_world
mkdir -p "$inbox/Downloads/Photos"
printf 'old' >"$inbox/Downloads/report.pdf"
printf 'old' >"$inbox/Downloads/README"
printf 'old' >"$inbox/Downloads/report (2).pdf"
printf 'new' >"$downloads/report.pdf"
printf 'new' >"$downloads/README"
mkdir "$downloads/Photos"
printf 'new' >"$downloads/Photos/two.jpg"
old "$downloads"
run_collector --source "$downloads"
assert_equals old "$(<"$inbox/Downloads/report.pdf")" 'a taken name must keep its content'
assert_equals new "$(<"$inbox/Downloads/report (3).pdf")" 'the suffix must come before the extension and skip a taken one'
assert_equals new "$(<"$inbox/Downloads/README (2)")" 'a name with no extension must get the suffix at the end'
assert_equals new "$(<"$inbox/Downloads/Photos (2)/two.jpg")" 'a folder must get the suffix at the end of its name'

###############################################################
# => --dry-run
###############################################################

reset_world
printf 'a' >"$downloads/report.pdf"
old "$downloads"
run_collector --source "$downloads" --dry-run
assert_equals 0 "$status" 'a preview must succeed'
assert_exists "$downloads/report.pdf" 'a preview must not move anything'
assert_absent "$inbox" 'a preview must not create the inbox'
assert_contains "$output" 'Would move 1 item(s)' 'a preview must say what it would do'
assert_absent "$state/inbox-collector-last-run.journal" 'a preview must not write an undo journal'
assert_absent "$state/inbox-collector.last-success" 'a preview must not count as a success'

###############################################################
# => --undo
###############################################################

reset_world
printf 'a' >"$downloads/report.pdf"
printf 'b' >"$downloads/notes.txt"
mkdir "$downloads/Photos"
printf 'p' >"$downloads/Photos/one.jpg"
old "$downloads"
run_collector --source "$downloads"
assert_exists "$state/inbox-collector-last-run.journal" 'a run that moved something must journal it'
run_collector --undo
assert_equals 0 "$status" 'an undo must succeed'
assert_contains "$output" 'Restored 3 item(s); skipped 0 item(s); errors 0.' 'an undo must count what it restored'
assert_equals a "$(<"$downloads/report.pdf")" 'an undo must put the file back'
assert_exists "$downloads/Photos/one.jpg" 'an undo must put a folder back whole'
assert_absent "$state/inbox-collector-last-run.journal" 'a complete undo must retire the journal'
run_collector --undo
assert_contains "$output" 'No inbox collector run is available to undo' 'an undo with no journal must say so'

# Something took the original place: the item stays collected, the journal keeps it.
reset_world
printf 'a' >"$downloads/report.pdf"
printf 'b' >"$downloads/notes.txt"
old "$downloads"
run_collector --source "$downloads"
printf 'newer' >"$downloads/report.pdf"
run_collector --undo
assert_contains "$output" 'Restored 1 item(s); skipped 1 item(s); errors 0.' 'an undo must skip an occupied place'
assert_equals newer "$(<"$downloads/report.pdf")" 'an undo must never overwrite'
assert_equals a "$(<"$inbox/Downloads/report.pdf")" 'a skipped item must stay in the inbox'
assert_exists "$state/inbox-collector-last-run.journal" 'a skipped item must stay journaled'
rm "$downloads/report.pdf"
run_collector --undo
assert_contains "$output" 'Restored 1 item(s)' 'a later undo must finish the job'
assert_absent "$state/inbox-collector-last-run.journal" 'the journal must go once everything is back'

###############################################################
# => The source list
###############################################################

reset_world
mkdir -p "$home/Descargas" "$home/Capturas" "$home/Stuff" "$home/Other"
printf 'XDG_DOWNLOAD_DIR="$HOME/Descargas"\nXDG_PICTURES_DIR="$HOME/Capturas"\n' >"$home/.config/user-dirs.dirs"
printf 'a' >"$home/Descargas/one.txt"
printf 'b' >"$home/Capturas/Screenshots-shot.png"
mkdir -p "$home/Capturas/Screenshots"
printf 'c' >"$home/Capturas/Screenshots/two.png"
printf 'd' >"$home/Stuff/three.txt"
printf 'e' >"$home/Other/four.txt"
mkdir -p "$home/.config/cuberhaus-automations"
cat >"$home/.config/cuberhaus-automations/inbox-collector-sources.txt" <<'EOF'
# the drop folders

xdg:DOWNLOAD
xdg:PICTURES/Screenshots
  ~/Stuff
$HOME/Other/
/does/not/exist
EOF
old "$home/Descargas" "$home/Capturas" "$home/Stuff" "$home/Other"
run_collector
assert_equals 0 "$status" 'a list with a missing folder must still succeed'
assert_exists "$inbox/Descargas/one.txt" 'xdg:DOWNLOAD must follow user-dirs.dirs, which is how a Spanish desktop names it'
assert_exists "$inbox/Screenshots/two.png" 'xdg:PICTURES/sub must be a folder under the XDG pictures folder'
assert_exists "$inbox/Stuff/three.txt" '~ must expand'
assert_exists "$inbox/Other/four.txt" '$HOME must expand, and a trailing slash must not matter'
assert_exists "$home/Capturas/Screenshots-shot.png" 'a file beside a source is not part of it'
assert_contains "$(<"$state/inbox-collector.log")" '[skip] Source does not exist: /does/not/exist' \
    'a missing source must be logged and skipped'

# Without user-dirs.dirs the XDG folders have their usual names.
reset_world
printf 'a' >"$downloads/one.txt"
old "$downloads"
printf 'xdg:DOWNLOAD\n' >"$home/.config/list.txt"
run_collector --source-list "$home/.config/list.txt"
assert_exists "$inbox/Downloads/one.txt" 'xdg:DOWNLOAD must fall back to ~/Downloads'

# --additional-source adds to the list; --source replaces it.
reset_world
mkdir -p "$home/Extra"
printf 'a' >"$home/Extra/x.txt"
printf 'b' >"$downloads/y.txt"
old "$home/Extra" "$downloads"
printf '%s\n' "$downloads" >"$home/.config/list.txt"
run_collector --source-list "$home/.config/list.txt" --additional-source "$home/Extra"
assert_exists "$inbox/Extra/x.txt" '--additional-source must be collected too'
assert_exists "$inbox/Downloads/y.txt" 'the list must still be collected'

###############################################################
# => The inbox name
###############################################################

reset_world
mkdir -p "$desktop/0 - Inbox"
printf 'a' >"$downloads/one.txt"
old "$downloads"
run_collector --source "$downloads"
assert_exists "$desktop/0 - Inbox/Downloads/one.txt" 'an existing "0 - Inbox" must be used when no name is given'
assert_absent "$inbox" 'the canonical inbox must not be created beside an existing old-style one'
run_collector --source "$downloads" --inbox-name Entrada --minimum-age-minutes 0
assert_equals 0 "$status" 'a named inbox must work'

###############################################################
# => Protected folders
###############################################################

reset_world
for name in '0 Inbox' '1 Projects' '2 Areas' '3 Resources' '4 Archive' Shortcuts; do
    mkdir -p "$desktop/$name"
    printf 'x' >"$desktop/$name/keep.txt"
done
printf 'a' >"$desktop/loose.txt"
old "$desktop"
run_collector --source "$desktop"
assert_exists "$inbox/Desktop/loose.txt" 'a loose file on the Desktop must be collected when the Desktop is a source'
for name in '1 Projects' '2 Areas' '3 Resources' '4 Archive' Shortcuts; do
    assert_exists "$desktop/$name/keep.txt" "$name must never be collected"
done
assert_exists "$inbox/keep.txt" 'the inbox must never be collected into itself'

###############################################################
# => --add-source
###############################################################

reset_world
mkdir -p "$home/Projects/drop"
list="$home/.config/cuberhaus-automations/inbox-collector-sources.txt"
run_collector --add-source "$home/Projects/drop"
assert_equals 0 "$status" 'adding a source must succeed'
assert_contains "$output" "Added inbox collector source: $home/Projects/drop" 'adding must say what it added'
assert_equals \~/Projects/drop "$(<"$list")" 'a folder in the home must be stored with ~'
run_collector --add-source "$home/Projects/drop"
assert_contains "$output" 'already configured' 'adding twice must not duplicate'
assert_equals 1 "$(wc -l <"$list")" 'the list must hold the folder once'
run_collector --add-source "$home/nowhere"
assert_equals 1 "$status" 'a folder that does not exist must be refused'
assert_equals 1 "$(wc -l <"$list")" 'a refused folder must not be stored'

###############################################################
# => Arguments
###############################################################

reset_world
run_collector --bogus
assert_equals 2 "$status" 'an unknown option must exit with 2'
assert_contains "$output" 'Unknown argument: --bogus' 'an unknown option must say so'
run_collector --help
assert_equals 0 "$status" '--help must succeed'
assert_contains "$output" 'Usage: inbox-collector' '--help must show the usage'
run_collector --desktop "$home/nowhere" --source "$downloads"
assert_equals 1 "$status" 'a missing Desktop must fail'
assert_contains "$output" 'Desktop directory does not exist' 'a missing Desktop must say so'

###############################################################
# => Notifications
###############################################################

reset_world
printf 'a' >"$downloads/report.pdf"
printf 'b' >"$downloads/notes.txt"
printf 'c' >"$downloads/fresh.txt"
old "$downloads/report.pdf" "$downloads/notes.txt"
run_collector --source "$downloads" --notify
assert_equals 1 "$(printf '%s\n' "$calls" | grep -cF 'Notify')" 'a run that moved something must send one summary'
assert_contains "$calls" "['Inbox collector finished']" 'the summary must carry the title'
assert_contains "$calls" 'Moved 2 items into 0 Inbox.\nLeft 1 alone (too recent or protected).' \
    'the summary must say what moved and what was left'

reset_world
run_collector --source "$downloads" --notify
assert_equals '' "$calls" 'a run with nothing to collect must stay silent'
printf 'c' >"$downloads/fresh.txt"
run_collector --source "$downloads" --notify
assert_equals '' "$calls" 'a run that only left things alone must stay silent'

reset_world
run_collector --source "$downloads"
assert_equals '' "$calls" 'a run without --notify must never notify'
printf 'a' >"$downloads/report.pdf"
old "$downloads"
run_collector --source "$downloads"
assert_equals '' "$calls" 'a run that moved something must stay silent without --notify'

reset_world
printf 'a' >"$downloads/report.pdf"
old "$downloads"
run_collector --source "$downloads" --dry-run --notify
assert_contains "$calls" 'Would move 1 item into 0 Inbox.' 'a preview with --notify must say it would move'

# An error shows even though nothing moved, and exits with 1.
reset_world
printf 'a' >"$downloads/FAIL.txt"
old "$downloads"
extra_path="$case_dir/failing-bin:"
mkdir -p "$case_dir/failing-bin"
cp "$case_dir/mv-that-fails" "$case_dir/failing-bin/mv"
run_collector --source "$downloads" --notify
assert_equals 1 "$status" 'a move that fails must fail the run'
assert_contains "$calls" "['Inbox collector finished with problems']" 'an error must show, quiet or not'
assert_contains "$calls" '1 item(s) could not be moved; see the log.' 'the warning must point at the log'
assert_exists "$downloads/FAIL.txt" 'a failed move must leave the item where it was'

# A missing Desktop is a failure the notification explains.
reset_world
run_collector --desktop "$home/nowhere" --source "$downloads" --notify
assert_contains "$calls" "['Inbox collector failed']" 'a missing Desktop must show as a failure'
assert_contains "$calls" 'Desktop directory does not exist' 'the failure must say why'

printf 'inbox-collector tests passed.\n'
