#!/usr/bin/env bash
# Unit tests for .local/scripts/lib/cuberhaus-progress.sh, the library behind the progress bar
# and the summary notification of the scheduled automations.
#
# They guard what makes a background notifier either useless or a nuisance:
#   - a run that ends before the delay must not flash a bar, only its summary;
#   - the bar is one notification, replaced in place (the id the daemon answered with), and the
#     summary replaces the bar, also when the last bar update was sent by the subshell of a pipeline;
#   - a run with nothing to do (--quiet) shows nothing and takes an open bar down, but a warning
#     or a failure always shows;
#   - text that reaches the daemon is escaped, so a package named after a quote or an ampersand
#     cannot break the call;
#   - as root the notification goes to each active graphical session through that user's own bus,
#     and to nobody when no one is logged in;
#   - nothing here may ever fail the job that sends it, with or without a notification daemon.
#
# Every case runs in a clean `env -i` shell with a throwaway HOME. PATH holds stub commands and a few
# real tools, so no notification, runuser or id can reach the real machine.

# The scripts handed to the child shells are single-quoted on purpose.
# shellcheck disable=SC2016

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
library="${LIBRARY:-$repo_root/.local/scripts/lib/cuberhaus-progress.sh}"
case_dir="$(mktemp -d)"
trap 'rm -rf "$case_dir"' EXIT

home="$case_dir/home"
stubs="$case_dir/stubs"
bin="$case_dir/bin"
calls_log="$case_dir/calls.log"
bash_bin="$(command -v bash)"
mkdir -p "$home" "$stubs" "$bin" "$case_dir/tmp"

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

# count_of TEXT NEEDLE: how many lines of TEXT contain NEEDLE.
count_of() {
    local count
    count=$(printf '%s\n' "$1" | grep -cF -- "$2" || true)
    printf '%s\n' "$count"
}

###############################################################
# => Stub commands
###############################################################

write_stub() {
    {
        printf '#!%s\n' "$bash_bin"
        cat
    } >"$stubs/$1"
    chmod +x "$stubs/$1"
}

# gdbus logs every argument in brackets. Notify answers with a notification id the way the daemon does.
write_stub gdbus <<'EOF'
printf 'gdbus' >> "$STUB_LOG"
printf ' [%s]' "$@" >> "$STUB_LOG"
printf '\n' >> "$STUB_LOG"
[[ -n ${STUB_GDBUS_FAIL:-} ]] && exit 1
[[ "$*" == *.Notify* ]] && printf '(uint32 %s,)\n' "${STUB_NOTIFY_ID:-41}"
exit 0
EOF

write_stub notify-send <<'EOF'
printf 'notify-send' >> "$STUB_LOG"
printf ' [%s]' "$@" >> "$STUB_LOG"
printf '\n' >> "$STUB_LOG"
exit 0
EOF

# The sessions of the machine: STUB_SESSIONS holds "ID UID USER" lines and STUB_SESSION_<ID> the
# properties of that session.
write_stub loginctl <<'EOF'
case $1 in
list-sessions)
    while read -r id uid user; do
        [[ -n $id ]] && printf '%s %s %s seat0 -\n' "$id" "$uid" "$user"
    done <<< "$STUB_SESSIONS"
    ;;
show-session)
    var="STUB_SESSION_$2"
    printf '%s\n' "${!var}"
    ;;
esac
EOF

# runuser logs who it runs as, then runs the command, which is how the real one hands over.
write_stub runuser <<'EOF'
printf 'runuser [%s]\n' "$2" >> "$STUB_LOG"
shift 3
exec "$@"
EOF

# id answers for the user the case pretends to be.
write_stub id <<'EOF'
case $1 in
-u) printf '%s\n' "${STUB_UID:-1000}" ;;
-un) printf '%s\n' "${STUB_USER:-lara}" ;;
esac
EOF


# run_case SCRIPT: run SCRIPT after loading the library, with the stubs named in case_tools on PATH.
# Sets `status`, `output` (stdout and stderr) and `calls` (what the stubs saw). Case-specific
# environment goes in case_env.
run_case() {
    local tool
    rm -rf "$bin"
    mkdir -p "$bin"
    for tool in date mktemp rm cat grep tail env; do
        ln -s "$(command -v "$tool")" "$bin/$tool"
    done
    for tool in "${case_tools[@]}"; do
        ln -sf "$stubs/$tool" "$bin/$tool"
    done
    : >"$calls_log"
    status=0
    output="$(
        env -i HOME="$home" PATH="$bin" TMPDIR="$case_dir/tmp" STUB_LOG="$calls_log" \
            "${case_env[@]}" \
            "$bash_bin" --noprofile --norc -c 'set -euo pipefail; source "$1"; shift; eval "$1"' bash "$library" "$1" 2>&1
    )" || status=$?
    calls="$(<"$calls_log")"
}

# set_case [TOOL...]: the stubs on PATH. The id stub is always there, so the cases give the same
# answer when the suite itself runs as root, in a container for example.
set_case() {
    case_tools=(id "$@")
    case_env=(CUBERHAUS_PROGRESS_NOW=1000 DBUS_SESSION_BUS_ADDRESS=unix:path=/test/bus)
}

###############################################################
# => No notification daemon tools: silent and harmless
###############################################################

set_case
run_case 'progress_start t "Title" Doing item 0
progress_update current=x index=1 total=2
progress_finish success "a line"
progress_abort 3
echo ok'
assert_equals 0 "$status" 'no gdbus or notify-send must not fail the job'
assert_equals ok "$output" 'no backend must print nothing'

# Calls before progress_start do nothing at all, so a script notifies only when asked to.
set_case gdbus
run_case 'progress_update current=x total=2; progress_finish failure; progress_abort 1; echo ok'
assert_equals ok "$output" 'progress calls before progress_start must be no-ops'
assert_equals '' "$calls" 'progress calls before progress_start must not notify'

###############################################################
# => The bar: one notification, replaced in place
###############################################################

set_case gdbus
run_case 'progress_start pull "Workspace pull" Pulling repository 0
progress_update total=4 current="Pulling alpha" index=1 done=0
CUBERHAUS_PROGRESS_NOW=1005
progress_update current="Pulling beta" index=2 done=1
echo ok'
assert_equals 0 "$status" 'a bar must not fail the job'
assert_equals 2 "$(count_of "$calls" 'Notify')" 'two changes five seconds apart must send two notifications'
first=$(printf '%s\n' "$calls" | grep -F 'Notify' | sed -n 1p)
second=$(printf '%s\n' "$calls" | grep -F 'Notify' | sed -n 2p)
assert_contains "$first" "[uint32 0]" 'the first notification must not replace anything'
assert_contains "$first" "['Workspace pull']" 'the title must be the one given to progress_start'
assert_contains "$first" "<int32 0>" 'no work done yet must show an empty bar'
assert_contains "$first" 'Pulling alpha\nPulling 1 of 4 - 0 s' 'the body must name the current item and the count'
assert_contains "$second" "[uint32 41]" 'the next update must replace the notification the daemon answered with'
assert_contains "$second" '<int32 25>' 'one of four done must fill a quarter of the bar'
assert_contains "$second" 'Pulling 2 of 4 - 5 s' 'the status line must show the elapsed time'
assert_contains "$calls" "'x-canonical-private-synchronous': <'cuberhaus-pull'>" \
    'the synchronous hint lets GNOME and dunst replace the bar even when the id is lost'

# With no total the line counts the unit, and a percentage drives the bar directly.
set_case gdbus
run_case 'progress_start pkg "Packages" Upgrading package 0
progress_update current="libfoo" index=3
CUBERHAUS_PROGRESS_NOW=1002
progress_update percent=64 current="Unpacking libbar"'
assert_contains "$calls" 'Upgrading package 3 - 0 s' 'without a total the status line must count the unit'
assert_contains "$calls" '<int32 64>' 'percent= must set the bar directly'
set_case gdbus
run_case 'progress_start pkg "Packages" Upgrading package 0
progress_update percent=250'
assert_contains "$calls" '<int32 100>' 'a bar must never pass 100'

###############################################################
# => Delay and throttle
###############################################################

set_case gdbus
run_case 'progress_start pull "Workspace pull" Pulling repository 10
progress_update total=4 index=1 done=0 current=a
CUBERHAUS_PROGRESS_NOW=1009
progress_update index=2 done=1 current=b
CUBERHAUS_PROGRESS_NOW=1011
progress_update index=3 done=2 current=c'
assert_equals 1 "$(count_of "$calls" 'Notify')" 'no bar may show before the delay, and the first one after it must'
assert_contains "$calls" 'c\nPulling 3 of 4 - 11 s' 'the first bar after the delay must show where the run is now'

set_case gdbus
run_case 'progress_start pkg "Packages" Upgrading package 0
for n in 1 2 3 4 5; do progress_update percent="$n" current="step $n"; done
CUBERHAUS_PROGRESS_NOW=1001
progress_update percent=50 current="step 50"
progress_update percent=50 current="step 50"'
assert_equals 2 "$(count_of "$calls" 'Notify')" 'updates in the same second must collapse to one, and an unchanged bar must not resend'

###############################################################
# => The summary replaces the bar
###############################################################

set_case gdbus
run_case 'progress_start pull "Workspace pull" Pulling repository 0
progress_update total=2 index=1 done=0 current=a
CUBERHAUS_PROGRESS_NOW=1012
progress_finish success "3 updated, 48 already up to date."
echo done'
assert_equals 0 "$status" 'finishing must not fail the job'
summary=$(printf '%s\n' "$calls" | grep -F 'Notify' | sed -n 2p)
assert_contains "$summary" "[uint32 41]" 'the summary must replace the bar'
assert_contains "$summary" "['Workspace pull finished']" 'a success must say finished'
assert_contains "$summary" '3 updated, 48 already up to date.\nTook 12 s.' 'the summary must carry the lines and the time'
assert_not_contains "$summary" '<int32' 'a summary must not carry a bar'
assert_contains "$summary" "['dialog-information']" 'a success must use the information icon'
assert_equals 'done' "$output" 'finishing must print nothing'

# Only the first ending counts, so a failure trap after a normal finish cannot overwrite the summary.
set_case gdbus
run_case 'progress_start pull "Workspace pull" Pulling repository 0
progress_finish success "ok"
progress_finish failure "late"
progress_abort 9'
assert_equals 1 "$(count_of "$calls" 'Notify')" 'a run must end once'

# A summary alone, from a run that never reached the delay.
set_case gdbus
run_case 'progress_start pull "Workspace pull" Pulling repository 10
progress_update total=2 index=1 current=a
progress_finish warning --message "1 failed: beta." "1 updated."'
assert_equals 1 "$(count_of "$calls" 'Notify')" 'a short run must only produce its summary'
assert_contains "$calls" "[uint32 0]" 'a summary with no bar before it replaces nothing'
assert_contains "$calls" "['Workspace pull finished with problems']" 'a warning must say so'
assert_contains "$calls" '1 failed: beta.\n1 updated.' 'the message must come before the summary lines'
assert_contains "$calls" "['dialog-warning']" 'a warning must use the warning icon'

###############################################################
# => Quiet: nothing to report
###############################################################

set_case gdbus
run_case 'progress_start pull "Workspace pull" Pulling repository 0
progress_update total=2 index=1 current=a
progress_finish success --quiet "0 updated, 51 already up to date."'
assert_equals 1 "$(count_of "$calls" 'Notify')" 'a quiet success must leave only the bar it already showed'
assert_equals 1 "$(count_of "$calls" 'CloseNotification')" 'a quiet success must take the bar down'
assert_contains "$calls" '[uint32 41]' 'the bar that is closed must be the one that was shown'

set_case gdbus
run_case 'progress_start pull "Workspace pull" Pulling repository 10
progress_update total=2 index=1 current=a
progress_finish success --quiet "nothing"'
assert_equals '' "$calls" 'a quiet success before the delay must not notify at all'

for outcome in warning failure; do
    set_case gdbus
    run_case "progress_start pull 'Workspace pull' Pulling repository 10
progress_finish $outcome --quiet --message 'it broke'"
    assert_equals 1 "$(count_of "$calls" 'Notify')" "a $outcome must show even when --quiet was passed"
done
set_case gdbus
run_case 'progress_start pull "Workspace pull" Pulling repository 10
progress_finish failure --message "it broke"'
assert_contains "$calls" "['Workspace pull failed']" 'a failure must say failed'
assert_contains "$calls" "['dialog-error']" 'a failure must use the error icon'

###############################################################
# => The id survives a subshell
###############################################################

# The apt and pacman parsers are the last stage of a pipeline: a subshell that sends the bar. The
# summary of the parent must still replace what that subshell showed.
set_case gdbus
run_case 'progress_start sys "System packages" Upgrading package 0
printf "x\n" | { read -r _; progress_update percent=40 current="Unpacking"; }
CUBERHAUS_PROGRESS_NOW=1020
progress_finish success "done"'
assert_equals 2 "$(count_of "$calls" 'Notify')" 'the subshell bar and the summary must both be sent'
assert_contains "$(printf '%s\n' "$calls" | grep -F 'Notify' | sed -n 2p)" "[uint32 41]" \
    'the summary must replace the bar a pipeline subshell showed'

# The private directory is gone once the run is over.
set_case gdbus
rm -rf "${case_dir:?}/tmp" && mkdir "$case_dir/tmp"
run_case 'progress_start pull "Workspace pull" Pulling repository 0
progress_finish success "ok"'
assert_equals 0 "$(find "$case_dir/tmp" -mindepth 1 | wc -l)" 'the id directory must be removed when the run ends'

###############################################################
# => Escaping
###############################################################

set_case gdbus
run_case 'progress_start pkg "Bob'"'"'s <packages> & more" Upgrading package 0
progress_update current="it'"'"'s a \ backslash & <tag>" index=1
progress_finish success "line one" "line two"'
assert_contains "$calls" "['Bob\\'s &lt;packages&gt; &amp; more']" 'quotes must be escaped for GVariant and markup for the daemon'
assert_contains "$calls" $'it\\\'s a \\\\ backslash &amp; &lt;tag&gt;' 'a backslash must be doubled'
assert_contains "$calls" 'line one\nline two' 'summary lines must be joined with an escaped newline, never a raw one'

###############################################################
# => A daemon that is not there never fails the job
###############################################################

set_case gdbus
case_env+=(STUB_GDBUS_FAIL=1)
run_case 'progress_start pull "Workspace pull" Pulling repository 0
progress_update total=2 index=1 current=a
progress_finish success "ok"
echo survived'
assert_equals 0 "$status" 'a failing daemon must not fail the job'
assert_equals survived "$output" 'a failing daemon must not print an error into the job log'

###############################################################
# => notify-send fallback
###############################################################

set_case notify-send
run_case 'progress_start pull "Workspace pull" Pulling repository 0
progress_update total=4 index=2 done=1 current="Pulling beta"
CUBERHAUS_PROGRESS_NOW=1003
progress_finish success "ok"'
assert_equals 2 "$(count_of "$calls" 'notify-send')" 'the fallback must send the bar and the summary'
assert_contains "$calls" '[--hint=int:value:25]' 'the fallback must pass the bar as the value hint'
assert_contains "$calls" '[--hint=string:x-canonical-private-synchronous:cuberhaus-pull]' \
    'the fallback must replace the bar through the synchronous hint'
assert_contains "$calls" '[Workspace pull finished]' 'the fallback summary must carry the title'

# gdbus wins when both exist, and CUBERHAUS_NOTIFY_BACKEND picks one by hand.
set_case gdbus notify-send
run_case 'progress_start p T V U 0; progress_finish success x'
assert_equals 1 "$(count_of "$calls" 'gdbus')" 'gdbus must be preferred'
assert_equals 0 "$(count_of "$calls" 'notify-send')" 'notify-send must stay unused when gdbus exists'
set_case gdbus notify-send
case_env+=(CUBERHAUS_NOTIFY_BACKEND=notify-send)
run_case 'progress_start p T V U 0; progress_finish success x'
assert_equals 1 "$(count_of "$calls" 'notify-send')" 'CUBERHAUS_NOTIFY_BACKEND=notify-send must pick it'
set_case gdbus notify-send
case_env+=(CUBERHAUS_NOTIFY_BACKEND=none)
run_case 'progress_start p T V U 0; progress_finish success x'
assert_equals '' "$calls" 'CUBERHAUS_NOTIFY_BACKEND=none must send nothing'

###############################################################
# => Abort: a run that dies is reported
###############################################################

set_case gdbus
run_case 'progress_start pull "Workspace pull" Pulling repository 0
progress_abort 3'
assert_contains "$calls" "['Workspace pull failed']" 'an aborted run must end as a failure'
assert_contains "$calls" 'Stopped unexpectedly (exit status 3).' 'the failure must say how the script ended'
set_case gdbus
run_case 'progress_start pull "Workspace pull" Pulling repository 10
progress_abort 0'
assert_equals '' "$calls" 'an exit with status 0 that never finished must stay silent'

###############################################################
# => Root: the notification goes to each graphical session
###############################################################

root_sessions='c1 1000 alice
c2 1001 bob
c3 1000 alice
c4 1002 carol
c5 0 root'
set_case gdbus loginctl runuser id
case_env+=(STUB_UID=0 STUB_USER=root "STUB_SESSIONS=$root_sessions"
    'STUB_SESSION_c1=Type=wayland
State=active
Name=alice
User=1000'
    'STUB_SESSION_c2=Type=tty
State=active
Name=bob
User=1001'
    'STUB_SESSION_c3=Type=x11
State=active
Name=alice
User=1000'
    'STUB_SESSION_c4=Type=wayland
State=closing
Name=carol
User=1002'
    'STUB_SESSION_c5=Type=x11
State=active
Name=root
User=0')
run_case 'progress_start sys "System packages" Upgrading package 0
progress_update total=10 index=1 done=0 current="libfoo"
progress_finish success "10 upgraded."'
assert_equals 0 "$status" 'relaying as root must not fail the job'
assert_equals 2 "$(count_of "$calls" 'runuser [alice]')" 'alice has two sessions but must be notified once per message'
assert_equals 0 "$(count_of "$calls" 'runuser [bob]')" 'a text console is not a desktop'
assert_equals 0 "$(count_of "$calls" 'runuser [carol]')" 'a closing session must be skipped'
assert_equals 0 "$(count_of "$calls" 'runuser [root]')" 'root must not be notified'
assert_contains "$calls" 'gdbus [call] [--session]' 'the relayed call must use the session bus'

# The user's own bus is what the call must reach; the helper env decides it. Check it end to end.
cat >"$stubs/gdbus" <<EOF
#!$bash_bin
printf 'bus=%s runtime=%s\n' "\$DBUS_SESSION_BUS_ADDRESS" "\$XDG_RUNTIME_DIR" >> "\$STUB_LOG"
[[ "\$*" == *.Notify* ]] && printf '(uint32 7,)\n'
exit 0
EOF
chmod +x "$stubs/gdbus"
run_case 'progress_start sys "System packages" Upgrading package 0
progress_finish success "ok"'
assert_contains "$calls" 'bus=unix:path=/run/user/1000/bus runtime=/run/user/1000' \
    'root must hand the user their own session bus'

# Nobody logged in: nothing to notify, and no error.
set_case gdbus loginctl runuser id
case_env+=(STUB_UID=0 STUB_USER=root 'STUB_SESSIONS=')
run_case 'progress_start sys "System packages" Upgrading package 0
progress_finish success "ok"
echo ok'
assert_equals ok "$output" 'a machine with no session must stay silent'
assert_equals '' "$calls" 'a machine with no session must not call the daemon'

printf 'progress library tests passed.\n'
