#!/usr/bin/env bash
# Tests for --notify in workspace-pull (.local/scripts/automation/).
#
# They guard what turns a background job into one you can follow without a terminal:
#   - without --notify a job is exactly as silent as before;
#   - with it, a job that changed something ends with one summary, and one that had nothing to do
#     stays silent, but a failure always shows and still fails the job;
#   - the bar follows the repositories and the summary replaces it, never the other way round;
#   - what the job did before is untouched: pulls, exit statuses, the last-success file, the lock.
#
# Every case runs on a restricted PATH: the stubs below plus a few real tools. The real
# notification daemon cannot be reached, so nothing is shown.

# The scripts handed to the child shells are single-quoted on purpose.
# shellcheck disable=SC2016

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
automation="${AUTOMATION_DIR:-$repo_root/.local/scripts/automation}"
case_dir="$(mktemp -d)"
trap 'rm -rf "$case_dir"' EXIT

home="$case_dir/home"
stubs="$case_dir/stubs"
bin="$case_dir/bin"
calls_log="$case_dir/calls.log"
state="$case_dir/state"
bash_bin="$(command -v bash)"
real_date="$(command -v date)"
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

assert_exists() { [[ -e $1 ]] || fail "$2 (missing: $1)"; }
assert_absent() { [[ ! -e $1 ]] || fail "$2 (still there: $1)"; }

# notifications: the Notify calls of the last run, one per line.
notifications() {
    printf '%s\n' "$calls" | grep -F 'Notify' || true
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

write_stub gdbus <<'EOF'
printf 'gdbus' >> "$STUB_LOG"
printf ' [%s]' "$@" >> "$STUB_LOG"
printf '\n' >> "$STUB_LOG"
[[ "$*" == *.Notify* ]] && printf '(uint32 41,)\n'
exit 0
EOF

write_stub notify-send <<'EOF'
case ${1:-} in
--help*) printf '%s\n' '-A, --action=[NAME=]Text'; exit 0 ;;
esac
printf 'notify-send' >> "$STUB_LOG"
printf ' [%s]' "$@" >> "$STUB_LOG"
printf '\n' >> "$STUB_LOG"
exit 0
EOF

write_stub systemd-run <<'EOF'
printf 'systemd-run' >> "$STUB_LOG"
printf ' [%s]' "$@" >> "$STUB_LOG"
printf '\n' >> "$STUB_LOG"
exit 0
EOF

write_stub xdg-terminal-exec <<'EOF'
printf 'xdg-terminal-exec %s\n' "$*" >> "$STUB_LOG"
exit 0
EOF

# The shell --fix-failed opens at the end: it only reports where it landed.
write_stub fix-shell <<'EOF'
printf 'fix-shell [%s]\n' "$PWD" >> "$STUB_LOG"
exit 0
EOF

# The user the case pretends to be.
write_stub id <<'EOF'
case $1 in
-u) echo 1000 ;;
-un) echo lara ;;
esac
EOF

write_stub uname <<'EOF'
echo Linux
EOF

# A clock that moves 11 seconds each time it is read, so the ten second delay of a bar passes at once.
write_stub date <<EOF
if [[ \${1:-} == +%s && -n \${STUB_CLOCK:-} ]]; then
    now=\$(( \$(<"\$STUB_CLOCK") + 11 ))
    echo "\$now" > "\$STUB_CLOCK"
    echo "\$now"
else
    exec "$real_date" "\$@"
fi
EOF

###############################################################
# => Harness
###############################################################

# run_job SCRIPT ARGS...: run an automation on a PATH of stubs and the few real tools it needs.
# case_tools names the stubs that are "installed"; case_env is extra environment.
# Sets `status`, `output`, `calls`.
run_job() {
    local name=$1 tool
    shift
    rm -rf "$bin"
    mkdir -p "$bin"
    for tool in env mktemp rm rmdir mkdir date grep tail tr tee mkfifo sed awk cat sort dirname find git head wc touch; do
        ln -sf "$(command -v "$tool")" "$bin/$tool"
    done
    for tool in "${case_tools[@]}"; do
        ln -sf "$stubs/$tool" "$bin/$tool"
    done
    : >"$calls_log"
    status=0
    output="$(
        env -i HOME="$home" PATH="$bin" TMPDIR="$case_dir/tmp" XDG_STATE_HOME="$state" \
            DBUS_SESSION_BUS_ADDRESS=unix:path=/test/bus STUB_LOG="$calls_log" "${case_env[@]}" \
            "$bash_bin" "$automation/$name" "$@" 2>&1
    )" || status=$?
    calls="$(<"$calls_log")"
}

reset_world() {
    rm -rf "$home" "$state" "$case_dir/tmp" "$case_dir/clock"
    mkdir -p "$home" "$state" "$case_dir/tmp"
    printf '[user]\n\tname = Test\n\temail = test@example.invalid\n[init]\n\tdefaultBranch = main\n' >"$home/.gitconfig"
    case_tools=(id gdbus uname)
    case_env=(STUB_RUN=1)
}

with_clock() {
    printf '1000\n' >"$case_dir/clock"
    case_tools+=(date)
    case_env+=("STUB_CLOCK=$case_dir/clock")
}

last_success() { printf '%s' "$state/cuberhaus-automations/$1.last-success"; }

###############################################################
# => workspace-pull
###############################################################

git_quiet() { git -C "$1" "${@:2}" >/dev/null 2>&1; }

# make_workspace: a workspace with three repositories under $case_dir/work:
#   alpha  has a new commit upstream (a pull updates it)
#   beta   is up to date
#   gamma  has no upstream (a pull skips it)
make_workspace() {
    local work="$case_dir/work" name
    workspace=$work
    rm -rf "$work" "$case_dir/remotes" "$case_dir/pusher"
    mkdir -p "$work" "$case_dir/remotes"
    for name in alpha beta; do
        git init -q --bare -b main "$case_dir/remotes/$name.git"
        git clone -q "$case_dir/remotes/$name.git" "$work/$name" 2>/dev/null
        git_quiet "$work/$name" checkout -B main
        printf '%s\n' one >"$work/$name/file"
        git_quiet "$work/$name" add file
        git_quiet "$work/$name" -c user.name=T -c user.email=t@e.invalid commit -m first
        git_quiet "$work/$name" push -u origin main
    done
    git init -q -b main "$work/gamma"
    git clone -q "$case_dir/remotes/alpha.git" "$case_dir/pusher" 2>/dev/null
    printf '%s\n' two >"$case_dir/pusher/file"
    git_quiet "$case_dir/pusher" -c user.name=T -c user.email=t@e.invalid commit -am second
    git_quiet "$case_dir/pusher" push origin main
}

reset_world
make_workspace
run_job workspace-pull --root "$workspace" --notify
assert_equals 0 "$status" 'a pull with something to update must succeed'
assert_contains "$output" '[PULL]' 'the pull output must still reach the log'
assert_equals two "$(<"$workspace/alpha/file")" 'alpha must have been fast-forwarded'
assert_contains "$output" 'Workspace pull checked 3 repositories with 0 failure(s).' 'the log line must be unchanged'
assert_equals 1 "$(count_of "$(notifications)" 'Notify')" 'a run that updated something must send one summary'
assert_contains "$calls" "['Workspace pull finished']" 'the summary must carry the title'
assert_contains "$calls" '1 updated, 1 already up to date.\n1 skipped (no upstream).' 'the summary must count updated, current and skipped repositories'
assert_exists "$(last_success workspace-pull)" 'a good run must record its time'

# Nothing left to update: silent, and still a success.
rm -f "$(last_success workspace-pull)"
run_job workspace-pull --root "$workspace" --notify
assert_equals 0 "$status" 'a pull with nothing to update must succeed'
assert_equals '' "$calls" 'a pull with nothing to update must stay silent'
assert_exists "$(last_success workspace-pull)" 'a quiet run is still a success'

# Without --notify nothing is shown, whatever happened.
make_workspace
run_job workspace-pull --root "$workspace"
assert_equals 0 "$status" 'a plain pull must succeed'
assert_equals '' "$calls" 'a pull without --notify must not notify'

# A diverged repository cannot be fast-forwarded: a warning, the job fails, no last-success.
make_workspace
printf '%s\n' local >"$workspace/beta/local-only"
git_quiet "$workspace/beta" add local-only
git_quiet "$workspace/beta" -c user.name=T -c user.email=t@e.invalid commit -m local
git clone -q "$case_dir/remotes/beta.git" "$case_dir/pusher2" 2>/dev/null
printf '%s\n' upstream >"$case_dir/pusher2/upstream-only"
git_quiet "$case_dir/pusher2" add upstream-only
git_quiet "$case_dir/pusher2" -c user.name=T -c user.email=t@e.invalid commit -m upstream
git_quiet "$case_dir/pusher2" push origin main
rm -rf "$case_dir/pusher2" "$state"
mkdir -p "$state"
run_job workspace-pull --root "$workspace" --notify
assert_equals 1 "$status" 'a repository that cannot be fast-forwarded must fail the job, as before'
assert_contains "$output" '[FAIL]' 'the failure must still be logged'
assert_contains "$calls" "['Workspace pull finished with problems']" 'a failure must show'
assert_contains "$calls" 'Failed: beta.' 'the notification must name the repository'
assert_absent "$(last_success workspace-pull)" 'a failed run must not count as a success'

# With a terminal launcher the same failure is clickable, saves the failed paths for the click,
# and a clean run afterwards clears them again.
make_workspace
printf '%s\n' local >"$workspace/beta/local-only"
git_quiet "$workspace/beta" add local-only
git_quiet "$workspace/beta" -c user.name=T -c user.email=t@e.invalid commit -m local
git clone -q "$case_dir/remotes/beta.git" "$case_dir/pusher2" 2>/dev/null
printf '%s\n' upstream >"$case_dir/pusher2/upstream-only"
git_quiet "$case_dir/pusher2" add upstream-only
git_quiet "$case_dir/pusher2" -c user.name=T -c user.email=t@e.invalid commit -m upstream
git_quiet "$case_dir/pusher2" push origin main
rm -rf "$case_dir/pusher2" "$state"
mkdir -p "$state"
case_tools+=(notify-send systemd-run xdg-terminal-exec)
run_job workspace-pull --root "$workspace" --notify
assert_equals 1 "$status" 'a diverged repository must still fail the job when the summary is clickable'
failures_file="$state/cuberhaus-automations/workspace-pull.last-failures"
assert_equals "$workspace/beta" "$(<"$failures_file")" 'the click needs the failed repository paths saved'
assert_contains "$calls" 'systemd-run' 'a clickable failure must leave a waiter behind'
assert_contains "$calls" '[button=Fix in terminal]' 'the failure summary must offer the fix in a terminal'
assert_contains "$calls" '--fix-failed' 'the click must run workspace-pull in fix mode'
assert_contains "$calls" "/bin/bash $automation/workspace-pull --fix-failed" 'the click must run the script through /bin/bash, as the service does, since it is not executable'
assert_equals 0 "$(count_of "$(notifications)" 'Notify')" 'the clickable summary replaces the plain one'

# The click itself: a terminal shell in the failed repository, with its status explained.
case_env+=("CUBERHAUS_FIX_SHELL=$stubs/fix-shell")
run_job workspace-pull --fix-failed
assert_equals 0 "$status" 'fix mode must open its shell'
assert_contains "$output" 'could not be pulled' 'fix mode must list the failed repositories'
assert_contains "$output" 'ahead 1' 'fix mode must show why the pull failed'
assert_contains "$output" 'git pull --ff-only' 'fix mode must say how to retry'
assert_contains "$calls" "fix-shell [$workspace/beta]" 'fix mode must land the shell in the failed repository'

make_workspace
case_tools=(id gdbus uname)
case_env=(STUB_RUN=1)
run_job workspace-pull --root "$workspace" --notify
assert_equals 0 "$status" 'the clean run after a failure must succeed'
assert_absent "$failures_file" 'a clean run must clear the saved failures'

# A missing workspace is a failure with a message.
reset_world
run_job workspace-pull --root "$case_dir/nowhere" --notify
assert_equals 1 "$status" 'a missing workspace must fail'
assert_contains "$calls" "['Workspace pull failed']" 'a missing workspace must show as a failure'
assert_contains "$calls" "Workspace directory does not exist: $case_dir/nowhere" 'the failure must say why'

# A recent success is skipped before any notification starts.
reset_world
mkdir -p "$state/cuberhaus-automations"
"$real_date" +%s >"$(last_success workspace-pull)"
run_job workspace-pull --root "$case_dir/nowhere" --if-due-seconds 3600 --notify
assert_equals 0 "$status" 'a run that is not due must succeed'
assert_contains "$output" 'completed recently. Skipping.' 'a run that is not due must say so'
assert_equals '' "$calls" 'a run that is not due must not notify'

# A long run shows a bar that follows the repositories, and the summary replaces it.
make_workspace
reset_world
with_clock
run_job workspace-pull --root "$workspace" --notify
assert_contains "$calls" 'Pulling alpha' 'the bar must name the repository being pulled'
assert_contains "$calls" '<int32 33>' 'one of three repositories done must fill a third of the bar'
last=$(notifications | tail -n 1)
assert_contains "$last" "['Workspace pull finished']" 'the last notification must be the summary'
assert_contains "$last" '[uint32 41]' 'the summary must replace the bar'

# The lock still keeps two pulls apart, and a lock left by a run that died is cleaned up by the trap.
reset_world
make_workspace
mkdir -p "$case_dir/tmp/cuberhaus-workspace-pull.lock"
run_job workspace-pull --root "$workspace" --notify
assert_contains "$output" 'Another workspace pull is running. Skipping.' 'a held lock must skip the run'
assert_equals '' "$calls" 'a skipped run must not notify'
rmdir "$case_dir/tmp/cuberhaus-workspace-pull.lock"
run_job workspace-pull --root "$workspace" --notify
assert_absent "$case_dir/tmp/cuberhaus-workspace-pull.lock" 'a finished run must release the lock'

run_job workspace-pull --bogus
assert_equals 2 "$status" 'an unknown option must exit with 2'
run_job workspace-pull --help
assert_contains "$output" 'Usage: workspace-pull' '--help must show the usage'

printf 'automation notification tests passed.\n'
