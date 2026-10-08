#!/usr/bin/env bash
# Tests for --notify in workspace-pull, user-package-maintenance and system-maintenance
# (.local/scripts/automation/).
#
# They guard what turns a background job into one you can follow without a terminal:
#   - without --notify a job is exactly as silent as before;
#   - with it, a job that changed something ends with one summary, and one that had nothing to do
#     stays silent, but a failure always shows and still fails the job;
#   - the bar follows the job (repositories, packages, apt's status feed, pacman's transaction) and
#     the summary replaces it, never the other way round;
#   - the system job runs as root, so its notification goes to the desktop user's session;
#   - what each job did before is untouched: pulls, exit statuses, the last-success file, the lock.
#
# Every case runs on a restricted PATH: the stubs below plus a few real tools. The real apt-get,
# pacman, brew, yay and notification daemon cannot be reached, so nothing is installed or shown.

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
data="$case_dir/data"
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

assert_not_contains() {
    [[ $1 != *"$2"* ]] || fail "$3 (unexpected: $2) in:"$'\n'"$1"
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
        printf 'DATA=%q\n' "$data"
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

# The uid the case pretends to be: 1000 by default, root with STUB_UID=0.
write_stub id <<'EOF'
case $1 in
-u) echo "${STUB_UID:-1000}" ;;
-un) echo "${STUB_USER:-lara}" ;;
esac
EOF

# One active graphical session, for the system job to find.
write_stub loginctl <<'EOF'
case $1 in
list-sessions) echo 'c1 1000 alice seat0 -' ;;
show-session) printf 'Type=wayland\nState=active\nName=alice\nUser=1000\n' ;;
esac
EOF
write_stub runuser <<'EOF'
printf 'runuser [%s]\n' "$2" >> "$STUB_LOG"
shift 3
exec "$@"
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

write_stub brew <<'EOF'
printf 'brew %s\n' "$*" >> "$STUB_LOG"
case $1 in
update) [[ -e $DATA/brew.update.fail ]] && exit 1 ;;
outdated) cat "$DATA/brew.outdated" 2>/dev/null || true ;;
upgrade)
    cat "$DATA/brew.upgrade" 2>/dev/null || true
    [[ -e $DATA/brew.upgrade.fail ]] && exit 1
    ;;
esac
exit 0
EOF

write_stub yay <<'EOF'
printf 'yay %s\n' "$*" >> "$STUB_LOG"
case $1 in
-Qua) cat "$DATA/yay.outdated" 2>/dev/null || true ;;
-Sua)
    cat "$DATA/yay.upgrade" 2>/dev/null || true
    [[ -e $DATA/yay.upgrade.fail ]] && exit 1
    ;;
esac
exit 0
EOF

# apt-get writes its status feed to file descriptor 3, as APT::Status-Fd=3 asks.
write_stub apt-get <<'EOF'
printf 'apt-get %s\n' "$*" >> "$STUB_LOG"
case $1 in
update)
    [[ -e $DATA/apt.update.fail ]] && exit 1
    echo 'Hit:1 http://archive.example/ stable InRelease'
    ;;
full-upgrade)
    [[ $* == *Status-Fd=3* ]] || exit 64
    cat "$DATA/apt.status" >&3 2>/dev/null || true
    cat "$DATA/apt.stdout" 2>/dev/null || true
    [[ -e $DATA/apt.upgrade.fail ]] && exit 100
    ;;
esac
exit 0
EOF

write_stub pacman <<'EOF'
printf 'pacman %s\n' "$*" >> "$STUB_LOG"
cat "$DATA/pacman.out" 2>/dev/null || true
[[ -e $DATA/pacman.fail ]] && exit 1
exit 0
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
            DBUS_SESSION_BUS_ADDRESS=unix:path=/test/bus STUB_LOG="$calls_log" \
            CUBERHAUS_LOCK_DIR="$case_dir/system.lock" "${case_env[@]}" \
            "$bash_bin" "$automation/$name" "$@" 2>&1
    )" || status=$?
    calls="$(<"$calls_log")"
}

reset_world() {
    rm -rf "$home" "$state" "$data" "$case_dir/tmp" "$case_dir/system.lock" "$case_dir/clock"
    mkdir -p "$home" "$state" "$data" "$case_dir/tmp"
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

###############################################################
# => user-package-maintenance
###############################################################

brew_output='==> Upgrading 2 outdated packages:
wget 1.21 -> 1.24
jq 1.6 -> 1.7
==> Upgrading wget
==> Pouring wget--1.24.bottle.tar.gz
==> Upgrading jq
==> Pouring jq--1.7.bottle.tar.gz'

reset_world
case_tools+=(brew)
printf 'wget\njq\n' >"$data/brew.outdated"
printf '%s\n' "$brew_output" >"$data/brew.upgrade"
run_job user-package-maintenance --notify
assert_equals 0 "$status" 'an upgrade must succeed'
assert_contains "$output" '==> Upgrading wget' 'the brew output must still reach the log'
assert_contains "$calls" 'brew update' 'brew update must run'
assert_contains "$calls" 'brew upgrade' 'brew upgrade must run'
assert_equals 1 "$(count_of "$(notifications)" 'Notify')" 'an upgrade must end with one summary'
assert_contains "$calls" "['User package maintenance finished']" 'the summary must carry the title'
assert_contains "$calls" 'Upgraded 2 packages.' 'the summary must count the packages'
assert_exists "$(last_success user-package-maintenance)" 'a good run must record its time'

# The bar follows the "==> Upgrading <name>" lines, not the header that precedes them.
reset_world
case_tools+=(brew)
with_clock
printf 'wget\njq\n' >"$data/brew.outdated"
printf '%s\n' "$brew_output" >"$data/brew.upgrade"
run_job user-package-maintenance --notify
assert_contains "$calls" 'Upgrading wget' 'the bar must name the package being upgraded'
assert_contains "$calls" 'Upgrading 2 of 2' 'the status line must count the packages of the whole run'
assert_contains "$calls" '<int32 50>' 'the second of two packages must show half the bar done'
assert_not_contains "$calls" 'Upgrading 3 of 2' 'the header line must not count as a package'
last=$(notifications | tail -n 1)
assert_contains "$last" "['User package maintenance finished']" 'the last notification must be the summary'
assert_contains "$last" '[uint32 41]' 'the summary must replace the bar'

# Nothing outdated: silent, still recorded.
reset_world
case_tools+=(brew)
run_job user-package-maintenance --notify
assert_equals 0 "$status" 'a run with nothing to upgrade must succeed'
assert_equals 0 "$(count_of "$(notifications)" 'Notify')" 'a run with nothing to upgrade must not show a banner'
assert_exists "$(last_success user-package-maintenance)" 'a quiet run is still a success'

# A failing upgrade fails the job, as before, and shows.
reset_world
case_tools+=(brew)
printf 'wget\n' >"$data/brew.outdated"
: >"$data/brew.upgrade.fail"
run_job user-package-maintenance --notify
assert_equals 1 "$status" 'a failing brew upgrade must fail the job'
assert_contains "$calls" "['User package maintenance failed']" 'a failure must show'
assert_contains "$calls" 'brew upgrade failed.' 'the failure must say which step'
assert_absent "$(last_success user-package-maintenance)" 'a failed run must not count as a success'

reset_world
case_tools+=(brew)
: >"$data/brew.update.fail"
run_job user-package-maintenance --notify
assert_equals 1 "$status" 'a failing brew update must fail the job'
assert_contains "$calls" 'brew update failed.' 'the failure must say which step'

# Homebrew and yay share one bar: the AUR packages count after the Homebrew ones.
reset_world
case_tools+=(brew yay)
with_clock
printf 'wget\n' >"$data/brew.outdated"
printf '==> Upgrading wget\n' >"$data/brew.upgrade"
printf 'aur-one 1 -> 2\naur-two 3 -> 4\n' >"$data/yay.outdated"
printf '==> Making package: aur-one 2-1 (Thu 08 Oct 2026)\n==> Making package: aur-two 4-1 (Thu 08 Oct 2026)\n' >"$data/yay.upgrade"
run_job user-package-maintenance --notify
assert_equals 0 "$status" 'a run with both managers must succeed'
assert_contains "$calls" 'yay -Sua --noconfirm --needed' 'yay must run with its usual arguments'
assert_contains "$calls" 'Upgrading aur-two' 'the bar must name the AUR package being built'
assert_contains "$calls" 'Upgrading 3 of 3' 'the AUR packages must count after the Homebrew ones'
assert_contains "$calls" 'Upgraded 3 packages.' 'the summary must add both managers up'

# No package manager: nothing to do, silent, still recorded.
reset_world
run_job user-package-maintenance --notify
assert_equals 0 "$status" 'a machine with no manager must succeed'
assert_contains "$output" 'No supported user package manager found' 'the log must say so'
assert_equals 0 "$(count_of "$(notifications)" 'Notify')" 'no manager must not show a banner'

# --if-due-seconds still works, in either position.
reset_world
case_tools+=(brew)
mkdir -p "$state/cuberhaus-automations"
"$real_date" +%s >"$(last_success user-package-maintenance)"
run_job user-package-maintenance --notify --if-due-seconds 3600
assert_contains "$output" 'completed recently. Skipping.' '--if-due-seconds must work after --notify'
run_job user-package-maintenance --if-due-seconds 3600 --notify
assert_contains "$output" 'completed recently. Skipping.' '--if-due-seconds must work before --notify'
assert_equals '' "$calls" 'a run that is not due must not call brew or notify'

# Without --notify nothing is shown.
reset_world
case_tools+=(brew)
printf 'wget\n' >"$data/brew.outdated"
printf '==> Upgrading wget\n' >"$data/brew.upgrade"
run_job user-package-maintenance
assert_equals 0 "$(count_of "$(notifications)" 'Notify')" 'a run without --notify must not notify'

run_job user-package-maintenance --bogus
assert_equals 2 "$status" 'an unknown option must exit with 2'

###############################################################
# => system-maintenance
###############################################################

root_env=(STUB_UID=0 STUB_USER=root)
system_tools=(id gdbus uname loginctl runuser)

apt_status='dlstatus:1:0.0000:Retrieving file 1 of 2
dlstatus:1:100.0000:Retrieving file 2 of 2
pmstatus:dpkg-exec:0:Running dpkg
pmstatus:libfoo:50.0000:Unpacking libfoo (2.0) over (1.0) ...
pmstatus:libbar:100.0000:Setting up libbar (3.0) ...'
apt_stdout='Reading package lists...
3 upgraded, 1 newly installed, 0 to remove and 2 not upgraded.'

reset_world
case_tools=("${system_tools[@]}" apt-get)
case_env=("${root_env[@]}")
with_clock
printf '%s\n' "$apt_status" >"$data/apt.status"
printf '%s\n' "$apt_stdout" >"$data/apt.stdout"
run_job system-maintenance --notify
assert_equals 0 "$status" 'an apt upgrade must succeed'
assert_contains "$calls" 'apt-get update' 'apt-get update must run'
assert_contains "$calls" 'apt-get full-upgrade -y -o APT::Status-Fd=3' 'apt-get full-upgrade must keep its arguments and add the status feed'
assert_contains "$output" '3 upgraded, 1 newly installed' 'apt output must still reach the log'
assert_not_contains "$output" 'pmstatus' 'the status feed must not leak into the log'
assert_contains "$calls" 'runuser [alice]' 'root must notify the desktop user'
assert_contains "$calls" 'Unpacking libfoo' 'the bar must show what dpkg is doing'
assert_contains "$calls" '<int32 70>' 'half way through the install must show 40 + 50 * 60 / 100 percent after a download'
assert_contains "$calls" 'Retrieving file 2 of 2 (100 %)' 'the download must be named while it runs'
last=$(notifications | tail -n 1)
assert_contains "$last" "['System package maintenance finished']" 'the last notification must be the summary, not a late bar'
assert_contains "$last" '3 upgraded, 1 newly installed, 0 removed.' 'the summary must count what apt did'
assert_contains "$last" '[uint32 41]' 'the summary must replace the bar'
assert_absent "$case_dir/system.lock" 'a finished run must release the lock'
assert_equals 0 "$(find "$case_dir/tmp" -mindepth 1 | wc -l)" 'a finished run must remove its temporary files'

# Without a download the install fills the whole bar.
reset_world
case_tools=("${system_tools[@]}" apt-get)
case_env=("${root_env[@]}")
with_clock
printf 'pmstatus:libfoo:50.0000:Unpacking libfoo\n' >"$data/apt.status"
printf '1 upgraded, 0 newly installed, 0 to remove and 0 not upgraded.\n' >"$data/apt.stdout"
run_job system-maintenance --notify
assert_contains "$calls" '<int32 50>' 'with nothing to download, 50 percent of the install is half the bar'

# Nothing to upgrade: silent.
reset_world
case_tools=("${system_tools[@]}" apt-get)
case_env=("${root_env[@]}")
printf '0 upgraded, 0 newly installed, 0 to remove and 0 not upgraded.\n' >"$data/apt.stdout"
run_job system-maintenance --notify
assert_equals 0 "$status" 'a run with nothing to upgrade must succeed'
assert_equals 0 "$(count_of "$(notifications)" 'Notify')" 'a run with nothing to upgrade must not show a banner'

# Failures show and still fail the job.
reset_world
case_tools=("${system_tools[@]}" apt-get)
case_env=("${root_env[@]}")
: >"$data/apt.upgrade.fail"
run_job system-maintenance --notify
assert_equals 1 "$status" 'a failing full-upgrade must fail the job'
assert_contains "$calls" "['System package maintenance failed']" 'a failure must show'
assert_contains "$calls" 'apt-get full-upgrade failed.' 'the failure must say which step'
assert_absent "$case_dir/system.lock" 'a failed run must release the lock'

reset_world
case_tools=("${system_tools[@]}" apt-get)
case_env=("${root_env[@]}")
: >"$data/apt.update.fail"
run_job system-maintenance --notify
assert_equals 1 "$status" 'a failing apt-get update must fail the job'
assert_contains "$calls" 'apt-get update failed.' 'the failure must say which step'

# pacman: the bar follows the "(n/m) upgrading <package>" lines of the transaction.
pacman_out=$'resolving dependencies...\n:: Processing package changes...\n(1/2) upgrading foo\r(2/2) upgrading bar\n(1/3) Arming ConditionNeedsUpdate...'
reset_world
case_tools=("${system_tools[@]}" pacman)
case_env=("${root_env[@]}")
with_clock
printf '%s\n' "$pacman_out" >"$data/pacman.out"
run_job system-maintenance --notify
assert_equals 0 "$status" 'a pacman upgrade must succeed'
assert_contains "$calls" 'pacman -Syu --noconfirm --noprogressbar' 'pacman must keep its arguments and drop the progress bar'
assert_contains "$output" '(1/2) upgrading foo' 'the pacman output must still reach the log'
assert_contains "$calls" 'Upgrading bar' 'the bar must name the package'
assert_contains "$calls" 'Upgrading 2 of 2' 'the status line must count the transaction'
assert_not_contains "$calls" 'Arming' 'a hook line must not count as a package'
last=$(notifications | tail -n 1)
assert_contains "$last" 'Upgraded 2 packages.' 'the summary must count the packages'

reset_world
case_tools=("${system_tools[@]}" pacman)
case_env=("${root_env[@]}")
printf ' there is nothing to do\n' >"$data/pacman.out"
run_job system-maintenance --notify
assert_equals 0 "$status" 'a run with nothing to upgrade must succeed'
assert_equals 0 "$(count_of "$(notifications)" 'Notify')" 'a pacman run with nothing to do must stay silent'

reset_world
case_tools=("${system_tools[@]}" pacman)
case_env=("${root_env[@]}")
: >"$data/pacman.fail"
run_job system-maintenance --notify
assert_equals 1 "$status" 'a failing pacman must fail the job'
assert_contains "$calls" 'pacman -Syu failed.' 'the failure must say which step'

# No package manager at all.
reset_world
case_tools=("${system_tools[@]}")
case_env=("${root_env[@]}")
run_job system-maintenance --notify
assert_equals 1 "$status" 'a machine with no package manager must fail, as before'
assert_contains "$output" 'No supported system package manager found' 'the log must say so'
assert_contains "$calls" "['System package maintenance failed']" 'the failure must show'

# Nobody logged in: the job still runs, nothing is shown.
reset_world
case_tools=(id gdbus uname apt-get)
case_env=("${root_env[@]}")
printf '%s\n' "$apt_stdout" >"$data/apt.stdout"
run_job system-maintenance --notify
assert_equals 0 "$status" 'a machine with nobody logged in must still be maintained'
assert_equals 0 "$(count_of "$calls" 'gdbus')" 'nobody to notify, so no call to the daemon'

# Without --notify the job is silent; and it still refuses to run as anyone but root.
reset_world
case_tools=("${system_tools[@]}" apt-get)
case_env=("${root_env[@]}")
printf '%s\n' "$apt_stdout" >"$data/apt.stdout"
run_job system-maintenance
assert_equals 0 "$status" 'a plain run must succeed'
assert_equals 0 "$(count_of "$calls" 'gdbus')" 'a run without --notify must not notify'

reset_world
case_tools=("${system_tools[@]}" apt-get)
run_job system-maintenance --notify
assert_equals 1 "$status" 'a user must not be able to run the system job'
assert_contains "$output" 'must run as root' 'the refusal must say why'
assert_not_contains "$calls" 'apt-get' 'a refused run must not touch apt'

# A held lock skips the run.
reset_world
case_tools=("${system_tools[@]}" apt-get)
case_env=("${root_env[@]}")
mkdir -p "$case_dir/system.lock"
run_job system-maintenance --notify
assert_contains "$output" 'Another system package maintenance process is running.' 'a held lock must skip the run'
assert_equals '' "$calls" 'a skipped run must not touch apt or notify'

run_job system-maintenance --bogus
assert_equals 2 "$status" 'an unknown option must exit with 2'

printf 'automation notification tests passed.\n'
