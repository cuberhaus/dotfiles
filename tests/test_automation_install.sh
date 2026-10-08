#!/usr/bin/env bash
# Tests for the Linux paths of the automation install, uninstall and observe scripts.
#
# systemctl, sudo and journalctl are stubs that only record their arguments, uname and the WSL
# check are faked, and HOME is a throwaway folder that holds a copy of what Stow would link, so
# nothing is installed, started or removed. REPO_ROOT points the test at a mutated copy of the
# repository.

set -euo pipefail

repo_root="${REPO_ROOT:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
automation="$repo_root/.local/scripts/automation"
case_dir="$(mktemp -d)"
trap 'rm -rf "$case_dir"' EXIT
bin="$case_dir/bin"
home="$case_dir/home"
calls="$case_dir/calls.log"

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

assert_status() { # expected message
    [[ $status == "$1" ]] || fail "$2: expected exit status $1, got $status; output: $output"
}

assert_output() { # needle message
    [[ $output == *"$1"* ]] || fail "$2: expected '$1' in: $output"
}

refute_output() { # needle message
    [[ $output != *"$1"* ]] || fail "$2: unexpected '$1' in: $output"
}

assert_logged() { # exact line, message
    grep -Fxq -- "$1" "$calls" || fail "$2: missing the line '$1' in: $(cat "$calls")"
}

refute_logged() { # substring, message
    ! grep -Fq -- "$1" "$calls" || fail "$2: unexpected '$1' in: $(cat "$calls")"
}

assert_nothing_logged() { # message
    [[ ! -s $calls ]] || fail "$1: expected no command to run, got: $(cat "$calls")"
}

###############################################################
# => Stubs
###############################################################

mkdir -p "$bin"
real_grep="$(command -v grep)"
cat >"$bin/grep" <<EOF
#!/usr/bin/env bash
# The scripts ask whether /proc/version mentions Microsoft (WSL); the answer is no unless STUB_WSL is set.
for argument in "\$@"; do
    if [[ \$argument == /proc/version ]]; then
        [[ -n \${STUB_WSL:-} ]] && exit 0
        exit 1
    fi
done
exec "$real_grep" "\$@"
EOF
for tool in systemctl sudo journalctl; do
    cat >"$bin/$tool" <<EOF
#!/usr/bin/env bash
printf '$tool %s\n' "\$*" >>"$calls"
EOF
done
cat >"$bin/uname" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "${STUB_UNAME:-Linux}"
EOF
chmod +x "$bin"/*

extra_env=(STUB_UNAME=Linux)

# deploy: what `make restow` leaves in the home folder.
deploy() {
    rm -rf "$home"
    mkdir -p "$home/.local/scripts" "$home/.config/systemd"
    cp -R "$repo_root/.local/scripts/automation" "$repo_root/.local/scripts/lib" "$home/.local/scripts/"
    cp -R "$repo_root/.config/systemd/user" "$home/.config/systemd/user"
    cp -R "$repo_root/.config/cuberhaus-automations" "$home/.config/cuberhaus-automations"
}

# run SCRIPT [ARGS...]: sets $status and $output (stdout and stderr), and records the stub calls.
run() {
    local script=$1
    shift
    : >"$calls"
    status=0
    output=$(env HOME="$home" XDG_STATE_HOME="$home/.local/state" PATH="$bin:$PATH" "${extra_env[@]}" \
        bash "$automation/$script" "$@" 2>&1) || status=$?
}

user_timers='cuberhaus-user-package-maintenance.timer cuberhaus-workspace-pull.timer cuberhaus-inbox-collector.timer cuberhaus-desktop-shortcut-cleanup.timer'
root_files='/etc/systemd/system/cuberhaus-system-maintenance.service /etc/systemd/system/cuberhaus-system-maintenance.timer /usr/local/libexec/cuberhaus-system-maintenance /usr/local/libexec/cuberhaus-progress.sh'

###############################################################
# => install
###############################################################

deploy
run install
assert_status 0 'install'
assert_output 'automation schedules are installed' 'install says it finished'
assert_logged 'systemctl --user daemon-reload' 'install reloads the user manager'
assert_logged "systemctl --user enable --now $user_timers" 'install enables all four user timers'
assert_logged 'sudo install -d -m 0755 /usr/local/libexec' 'install makes the root-owned folder'
assert_logged "sudo install -m 0755 $automation/system-maintenance /usr/local/libexec/cuberhaus-system-maintenance" \
    'install copies the root job'
assert_logged "sudo install -m 0644 $automation/../lib/cuberhaus-progress.sh /usr/local/libexec/cuberhaus-progress.sh" \
    'install copies the notification library next to the root job, not executable'
refute_logged 'install -m 0755 '"$automation"'/../lib' 'the library is data, not a program'
assert_logged "sudo install -m 0644 $repo_root/.local/share/cuberhaus-automations/systemd/cuberhaus-system-maintenance.service $repo_root/.local/share/cuberhaus-automations/systemd/cuberhaus-system-maintenance.timer /etc/systemd/system/" \
    'install copies the root units'
assert_logged 'sudo systemctl enable --now cuberhaus-system-maintenance.timer' 'install enables the root timer'

# A file that Stow has not linked yet stops the install before anything is enabled or copied as root.
for missing in \
    .config/cuberhaus-automations/inbox-collector-sources.txt \
    .config/systemd/user/cuberhaus-desktop-shortcut-cleanup.timer \
    .local/scripts/lib/cuberhaus-progress.sh \
    .local/scripts/automation/inbox-collector; do
    deploy
    rm "$home/$missing"
    run install
    assert_status 1 "install without $missing"
    assert_output "$missing" "install names the missing $missing"
    assert_output 'make restow' "install tells how to fix a missing $missing"
    assert_nothing_logged "install without $missing"
done

extra_env=(STUB_WSL=1)
deploy
run install
assert_status 0 'install under WSL'
assert_output 'WSL detected' 'install under WSL'
assert_nothing_logged 'install under WSL'
extra_env=(STUB_UNAME=Linux)

###############################################################
# => uninstall
###############################################################

deploy
run uninstall --dry-run
assert_status 0 'uninstall --dry-run'
assert_output "[dry-run] systemctl --user disable --now $user_timers" 'the dry run lists the four user timers'
assert_output '[dry-run] sudo systemctl disable --now cuberhaus-system-maintenance.timer' 'the dry run lists the root timer'
assert_output "[dry-run] sudo rm -f $root_files" 'the dry run lists the root files, library included'
assert_nothing_logged 'uninstall --dry-run'

run uninstall
assert_status 0 'uninstall'
assert_logged "systemctl --user disable --now $user_timers" 'uninstall disables the four user timers'
assert_logged 'sudo systemctl disable --now cuberhaus-system-maintenance.timer' 'uninstall disables the root timer'
assert_logged "sudo rm -f $root_files" 'uninstall removes the root job and its library'
assert_output 'Logs and state were preserved' 'uninstall keeps logs and state'

###############################################################
# => observe
###############################################################

state="$home/.local/state/cuberhaus-automations"
mkdir -p "$state"
printf '%s\n' 1700000000 >"$state/inbox-collector.last-success"
run observe digest
assert_status 0 'observe digest'
for name in user-package-maintenance workspace-pull inbox-collector desktop-shortcut-cleanup; do
    assert_output "$name" "the Linux digest lists $name"
done
never_ran='(^|'$'\n'')desktop-shortcut-cleanup +never recorded('$'\n''|$)'
[[ $output =~ $never_ran ]] || fail "a task that never ran says so: $output"
# 1700000000 is 2023-11-14 22:13 UTC, the 14th or the 15th depending on the time zone.
recorded='(^|'$'\n'')inbox-collector +2023-11-1[45] '
[[ $output =~ $recorded ]] || fail "a recorded run shows its time: $output"

extra_env=(STUB_UNAME=Darwin)
run observe digest
assert_status 0 'observe digest on macOS'
assert_output 'workspace-pull' 'the macOS digest lists workspace-pull'
refute_output 'inbox-collector' 'the Linux-only tasks are not listed on macOS'
refute_output 'desktop-shortcut-cleanup' 'the Linux-only tasks are not listed on macOS'
extra_env=(STUB_UNAME=Linux)

run observe logs
assert_status 0 'observe logs'
assert_logged 'journalctl -n 100 --no-pager -u cuberhaus-system-maintenance.service' 'the root journal'
assert_logged 'journalctl --user -n 100 --no-pager -u cuberhaus-user-package-maintenance.service -u cuberhaus-workspace-pull.service -u cuberhaus-inbox-collector.service -u cuberhaus-desktop-shortcut-cleanup.service' \
    'the user journals of all four jobs'

printf 'automation install tests passed.\n'
