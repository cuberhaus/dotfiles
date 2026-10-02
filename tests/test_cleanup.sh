#!/usr/bin/env bash
# Tests for .local/scripts/bin/cleanup.
#
# They guard what the old one-string alias got wrong:
#   - Docker pruned the build cache, then images, then containers, so an image that
#     a stopped container still used survived the image prune. Containers go first.
#   - A stopped Docker daemon printed one error per Docker command. It is checked once.
#   - apt-get autoclean ran right before apt-get clean, which removes everything
#     autoclean would.
#   - Arguments typed after the alias were appended to its last Docker command, so
#     there was no --dry-run and no --help.
#   - Arch and Manjaro ran the `yay` wrapper function (yay -S --noconfirm --needed)
#     instead of the yay binary, and cleaned no pacman cache without yay.
#
# And the rules for Docker volumes, which hold data that nothing can rebuild:
#   - Only volumes that no container uses are offered, one at a time, with what Docker
#     knows about them.
#   - Only an explicit "y" deletes. Enter, n, q, an unreadable answer and the end of
#     the input all keep the volume.
#   - A volume is removed by name, without -f, and `docker volume prune` is never used.
#   - With no terminal to ask on, or under --dry-run, nothing is deleted.
#   - The "l" answer lists the volume from a throwaway container that mounts it
#     read-only, without network and without pulling an image.
#
# Every case runs in a clean `env -i` shell with a throwaway HOME. PATH holds only
# stub commands (plus the real awk and cat), so nothing real (sudo, apt-get, snap,
# docker) can start and a tool that is not stubbed behaves as not installed.
#
# The answers to the prompts are typed by a small Python helper on a pseudo-terminal,
# because the script only asks when stdin and stdout are terminals.

# The scripts handed to the child shells below are single-quoted on purpose: their
# variables must be expanded by the child shell, not by this one.
# shellcheck disable=SC2016

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
script="$repo_root/.local/scripts/bin/cleanup"
aliases="$repo_root/.config/zsh/aliases"
case_dir="$(mktemp -d)"
trap 'rm -rf "$case_dir"' EXIT

home="$case_dir/home"
stubs="$case_dir/stubs" # every stub command; a case links the ones it installs into $bin
bin="$case_dir/bin"
state_dir="$case_dir/state"
calls_log="$case_dir/calls.log"
bash_bin="$(command -v bash)"
awk_bin="$(command -v awk)"
cat_bin="$(command -v cat)"
sleep_bin="$(command -v sleep)"
mkdir -p "$home" "$stubs"

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
    [[ $1 == *"$2"* ]] || fail "$3 (missing: $2)"
}

assert_not_contains() {
    [[ $1 != *"$2"* ]] || fail "$3 (unexpected: $2)"
}

# section TEXT FROM UNTIL: the lines of TEXT from the line FROM up to, not including, UNTIL.
section() {
    printf '%s\n' "$1" | "$awk_bin" -v from="$2" -v until_line="$3" \
        '$0 == from { on = 1 } $0 == until_line { on = 0 } on'
}

###############################################################
# => Stub commands
###############################################################

# write_stub NAME: the script body comes from stdin.
write_stub() {
    {
        printf '#!%s\n' "$bash_bin"
        cat
    } > "$stubs/$1"
    chmod +x "$stubs/$1"
}

# These log their command line and fail when it starts with $STUB_FAIL. None of them
# does any work, so even the sudo stub cannot start apt-get.
for name in sudo nvim pip3 yarn npm uv brew gem yay pacman; do
    write_stub "$name" <<'EOF'
printf '%s %s\n' "${0##*/}" "$*" >> "$STUB_LOG"
[[ "${0##*/} $*" == "${STUB_FAIL:-@none@}"* ]] && exit 1
exit 0
EOF
done

# `docker info` is the daemon probe: $STUB_DOCKER_INFO_STATUS is its exit status.
# Volumes: $STUB_VOLUMES lists the unused ones, and the case statements below hold what
# Docker would report about them. Images: $STUB_IMAGES lists the local ones, and those
# in $STUB_NO_LS_IMAGES have no ls (the exit status of a missing executable, 127).
# $STUB_DF_FAIL makes `docker system df` fail (no sizes).
write_stub docker <<'EOF'
printf 'docker %s\n' "$*" >> "$STUB_LOG"
[[ $1 == info ]] && exit "${STUB_DOCKER_INFO_STATUS:-0}"
if [[ "docker $*" == "${STUB_FAIL:-@none@}"* ]]; then
    printf 'Error response from daemon: %s failed\n' "$*" >&2
    exit 1
fi
case "$1 ${2:-}" in
    'volume ls')
        for volume in ${STUB_VOLUMES:-}; do
            printf '%s\n' "$volume"
        done
        ;;
    'volume inspect')
        # $STUB_INSPECT_DELAY (seconds) makes Docker slow to answer, so a test can type
        # keys while the script is still working.
        [[ -n ${STUB_INSPECT_DELAY:-} ]] && sleep "$STUB_INSPECT_DELAY"
        # The volume is the last argument. Fields: created, driver, Compose project,
        # Compose volume, options (joined by the unit separator, as the script asks).
        for volume; do :; done
        case "$volume" in
            app_pgdata) fields=(2026-09-07T10:41:23+02:00 local app pgdata '') ;;
            nfs_share) fields=(2026-09-07T10:41:23+02:00 nfs '' '' 'type=nfs o=addr=10.0.0.5 ') ;;
            *) fields=(2026-09-07T10:41:23+02:00 local '' '' '') ;;
        esac
        printf '%s\037%s\037%s\037%s\037%s\n' "${fields[@]}"
        ;;
    'system df')
        [[ -n ${STUB_DF_FAIL:-} ]] && exit 1
        printf '%s\t%s\n' \
            in_use_volume 3.2GB \
            app_pgdata 11.11GB \
            scratch_data 1.5MB \
            many_files 120kB \
            nfs_share 0B \
            9a1a0c3068656c37c200d22f268ffe28eaaf797d0e7f49196a7515d2f72300c8 11.11GB
        ;;
    'image ls')
        for image in ${STUB_IMAGES:-}; do
            printf '%s\n' "$image"
        done
        ;;
    run*)
        # The peek: --mount type=volume,source=NAME,... IMAGE -A -1 -F /cleanup-peek
        name='' image='' previous=''
        for argument; do
            if [[ $argument == type=volume,source=* ]]; then
                name=${argument#*source=}
                name=${name%%,*}
            fi
            [[ $argument == -A ]] && image=$previous
            previous=$argument
        done
        [[ " ${STUB_NO_LS_IMAGES:-} " == *" $image "* ]] && exit 127
        case "$name" in
            app_pgdata) printf 'PG_VERSION\nbase/\nglobal/\n' ;;
            many_files)
                for number in {1..20}; do
                    printf 'file%s\n' "$number"
                done
                ;;
        esac
        ;;
esac
exit 0
EOF

# Two disabled revisions (a base snap and an application) among current ones.
write_stub snap <<'EOF'
printf 'snap %s\n' "$*" >> "$STUB_LOG"
if [[ $* == 'list --all' ]]; then
    printf 'Name     Version   Rev   Tracking       Publisher   Notes\n'
    printf 'core18   20250101  2999  latest/stable  canonical✓  base,disabled\n'
    printf 'core18   20250201  3000  latest/stable  canonical✓  base\n'
    printf 'firefox  140.0     4900  latest/stable  mozilla✓    disabled\n'
    printf 'firefox  141.0     4950  latest/stable  mozilla✓    -\n'
    printf 'snapd    2.70      100   latest/stable  canonical✓  snapd\n'
fi
exit 0
EOF

# Each call reports $STUB_DF_DELTA_KIB (default 3 GiB) more free space than the previous
# one, starting at 100 GiB, and lists the same filesystem twice, the way df does for / and
# $HOME on one partition. With $STUB_DF_HOME_MOUNT, $HOME is instead on a second filesystem
# mounted there, which starts at 50 GiB.
write_stub df <<'EOF'
printf 'df %s\n' "$*" >> "$STUB_LOG"
calls=0
[[ -f $STUB_STATE/df.count ]] && read -r calls < "$STUB_STATE/df.count"
calls=$((calls + 1))
printf '%s\n' "$calls" > "$STUB_STATE/df.count"
moved=$(((calls - 1) * ${STUB_DF_DELTA_KIB:-3145728}))
printf 'Filesystem 1024-blocks Used Available Capacity Mounted on\n'
printf '/dev/test 999999999 1 %s 1%% /\n' $((104857600 + moved))
if [[ -n ${STUB_DF_HOME_MOUNT:-} ]]; then
    printf '/dev/test2 999999999 1 %s 1%% %s\n' $((52428800 + moved)) "$STUB_DF_HOME_MOUNT"
else
    printf '/dev/test 999999999 1 %s 1%% /\n' $((104857600 + moved))
fi
EOF

# A fixed clock, so a volume made at 10:41 is always 25 days old. -d fails when
# $STUB_DATE_FAIL is set, the way BSD and macOS date do.
write_stub date <<'EOF'
now=1790000000
if [[ ${1:-} == -d ]]; then
    [[ -n ${STUB_DATE_FAIL:-} ]] && exit 1
    printf '%s\n' $((now - 25 * 86400 - 3600))
else
    printf '%s\n' "$now"
fi
EOF

###############################################################
# => Pseudo-terminal helper
###############################################################

python_bin=''
if command -v python3 > /dev/null 2>&1; then
    python_bin="$(command -v python3)"
else
    printf 'SKIP: python3 is not installed; the interactive volume prompts are not tested.\n'
fi

pty_helper="$case_dir/pty_session.py"
cat > "$pty_helper" <<'EOF'
"""Run a command on a pseudo-terminal and type the replies to its prompts.

usage: pty_session.py OUTPUT_FILE [EXPECT SEND]... -- COMMAND [ARG]...

Waits until EXPECT (plain text) appears in the output after the previous match, then
types SEND. Everything the command printed goes to OUTPUT_FILE with CRLF turned into LF.
Exits with the command's status, 124 when it hangs and 125 when it ends before every
EXPECT appeared.
"""
import os
import pty
import select
import signal
import sys
import time

TIMEOUT = 20.0


def main():
    args = sys.argv[1:]
    output_file = args[0]
    separator = args.index('--')
    steps = args[1:separator]
    command = args[separator + 1:]
    pending = [(steps[i].encode(), steps[i + 1].encode()) for i in range(0, len(steps), 2)]

    pid, fd = pty.fork()
    if pid == 0:
        try:
            os.execv(command[0], command)
        finally:
            os._exit(127)

    captured = b''
    searched_from = 0
    timed_out = False
    deadline = time.monotonic() + TIMEOUT
    while True:
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            timed_out = True
            break
        readable, _, _ = select.select([fd], [], [], min(remaining, 1.0))
        if not readable:
            continue
        try:
            chunk = os.read(fd, 4096)
        except OSError:  # Linux reports the end of the output as EIO
            break
        if not chunk:
            break
        captured += chunk
        while pending:
            index = captured.find(pending[0][0], searched_from)
            if index < 0:
                break
            searched_from = index + len(pending[0][0])
            try:
                os.write(fd, pending[0][1])
            except OSError:
                pass
            pending.pop(0)

    if timed_out:
        os.kill(pid, signal.SIGKILL)
    _, wait_status = os.waitpid(pid, 0)
    if os.WIFEXITED(wait_status):
        code = os.WEXITSTATUS(wait_status)
    else:
        code = 128 + os.WTERMSIG(wait_status)

    text = captured.decode('utf-8', errors='replace').replace('\r\n', '\n').replace('\r', '')
    with open(output_file, 'w', encoding='utf-8') as handle:
        handle.write(text)
    if timed_out:
        sys.stderr.write('pty_session: the command did not finish in time\n')
        sys.exit(124)
    if pending:
        sys.stderr.write('pty_session: the command ended before %r appeared\n' % pending[0][0].decode())
        sys.exit(125)
    sys.exit(code)


main()
EOF

###############################################################
# => Harness
###############################################################

# set_machine DISTRO OSTYPE TOOL...
# Describe the machine of the next run: its distribution, its OSTYPE and the stubbed
# tools that are "installed". Extra environment for the stubs goes in case_env.
set_machine() {
    case_distro=$1
    case_ostype=$2
    shift 2
    case_tools=("$@")
    case_env=(NO_COLOR=1)
    tty_steps=()
    rm -rf "$home/.config"
}

# prepare_case: the clean PATH of the next run, and an empty call log.
prepare_case() {
    local tool
    rm -rf "$bin" "$state_dir"
    mkdir -p "$bin" "$state_dir"
    ln -s "$awk_bin" "$bin/awk"
    ln -s "$cat_bin" "$bin/cat"
    ln -s "$sleep_bin" "$bin/sleep"
    ln -s "$stubs/date" "$bin/date"
    for tool in ${case_tools[@]+"${case_tools[@]}"}; do
        ln -s "$stubs/$tool" "$bin/$tool"
    done
    : > "$calls_log"
}

# in_case_env COMMAND...: run COMMAND in an otherwise empty environment on the machine
# set_machine described. The OSTYPE bash would set itself is overridden through the environment.
in_case_env() {
    env -i HOME="$home" PATH="$bin" TERM=dumb OSTYPE="$case_ostype" DISTRO="$case_distro" \
        STUB_LOG="$calls_log" STUB_STATE="$state_dir" "${case_env[@]}" "$@"
}

# run_cleanup [ARGS...]
# Run `cleanup ARGS` with no terminal. Sets `status` (its exit status), `output` (stdout
# and stderr) and `calls` (what the stubs saw).
run_cleanup() {
    prepare_case
    status=0
    output="$(in_case_env "$bash_bin" "$script" "$@" 2>&1 < /dev/null)" || status=$?
    calls="$(< "$calls_log")"
}

# run_cleanup_tty [ARGS...]
# Run `cleanup ARGS` on a pseudo-terminal and type tty_steps into it: pairs of the prompt
# text to wait for and the keys to send. Sets the same variables as run_cleanup.
run_cleanup_tty() {
    local out_file="$case_dir/tty.out"
    [[ -n $python_bin ]] || return 1
    prepare_case
    status=0
    in_case_env "$python_bin" "$pty_helper" "$out_file" ${tty_steps[@]+"${tty_steps[@]}"} -- \
        "$bash_bin" "$script" "$@" || status=$?
    output="$(< "$out_file")"
    calls="$(< "$calls_log")"
    if [[ $status == 124 || $status == 125 ]]; then
        fail "the terminal session did not go as scripted (status $status). It printed:
$output"
    fi
}

df_call="df -Pk / $home"
volume_prompt='[q]uit: '
volume_list_call='docker volume ls --filter dangling=true --format {{.Name}}'
volume_sizes_call='docker system df -v --format {{range .Volumes}}{{.Name}}\t{{.Size}}\n{{end}}'
anonymous_volume=9a1a0c3068656c37c200d22f268ffe28eaaf797d0e7f49196a7515d2f72300c8

###############################################################
# => The command itself
###############################################################

[[ -x $script ]] || fail 'cleanup must be executable'
assert_equals '#!/usr/bin/env bash' "$(sed -n 1p "$script")" \
    'cleanup must start with the env bash shebang'
[[ "$(sed -n 2p "$script")" == '# Description: '?* ]] ||
    fail 'line 2 of cleanup must be a "# Description: ..." line: the commands catalog shows it'

# A function or alias named cleanup would shadow the script in every zsh session.
if grep -Eq '^[[:space:]]*(alias[[:space:]]+cleanup=|function[[:space:]]+cleanup\b|cleanup[[:space:]]*\(\))' "$aliases"; then
    fail "$aliases must not define cleanup: it would shadow .local/scripts/bin/cleanup"
fi

# Run through its shebang, the way a shell finds it on PATH: env finds bash on PATH.
set_machine ubuntu linux-gnu
prepare_case
ln -s "$bash_bin" "$bin/bash"
status=0
output="$(in_case_env "$script" --help 2>&1 < /dev/null)" || status=$?
assert_equals 0 "$status" 'cleanup must run when executed directly'
assert_contains "$output" 'Usage: cleanup [-n|--dry-run]' 'cleanup must run when executed directly'

###############################################################
# => Ubuntu: every step, in order
###############################################################

ubuntu_tools=(sudo brew nvim pip3 yarn npm uv docker snap df)

ubuntu_calls="$df_call
sudo apt-get clean
sudo apt-get autoremove
snap list --all
sudo snap remove core18 --revision=2999
sudo snap remove firefox --revision=4900
brew cleanup
nvim +PlugClean +qall
pip3 cache purge
yarn cache clean
npm cache clean --force
uv cache prune
docker info
docker container prune -f --filter until=168h
$volume_list_call
docker image prune -af --filter until=168h
docker builder prune -af --filter until=168h
$df_call"

set_machine ubuntu linux-gnu "${ubuntu_tools[@]}"
run_cleanup
assert_equals 0 "$status" 'a clean run must succeed'
assert_equals "$ubuntu_calls" "$calls" \
    'on Ubuntu cleanup must run every step once, in order: containers before volumes before images, disabled snaps only, no autoclean, df without -h'
# One header per step, then the free space once (df lists / and $HOME separately even
# though they share a filesystem here). Output that is not a terminal has no colour codes.
assert_equals "==> apt package cache
==> unused apt packages (asks first)
==> disabled snap core18 (revision 2999)
==> disabled snap firefox (revision 4900)
==> Homebrew cache
==> Neovim plugins missing from the config (asks first)
==> pip cache
==> yarn cache
==> npm cache
==> uv cache
==> Docker stopped containers
==> Docker unused volumes
    none
==> Docker unused images
==> Docker build cache
Free space on /: 100.00 GiB before, 103.00 GiB after (3.0 GiB more)" "$output" \
    'a clean run must print one header per step, "none" for no unused volumes, and the free space once, and nothing else'

###############################################################
# => --dry-run: only read-only probes run
###############################################################

for flag in --dry-run -n; do
    set_machine ubuntu linux-gnu "${ubuntu_tools[@]}"
    run_cleanup "$flag"
    assert_equals 0 "$status" "$flag: a preview must succeed"
    assert_equals "snap list --all
docker info
$volume_list_call" "$calls" \
        "$flag: a preview may only probe (snap list, docker info, docker volume ls); it must not call sudo, df or any pruning tool"
    assert_contains "$output" 'would run: sudo apt-get clean' "$flag: a preview must show the apt step"
    assert_contains "$output" 'would run: sudo snap remove firefox --revision=4900' \
        "$flag: a preview must list the disabled snap revisions it found"
    assert_contains "$output" 'would run: docker container prune -f --filter until=168h' \
        "$flag: a preview must show the Docker steps"
    assert_contains "$output" 'Dry run: nothing was changed.' "$flag: a preview must say it changed nothing"
    assert_not_contains "$output" 'Free space on' \
        "$flag: a preview frees nothing, so it must not report free space"
done

###############################################################
# => Docker
###############################################################

set_machine ubuntu linux-gnu sudo docker df
case_env=(NO_COLOR=1 STUB_DOCKER_INFO_STATUS=1)
run_cleanup
assert_equals 0 "$status" 'a stopped Docker daemon is skipped, not a failure'
assert_equals "$df_call
sudo apt-get clean
sudo apt-get autoremove
docker info
$df_call" "$calls" 'with the daemon down cleanup must probe it once and prune nothing'
assert_contains "$output" 'daemon is not reachable' 'skipping Docker must say why'

set_machine ubuntu linux-gnu sudo docker df
case_env=(NO_COLOR=1 'STUB_FAIL=docker image prune')
run_cleanup
assert_contains "$calls" 'docker builder prune' 'a failed Docker step must not stop the steps after it'

###############################################################
# => A failing step does not stop the rest
###############################################################

set_machine ubuntu linux-gnu "${ubuntu_tools[@]}"
case_env=(NO_COLOR=1 'STUB_FAIL=npm cache clean')
run_cleanup
assert_equals 1 "$status" 'cleanup must fail when a step fails'
assert_equals "$ubuntu_calls" "$calls" 'steps after a failed one must still run'
assert_contains "$output" 'Did not finish: npm cache' 'cleanup must name the step that failed'

###############################################################
# => Other platforms
###############################################################

# Arch defines a yay() wrapper (yay -S --noconfirm --needed) in the zsh aliases; cleanup
# must run the binary, which a script does by not being a function.
set_machine arch linux-gnu sudo yay pacman nvim df
run_cleanup
assert_equals "$df_call
yay -Sc
nvim +PlugClean +qall
$df_call" "$calls" 'on Arch cleanup must run yay -Sc'

# Without yay nothing would clean the pacman cache.
set_machine manjaro linux-gnu sudo pacman nvim df
run_cleanup
assert_equals "$df_call
sudo pacman -Sc
nvim +PlugClean +qall
$df_call" "$calls" 'on Manjaro without yay cleanup must fall back to pacman'

set_machine '' darwin23 sudo brew gem nvim df
run_cleanup
assert_equals "$df_call
brew cleanup
sudo gem cleanup
nvim +PlugClean +qall
$df_call" "$calls" 'on macOS cleanup must clean Homebrew and RubyGems, not apt'

# WSL: apt applies, snap does not exist there.
set_machine ubuntu_windows linux-gnu sudo df
run_cleanup
assert_equals "$df_call
sudo apt-get clean
sudo apt-get autoremove
$df_call" "$calls" 'on Ubuntu for Windows cleanup must run the apt steps'

# A distribution without package-manager steps still cleans the caches.
set_machine gentoo linux-gnu sudo nvim df
run_cleanup
assert_equals "$df_call
nvim +PlugClean +qall
$df_call" "$calls" 'on an unlisted distribution cleanup must skip the package-manager steps'

###############################################################
# => The distribution comes from ~/.config/distro when the environment lacks it
###############################################################

# zsh exports DISTRO at startup; bash, ssh and cron sessions only have the file.
set_machine '' linux-gnu sudo df
mkdir -p "$home/.config"
printf '#! /bin/sh\nexport DISTRO=ubuntu\n' > "$home/.config/distro"
run_cleanup
assert_equals "$df_call
sudo apt-get clean
sudo apt-get autoremove
$df_call" "$calls" 'without DISTRO in the environment cleanup must read ~/.config/distro'

set_machine arch linux-gnu sudo pacman df
mkdir -p "$home/.config"
printf '#! /bin/sh\nexport DISTRO=ubuntu\n' > "$home/.config/distro"
run_cleanup
assert_equals "$df_call
sudo pacman -Sc
$df_call" "$calls" 'DISTRO from the environment must win over ~/.config/distro'

###############################################################
# => The free-space line
###############################################################

# It must read without explanation: both measurements, labelled, and the change in words.
# The old "76.4 GiB (+96 MiB)" was taken for a before-and-after pair or for a saving.
# A distribution without package steps and no other tool leaves only this line.
set_machine gentoo linux-gnu df
run_cleanup
assert_equals 'Free space on /: 100.00 GiB before, 103.00 GiB after (3.0 GiB more)' "$output" \
    'the report must show the free space before and after the run, and the change in words'

# The change is in the largest unit that fits, and says which way it went.
for scenario in \
    'STUB_DF_DELTA_KIB=98304|Free space on /: 100.00 GiB before, 100.09 GiB after (96 MiB more)' \
    'STUB_DF_DELTA_KIB=300|Free space on /: 100.00 GiB before, 100.00 GiB after (300 KiB more)' \
    'STUB_DF_DELTA_KIB=0|Free space on /: 100.00 GiB before, 100.00 GiB after (unchanged)' \
    'STUB_DF_DELTA_KIB=-1048576|Free space on /: 100.00 GiB before, 99.00 GiB after (1.0 GiB less)' \
    'STUB_DF_DELTA_KIB=-5120|Free space on /: 100.00 GiB before, 100.00 GiB after (5 MiB less)'; do
    set_machine gentoo linux-gnu df
    case_env=(NO_COLOR=1 "${scenario%%|*}")
    run_cleanup
    assert_equals 0 "$status" "${scenario%%|*}: a report is not a failure"
    assert_equals "${scenario#*|}" "$output" "${scenario%%|*}: the change must be stated in words, in a fitting unit"
done

# A home directory on its own filesystem is a second disk and gets a second line.
set_machine gentoo linux-gnu df
case_env=(NO_COLOR=1 STUB_DF_HOME_MOUNT=/home)
run_cleanup
assert_equals 'Free space on /: 100.00 GiB before, 103.00 GiB after (3.0 GiB more)
Free space on /home: 50.00 GiB before, 53.00 GiB after (3.0 GiB more)' "$output" \
    'a home on its own filesystem must get its own line, and one on the same filesystem as / must not'

###############################################################
# => Tools that are not installed are skipped quietly
###############################################################

set_machine ubuntu linux-gnu sudo df
run_cleanup
assert_equals 0 "$status" 'missing tools are not an error'
assert_not_contains "$output" 'Docker' 'a tool that is not installed must not be mentioned'
assert_not_contains "$output" 'snap' 'a tool that is not installed must not be mentioned'

###############################################################
# => Usage
###############################################################

set_machine ubuntu linux-gnu "${ubuntu_tools[@]}"
run_cleanup --help
assert_equals 0 "$status" '--help must succeed'
assert_contains "$output" 'Usage: cleanup [-n|--dry-run]' '--help must print the usage'
assert_contains "$output" 'Docker volumes' '--help must explain what happens to volumes'
assert_equals '' "$calls" '--help must run nothing'

set_machine ubuntu linux-gnu "${ubuntu_tools[@]}"
run_cleanup --bogus
assert_equals 2 "$status" 'an unknown option must be a usage error'
assert_contains "$output" 'unknown option: --bogus' 'the bad option must be named'
assert_equals '' "$calls" 'an unknown option must run nothing'

###############################################################
# => Docker volumes without a terminal: listed, never deleted
###############################################################

volume_tools=(sudo docker df)

set_machine ubuntu linux-gnu "${volume_tools[@]}"
case_env=(NO_COLOR=1 STUB_VOLUMES=app_pgdata)
run_cleanup
assert_equals 0 "$status" 'listing unused volumes is not a failure'
assert_equals '==> Docker unused volumes
    1 volume is not used by any container.
    Deleting a volume destroys its data for good.

    [1/1] app_pgdata
          11.11GB, created 2026-09-07 10:41 (25 days ago)
          named volume of Compose project "app" (volume "pgdata")
          driver local, used by no container

    No terminal to ask on, so nothing was deleted. Run cleanup in a terminal to choose.' \
    "$(section "$output" '==> Docker unused volumes' '==> Docker unused images')" \
    'an unused volume must be described with its size, age, origin and driver, and left alone without a terminal'
assert_contains "$calls" "$volume_sizes_call" 'sizes come from one docker system df -v call'
assert_contains "$calls" 'docker volume inspect --format ' 'details come from docker volume inspect'
assert_not_contains "$calls" 'docker volume rm' 'without a terminal no volume may be deleted'
assert_not_contains "$calls" 'docker volume prune' 'docker volume prune deletes everything at once and must never run'
assert_not_contains "$calls" 'docker run' 'without a terminal nothing may start a container'
assert_contains "$calls" 'docker image prune' 'the steps after the volumes must still run'

# Three volumes: a named one, an anonymous one and one from a driver that keeps its data elsewhere.
set_machine ubuntu linux-gnu "${volume_tools[@]}"
case_env=(NO_COLOR=1 "STUB_VOLUMES=scratch_data $anonymous_volume nfs_share")
run_cleanup
volumes_output="$(section "$output" '==> Docker unused volumes' '==> Docker unused images')"
assert_contains "$volumes_output" '3 volumes are not used by any container.' \
    'the number of unused volumes must be stated'
assert_contains "$volumes_output" '[1/3] scratch_data' 'volumes must be numbered'
assert_contains "$volumes_output" '          1.5MB, created 2026-09-07 10:41 (25 days ago)
          named volume
' 'a plain named volume must say so and show its own size, not another volume'"'"'s'
assert_contains "$volumes_output" "[2/3] $anonymous_volume" 'the anonymous volume must be listed in full'
assert_contains "$volumes_output" 'anonymous volume (a container asked Docker for one and never named it)' \
    'a volume with a 64-character hexadecimal name must be called anonymous'
assert_contains "$volumes_output" '[3/3] nfs_share' 'volumes must be numbered'
assert_contains "$volumes_output" 'named volume, options: type=nfs o=addr=10.0.0.5' \
    'driver options must be shown'
assert_contains "$volumes_output" 'driver nfs, used by no container (its data may live off this machine)' \
    'a volume from another driver must warn that its data may live elsewhere'
assert_not_contains "$calls" 'docker volume rm' 'without a terminal no volume may be deleted'

# A hexadecimal name of any other length is a name somebody chose, not Docker's.
set_machine ubuntu linux-gnu "${volume_tools[@]}"
case_env=(NO_COLOR=1 STUB_VOLUMES=deadbeef)
run_cleanup
assert_contains "$output" '          named volume
' 'a short hexadecimal name is still a named volume'
assert_not_contains "$output" 'anonymous' 'only a 64-character hexadecimal name marks an anonymous volume'

# Missing information is reported as missing, not guessed.
set_machine ubuntu linux-gnu "${volume_tools[@]}"
case_env=(NO_COLOR=1 STUB_VOLUMES=app_pgdata STUB_DF_FAIL=1 STUB_DATE_FAIL=1)
run_cleanup
assert_equals 0 "$status" 'missing sizes and ages are not a failure'
assert_contains "$output" '          size unknown, created 2026-09-07 10:41
' 'without sizes and an age the volume must still be described'
assert_not_contains "$output" 'days ago' 'an age date could not compute must not be invented'

# A failing listing is a failure of that step only.
set_machine ubuntu linux-gnu "${volume_tools[@]}"
case_env=(NO_COLOR=1 'STUB_FAIL=docker volume ls')
run_cleanup
assert_equals 1 "$status" 'a failed volume listing must fail cleanup'
assert_contains "$output" '!! Docker unused volumes did not finish' 'the failed listing must be reported'
assert_contains "$output" 'Error response from daemon' "Docker's own error must be shown"
assert_contains "$output" 'Did not finish: Docker unused volumes' 'the failed step must be named in the summary'
assert_contains "$calls" 'docker image prune' 'a failed listing must not stop the steps after it'
assert_not_contains "$calls" 'docker volume rm' 'a failed listing must delete nothing'

# --dry-run lists them as well, and says what it could not see yet.
set_machine ubuntu linux-gnu docker
case_env=(NO_COLOR=1 STUB_VOLUMES=app_pgdata)
run_cleanup --dry-run
assert_contains "$output" '[1/1] app_pgdata' 'a preview must show the unused volumes'
assert_contains "$output" 'Would ask before deleting each of them.' 'a preview must say it only asks'
assert_contains "$output" 'Volumes held only by the stopped containers pruned above are not listed yet.' \
    'a preview must say that its container prune did not run, so more volumes may become unused'
assert_not_contains "$calls" 'docker volume rm' 'a preview must delete nothing'
assert_not_contains "$calls" 'docker container prune' 'a preview must not prune containers'

###############################################################
# => Docker volumes on a terminal: asked one by one
###############################################################

if [[ -n $python_bin ]]; then
    # y deletes exactly the volume that was asked about, by name and without -f.
    set_machine ubuntu linux-gnu "${volume_tools[@]}"
    case_env=(NO_COLOR=1 STUB_VOLUMES=app_pgdata)
    tty_steps=("$volume_prompt" $'y\n')
    run_cleanup_tty
    assert_equals 0 "$status" 'deleting a volume must succeed'
    assert_equals '==> Docker unused volumes
    1 volume is not used by any container.
    Deleting a volume destroys its data for good.

    [1/1] app_pgdata
          11.11GB, created 2026-09-07 10:41 (25 days ago)
          named volume of Compose project "app" (volume "pgdata")
          driver local, used by no container
    Delete this volume? [y]es / [N]o / [l]ist contents / [q]uit: y
==> Delete Docker volume app_pgdata

    Deleted 1 of 1 unused volumes.' \
        "$(section "$output" '==> Docker unused volumes' '==> Docker unused images')" \
        'the terminal session must show the volume, ask, delete it and count it'
    assert_contains "$calls" $'\ndocker volume rm app_pgdata\n' \
        'y must delete the volume by name, without -f'
    assert_equals 1 "$(printf '%s\n' "$calls" | grep -c '^docker volume rm')" 'exactly one volume may be deleted'
    assert_not_contains "$calls" 'docker volume prune' 'docker volume prune deletes everything at once and must never run'
    assert_contains "$calls" 'docker image prune' 'the steps after the volumes must still run'

    # Anything but an explicit y keeps the volume. Enter is the default and means no.
    for answer in $'\n' $'n\n' $'N\n' $'no\n'; do
        set_machine ubuntu linux-gnu "${volume_tools[@]}"
        case_env=(NO_COLOR=1 STUB_VOLUMES=app_pgdata)
        tty_steps=("$volume_prompt" "$answer")
        run_cleanup_tty
        assert_equals 0 "$status" "answer '${answer%$'\n'}': keeping a volume is not a failure"
        assert_not_contains "$calls" 'docker volume rm' "answer '${answer%$'\n'}' must keep the volume"
        assert_contains "$output" 'Deleted 0 of 1 unused volumes.' "answer '${answer%$'\n'}' must be counted as no"
        assert_contains "$calls" 'docker image prune' "answer '${answer%$'\n'}': the steps after the volumes must still run"
    done

    # Every accepted spelling of yes.
    for answer in $'Y\n' $'yes\n' $'Yes\n' $'YES\n'; do
        set_machine ubuntu linux-gnu "${volume_tools[@]}"
        case_env=(NO_COLOR=1 STUB_VOLUMES=app_pgdata)
        tty_steps=("$volume_prompt" "$answer")
        run_cleanup_tty
        assert_contains "$calls" $'\ndocker volume rm app_pgdata\n' "answer '${answer%$'\n'}' must mean yes"
    done

    # Two volumes: the answers are per volume, in order.
    set_machine ubuntu linux-gnu "${volume_tools[@]}"
    case_env=(NO_COLOR=1 'STUB_VOLUMES=scratch_data app_pgdata')
    tty_steps=("$volume_prompt" $'n\n' "$volume_prompt" $'y\n')
    run_cleanup_tty
    assert_contains "$output" '[1/2] scratch_data' 'the first volume must be asked about first'
    assert_contains "$output" '[2/2] app_pgdata' 'the second volume must be asked about second'
    assert_not_contains "$calls" 'docker volume rm scratch_data' 'n must keep the first volume'
    assert_contains "$calls" $'\ndocker volume rm app_pgdata\n' 'y must delete the second volume'
    assert_contains "$output" 'Deleted 1 of 2 unused volumes.' 'the count must match the answers'

    # q stops asking; the volumes not asked about are kept.
    set_machine ubuntu linux-gnu "${volume_tools[@]}"
    case_env=(NO_COLOR=1 'STUB_VOLUMES=scratch_data app_pgdata')
    tty_steps=("$volume_prompt" $'q\n')
    run_cleanup_tty
    assert_equals 0 "$status" 'q is not a failure'
    assert_not_contains "$calls" 'docker volume rm' 'q must keep every volume'
    assert_contains "$output" 'Skipping the remaining volumes.' 'q must say it stops asking'
    assert_not_contains "$output" '[2/2]' 'q must not ask about the next volume'
    assert_contains "$calls" 'docker image prune' 'q must not stop the steps after the volumes'

    # q on the last volume has nothing left to skip.
    set_machine ubuntu linux-gnu "${volume_tools[@]}"
    case_env=(NO_COLOR=1 STUB_VOLUMES=app_pgdata)
    tty_steps=("$volume_prompt" $'q\n')
    run_cleanup_tty
    assert_not_contains "$output" 'Skipping the remaining volumes.' 'q on the last volume must not claim to skip any'
    assert_contains "$output" 'Deleted 0 of 1 unused volumes.' 'q on the last volume must keep it'

    # Keys typed before the question is on screen must not answer it: a y meant for an
    # earlier prompt (one Enter too many at apt or nvim) would delete data. The slow
    # docker volume inspect gives the helper time to type the y while the script is busy.
    set_machine ubuntu linux-gnu "${volume_tools[@]}"
    case_env=(NO_COLOR=1 STUB_VOLUMES=app_pgdata STUB_INSPECT_DELAY=0.5)
    tty_steps=('==> Docker unused volumes' $'y\n' "$volume_prompt" $'n\n')
    run_cleanup_tty
    assert_equals 0 "$status" 'discarding typed-ahead keys is not a failure'
    assert_not_contains "$calls" 'docker volume rm' 'a y typed before the question appeared must not delete the volume'
    assert_contains "$output" 'Deleted 0 of 1 unused volumes.' 'only the answer given after the question counts'

    # The end of the input (Ctrl-D) is a quit, not a yes.
    set_machine ubuntu linux-gnu "${volume_tools[@]}"
    case_env=(NO_COLOR=1 'STUB_VOLUMES=scratch_data app_pgdata')
    tty_steps=("$volume_prompt" $'\004')
    run_cleanup_tty
    assert_equals 0 "$status" 'the end of the input is not a failure'
    assert_not_contains "$calls" 'docker volume rm' 'the end of the input must keep every volume'
    assert_contains "$output" 'Skipping the remaining volumes.' 'the end of the input must stop asking'

    # An unreadable answer is asked again, and a later y still works.
    set_machine ubuntu linux-gnu "${volume_tools[@]}"
    case_env=(NO_COLOR=1 STUB_VOLUMES=app_pgdata)
    tty_steps=("$volume_prompt" $'maybe\n' "$volume_prompt" $'y\n')
    run_cleanup_tty
    assert_contains "$output" 'Please answer y, n, l or q.' 'an unreadable answer must be explained'
    assert_equals 1 "$(printf '%s\n' "$calls" | grep -c '^docker volume rm')" \
        'an unreadable answer must delete nothing; the following y deletes the volume once'

    # A volume that is still in use makes docker refuse. That is a failed step, reported, and
    # the other volumes and steps carry on. Nothing forces the removal.
    set_machine ubuntu linux-gnu "${volume_tools[@]}"
    case_env=(NO_COLOR=1 'STUB_VOLUMES=app_pgdata scratch_data' 'STUB_FAIL=docker volume rm app_pgdata')
    tty_steps=("$volume_prompt" $'y\n' "$volume_prompt" $'y\n')
    run_cleanup_tty
    assert_equals 1 "$status" 'a volume docker refuses to delete must fail cleanup'
    assert_contains "$output" '!! Delete Docker volume app_pgdata did not finish' 'the refusal must be reported'
    assert_contains "$output" 'Did not finish: Delete Docker volume app_pgdata' 'the refusal must be named in the summary'
    assert_contains "$calls" $'\ndocker volume rm scratch_data\n' 'the next volume must still be offered and deleted'
    assert_contains "$output" 'Deleted 1 of 2 unused volumes.' 'only the volume that was removed counts'
    assert_not_contains "$calls" 'docker volume rm -f' 'a refused removal must never be forced'
    assert_contains "$calls" 'docker image prune' 'the steps after the volumes must still run'

    # Under --dry-run nothing is asked, even on a terminal.
    set_machine ubuntu linux-gnu "${volume_tools[@]}"
    case_env=(NO_COLOR=1 STUB_VOLUMES=app_pgdata)
    run_cleanup_tty --dry-run
    assert_equals 0 "$status" 'a preview must succeed on a terminal'
    assert_not_contains "$output" 'Delete this volume?' 'a preview must not ask'
    assert_contains "$output" 'Would ask before deleting each of them.' 'a preview must say it only asks'
    assert_not_contains "$calls" 'docker volume rm' 'a preview must delete nothing'

    ###########################################################
    # => The "l" answer: what is in the volume?
    ###########################################################

    # The listing comes from the first local image that has ls, through a read-only,
    # offline mount that is never pulled. noimg has no ls; okimg has.
    set_machine ubuntu linux-gnu "${volume_tools[@]}"
    case_env=(NO_COLOR=1 STUB_VOLUMES=app_pgdata 'STUB_IMAGES=noimg okimg spare' STUB_NO_LS_IMAGES=noimg)
    tty_steps=("$volume_prompt" $'l\n' "$volume_prompt" $'n\n')
    run_cleanup_tty
    assert_equals 0 "$status" 'listing a volume then keeping it must succeed'
    assert_contains "$output" '          contents (top level):
            PG_VERSION
            base/
            global/
' 'l must list the top level of the volume'
    assert_equals 2 "$(printf '%s\n' "$calls" | grep -c '^docker run ')" \
        'l must try the next local image when one has no ls, and stop at the first that works'
    assert_contains "$calls" 'docker run --rm --pull never --network none --user 0:0 --entrypoint ls --mount type=volume,source=app_pgdata,target=/cleanup-peek,readonly,volume-nocopy okimg -A -1 -F /cleanup-peek' \
        'the listing must use a throwaway container: read-only mount, no pulling, no network, only ls'
    assert_not_contains "$calls" 'docker volume rm' 'listing must not delete, and n after it must keep the volume'
    assert_equals 2 "$(printf '%s\n' "$output" | grep -c 'Delete this volume?')" \
        'after l the same volume must be asked about again'

    # The answer after a listing is still an explicit choice.
    set_machine ubuntu linux-gnu "${volume_tools[@]}"
    case_env=(NO_COLOR=1 STUB_VOLUMES=app_pgdata STUB_IMAGES=okimg)
    tty_steps=("$volume_prompt" $'l\n' "$volume_prompt" $'y\n')
    run_cleanup_tty
    assert_contains "$calls" $'\ndocker volume rm app_pgdata\n' 'y after a listing must delete the volume'

    # No local image can list it.
    set_machine ubuntu linux-gnu "${volume_tools[@]}"
    case_env=(NO_COLOR=1 STUB_VOLUMES=app_pgdata 'STUB_IMAGES=noimg' STUB_NO_LS_IMAGES=noimg)
    tty_steps=("$volume_prompt" $'l\n' "$volume_prompt" $'n\n')
    run_cleanup_tty
    assert_contains "$output" 'contents: not available (no local image with ls could mount it)' \
        'l must say when no local image can list the volume'
    set_machine ubuntu linux-gnu "${volume_tools[@]}"
    case_env=(NO_COLOR=1 STUB_VOLUMES=app_pgdata)
    tty_steps=("$volume_prompt" $'l\n' "$volume_prompt" $'n\n')
    run_cleanup_tty
    assert_contains "$output" 'contents: not available' 'l must say when there is no local image at all'
    assert_not_contains "$calls" 'docker run' 'with no local image nothing may be run'

    # An empty volume.
    set_machine ubuntu linux-gnu "${volume_tools[@]}"
    case_env=(NO_COLOR=1 STUB_VOLUMES=scratch_data STUB_IMAGES=okimg)
    tty_steps=("$volume_prompt" $'l\n' "$volume_prompt" $'n\n')
    run_cleanup_tty
    assert_contains "$output" 'contents: empty' 'l must say when the volume is empty'

    # A long listing is cut, and says by how much.
    set_machine ubuntu linux-gnu "${volume_tools[@]}"
    case_env=(NO_COLOR=1 STUB_VOLUMES=many_files STUB_IMAGES=okimg)
    tty_steps=("$volume_prompt" $'l\n' "$volume_prompt" $'n\n')
    run_cleanup_tty
    assert_contains "$output" '            file15
            ... and 5 more
' 'l must show at most 15 entries and count the rest'
    assert_not_contains "$output" 'file16' 'l must not print past the 15th entry'

    # Only a local volume is mounted: another driver's data may be remote.
    set_machine ubuntu linux-gnu "${volume_tools[@]}"
    case_env=(NO_COLOR=1 STUB_VOLUMES=nfs_share STUB_IMAGES=okimg)
    tty_steps=("$volume_prompt" $'l\n' "$volume_prompt" $'n\n')
    run_cleanup_tty
    assert_contains "$output" 'contents: only local volumes can be listed' \
        'l must refuse volumes of other drivers'
    assert_not_contains "$calls" 'docker run' 'a volume of another driver must never be mounted'

    ###########################################################
    # => Colour is for terminals only
    ###########################################################

    set_machine ubuntu linux-gnu "${volume_tools[@]}"
    case_env=(STUB_VOLUMES=app_pgdata)
    tty_steps=("$volume_prompt" $'n\n')
    run_cleanup_tty
    assert_contains "$output" $'\033[34m==> Docker unused volumes\033[0m' 'a terminal must get blue step headers'
    assert_contains "$output" $'\033[33mDeleting a volume destroys its data for good.\033[0m' \
        'a terminal must get the data-loss warning in yellow'

    set_machine ubuntu linux-gnu "${volume_tools[@]}"
    case_env=(NO_COLOR=1 STUB_VOLUMES=app_pgdata)
    tty_steps=("$volume_prompt" $'n\n')
    run_cleanup_tty
    assert_not_contains "$output" $'\033' 'NO_COLOR must switch every colour code off, even on a terminal'
fi

printf 'cleanup tests passed.\n'
