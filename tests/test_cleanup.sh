#!/usr/bin/env bash
# Unit tests for the `cleanup` function in .config/zsh/aliases.
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
#   - Aliases are expanded inside function bodies, so a plain `df` would run as the
#     `df -h` this file aliases it to.
#
# Every case runs in a clean `env -i` shell with a throwaway HOME. PATH holds only
# stub commands (plus the real awk), so nothing real (sudo, apt-get, snap, docker)
# can start and a tool that is not stubbed behaves as not installed.

# The scripts handed to the child shells below are single-quoted on purpose: their
# variables must be expanded by the child shell, not by this one.
# shellcheck disable=SC2016

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
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
write_stub docker <<'EOF'
printf 'docker %s\n' "$*" >> "$STUB_LOG"
[[ $1 == info ]] && exit "${STUB_DOCKER_INFO_STATUS:-0}"
[[ "docker $*" == "${STUB_FAIL:-@none@}"* ]] && exit 1
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

# Each call reports 3 GiB more free space than the previous one, starting at 100 GiB,
# and lists the same filesystem twice, the way df does for / and $HOME on one partition.
write_stub df <<'EOF'
printf 'df %s\n' "$*" >> "$STUB_LOG"
calls=0
[[ -f $STUB_STATE/df.count ]] && read -r calls < "$STUB_STATE/df.count"
calls=$((calls + 1))
printf '%s\n' "$calls" > "$STUB_STATE/df.count"
available=$((104857600 + (calls - 1) * 3145728))
printf 'Filesystem 1024-blocks Used Available Capacity Mounted on\n'
printf '/dev/test 999999999 1 %s 1%% /\n' "$available"
printf '/dev/test 999999999 1 %s 1%% /\n' "$available"
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
    case_env=(STUB_CASE=1)
}

# run_cleanup SHELL [ARGS...]
# Load the aliases into a clean SHELL on the machine set_machine described and run
# `cleanup ARGS`. Sets `status` (its exit status), `output` (stdout and stderr) and
# `calls` (what the stubs saw).
run_cleanup() {
    local shell_name=$1 shell_bin setup flags=() tool
    shift
    shell_bin="$(command -v "$shell_name")"
    case "$shell_name" in
        bash)
            flags=(--noprofile --norc)
            # Aliases such as `df -h` only reach function bodies when they expand.
            setup='shopt -s expand_aliases'
            ;;
        zsh)
            flags=(-f)
            setup=':'
            ;;
    esac

    rm -rf "$bin" "$state_dir"
    mkdir -p "$bin" "$state_dir"
    ln -s "$awk_bin" "$bin/awk"
    for tool in "${case_tools[@]}"; do
        ln -s "$stubs/$tool" "$bin/$tool"
    done

    : > "$calls_log"
    status=0
    # The shell sets OSTYPE itself at startup and ignores the environment, so the
    # script assigns it before the aliases are loaded.
    output="$(
        env -i HOME="$home" PATH="$bin" TERM=dumb DISTRO="$case_distro" \
            STUB_LOG="$calls_log" STUB_STATE="$state_dir" "${case_env[@]}" \
            "$shell_bin" "${flags[@]}" -c "$setup"$'\n''
                aliases_file=$1
                OSTYPE=$2
                shift 2
                if [ -n "${STALE_ALIAS:-}" ]; then
                    alias cleanup="echo OLD-ALIAS-RAN"
                fi
                source "$aliases_file"
                cleanup "$@"
            ' "$shell_name" "$aliases" "$case_ostype" "$@" 2>&1
    )" || status=$?
    calls="$(< "$calls_log")"
}

shells=(bash)
if command -v zsh > /dev/null 2>&1; then
    shells+=(zsh)
else
    printf 'SKIP: zsh is not installed; testing Bash only.\n'
fi

df_call="df -Pk / $home"

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
docker image prune -af --filter until=168h
docker builder prune -af --filter until=168h
$df_call"

for shell_name in "${shells[@]}"; do
    set_machine ubuntu linux-gnu "${ubuntu_tools[@]}"
    run_cleanup "$shell_name"
    assert_equals 0 "$status" "$shell_name: a clean run must succeed"
    assert_equals "$ubuntu_calls" "$calls" \
        "$shell_name: on Ubuntu cleanup must run every step once, in order: containers before images, disabled snaps only, no autoclean, df without -h"
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
==> Docker unused images
==> Docker build cache
Free space on /: 103.0 GiB (+3.0 GiB)" "$output" \
        "$shell_name: a clean run must print one header per step and the free space once, and nothing else"
done

###############################################################
# => --dry-run: only read-only probes run
###############################################################

for shell_name in "${shells[@]}"; do
    for flag in --dry-run -n; do
        set_machine ubuntu linux-gnu "${ubuntu_tools[@]}"
        run_cleanup "$shell_name" "$flag"
        assert_equals 0 "$status" "$shell_name $flag: a preview must succeed"
        assert_equals $'snap list --all\ndocker info' "$calls" \
            "$shell_name $flag: a preview may only probe (snap list, docker info); it must not call sudo, df or any pruning tool"
        assert_contains "$output" 'would run: sudo apt-get clean' \
            "$shell_name $flag: a preview must show the apt step"
        assert_contains "$output" 'would run: sudo snap remove firefox --revision=4900' \
            "$shell_name $flag: a preview must list the disabled snap revisions it found"
        assert_contains "$output" 'would run: docker container prune -f --filter until=168h' \
            "$shell_name $flag: a preview must show the Docker steps"
        assert_contains "$output" 'Dry run: nothing was changed.' \
            "$shell_name $flag: a preview must say it changed nothing"
        assert_not_contains "$output" 'Free space on' \
            "$shell_name $flag: a preview frees nothing, so it must not report free space"
    done
done

###############################################################
# => Docker
###############################################################

for shell_name in "${shells[@]}"; do
    set_machine ubuntu linux-gnu sudo docker df
    case_env=(STUB_DOCKER_INFO_STATUS=1)
    run_cleanup "$shell_name"
    assert_equals 0 "$status" "$shell_name: a stopped Docker daemon is skipped, not a failure"
    assert_equals "$df_call
sudo apt-get clean
sudo apt-get autoremove
docker info
$df_call" "$calls" "$shell_name: with the daemon down cleanup must probe it once and prune nothing"
    assert_contains "$output" 'daemon is not reachable' \
        "$shell_name: skipping Docker must say why"

    set_machine ubuntu linux-gnu sudo docker df
    case_env=(STUB_FAIL='docker image prune')
    run_cleanup "$shell_name"
    assert_contains "$calls" 'docker builder prune' \
        "$shell_name: a failed Docker step must not stop the steps after it"
done

###############################################################
# => A failing step does not stop the rest
###############################################################

for shell_name in "${shells[@]}"; do
    set_machine ubuntu linux-gnu "${ubuntu_tools[@]}"
    case_env=(STUB_FAIL='npm cache clean')
    run_cleanup "$shell_name"
    assert_equals 1 "$status" "$shell_name: cleanup must fail when a step fails"
    assert_equals "$ubuntu_calls" "$calls" \
        "$shell_name: steps after a failed one must still run"
    assert_contains "$output" 'Did not finish: npm cache' \
        "$shell_name: cleanup must name the step that failed"
done

###############################################################
# => Other platforms
###############################################################

for shell_name in "${shells[@]}"; do
    # Arch defines a yay() wrapper (yay -S --noconfirm --needed); cleanup needs the binary.
    set_machine arch linux-gnu sudo yay pacman nvim df
    run_cleanup "$shell_name"
    assert_equals "$df_call
yay -Sc
nvim +PlugClean +qall
$df_call" "$calls" "$shell_name: on Arch cleanup must run yay -Sc itself, not the yay wrapper function"

    # Without yay nothing would clean the pacman cache.
    set_machine manjaro linux-gnu sudo pacman nvim df
    run_cleanup "$shell_name"
    assert_equals "$df_call
sudo pacman -Sc
nvim +PlugClean +qall
$df_call" "$calls" "$shell_name: on Manjaro without yay cleanup must fall back to pacman"

    set_machine '' darwin23 sudo brew gem nvim df
    run_cleanup "$shell_name"
    assert_equals "$df_call
brew cleanup
sudo gem cleanup
nvim +PlugClean +qall
$df_call" "$calls" "$shell_name: on macOS cleanup must clean Homebrew and RubyGems, not apt"

    # WSL: apt applies, snap does not exist there.
    set_machine ubuntu_windows linux-gnu sudo df
    run_cleanup "$shell_name"
    assert_equals "$df_call
sudo apt-get clean
sudo apt-get autoremove
$df_call" "$calls" "$shell_name: on Ubuntu for Windows cleanup must run the apt steps"

    # A distribution without package-manager steps still cleans the caches.
    set_machine gentoo linux-gnu sudo nvim df
    run_cleanup "$shell_name"
    assert_equals "$df_call
nvim +PlugClean +qall
$df_call" "$calls" "$shell_name: on an unlisted distribution cleanup must skip the package-manager steps"
done

###############################################################
# => Tools that are not installed are skipped quietly
###############################################################

for shell_name in "${shells[@]}"; do
    set_machine ubuntu linux-gnu sudo df
    run_cleanup "$shell_name"
    assert_equals 0 "$status" "$shell_name: missing tools are not an error"
    assert_not_contains "$output" 'Docker' \
        "$shell_name: a tool that is not installed must not be mentioned"
    assert_not_contains "$output" 'snap' \
        "$shell_name: a tool that is not installed must not be mentioned"
done

###############################################################
# => Usage
###############################################################

for shell_name in "${shells[@]}"; do
    set_machine ubuntu linux-gnu "${ubuntu_tools[@]}"
    run_cleanup "$shell_name" --help
    assert_equals 0 "$status" "$shell_name: --help must succeed"
    assert_contains "$output" 'Usage: cleanup [-n|--dry-run]' "$shell_name: --help must print the usage"
    assert_equals '' "$calls" "$shell_name: --help must run nothing"

    set_machine ubuntu linux-gnu "${ubuntu_tools[@]}"
    run_cleanup "$shell_name" --bogus
    assert_equals 2 "$status" "$shell_name: an unknown option must be a usage error"
    assert_contains "$output" 'unknown option: --bogus' "$shell_name: the bad option must be named"
    assert_equals '' "$calls" "$shell_name: an unknown option must run nothing"
done

###############################################################
# => Reloading over the old alias
###############################################################

# cleanup used to be an alias. A shell that still has it loaded must be able to
# source this file again: zsh and Bash both reject a function named like a live alias.
for shell_name in "${shells[@]}"; do
    set_machine ubuntu linux-gnu "${ubuntu_tools[@]}"
    case_env=(STALE_ALIAS=1)
    run_cleanup "$shell_name" --help
    assert_equals 0 "$status" "$shell_name: sourcing over a live cleanup alias must work"
    assert_contains "$output" 'Usage: cleanup' \
        "$shell_name: the function must replace the old alias"
    assert_not_contains "$output" 'OLD-ALIAS-RAN' \
        "$shell_name: the old alias must not survive the reload"
done

printf 'cleanup tests passed.\n'
