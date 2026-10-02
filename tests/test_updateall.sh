#!/usr/bin/env bash
# Unit tests for the `update` and `updateall` functions and pip_user_upgrade in
# .config/zsh/aliases.
#
# They guard what the old one-string aliases got wrong:
#   - zsh-syntax-highlighting paints an alias red when any command inside it is not
#     installed, however well guarded (emacs, gcloud). They must stay functions.
#   - nvim ran as a background job. It draws a full-screen UI, so an interactive
#     shell stopped it ("suspended (tty output)") before any plugin was updated.
#     Every step except Emacs must finish before the function returns.
#   - `pip install --user` ran on a PEP 668 "externally managed" Python, where it
#     always fails. pip may only touch an interpreter that allows it.
#   - Arch and Manjaro ran the `yay` wrapper function (yay -S --noconfirm --needed)
#     instead of the yay binary for `yay -Syu`.
#   - Arguments typed after the alias were appended to its last command, so there was
#     no --dry-run and no --help, and a step that failed went unnoticed.
#   - On macOS, xcode-select --install fails once the tools are installed. That is
#     not a failure.
#
# Every case runs in a clean `env -i` shell with a throwaway HOME. PATH holds only
# stub commands (plus the real sleep), so nothing real (sudo, apt-get, brew, gcloud,
# nvim, pip) can start and a tool that is not stubbed behaves as not installed.
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
calls_log="$case_dir/calls.log"
emacs_log="$case_dir/emacs.log"
pip_log="$case_dir/pip.log"
bash_bin="$(command -v bash)"
sleep_bin="$(command -v sleep)"
mkdir -p "$home" "$stubs" "$bin"

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

# headers_of TEXT: the step headers of an update run, one per line.
headers_of() {
    printf '%s\n' "$1" | grep '^==> ' || true
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
# does any work, so even the sudo stub cannot start apt-get or pacman. Nothing may
# call pip3 any more; installing a stub for it makes a regression visible.
for name in sudo gcloud pip3 npm brew yay snap; do
    write_stub "$name" <<'EOF'
printf '%s %s\n' "${0##*/}" "$*" >> "$STUB_LOG"
[[ "${0##*/} $*" == "${STUB_FAIL:-@none@}"* ]] && exit 1
exit 0
EOF
done

# xcode-select --install fails with "already installed" once the tools are there:
# $STUB_XCODE_STATUS is its exit status.
write_stub xcode-select <<'EOF'
printf 'xcode-select %s\n' "$*" >> "$STUB_LOG"
exit "${STUB_XCODE_STATUS:-1}"
EOF

# Emacs updates in a background job, so its order against the other steps is not
# defined. It logs to a file of its own. A real Emacs keeps its window open until the
# user closes it, so updateall must never wait for it. With $STUB_EMACS_GO set, the
# stub does the same: it waits for that file, which the shell creates only after the
# function returned, and records whether it appeared. A function that waited for Emacs
# would deadlock here until the 5 s limit.
write_stub emacs <<'EOF'
printf 'emacs %s\n' "$*" >> "$STUB_EMACS_LOG"
if [[ -n ${STUB_EMACS_GO:-} ]]; then
    for ((tick = 0; tick < 100; tick++)); do
        [[ -e $STUB_EMACS_GO ]] && break
        sleep 0.05
    done
    if [[ -e $STUB_EMACS_GO ]]; then
        printf 'emacs saw the function return\n' >> "$STUB_EMACS_LOG"
    else
        printf 'emacs was still waiting for the function to return\n' >> "$STUB_EMACS_LOG"
    fi
fi
EOF

# nvim takes a moment, like a real plugin update. The second line only lands before
# the function returns if the function waited for nvim instead of leaving it running
# in the background.
write_stub nvim <<'EOF'
printf 'nvim %s\n' "$*" >> "$STUB_LOG"
sleep 0.3
printf 'nvim finished\n' >> "$STUB_LOG"
[[ "nvim $*" == "${STUB_FAIL:-@none@}"* ]] && exit 1
exit 0
EOF

# python3 answers the PEP 668 probe (-c) with STUB_PROBE_STATUS (1: externally managed)
# and records every other call.
write_stub python3 <<'EOF'
if [[ ${1:-} == -c ]]; then
    exit "${STUB_PROBE_STATUS:-1}"
fi
printf 'python3 %s\n' "$*" >> "$STUB_LOG"
EOF

###############################################################
# => Harness
###############################################################

# pick_shell SHELL: set shell_bin, flags and setup to start SHELL without any
# startup file.
pick_shell() {
    shell_bin="$(command -v "$1")"
    case "$1" in
        bash)
            flags=(--noprofile --norc)
            # Aliases such as `rm -vI` only reach function bodies when they expand.
            setup='shopt -s expand_aliases'
            ;;
        zsh)
            flags=(-f)
            setup=':'
            ;;
    esac
}

# set_machine DISTRO OSTYPE TOOL...
# Describe the machine of the next run: its distribution, its OSTYPE and the tools
# that are "installed": stub names, plus antigen, which is a shell function. Extra
# environment for the stubs goes in case_env.
set_machine() {
    case_distro=$1
    case_ostype=$2
    shift 2
    case_tools=("$@")
    case_env=(STUB_CASE=1)
}

# run_update SHELL COMMAND [ARGS...]
# Load the aliases into a clean SHELL on the machine set_machine described and run
# `COMMAND ARGS`. Sets `status` (its exit status), `output` (stdout and stderr),
# `calls` (what the stubs saw) and `emacs_calls` (what the Emacs stub saw).
run_update() {
    local shell_name=$1 tool stub_antigen=0
    shift
    pick_shell "$shell_name"

    rm -rf "$bin"
    mkdir -p "$bin"
    ln -s "$sleep_bin" "$bin/sleep"
    for tool in "${case_tools[@]}"; do
        if [[ $tool == antigen ]]; then
            stub_antigen=1
        else
            ln -s "$stubs/$tool" "$bin/$tool"
        fi
    done

    : > "$calls_log"
    : > "$emacs_log"
    status=0
    # The shell sets OSTYPE itself at startup and ignores the environment, so the
    # script assigns it before the aliases are loaded.
    output="$(
        env -i HOME="$home" PATH="$bin" TERM=dumb DISTRO="$case_distro" \
            STUB_LOG="$calls_log" STUB_EMACS_LOG="$emacs_log" STUB_ANTIGEN="$stub_antigen" \
            "${case_env[@]}" \
            "$shell_bin" "${flags[@]}" -c "$setup"$'\n''
                aliases_file=$1
                OSTYPE=$2
                shift 2
                if [ -n "${STALE_ALIAS:-}" ]; then
                    alias update="echo OLD-ALIAS-RAN"
                    alias updateall="echo OLD-ALIAS-RAN"
                fi
                if [ "${STUB_ANTIGEN:-0}" = 1 ]; then
                    antigen() { printf "antigen %s\n" "$*" >> "$STUB_LOG"; }
                fi
                source "$aliases_file"
                "$@"
                rc=$?
                if [ -n "${STUB_MARK_RETURN:-}" ]; then
                    printf "returned\n" >> "$STUB_LOG"
                fi
                if [ -n "${STUB_EMACS_GO:-}" ]; then
                    : > "$STUB_EMACS_GO"
                fi
                exit "$rc"
            ' "$shell_name" "$aliases" "$case_ostype" "$@" 2>&1
    )" || status=$?
    calls="$(< "$calls_log")"
    emacs_calls="$(< "$emacs_log")"
}

# kind_of SHELL NAME
# Print what the aliases file leaves NAME as in a clean SHELL: function, alias, ...
kind_of() {
    pick_shell "$1"
    env -i HOME="$home" PATH="$bin" TERM=dumb DISTRO=ubuntu \
        "$shell_bin" "${flags[@]}" -c "$setup"$'\n''
            OSTYPE=linux-gnu
            source "$1"
            if [ -n "${ZSH_VERSION:-}" ]; then
                kind=$(whence -w "$2")
                printf "%s\n" "${kind##* }"
            else
                type -t "$2"
            fi
        ' "$1" "$aliases" "$2"
}

shells=(bash)
if command -v zsh > /dev/null 2>&1; then
    shells+=(zsh)
else
    printf 'SKIP: zsh is not installed; testing Bash only.\n'
fi

###############################################################
# => Functions, not aliases
###############################################################

for shell_name in "${shells[@]}"; do
    for name in update updateall; do
        assert_equals function "$(kind_of "$shell_name" "$name")" \
            "$shell_name: $name must be a function; an alias is painted red whenever a tool inside it is not installed, and cannot take --dry-run"
    done

    # A shell that loaded an older copy of the file still has the alias. Sourcing the
    # new file must replace it instead of failing to parse the function definition.
    set_machine ubuntu linux-gnu sudo
    case_env+=(STALE_ALIAS=1)
    run_update "$shell_name" updateall --dry-run
    assert_equals 0 "$status" "$shell_name: sourcing over a live alias must still define the function"
    assert_not_contains "$output" 'OLD-ALIAS-RAN' \
        "$shell_name: the stale alias must not run"
    assert_contains "$output" 'would run: sudo apt-get update' \
        "$shell_name: the function must replace the stale alias"
done

###############################################################
# => Ubuntu: updateall runs every step to completion, in order
###############################################################

ubuntu_tools=(sudo gcloud pip3 nvim python3 emacs antigen)

managed_calls='sudo apt-get update
sudo apt-get full-upgrade
gcloud components update --quiet
antigen update
nvim +PlugUpgrade +PlugUpdate +qall
nvim finished
returned'

unmanaged_calls='sudo apt-get update
sudo apt-get full-upgrade
gcloud components update --quiet
python3 -m pip install --user --upgrade pip pynvim
antigen update
nvim +PlugUpgrade +PlugUpdate +qall
nvim finished
returned'

ubuntu_headers='==> apt package lists
==> apt packages
==> Emacs packages (started in the background)
==> Google Cloud CLI
==> pip and pynvim
==> Zsh plugins (antigen)
==> Neovim plugins'

for shell_name in "${shells[@]}"; do
    # "returned" is logged after the function. nvim finishing before it proves the
    # function waited; a background nvim would finish 0.3 s after it. The Emacs stub
    # proves the opposite: it only sees the go file once the function has returned.
    set_machine ubuntu linux-gnu "${ubuntu_tools[@]}"
    emacs_go="$case_dir/emacs.go"
    rm -f "$emacs_go"
    case_env+=(STUB_MARK_RETURN=1 STUB_EMACS_GO="$emacs_go")
    run_update "$shell_name" updateall
    assert_equals 0 "$status" "$shell_name: a clean updateall must succeed"
    assert_equals "$managed_calls" "$calls" \
        "$shell_name: on an externally managed Python updateall must finish every step in order and never call pip"
    assert_equals "$ubuntu_headers" "$(headers_of "$output")" \
        "$shell_name: updateall on Ubuntu must announce each step once, in order"
    assert_equals 'emacs -f auto-package-update-now
emacs saw the function return' "$emacs_calls" \
        "$shell_name: updateall must start the Emacs package update once, in the background, without waiting for it"
    assert_contains "$output" 'Skipping pip upgrade' \
        "$shell_name: a skipped pip upgrade must say so"

    set_machine ubuntu linux-gnu "${ubuntu_tools[@]}"
    case_env+=(STUB_MARK_RETURN=1 STUB_PROBE_STATUS=0)
    run_update "$shell_name" updateall
    assert_equals 0 "$status" "$shell_name: a clean updateall must succeed when pip is allowed"
    assert_equals "$unmanaged_calls" "$calls" \
        "$shell_name: on a Python that allows it updateall must upgrade pip and pynvim once, for the user"
    assert_not_contains "$output" 'Skipping pip upgrade' \
        "$shell_name: nothing was skipped, so nothing may say so"

    # update is the operating system only, whatever tools are installed.
    set_machine ubuntu linux-gnu "${ubuntu_tools[@]}"
    run_update "$shell_name" update
    assert_equals 0 "$status" "$shell_name: update must succeed"
    assert_equals $'sudo apt-get update\nsudo apt-get full-upgrade' "$calls" \
        "$shell_name: update on Ubuntu must run apt and nothing else"
    assert_equals $'==> apt package lists\n==> apt packages' "$(headers_of "$output")" \
        "$shell_name: update on Ubuntu must announce the two apt steps"
    assert_equals '' "$emacs_calls" "$shell_name: update must not start Emacs"
done

###############################################################
# => Ubuntu on Windows (WSL): the apt steps and the tools, no Emacs or gcloud
###############################################################

for shell_name in "${shells[@]}"; do
    set_machine ubuntu_windows linux-gnu "${ubuntu_tools[@]}"
    run_update "$shell_name" updateall
    assert_equals 0 "$status" "$shell_name: a clean updateall must succeed on Ubuntu for Windows"
    assert_equals 'sudo apt-get update
sudo apt-get full-upgrade
antigen update
nvim +PlugUpgrade +PlugUpdate +qall
nvim finished' "$calls" \
        "$shell_name: Ubuntu for Windows must run apt, the plugin updates and nothing else"
    assert_equals '==> apt package lists
==> apt packages
==> pip and pynvim
==> Zsh plugins (antigen)
==> Neovim plugins' "$(headers_of "$output")" \
        "$shell_name: Ubuntu for Windows must not announce Emacs or the Google Cloud CLI"
    assert_equals '' "$emacs_calls" "$shell_name: Ubuntu for Windows must not start Emacs"
done

###############################################################
# => Arch and Manjaro: yay when installed, otherwise pacman
###############################################################

for shell_name in "${shells[@]}"; do
    # The aliases file defines a yay() wrapper (yay -S --noconfirm --needed) for Arch.
    # `yay -Syu` must reach the binary instead, or the wrapper adds -S in front of it.
    set_machine arch linux-gnu sudo yay snap npm emacs nvim python3 antigen
    run_update "$shell_name" updateall
    assert_equals 0 "$status" "$shell_name: a clean updateall must succeed on Arch"
    assert_equals 'yay -Syu
sudo snap refresh
npm update
antigen update
nvim +PlugUpgrade +PlugUpdate +qall
nvim finished' "$calls" \
        "$shell_name: Arch with yay must upgrade through the yay binary, not the yay() wrapper, then Snap, npm and the plugins"
    assert_equals '==> pacman and AUR packages
==> Snap packages
==> npm packages (current directory)
==> Emacs packages (started in the background)
==> pip and pynvim
==> Zsh plugins (antigen)
==> Neovim plugins' "$(headers_of "$output")" \
        "$shell_name: Arch with yay must announce each step once, in order"
    assert_equals 'emacs -f auto-package-update-now' "$emacs_calls" \
        "$shell_name: updateall on Arch must start the Emacs package update"

    # Without the yay binary the yay() wrapper still exists as a function, so `command -v yay`
    # would claim yay is installed. pacman must run instead.
    set_machine manjaro linux-gnu sudo snap npm nvim python3 antigen
    run_update "$shell_name" updateall
    assert_equals 0 "$status" "$shell_name: a clean updateall must succeed on Manjaro"
    assert_equals 'sudo pacman -Syu
sudo snap refresh
npm update
antigen update
nvim +PlugUpgrade +PlugUpdate +qall
nvim finished' "$calls" \
        "$shell_name: Manjaro without yay must upgrade with pacman, not try the missing yay"

    # update never uses yay, and skips Snap when it is not installed.
    set_machine arch linux-gnu sudo yay nvim antigen
    run_update "$shell_name" update
    assert_equals 0 "$status" "$shell_name: update must succeed on Arch"
    assert_equals 'sudo pacman -Syu' "$calls" \
        "$shell_name: update on Arch must run pacman only, even with yay installed"
done

###############################################################
# => macOS: Homebrew, software updates, command line tools, npm
###############################################################

mac_tools=(sudo brew xcode-select npm nvim python3 antigen)

for shell_name in "${shells[@]}"; do
    # The stub xcode-select exits 1, as the real one does when the tools are installed.
    set_machine '' darwin21 "${mac_tools[@]}"
    run_update "$shell_name" updateall
    assert_equals 0 "$status" "$shell_name: xcode-select failing because the tools are installed is not a failure"
    assert_equals 'brew update
brew upgrade
sudo softwareupdate -i -a
xcode-select --install
npm install npm -g
antigen update
nvim +PlugUpgrade +PlugUpdate +qall
nvim finished' "$calls" \
        "$shell_name: updateall on macOS must run Homebrew, the software update, the command line tools, npm and the plugins"
    assert_equals '==> Homebrew package lists
==> Homebrew packages
==> macOS software updates
==> Command line tools
==> npm itself
==> pip and pynvim
==> Zsh plugins (antigen)
==> Neovim plugins' "$(headers_of "$output")" \
        "$shell_name: updateall on macOS must announce each step once, in order"
    assert_not_contains "$output" 'did not finish' \
        "$shell_name: the best-effort command line tools step must not be reported as failed"

    set_machine '' darwin21 "${mac_tools[@]}"
    run_update "$shell_name" update
    assert_equals 0 "$status" "$shell_name: update must succeed on macOS"
    assert_equals 'brew update
brew upgrade
sudo softwareupdate -i -a
xcode-select --install' "$calls" \
        "$shell_name: update on macOS must stop after the system steps"

    # Homebrew is optional; the software update is not.
    set_machine '' darwin21 sudo xcode-select
    run_update "$shell_name" update
    assert_equals 0 "$status" "$shell_name: update must succeed on macOS without Homebrew"
    assert_equals $'sudo softwareupdate -i -a\nxcode-select --install' "$calls" \
        "$shell_name: macOS without Homebrew must skip only the Homebrew steps"
done

###############################################################
# => A system with no operating-system update configured
###############################################################

for shell_name in "${shells[@]}"; do
    set_machine gentoo linux-gnu sudo npm gcloud emacs nvim python3 antigen
    run_update "$shell_name" update
    assert_equals 0 "$status" "$shell_name: update must not fail on a system it does not know"
    assert_equals '' "$calls" "$shell_name: update must not guess a package manager"
    assert_contains "$output" 'No operating-system update is set up for OSTYPE=linux-gnu DISTRO=gentoo' \
        "$shell_name: update must say why it did nothing"

    # The plugin updates do not depend on the operating system. npm, Emacs and the Google
    # Cloud CLI are chosen per system, so an unknown system gets none of them.
    run_update "$shell_name" updateall
    assert_equals 0 "$status" "$shell_name: updateall must not fail on a system it does not know"
    assert_equals 'antigen update
nvim +PlugUpgrade +PlugUpdate +qall
nvim finished' "$calls" \
        "$shell_name: updateall on an unknown system must run the plugin updates only"
    assert_contains "$output" 'No operating-system update is set up' \
        "$shell_name: updateall must say it skipped the operating system"
done

###############################################################
# => Tools that are not installed are skipped
###############################################################

for shell_name in "${shells[@]}"; do
    # Only sudo exists: no gcloud, Emacs, nvim, python3 or antigen. No "command not found".
    set_machine ubuntu linux-gnu sudo
    run_update "$shell_name" updateall
    assert_equals 0 "$status" "$shell_name: missing tools must not fail updateall"
    assert_equals $'sudo apt-get update\nsudo apt-get full-upgrade' "$calls" \
        "$shell_name: updateall must run only the steps whose tools are installed"
    assert_equals $'==> apt package lists\n==> apt packages\n==> pip and pynvim' "$(headers_of "$output")" \
        "$shell_name: updateall must announce only the steps that can run"
    assert_not_contains "$output" 'not found' \
        "$shell_name: a missing tool must be skipped silently, not reported by the shell"
done

###############################################################
# => --dry-run: nothing runs
###############################################################

for shell_name in "${shells[@]}"; do
    for flag in --dry-run -n; do
        set_machine ubuntu linux-gnu "${ubuntu_tools[@]}"
        run_update "$shell_name" updateall "$flag"
        assert_equals 0 "$status" "$shell_name $flag: a preview must succeed"
        assert_equals '' "$calls" \
            "$shell_name $flag: a preview must not run any command, not even the python3 probe"
        assert_equals '' "$emacs_calls" "$shell_name $flag: a preview must not start Emacs"
        assert_equals "$ubuntu_headers" "$(headers_of "$output")" \
            "$shell_name $flag: a preview must list the same steps as a real run"
        for would in 'sudo apt-get update' 'sudo apt-get full-upgrade' \
            'emacs -f auto-package-update-now &' 'gcloud components update --quiet' \
            'pip_user_upgrade' 'antigen update' 'nvim +PlugUpgrade +PlugUpdate +qall'; do
            assert_contains "$output" "would run: $would" \
                "$shell_name $flag: a preview must show how each step would run"
        done
        assert_contains "$output" 'Dry run: nothing was changed.' \
            "$shell_name $flag: a preview must say it changed nothing"

        set_machine ubuntu linux-gnu "${ubuntu_tools[@]}"
        run_update "$shell_name" update "$flag"
        assert_equals 0 "$status" "$shell_name $flag: update must preview too"
        assert_equals '' "$calls" "$shell_name $flag: update must not run any command in a preview"
        assert_contains "$output" 'would run: sudo apt-get full-upgrade' \
            "$shell_name $flag: update must show how it would run"
    done
done

###############################################################
# => A failing step does not stop the others
###############################################################

for shell_name in "${shells[@]}"; do
    # Both apt steps fail: the prefix matches sudo apt-get update and sudo apt-get full-upgrade.
    set_machine ubuntu linux-gnu "${ubuntu_tools[@]}"
    case_env+=('STUB_FAIL=sudo apt')
    run_update "$shell_name" updateall
    assert_equals 1 "$status" "$shell_name: a failed step must make updateall fail"
    assert_equals "$(printf '%s\n' "$managed_calls" | grep -v '^returned$')" "$calls" \
        "$shell_name: the steps after a failed apt step must still run"
    assert_contains "$output" '!! apt package lists did not finish' \
        "$shell_name: a failed step must be named when it fails"
    assert_contains "$output" '!! Did not finish: apt package lists, apt packages' \
        "$shell_name: updateall must summarise every failed step at the end"

    set_machine ubuntu linux-gnu "${ubuntu_tools[@]}"
    case_env+=('STUB_FAIL=nvim')
    run_update "$shell_name" updateall
    assert_equals 1 "$status" "$shell_name: a failed last step must make updateall fail"
    assert_contains "$output" '!! Did not finish: Neovim plugins' \
        "$shell_name: updateall must report the failed Neovim step"

    set_machine ubuntu linux-gnu "${ubuntu_tools[@]}"
    case_env+=('STUB_FAIL=sudo apt-get full-upgrade')
    run_update "$shell_name" update
    assert_equals 1 "$status" "$shell_name: a failed step must make update fail"
    assert_contains "$output" '!! Did not finish: apt packages' \
        "$shell_name: update must report the failed step"
done

###############################################################
# => --help and unknown options
###############################################################

for shell_name in "${shells[@]}"; do
    for flag in --help -h; do
        set_machine ubuntu linux-gnu "${ubuntu_tools[@]}"
        run_update "$shell_name" updateall "$flag"
        assert_equals 0 "$status" "$shell_name $flag: help must succeed"
        assert_equals '' "$calls" "$shell_name $flag: help must not run any command"
        assert_contains "$output" 'Usage: updateall [-n|--dry-run]' \
            "$shell_name $flag: help must show the usage"

        run_update "$shell_name" update "$flag"
        assert_contains "$output" 'Usage: update [-n|--dry-run]' \
            "$shell_name $flag: help must name the command it was asked on"
    done

    set_machine ubuntu linux-gnu "${ubuntu_tools[@]}"
    run_update "$shell_name" updateall --bogus
    assert_equals 2 "$status" "$shell_name: an unknown option must exit with status 2"
    assert_equals '' "$calls" "$shell_name: an unknown option must not run any command"
    assert_contains "$output" 'updateall: unknown option: --bogus (try updateall --help)' \
        "$shell_name: an unknown option must say what was wrong"
done

###############################################################
# => pip_user_upgrade without python3
###############################################################

mkdir -p "$case_dir/empty"
output="$(
    env -i HOME="$home" PATH="$case_dir/empty" "$bash_bin" --noprofile --norc -c '
        source "$1"
        pip_user_upgrade
        printf "rc=%s\n" "$?"
    ' bash "$aliases"
)"
assert_equals 'rc=0' "$output" 'pip_user_upgrade must quietly do nothing when python3 is missing'

###############################################################
# => pip_user_upgrade against real interpreters
###############################################################
# The stubs above never run the probe, so a typo in it would pass. Run it for real
# and compare its decision with the facts read straight off the interpreter. A fake
# `pip` package first on PYTHONPATH records the install instead of performing it.

mkdir -p "$case_dir/fakepip/pip"
: > "$case_dir/fakepip/pip/__init__.py"
cat > "$case_dir/fakepip/pip/__main__.py" <<'EOF'
import os
import sys

with open(os.environ["PIP_LOG"], "a") as pip_log:
    pip_log.write(" ".join(sys.argv[1:]) + "\n")
EOF

# interpreter_allows_user_pip PYTHON
# Succeed when PYTHON is neither marked EXTERNALLY-MANAGED nor inside a virtualenv,
# fail when pip must stay away, and return 2 when PYTHON cannot be probed.
interpreter_allows_user_pip() {
    local python=$1 stdlib in_venv
    stdlib="$("$python" -c 'import sysconfig; print(sysconfig.get_path("stdlib"))')" || return 2
    in_venv="$("$python" -c 'import sys; print(sys.prefix != sys.base_prefix)')" || return 2
    [[ ! -e $stdlib/EXTERNALLY-MANAGED && $in_venv == False ]]
}

# check_real_interpreter LABEL PYTHON
check_real_interpreter() {
    local label=$1 python=$2 verdict=0 expected=''
    interpreter_allows_user_pip "$python" || verdict=$?
    case "$verdict" in
        0) expected='install --user --upgrade pip pynvim' ;;
        1) expected='' ;;
        *)
            printf 'SKIP: %s: %s cannot be probed.\n' "$label" "$python"
            return 0
            ;;
    esac

    : > "$pip_log"
    env -i HOME="$home" PATH="${python%/*}:/usr/bin:/bin" \
        PYTHONPATH="$case_dir/fakepip" PIP_LOG="$pip_log" \
        "$bash_bin" --noprofile --norc -c 'source "$1"; pip_user_upgrade' bash "$aliases" > /dev/null
    assert_equals "$expected" "$(cat "$pip_log")" \
        "$label: pip_user_upgrade disagrees with the interpreter's own PEP 668 facts"
}

system_python="$(command -v python3 || true)"
if [[ -z $system_python ]]; then
    printf 'SKIP: python3 is not installed; skipping the real-interpreter cases.\n'
else
    check_real_interpreter 'system python3' "$system_python"

    # A virtualenv never takes `pip install --user`, whatever the system marks.
    if "$system_python" -m venv --without-pip "$case_dir/venv" > /dev/null 2>&1; then
        check_real_interpreter 'virtualenv python3' "$case_dir/venv/bin/python3"
    else
        printf 'SKIP: python3 -m venv is unavailable; skipping the virtualenv case.\n'
    fi
fi

printf 'update and updateall tests passed.\n'
