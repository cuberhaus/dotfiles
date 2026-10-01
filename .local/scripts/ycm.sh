#!/usr/bin/env bash
# Build or verify YouCompleteMe's compiled core (ycm_core) and its bundled clangd.
#
# The ycmd server behind YouCompleteMe refuses to start until ycm_core has been
# compiled for the Python that runs it ("The ycmd server SHUT DOWN"), and a
# plugin update can leave an outdated core behind. vim-plug builds the core
# through the 'do' hook in .vim/vimrc; this script is the idempotent safety net
# the bootstrap profiles run afterwards, and the command to run by hand when the
# server fails to start. It does nothing when the build is already usable.
#
# It never uses sudo and never installs system packages: each bootstrap profile
# declares the build tools, and a missing one is reported with a hint.
# Compatible with bash 3.2, because the macOS profile runs it with /bin/bash.
set -euo pipefail

YCM_DIR="${YCM_DIR:-$HOME/.vim/plugged/YouCompleteMe}"
# Options for the plugin's install.py. The default builds the core plus the
# bundled clangd for C-family languages, matching the 'do' hook in .vim/vimrc.
# An empty value builds the core only.
: "${YCM_INSTALL_ARGS=--clangd-completer}"

MODE=ensure
DRY_RUN=false
FORCE=false
YCMD_DIR=''
# What a recorded interpreter must be called to be executed: python, python3,
# python3.14, python3.13t. Anything else, such as an AppImage, is never run.
PYTHON_NAME_PATTERN='^python'

info() { printf '\033[1;34m[INFO]\033[0m  %s\n' "$*"; }
success() { printf '\033[1;32m[ OK ]\033[0m  %s\n' "$*"; }
warn() { printf '\033[1;33m[WARN]\033[0m  %s\n' "$*"; }
error() { printf '\033[1;31m[ERROR]\033[0m %s\n' "$*" >&2; }

usage() {
    cat <<'EOF'
Usage: ycm.sh [--check | --dry-run] [--force]

Build YouCompleteMe's compiled core (ycm_core) and, by default, its bundled
clangd, unless they already work. Run it when Vim or Neovim reports "The ycmd
server SHUT DOWN", and after a plugin update that left the core outdated.

Options:
  --check     Report whether YouCompleteMe is ready and build nothing. Exits 0
              when it is ready and 1 when something is missing or outdated.
  --dry-run   Show what would run without building, downloading, or syncing.
  --force     Rebuild even when everything already works.
  -h, --help  Show this help.

Environment:
  YCM_DIR           Plugin checkout (default: ~/.vim/plugged/YouCompleteMe).
  YCM_INSTALL_ARGS  Options for the plugin's install.py (default:
                    --clangd-completer). "--all" also builds the C#, Go, Rust,
                    Java, and TypeScript completers and needs their toolchains;
                    an empty value builds the core only.
  YCM_CORES         Maximum parallel compile jobs (default: every core).

A build compiles C++ for several minutes and downloads sources and clangd, so it
needs network access. It needs git, cmake, make, a C++ compiler, and the Python
headers, and it never installs them.
EOF
}

parse_args() {
    local argument

    MODE=ensure
    DRY_RUN=false
    FORCE=false
    for argument in "$@"; do
        case "$argument" in
            --check) MODE=check ;;
            --dry-run) DRY_RUN=true ;;
            --force) FORCE=true ;;
            -h | --help)
                usage
                exit 0
                ;;
            *)
                error "Unknown option: $argument"
                usage >&2
                exit 2
                ;;
        esac
    done
    if [[ "$MODE" == check && ("$DRY_RUN" == true || "$FORCE" == true) ]]; then
        error "--check only reports; it cannot be combined with --dry-run or --force."
        exit 2
    fi
}

## Run a command, or only describe it under --dry-run.
run() {
    if [[ "$DRY_RUN" == true ]]; then
        info "[dry-run] would run: $*"
        return 0
    fi
    "$@"
}

## Print the interpreter recorded by the last build, or nothing.
recorded_python() {
    local recorded=''
    if [[ -r "$YCMD_DIR/PYTHON_USED_DURING_BUILDING" ]]; then
        # build.py writes the path without a trailing newline, so read reports
        # end-of-file even though the variable is set.
        IFS= read -r recorded <"$YCMD_DIR/PYTHON_USED_DURING_BUILDING" || true
    fi
    printf '%s' "$recorded"
}

## Succeed unless the recorded interpreter is one that must not be executed.
##
## ycmd starts whatever executable PYTHON_USED_DURING_BUILDING names. A shell
## that exports ARGV0 (for example one started from an AppImage such as Cursor)
## makes zsh launch `python3 install.py` under that name, so the file can end up
## naming the AppImage instead of Python, and the server would start Cursor.
recorded_python_usable() {
    local recorded
    recorded="$(recorded_python)"
    [[ -z "$recorded" ]] && return 0
    [[ -x "$recorded" && "${recorded##*/}" =~ $PYTHON_NAME_PATTERN ]]
}

## Print the interpreter ycmd will run with: the recorded one, else python3.
server_python() {
    local recorded
    recorded="$(recorded_python)"
    if [[ -n "$recorded" ]] && recorded_python_usable; then
        printf '%s\n' "$recorded"
    else
        command -v python3 || true
    fi
}

## Succeed when ycmd can load a current ycm_core. This is the exact check ycmd
## runs at startup, which exits when it fails.
core_ready() {
    local python
    python="$(server_python)"
    [[ -n "$python" ]] || return 1
    "$python" - "$YCMD_DIR" >/dev/null 2>&1 <<'PY'
import sys

sys.path[0:0] = [sys.argv[1]]
from ycmd.utils import ImportAndCheckCore

sys.exit(ImportAndCheckCore())
PY
}

## Succeed when the install options request the bundled clangd.
clangd_wanted() {
    case " $YCM_INSTALL_ARGS " in
        *" --clangd-completer "* | *" --all "*) return 0 ;;
    esac
    return 1
}

clangd_binary() {
    printf '%s/third_party/clangd/output/bin/clangd' "$YCMD_DIR"
}

check_core() {
    if ! recorded_python_usable; then
        warn "The build recorded '$(recorded_python)' as the server's Python, but that is not an interpreter ycmd may run."
        return 1
    fi
    if core_ready; then
        success "ycm_core loads with $(server_python)."
        return 0
    fi
    warn "ycm_core is missing, outdated, or was built for another Python."
    return 1
}

check_clangd() {
    if ! clangd_wanted; then
        info "The bundled clangd is not requested by YCM_INSTALL_ARGS."
    elif [[ -x "$(clangd_binary)" ]]; then
        success "The bundled clangd is present."
    else
        warn "The bundled clangd is missing."
        return 1
    fi
}

## Report each component, then succeed only when everything is usable.
is_ready() {
    local ready=true

    check_core || ready=false
    check_clangd || ready=false
    [[ "$ready" == true ]]
}

python_has_headers() {
    local python

    python="$(command -v python3)" || return 1
    "$python" - >/dev/null 2>&1 <<'PY'
import os
import sys
import sysconfig

sys.exit(0 if os.path.exists(os.path.join(sysconfig.get_path("include"), "Python.h")) else 1)
PY
}

## Print the name of each missing build prerequisite, one per line.
missing_build_tools() {
    local tool

    for tool in git cmake make; do
        command -v "$tool" >/dev/null 2>&1 || printf '%s\n' "$tool"
    done
    if ! command -v c++ >/dev/null 2>&1 && ! command -v g++ >/dev/null 2>&1 &&
        ! command -v clang++ >/dev/null 2>&1; then
        printf '%s\n' 'a C++ compiler (g++ or clang++)'
    fi
    if ! command -v python3 >/dev/null 2>&1; then
        printf '%s\n' 'python3'
    elif ! python_has_headers; then
        printf '%s\n' 'the Python development headers (Python.h)'
    fi
}

require_build_tools() {
    local missing line

    missing="$(missing_build_tools)"
    [[ -z "$missing" ]] && return 0
    error "Cannot build YouCompleteMe; these prerequisites are missing:"
    while IFS= read -r line; do
        error "  - $line"
    done <<<"$missing"
    error "Install them with your package manager (Debian/Ubuntu: sudo apt install build-essential cmake git python3-dev) and run ycm.sh again."
    return 1
}

## Print the plugin submodules that are not at their pinned commit.
##
## An interrupted plugin install can leave a nested submodule uninitialised
## ('-') or on the wrong commit ('+'), and the build then fails with a missing
## file such as watchdog's setup.py.
submodule_problems() {
    git -C "$YCM_DIR" submodule status --recursive 2>/dev/null |
        awk '/^[-+U]/ { print $2 }' || true
}

sync_submodules() {
    local drifted line

    drifted="$(submodule_problems)"
    [[ -z "$drifted" ]] && return 0
    warn "These submodules are not at their pinned commits (an interrupted plugin install?):"
    while IFS= read -r line; do
        warn "  - $line"
    done <<<"$drifted"
    run git -C "$YCM_DIR" submodule update --init --recursive
}

build() {
    local python
    local -a args=()

    python="$(command -v python3)" || {
        error "python3 was not found."
        return 1
    }
    if [[ -n "$YCM_INSTALL_ARGS" ]]; then
        read -r -a args <<<"$YCM_INSTALL_ARGS"
    fi
    info "Building YouCompleteMe in $YCM_DIR; this compiles C++ and can take several minutes."
    if [[ "$DRY_RUN" == true ]]; then
        info "[dry-run] would run: $python install.py ${args[*]-}"
        return 0
    fi
    (cd "$YCM_DIR" && "$python" install.py ${args[@]+"${args[@]}"})
}

main() {
    parse_args "$@"
    YCMD_DIR="$YCM_DIR/third_party/ycmd"
    # A build must fail rather than stop to ask for credentials.
    export GIT_TERMINAL_PROMPT=0

    if [[ ! -f "$YCM_DIR/install.py" ]]; then
        error "YouCompleteMe is not installed in $YCM_DIR."
        error "Install the Vim plugins first, for example: nvim +PlugInstall +qall"
        return 1
    fi

    if [[ "$MODE" == check ]]; then
        is_ready
        return
    fi

    if is_ready && [[ "$FORCE" != true ]]; then
        success "YouCompleteMe is already built; use --force to rebuild it."
        return 0
    fi

    require_build_tools || return 1
    sync_submodules || return 1
    build || {
        error "The YouCompleteMe build failed; the output above shows why. Fix it and run ycm.sh again."
        return 1
    }

    if [[ "$DRY_RUN" == true ]]; then
        info "Dry run complete; nothing was changed."
        return 0
    fi
    if is_ready; then
        success "YouCompleteMe is ready. Restart its server with :YcmRestartServer or restart Vim."
        return 0
    fi
    error "The build finished but YouCompleteMe is still not usable; read the output above."
    return 1
}

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
    main "$@"
fi
