#!/usr/bin/env bash
# Tests for the YouCompleteMe provisioning: ycm.sh, the shared vim_plugins_install
# step, and the profile wiring around them. Hermetic: nothing is built,
# downloaded, or installed, and HOME is a temporary directory.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
YCM_SCRIPT="$REPO_ROOT/.local/scripts/ycm.sh"
BASE_FUNCTIONS="$REPO_ROOT/.local/scripts/bootstrap/base_functions"
BOOTSTRAP_DIR="$REPO_ROOT/.local/scripts/bootstrap"
CASE_DIR="$(mktemp -d)"
EVENT_LOG="$CASE_DIR/events.log"
RM_COMMAND="$(command -v rm)"
trap '"$RM_COMMAND" -rf "$CASE_DIR"' EXIT

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

assert_equals() {
    [ "$1" = "$2" ] || fail "$3: expected '$1' but got '$2'"
}

assert_contains() {
    case "$1" in
        *"$2"*) ;;
        *) fail "$3: expected to find '$2' in:"$'\n'"$1" ;;
    esac
}

assert_not_contains() {
    case "$1" in
        *"$2"*) fail "$3: did not expect '$2' in:"$'\n'"$1" ;;
    esac
}

export HOME="$CASE_DIR/home"
mkdir -p "$HOME"
unset YCM_DIR YCM_INSTALL_ARGS YCM_CORES
: > "$EVENT_LOG"

[ -x "$YCM_SCRIPT" ] || fail 'ycm.sh must stay executable'

# Sourcing defines the functions without running main.
source "$YCM_SCRIPT"

###############################################################################
# Real probes. These run before the machine-dependent ones are replaced below.
###############################################################################

test_missing_build_tools_are_reported_by_name() {
    local original_path="$PATH" tools="$CASE_DIR/tools-bin" output
    mkdir -p "$tools"

    # A python3 that cannot find the headers, and nothing else on PATH.
    printf '#!/bin/sh\n/bin/cat >/dev/null\nexit 1\n' > "$tools/python3"
    chmod +x "$tools/python3"
    PATH="$tools"
    output="$(missing_build_tools)"
    PATH="$original_path"
    for expected in git cmake make 'C++ compiler' 'Python development headers'; do
        assert_contains "$output" "$expected" "a machine without build tools must be told it lacks: $expected"
    done

    # Everything present, with clang++ standing in for the GNU compiler.
    printf '#!/bin/sh\n/bin/cat >/dev/null\nexit 0\n' > "$tools/python3"
    for tool in git cmake make clang++; do
        printf '#!/bin/sh\nexit 0\n' > "$tools/$tool"
        chmod +x "$tools/$tool"
    done
    PATH="$tools"
    output="$(missing_build_tools)"
    PATH="$original_path"
    assert_equals '' "$output" 'a machine with every build tool must not be told anything is missing'

    # No Python at all is reported as such, not as missing headers.
    "$RM_COMMAND" -f "$tools/python3"
    PATH="$tools"
    output="$(missing_build_tools)"
    PATH="$original_path"
    assert_equals 'python3' "$output" 'a machine without Python must be told python3 is missing'
}

test_submodule_problems_lists_drifted_submodules() {
    local child="$CASE_DIR/child" parent="$CASE_DIR/parent" first

    export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1
    export GIT_AUTHOR_NAME=test GIT_AUTHOR_EMAIL=test@example.invalid
    export GIT_COMMITTER_NAME=test GIT_COMMITTER_EMAIL=test@example.invalid
    git -c init.defaultBranch=main init -q "$child"
    git -C "$child" commit -q --allow-empty -m one
    first="$(git -C "$child" rev-parse HEAD)"
    git -C "$child" commit -q --allow-empty -m two
    git -c init.defaultBranch=main init -q "$parent"
    git -C "$parent" -c protocol.file.allow=always submodule add -q "$child" libs/child 2> /dev/null
    git -C "$parent" commit -q -m 'add the submodule'
    YCM_DIR="$parent"

    assert_equals '' "$(submodule_problems)" 'a submodule at its pinned commit is not a problem'

    git -C "$parent/libs/child" checkout -q "$first" 2> /dev/null
    assert_equals 'libs/child' "$(submodule_problems)" 'a submodule on another commit must be listed'

    git -C "$parent" -c protocol.file.allow=always submodule update -q --init
    assert_equals '' "$(submodule_problems)" 'a repaired submodule must no longer be listed'

    git -C "$parent" submodule deinit -q -f libs/child 2> /dev/null
    assert_equals 'libs/child' "$(submodule_problems)" 'an uninitialised submodule must be listed'

    YCM_DIR="$CASE_DIR/not-a-repository"
    mkdir -p "$YCM_DIR"
    assert_equals '' "$(submodule_problems)" 'a plugin directory that is not a repository has no submodule problems'
}

test_missing_build_tools_are_reported_by_name
test_submodule_problems_lists_drifted_submodules

###############################################################################
# ycm.sh against a fake plugin checkout
###############################################################################

# From here on the probes that depend on the machine are controlled by the test.
missing_build_tools() { printf '%s' "$FAKE_MISSING_TOOLS"; }
submodule_problems() { printf '%s' "$FAKE_SUBMODULE_PROBLEMS"; }
git() { printf 'git %s\n' "$*" >> "$EVENT_LOG"; }
sudo() { printf 'sudo %s\n' "$*" >> "$EVENT_LOG"; }

CASE_COUNT=0

## Create a fake YouCompleteMe checkout whose install.py "builds" by writing the
## marker files that the fake ycmd.utils checks. Points YCM_DIR at it.
new_plugin() {
    CASE_COUNT=$((CASE_COUNT + 1))
    YCM_DIR="$CASE_DIR/plugin-$CASE_COUNT"
    YCMD="$YCM_DIR/third_party/ycmd"
    mkdir -p "$YCMD/ycmd"
    : > "$YCMD/ycmd/__init__.py"
    cat > "$YCMD/ycmd/utils.py" << 'PY'
import os

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def ImportAndCheckCore():
    if os.path.exists(os.path.join(ROOT, "core.outdated")):
        return 4
    return 0 if os.path.exists(os.path.join(ROOT, "core.ok")) else 3
PY
    cat > "$YCM_DIR/install.py" << 'PY'
import os
import sys

root = os.path.dirname(os.path.abspath(__file__))
ycmd = os.path.join(root, "third_party", "ycmd")
with open(os.environ["FAKE_INSTALL_LOG"], "a") as log:
    log.write("install.py " + " ".join(sys.argv[1:]) + "|" + os.getcwd() + "\n")
mode = os.environ.get("FAKE_INSTALL_MODE", "ok")
if mode == "fail":
    sys.exit(1)
if mode == "noop":
    sys.exit(0)
open(os.path.join(ycmd, "core.ok"), "w").close()
outdated = os.path.join(ycmd, "core.outdated")
if os.path.exists(outdated):
    os.remove(outdated)
with open(os.path.join(ycmd, "PYTHON_USED_DURING_BUILDING"), "w") as recorded:
    recorded.write(sys.executable)
if "--clangd-completer" in sys.argv or "--all" in sys.argv:
    directory = os.path.join(ycmd, "third_party", "clangd", "output", "bin")
    os.makedirs(directory, exist_ok=True)
    clangd = os.path.join(directory, "clangd")
    open(clangd, "w").close()
    os.chmod(clangd, 0o755)
PY
    : > "$EVENT_LOG"
    export FAKE_INSTALL_LOG="$EVENT_LOG" FAKE_INSTALL_MODE=ok
    FAKE_MISSING_TOOLS=''
    FAKE_SUBMODULE_PROBLEMS=''
    YCM_INSTALL_ARGS='--clangd-completer'
}

## Mark the fake checkout as already built, the way a successful install.py would.
mark_built() {
    mkdir -p "$YCMD/third_party/clangd/output/bin"
    : > "$YCMD/core.ok"
    : > "$YCMD/third_party/clangd/output/bin/clangd"
    chmod +x "$YCMD/third_party/clangd/output/bin/clangd"
}

## The line install.py logs for a given argument string.
install_line() {
    printf 'install.py %s|%s' "$1" "$(cd "$YCM_DIR" && pwd -P)"
}

## Run main in a subshell; leaves STATUS and OUTPUT set. An `exit` inside main
## (usage errors) ends only the subshell.
run_main() {
    local output_file="$CASE_DIR/main.out"
    STATUS=0
    ( main "$@" ) > "$output_file" 2>&1 || STATUS=$?
    OUTPUT="$(<"$output_file")"
}

test_help_and_usage_errors() {
    new_plugin
    run_main --help
    assert_equals 0 "$STATUS" '--help must succeed'
    assert_contains "$OUTPUT" 'Usage: ycm.sh' '--help must print the usage'
    assert_contains "$OUTPUT" '--check' '--help must describe --check'

    run_main --bogus
    assert_equals 2 "$STATUS" 'an unknown option must be a usage error'
    assert_contains "$OUTPUT" 'Unknown option: --bogus' 'an unknown option must be named'

    run_main --check --force
    assert_equals 2 "$STATUS" '--check must refuse --force'
    run_main --check --dry-run
    assert_equals 2 "$STATUS" '--check must refuse --dry-run'
    assert_equals '' "$(cat "$EVENT_LOG")" 'usage errors must not build anything'
}

test_a_missing_plugin_is_explained() {
    new_plugin
    YCM_DIR="$CASE_DIR/not-installed"
    run_main
    assert_equals 1 "$STATUS" 'a missing plugin must fail'
    assert_contains "$OUTPUT" 'is not installed' 'a missing plugin must be named'
    assert_contains "$OUTPUT" 'PlugInstall' 'a missing plugin must say how to install it'
}

test_check_reports_without_building() {
    new_plugin
    run_main --check
    assert_equals 1 "$STATUS" '--check must fail for an unbuilt plugin'
    assert_contains "$OUTPUT" 'ycm_core is missing' '--check must say what is wrong'
    assert_contains "$OUTPUT" 'clangd is missing' '--check must report the missing clangd'
    assert_equals '' "$(cat "$EVENT_LOG")" '--check must not build'

    mark_built
    run_main --check
    assert_equals 0 "$STATUS" '--check must succeed for a built plugin'
    assert_contains "$OUTPUT" 'ycm_core loads' '--check must say the core works'
}

test_an_unbuilt_plugin_is_built_once() {
    new_plugin
    run_main
    assert_equals 0 "$STATUS" "an unbuilt plugin must be built: $OUTPUT"
    assert_equals "$(install_line --clangd-completer)" "$(cat "$EVENT_LOG")" \
        'install.py must run once, in the plugin directory, with the default options'
    assert_contains "$OUTPUT" 'YouCompleteMe is ready' 'a successful build must say so'

    : > "$EVENT_LOG"
    run_main
    assert_equals 0 "$STATUS" 'a second run must succeed'
    assert_contains "$OUTPUT" 'already built' 'a second run must find the build usable'
    assert_equals '' "$(cat "$EVENT_LOG")" 'a second run must not build again'

    run_main --force
    assert_equals 0 "$STATUS" '--force must succeed'
    assert_equals "$(install_line --clangd-completer)" "$(cat "$EVENT_LOG")" '--force must rebuild a working checkout'
    assert_not_contains "$(cat "$EVENT_LOG")" 'sudo' 'ycm.sh must never use sudo'
}

test_dry_run_changes_nothing() {
    new_plugin
    FAKE_SUBMODULE_PROBLEMS='third_party/ycmd/third_party/watchdog_deps/watchdog'
    run_main --dry-run
    assert_equals 0 "$STATUS" 'a dry run must succeed'
    assert_contains "$OUTPUT" '[dry-run] would run:' 'a dry run must describe what it would do'
    assert_contains "$OUTPUT" 'submodule update --init --recursive' 'a dry run must show the submodule repair'
    assert_contains "$OUTPUT" 'install.py --clangd-completer' 'a dry run must show the build'
    assert_equals '' "$(cat "$EVENT_LOG")" 'a dry run must neither build nor touch git'
    [ ! -e "$YCMD/core.ok" ] || fail 'a dry run must not create the core'
}

test_install_options_come_from_the_environment() {
    new_plugin
    YCM_INSTALL_ARGS='--all'
    run_main
    assert_equals 0 "$STATUS" 'a build with --all must succeed'
    assert_equals "$(install_line --all)" "$(cat "$EVENT_LOG")" 'YCM_INSTALL_ARGS must reach install.py'

    new_plugin
    YCM_INSTALL_ARGS=''
    run_main
    assert_equals 0 "$STATUS" 'a core-only build must succeed'
    assert_equals "$(install_line '')" "$(cat "$EVENT_LOG")" 'an empty YCM_INSTALL_ARGS must build the core only'
    assert_contains "$OUTPUT" 'not requested' 'a core-only build must not insist on clangd'
    run_main --check
    assert_equals 0 "$STATUS" 'a core-only build must count as ready'
}

test_missing_prerequisites_block_only_the_build() {
    new_plugin
    FAKE_MISSING_TOOLS=$'cmake\nthe Python development headers (Python.h)'
    run_main
    assert_equals 1 "$STATUS" 'a build without its tools must fail'
    assert_contains "$OUTPUT" 'cmake' 'the missing tool must be named'
    assert_contains "$OUTPUT" 'Python development headers' 'the missing headers must be named'
    assert_contains "$OUTPUT" 'python3-dev' 'the failure must say how to fix it'
    assert_equals '' "$(cat "$EVENT_LOG")" 'nothing may run when a prerequisite is missing'

    run_main --check
    assert_equals 1 "$STATUS" '--check still reports the unbuilt plugin'
    assert_not_contains "$OUTPUT" 'prerequisites' '--check must not need the build tools'

    mark_built
    run_main
    assert_equals 0 "$STATUS" 'a working build must not need the build tools'
}

test_build_failures_are_reported() {
    new_plugin
    FAKE_INSTALL_MODE=fail
    run_main
    assert_equals 1 "$STATUS" 'a failing install.py must fail'
    assert_contains "$OUTPUT" 'build failed' 'a failing install.py must be reported'

    new_plugin
    FAKE_INSTALL_MODE=noop
    run_main
    assert_equals 1 "$STATUS" 'a build that leaves the core unusable must fail'
    assert_contains "$OUTPUT" 'still not usable' 'an install.py that builds nothing must be reported'
}

test_an_outdated_core_is_rebuilt() {
    new_plugin
    mark_built
    : > "$YCMD/core.outdated"
    run_main --check
    assert_equals 1 "$STATUS" 'an outdated core must not count as ready'
    run_main
    assert_equals 0 "$STATUS" 'an outdated core must be rebuilt'
    assert_equals "$(install_line --clangd-completer)" "$(cat "$EVENT_LOG")" 'an outdated core must run install.py'
    [ ! -e "$YCMD/core.outdated" ] || fail 'the rebuild must replace the outdated core'
}

test_drifted_submodules_are_synced_before_the_build() {
    new_plugin
    FAKE_SUBMODULE_PROBLEMS='third_party/ycmd/third_party/watchdog_deps/watchdog'
    run_main
    assert_equals 0 "$STATUS" "a build with drifted submodules must succeed: $OUTPUT"
    assert_contains "$OUTPUT" 'watchdog' 'the drifted submodule must be named'
    assert_equals "git -C $YCM_DIR submodule update --init --recursive"$'\n'"$(install_line --clangd-completer)" \
        "$(cat "$EVENT_LOG")" 'submodules must be synced before install.py runs'

    new_plugin
    run_main
    assert_not_contains "$(cat "$EVENT_LOG")" 'git ' 'submodules that are in sync must not be touched'
}

test_a_recorded_interpreter_that_is_not_python_is_never_run() {
    new_plugin
    mark_built
    # What a shell exporting ARGV0 (an AppImage launcher) makes install.py record.
    printf '#!/bin/sh\ntouch "%s"\n' "$CASE_DIR/executed" > "$CASE_DIR/cursor.AppImage"
    chmod +x "$CASE_DIR/cursor.AppImage"
    printf '%s' "$CASE_DIR/cursor.AppImage" > "$YCMD/PYTHON_USED_DURING_BUILDING"

    run_main --check
    assert_equals 1 "$STATUS" 'a recorded interpreter that is not Python must fail the check'
    assert_contains "$OUTPUT" 'not an interpreter' 'the unusable interpreter must be named as the problem'
    [ ! -e "$CASE_DIR/executed" ] || fail 'a recorded interpreter that is not Python must never be executed'

    run_main
    assert_equals 0 "$STATUS" 'the next build must repair the recorded interpreter'
    assert_equals "$(install_line --clangd-completer)" "$(cat "$EVENT_LOG")" 'the repair must rebuild once'
    [ ! -e "$CASE_DIR/executed" ] || fail 'the repair must not execute the bogus interpreter either'
    run_main --check
    assert_equals 0 "$STATUS" 'the repaired build must pass the check'
}

test_the_script_runs_as_a_command() {
    new_plugin
    local status=0 output
    output="$(YCM_DIR="$YCM_DIR" "$YCM_SCRIPT" --check 2>&1)" || status=$?
    assert_equals 1 "$status" 'running the script must run main'
    assert_contains "$output" 'ycm_core is missing' 'running the script must report the plugin state'
    output="$("$YCM_SCRIPT" --help 2>&1)"
    assert_contains "$output" 'Usage: ycm.sh' 'running the script with --help must print the usage'
}

test_help_and_usage_errors
test_a_missing_plugin_is_explained
test_check_reports_without_building
test_an_unbuilt_plugin_is_built_once
test_dry_run_changes_nothing
test_install_options_come_from_the_environment
test_missing_prerequisites_block_only_the_build
test_build_failures_are_reported
test_an_outdated_core_is_rebuilt
test_drifted_submodules_are_synced_before_the_build
test_a_recorded_interpreter_that_is_not_python_is_never_run
test_the_script_runs_as_a_command

###############################################################################
# vim_plugins_install: the shared bootstrap step
###############################################################################

## Run vim_plugins_install in a subshell that sources base_functions, with a
## recording Neovim and ycm.sh. Leaves STATUS and OUTPUT set.
run_plugin_step() {
    local dotfiles="$CASE_DIR/dotfiles-root" output_file="$CASE_DIR/plugin-step.out"
    mkdir -p "$dotfiles/.local/scripts" "$HOME/.vim"
    cat > "$dotfiles/.local/scripts/ycm.sh" << 'EOF'
#!/usr/bin/env bash
printf 'ycm.sh\n' >> "$EVENT_LOG"
[ "${FAKE_YCM_FAIL:-false}" != true ]
EOF
    : > "$EVENT_LOG"
    STATUS=0
    (
        # shellcheck disable=SC1090
        source "$BASE_FUNCTIONS"
        export DOTFILES_ROOT="$dotfiles" EVENT_LOG
        nvim() {
            local stdin=closed
            if IFS= read -r _line; then stdin=open; fi
            printf 'nvim GIT_TERMINAL_PROMPT=%s stdin=%s %s\n' "${GIT_TERMINAL_PROMPT:-}" "$stdin" "$*" >> "$EVENT_LOG"
            return "${FAKE_NVIM_EXIT:-0}"
        }
        if [ "${FAKE_NO_NVIM:-false}" = true ]; then
            unset -f nvim
            # Neovim must be unfindable, so the search path is replaced on purpose.
            # shellcheck disable=SC2123
            PATH="$CASE_DIR/empty-bin"
        fi
        # Anything left on stdin would reach a plugin hook that prompts; Neovim must not read it.
        printf 'unread\n' | vim_plugins_install
    ) > "$output_file" 2>&1 || STATUS=$?
    OUTPUT="$(<"$output_file")"
}

test_the_plugin_step_installs_then_verifies() {
    mkdir -p "$CASE_DIR/empty-bin" "$HOME/.vim"
    : > "$HOME/.vim/vimrc"

    run_plugin_step
    assert_equals 0 "$STATUS" "the plugin step must succeed: $OUTPUT"
    assert_equals "nvim GIT_TERMINAL_PROMPT=0 stdin=closed --headless +PlugInstall --sync +qa!"$'\nycm.sh' \
        "$(cat "$EVENT_LOG")" 'the plugins must be installed headless, then YouCompleteMe verified'

    FAKE_NVIM_EXIT=3 run_plugin_step
    assert_equals 1 "$STATUS" 'a Neovim failure must fail the step'
    assert_contains "$(cat "$EVENT_LOG")" 'ycm.sh' 'YouCompleteMe must still be verified after a Neovim failure'

    FAKE_YCM_FAIL=true run_plugin_step
    assert_equals 1 "$STATUS" 'a failed YouCompleteMe build must fail the step'

    FAKE_NO_NVIM=true run_plugin_step
    assert_equals 1 "$STATUS" 'a machine without Neovim must fail the step'
    assert_contains "$OUTPUT" 'Neovim is not installed' 'a missing Neovim must be explained'
    assert_equals '' "$(cat "$EVENT_LOG")" 'nothing may run without Neovim'

    "$RM_COMMAND" -f "$HOME/.vim/vimrc"
    run_plugin_step
    assert_equals 1 "$STATUS" 'a missing vimrc must fail the step'
    assert_contains "$OUTPUT" 'vimrc' 'a missing vimrc must be named'
    assert_equals '' "$(cat "$EVENT_LOG")" 'nothing may run without the plugin list'
}

test_the_plugin_step_installs_then_verifies

###############################################################################
# Profile wiring and invariants
###############################################################################

test_the_script_default_matches_the_vim_plug_hook() {
    local hook script_default
    hook="$(sed -n "s/.*'do': 'python3 install\.py \(.*\)' }.*/\1/p" "$REPO_ROOT/.vim/vimrc")"
    # The quoted script is meant to expand in the child shell, not here.
    # shellcheck disable=SC2016
    script_default="$(env -u YCM_INSTALL_ARGS bash -c 'source "$1" && printf "%s" "$YCM_INSTALL_ARGS"' _ "$YCM_SCRIPT")"
    [ -n "$hook" ] || fail 'the vimrc must build YouCompleteMe from a vim-plug do hook'
    assert_equals "$hook" "$script_default" \
        "ycm.sh must build what the vim-plug hook builds, or the two would fight over the checkout"
}

test_the_script_leaves_nothing_of_the_old_design_behind() {
    local content
    content="$(<"$YCM_SCRIPT")"
    assert_not_contains "$content" 'ycm_build' 'ycm.sh must not build in a ycm_build directory under HOME any more'
    assert_not_contains "$content" '.vim/bundle' 'ycm.sh must not use the Vundle-era plugin directory'
    assert_not_contains "$content" 'apt-get' 'ycm.sh must not install system packages'
    assert_not_contains "$content" 'pacman' 'ycm.sh must not install system packages'
    assert_not_contains "$content" 'brew install' 'ycm.sh must not install system packages'
}

test_older_profiles_survive_a_failed_build() {
    local file
    for file in ubuntu_functions arch_functions mac_functions; do
        grep -Eq '(bash "[$]ycm"|ycm\.sh") *\|\|' "$BOOTSTRAP_DIR/$file" ||
            fail "$file must warn, not abort, when the YouCompleteMe build fails"
    done
    if grep -Fq 'plugged/YouCompleteMe/install.py' "$BOOTSTRAP_DIR/ubuntu_functions"; then
        fail 'ubuntu must not run install.py itself after ycm.sh has verified the build'
    fi
    grep -Fq 'python3-dev' "$BOOTSTRAP_DIR/ubuntu_functions" ||
        fail 'ubuntu must install the Python headers that ycm.sh no longer installs'
}

test_gentoo_declares_the_neovim_python_provider() {
    grep -Eq '^dev-python/pynvim( |$)' "$BOOTSTRAP_DIR/gentoo.packages" ||
        fail 'the Gentoo manifest must install the Python provider Neovim needs to load YouCompleteMe'
}

test_the_script_default_matches_the_vim_plug_hook
test_the_script_leaves_nothing_of_the_old_design_behind
test_older_profiles_survive_a_failed_build
test_gentoo_declares_the_neovim_python_provider

printf 'YouCompleteMe provisioning tests passed.\n'
