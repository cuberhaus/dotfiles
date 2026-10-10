#!/usr/bin/env bash
# Hermetic tests for the Cursor CLI step in bootstrap/base_functions (cursor_cli_install and
# cursor_cli_uninstall) and for how the profiles call them.  Nothing here touches the network or
# the real home directory: curl is a function that prints a small stand-in for Cursor's installer,
# HOME is a temporary directory, and PATH holds only the few real tools the stand-in needs, so a
# `cursor-agent` that is installed on the machine running the test cannot make a case skip.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BOOTSTRAP_DIR="$REPO_ROOT/.local/scripts/bootstrap"
SCRATCH="$(mktemp -d)"
trap 'rm -rf "$SCRATCH"' EXIT

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

assert_equals() {
    [ "$1" = "$2" ] || fail "$3: expected [$1], got [$2]"
}

assert_contains() {
    case "$1" in
        *"$2"*) ;;
        *) fail "$3: [$2] not found in [$1]" ;;
    esac
}

assert_not_contains() {
    case "$1" in
        *"$2"*) fail "$3: [$2] must not appear in [$1]" ;;
        *) ;;
    esac
}

assert_file() {
    [ -e "$1" ] || [ -L "$1" ] || fail "$2: $1 does not exist"
}

assert_no_file() {
    { [ ! -e "$1" ] && [ ! -L "$1" ]; } || fail "$2: $1 exists"
}

# shellcheck source=/dev/null
source "$BOOTSTRAP_DIR/base_functions"

# The real tools the stand-in installer, the two functions and these tests run.  They are linked
# into a folder that becomes the whole PATH, apart from ~/.local/bin, where the installer puts
# the CLI.
TOOLS="$SCRATCH/tools"
mkdir -p "$TOOLS"
for tool in awk bash cat chmod cut grep ln mkdir readlink rm sort touch; do
    ln -s "$(command -v "$tool")" "$TOOLS/$tool"
done

###############################################################
# => Stubs and a fresh sandbox for each test
###############################################################

# Every test runs in its own subshell (see the loop at the end), so these exports and the curl
# override never reach the next test.
new_sandbox() {
    export HOME="$SCRATCH/home-$1"
    export CALLS="$SCRATCH/calls-$1.log"
    export INSTALLER_RAN="$SCRATCH/installer-ran-$1"
    mkdir -p "$HOME"
    : > "$CALLS"
    PATH="$HOME/.local/bin:$TOOLS"
    export PATH
}

# How the stand-in installer behaves, set per test:
#   FAKE_CURL_FAIL=true       the download fails
#   FAKE_INSTALLER=exit1      the script runs and fails
#   FAKE_INSTALLER=no-links   the script runs and succeeds but links nothing
curl() {
    printf 'curl %s\n' "$*" >> "$CALLS"
    [ "${FAKE_CURL_FAIL:-}" != true ] || return 22
    case "${FAKE_INSTALLER:-}" in
        exit1)
            cat <<'INSTALLER'
touch "$INSTALLER_RAN"
exit 1
INSTALLER
            ;;
        no-links)
            cat <<'INSTALLER'
touch "$INSTALLER_RAN"
exit 0
INSTALLER
            ;;
        *)
            # The layout Cursor's installer makes: a versioned folder, and two absolute links.
            cat <<'INSTALLER'
touch "$INSTALLER_RAN"
version_dir="$HOME/.local/share/cursor-agent/versions/2099.01.01-test"
mkdir -p "$version_dir" "$HOME/.local/bin"
printf '#!/bin/sh\n' > "$version_dir/cursor-agent"
chmod +x "$version_dir/cursor-agent"
ln -sf "$version_dir/cursor-agent" "$HOME/.local/bin/agent"
ln -sf "$version_dir/cursor-agent" "$HOME/.local/bin/cursor-agent"
INSTALLER
            ;;
    esac
}

curl_calls() {
    grep -c '^curl ' "$CALLS" || true
}

###############################################################
# => cursor_cli_install
###############################################################

test_install_runs_cursors_installer_and_links_the_cli() {
    new_sandbox fresh
    local output
    output=$(cursor_cli_install 2>&1) || fail "install returned $?: $output"

    assert_file "$INSTALLER_RAN" "the downloaded installer ran"
    assert_equals 1 "$(curl_calls)" "downloads once"
    assert_contains "$(cat "$CALLS")" "curl -fsSL https://cursor.com/install" "downloads Cursor's installer, failing on HTTP errors"
    assert_equals "$HOME/.local/share/cursor-agent/versions/2099.01.01-test/cursor-agent" \
        "$(readlink "$HOME/.local/bin/cursor-agent")" "cursor-agent points into the program folder"
    assert_equals "$HOME/.local/share/cursor-agent/versions/2099.01.01-test/cursor-agent" \
        "$(readlink "$HOME/.local/bin/agent")" "agent points into the program folder"
    assert_contains "$output" "agent login" "tells the user how to sign in"
}

test_install_is_skipped_when_already_installed() {
    new_sandbox rerun
    cursor_cli_install >/dev/null 2>&1 || fail "the first install failed"
    rm -f "$INSTALLER_RAN"

    local output
    output=$(cursor_cli_install 2>&1) || fail "the second run returned $?: $output"

    assert_equals 1 "$(curl_calls)" "the second run does not download again"
    assert_no_file "$INSTALLER_RAN" "the second run does not run an installer"
    assert_contains "$output" "already installed" "says it skipped"
    assert_contains "$output" "agent update" "points at the updater"
}

test_install_is_skipped_when_cursor_agent_is_on_path_elsewhere() {
    new_sandbox elsewhere
    mkdir -p "$SCRATCH/other-bin"
    printf '#!/bin/sh\n' > "$SCRATCH/other-bin/cursor-agent"
    chmod +x "$SCRATCH/other-bin/cursor-agent"
    PATH="$SCRATCH/other-bin:$PATH"

    cursor_cli_install >/dev/null 2>&1 || fail "install returned non-zero for a CLI that is already on PATH"

    assert_equals 0 "$(curl_calls)" "does not download when the command exists"
    assert_no_file "$HOME/.local/bin/cursor-agent" "does not add a second copy"
}

test_install_fails_without_running_anything_when_the_download_fails() {
    new_sandbox download-fails
    export FAKE_CURL_FAIL=true
    local output
    if output=$(cursor_cli_install 2>&1); then
        fail "install succeeded although the download failed: $output"
    fi

    assert_contains "$output" "Could not download" "names the failed stage"
    assert_no_file "$INSTALLER_RAN" "nothing ran"
    assert_no_file "$HOME/.local/bin/cursor-agent" "nothing was linked"
}

test_install_fails_when_the_installer_fails() {
    new_sandbox installer-fails
    export FAKE_INSTALLER=exit1
    local output
    if output=$(cursor_cli_install 2>&1); then
        fail "install succeeded although the installer failed: $output"
    fi

    assert_file "$INSTALLER_RAN" "the installer did run"
    assert_contains "$output" "installer failed" "names the failed stage"
}

test_install_fails_when_the_installer_leaves_no_command() {
    new_sandbox no-links
    export FAKE_INSTALLER=no-links
    local output
    if output=$(cursor_cli_install 2>&1); then
        fail "install reported success without a cursor-agent command: $output"
    fi

    assert_contains "$output" "did not create" "names the missing command"
    assert_contains "$output" ".local/bin/cursor-agent" "names the path that is missing"
}

test_install_does_not_pipe_the_download_into_a_shell() {
    # A dropped connection must not run the first half of the script, so every installer in the
    # bootstrap downloads the script in full first.
    local offenders
    offenders=$(grep -rnE 'cursor\.com/install[^|]*\|[[:space:]]*(sudo )?(ba)?sh' "$BOOTSTRAP_DIR" || true)
    assert_equals "" "$offenders" "no bootstrap file pipes the Cursor installer into a shell"
}

###############################################################
# => cursor_cli_uninstall
###############################################################

test_uninstall_removes_the_program_and_its_links() {
    new_sandbox uninstall
    cursor_cli_install >/dev/null 2>&1 || fail "install failed"
    mkdir -p "$HOME/.cursor"
    printf '{}\n' > "$HOME/.cursor/cli-config.json"

    cursor_cli_uninstall

    assert_no_file "$HOME/.local/share/cursor-agent" "the program folder is gone"
    assert_no_file "$HOME/.local/bin/agent" "the agent link is gone"
    assert_no_file "$HOME/.local/bin/cursor-agent" "the cursor-agent link is gone"
    assert_file "$HOME/.cursor/cli-config.json" "the sign-in and settings are kept"
}

test_uninstall_keeps_a_command_it_did_not_install() {
    new_sandbox uninstall-foreign
    cursor_cli_install >/dev/null 2>&1 || fail "install failed"
    # Something else owns `agent`: a link elsewhere, and the cursor-agent name as a plain file.
    printf '#!/bin/sh\n' > "$SCRATCH/foreign-agent"
    ln -sf "$SCRATCH/foreign-agent" "$HOME/.local/bin/agent"
    rm -f "$HOME/.local/bin/cursor-agent"
    printf 'mine\n' > "$HOME/.local/bin/cursor-agent"

    cursor_cli_uninstall

    assert_equals "$SCRATCH/foreign-agent" "$(readlink "$HOME/.local/bin/agent")" "a link that points elsewhere stays"
    assert_equals "mine" "$(cat "$HOME/.local/bin/cursor-agent")" "a regular file stays"
    assert_no_file "$HOME/.local/share/cursor-agent" "the program folder is still removed"
}

test_uninstall_is_a_noop_when_nothing_is_installed() {
    new_sandbox uninstall-empty
    cursor_cli_uninstall || fail "uninstall returned $? on a machine without the CLI"
}

###############################################################
# => How the profiles use the two functions
###############################################################

# The text of one function: from its definition to the first line that is a lone closing brace.
function_text() {
    awk -v name="$2" '
        $0 ~ "^" name "[[:space:]]*\\(\\)" { inside = 1 }
        inside { print }
        inside && /^}/ { exit }
    ' "$BOOTSTRAP_DIR/$1"
}

test_the_installing_profiles_call_the_step_without_aborting() {
    local file function_name body
    for pair in ubuntu_functions:ai_tools_install arch_functions:ai_tools_install \
        mac_functions:ai_tools_install work_functions:gui_apps_install; do
        file=${pair%%:*}
        function_name=${pair##*:}
        body=$(function_text "$file" "$function_name")
        assert_contains "$body" "cursor_cli_install ||" "$file $function_name runs the step as a non-fatal command"
        assert_contains "$body" "warn \"Cursor CLI setup failed; rerun it with:" "$file $function_name tells how to rerun it"
    done
}

test_the_uninstallers_remove_the_step() {
    assert_contains "$(function_text uninstall_ubuntu_functions ubuntu_gui_uninstall)" "cursor_cli_uninstall" \
        "the Ubuntu uninstaller offers the step"
    assert_contains "$(function_text uninstall_work_functions gui_apps_uninstall)" "cursor_cli_uninstall" \
        "the work uninstaller offers the step"
}

test_the_functions_are_defined_in_the_shared_library_only() {
    local definitions
    definitions=$(grep -rnE '^cursor_cli_(un)?install[[:space:]]*\(\)' "$BOOTSTRAP_DIR" | cut -d: -f1 | sort -u)
    assert_equals "$BOOTSTRAP_DIR/base_functions" "$definitions" "one definition of each, in base_functions"
}

for test_name in \
    test_install_runs_cursors_installer_and_links_the_cli \
    test_install_is_skipped_when_already_installed \
    test_install_is_skipped_when_cursor_agent_is_on_path_elsewhere \
    test_install_fails_without_running_anything_when_the_download_fails \
    test_install_fails_when_the_installer_fails \
    test_install_fails_when_the_installer_leaves_no_command \
    test_install_does_not_pipe_the_download_into_a_shell \
    test_uninstall_removes_the_program_and_its_links \
    test_uninstall_keeps_a_command_it_did_not_install \
    test_uninstall_is_a_noop_when_nothing_is_installed \
    test_the_installing_profiles_call_the_step_without_aborting \
    test_the_uninstallers_remove_the_step \
    test_the_functions_are_defined_in_the_shared_library_only; do
    # A subshell keeps each test's stub overrides and exported variables to itself.
    ( "$test_name" ) || {
        printf 'not ok - %s\n' "$test_name" >&2
        exit 1
    }
    printf 'ok - %s\n' "$test_name"
done

printf 'cursor cli install tests passed.\n'
