#!/usr/bin/env bash
# Hermetic tests for the OpenLogi step in bootstrap/ubuntu_functions: the release lookup, the
# signature check that must pass before apt sees the package, the failure paths, the uninstall
# function, and how the Ubuntu profile uses them.  Nothing here touches the network, /etc, or
# the real apt: curl, minisign, dpkg, systemctl, sudo and apt-get are replaced by functions that
# act inside a temporary directory and record what they were asked to do.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BOOTSTRAP_DIR="$REPO_ROOT/.local/scripts/bootstrap"
SCRATCH="$(mktemp -d)"
# Resolved here, before the command() stub below exists, so this is the real builtin.
# shellcheck disable=SC2218
RM_COMMAND="$(command -v rm)"
trap '"$RM_COMMAND" -rf "$SCRATCH"' EXIT

RELEASES=https://github.com/AprilNEA/OpenLogi/releases
LATEST_URL="$RELEASES/tag/v1.2.3"
# The vendor's trust anchor (packaging/linux/install.sh in AprilNEA/OpenLogi).  This second copy
# makes changing the key in ubuntu_functions a deliberate act that fails this test until both agree.
VENDOR_KEY='RWRRkFtw+rqkvTlCTGKUszSE5dX9CK1teaQD45jO4P9rYlWLO4/nHVUF' # gitleaks:allow (public key)

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

# shellcheck source=/dev/null
source "$BOOTSTRAP_DIR/base_functions"
# shellcheck source=/dev/null
source "$BOOTSTRAP_DIR/ubuntu_functions"
# shellcheck source=/dev/null
source "$BOOTSTRAP_DIR/uninstall_ubuntu_functions"

###############################################################
# => Stubs: the only commands the step runs that could leave the sandbox
###############################################################

record() { printf '%s\n' "$*" >> "$CALLS"; }

# Run the command as the current user, and log it in SUDO_LOG apart from CALLS, so a test can
# tell the commands that ran as root from the ones that did not.
sudo() (
    printf '%s\n' "$*" >> "$SUDO_LOG"
    "$@"
)

# An install of a .deb notes whether apt could read it: the file is still there, in a folder that
# the unprivileged _apt user can enter (mktemp -d makes it 700, so the step has to open it up).
apt-get() {
    local last="${*: -1}" note=""
    if [[ "$last" == *.deb ]]; then
        if [ ! -f "$last" ]; then
            note=' (file MISSING)'
        elif [ -z "$(find "${last%/*}" -maxdepth 0 -perm 755)" ]; then
            note=' (folder closed to _apt)'
        else
            note=' (file present)'
        fi
    fi
    record "apt-get $*$note"
    case " $* " in
        *" remove "*) [ -z "${FAKE_APT_REMOVE_FAIL:-}" ] ;;
        *" install minisign "*) [ -z "${FAKE_NO_MINISIGN:-}" ] ;;
        *.deb\ *) [ -z "${FAKE_DEB_INSTALL_FAIL:-}" ] ;;
        *) return 0 ;;
    esac
}

dpkg() {
    [ "${1:-}" = --print-architecture ] || return 1
    printf '%s\n' "${FAKE_ARCH:-amd64}"
}

# Writes the requested URL into the output file.  A request with --write-out is the lookup of the
# latest release, which answers with the URL GitHub redirects to.  A URL that contains
# FAKE_CURL_FAIL_ON fails like curl --fail does on an HTTP error.
curl() {
    local output="" url="" write_out=""
    while [ $# -gt 0 ]; do
        case "$1" in
            --output) output="$2"; shift ;;
            --write-out) write_out="$2"; shift ;;
            --proto|--proto-redir|--retry) shift ;;
            -*) ;;
            *) url="$1" ;;
        esac
        shift
    done
    record "curl $url"
    if [ -n "${FAKE_CURL_FAIL_ON:-}" ] && [[ "$url" == *"$FAKE_CURL_FAIL_ON"* ]]; then
        return 22
    fi
    if [ -n "$write_out" ]; then
        printf '%s' "${FAKE_LATEST_URL:-$LATEST_URL}"
        return 0
    fi
    printf 'fake content of %s\n' "$url" > "$output"
}

# Behaves like the real tool: without -V (verify) it prints its usage and fails, and it needs both
# files.  Logs the key and the file names it was given.
minisign() {
    local verify="" key="" file="" signature=""
    while [ $# -gt 0 ]; do
        case "$1" in
            -V) verify=1 ;;
            -P) key="$2"; shift ;;
            -m) file="$2"; shift ;;
            -x) signature="$2"; shift ;;
            *) ;;
        esac
        shift
    done
    record "minisign -P $key -m ${file##*/} -x ${signature##*/}"
    [ -n "$verify" ] || return 1
    [ -f "$file" ] && [ -f "$signature" ] || return 1
    [ -z "${FAKE_MINISIGN_FAIL:-}" ]
}

systemctl() {
    record "systemctl $*"
    [ -z "${FAKE_SYSTEMCTL_FAIL:-}" ]
}

# `command -v openlogi-agent` and `command -v minisign` answer from FAKE_INSTALLED and
# FAKE_NO_MINISIGN instead of this machine's PATH.
command() {
    if [ "${1:-}" = -v ]; then
        case "${2:-}" in
            openlogi-agent) [ -n "${FAKE_INSTALLED:-}" ]; return ;;
            minisign) [ -z "${FAKE_NO_MINISIGN:-}" ]; return ;;
            *) ;;
        esac
    fi
    builtin command "$@"
}

new_case() {
    CASE="$SCRATCH/$1"
    mkdir -p "$CASE/tmp" "$CASE/home"
    export TMPDIR="$CASE/tmp"
    export HOME="$CASE/home"
    CALLS="$CASE/calls.log"
    SUDO_LOG="$CASE/sudo.log"
    : > "$CALLS"
    : > "$SUDO_LOG"
    unset OPENLOGI_VERSION FAKE_ARCH FAKE_INSTALLED FAKE_LATEST_URL FAKE_CURL_FAIL_ON \
        FAKE_MINISIGN_FAIL FAKE_NO_MINISIGN FAKE_DEB_INSTALL_FAIL FAKE_SYSTEMCTL_FAIL \
        FAKE_APT_REMOVE_FAIL
}

# Runs the step the way the bootstrap does (its failure is tested, not fatal): OUTPUT holds
# what it printed and STATUS how it ended.  The stubs write their logs to files, so the
# command substitution does not hide them.
run_install() {
    STATUS=0
    OUTPUT="$(openlogi_install 2>&1)" || STATUS=$?
}

# The recorded calls, with the random work directory the step creates written as <work>.
calls() { sed "s|$TMPDIR/[^/]*/|<work>/|" "$CALLS"; }

# The calls of a complete first install of release $1 (default v1.2.3) for architecture $2.
expected_first_install() {
    local tag="${1:-v1.2.3}" arch="${2:-amd64}"
    local package="openlogi-$tag-linux-$arch.deb"
    local url="$RELEASES/download/$tag/$package"
    printf '%s\n' \
        "curl $RELEASES/latest" \
        'apt-get -y install minisign' \
        "curl $url" \
        "curl $url.minisig" \
        "minisign -P $VENDOR_KEY -m $package -x $package.minisig" \
        "apt-get install -y <work>/$package (file present)" \
        'systemctl --user enable --now openlogi-agent.service'
}

assert_only_apt_ran_as_root() {
    local line
    [ -s "$SUDO_LOG" ] || fail 'sudo was never used, so nothing could have been installed'
    while IFS= read -r line; do
        case "$line" in
            apt-get\ *) ;;
            *) fail "sudo ran [$line]; only apt-get needs root" ;;
        esac
    done < "$SUDO_LOG"
}

assert_no_temp_files() {
    [ -z "$(find "$TMPDIR" -mindepth 1)" ] || fail "$1: the work directory was left in $TMPDIR"
}

###############################################################
# => openlogi_install
###############################################################

test_an_installed_openlogi_is_left_alone() {
    new_case installed
    export FAKE_INSTALLED=1

    run_install

    assert_equals 0 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'already installed' 'the explanation'
    assert_equals '' "$(calls)" 'commands run on a machine that has OpenLogi'
}

test_a_first_install_checks_the_signature_before_apt_opens_the_file() {
    new_case first-install

    run_install

    assert_equals 0 "$STATUS" "the status (output: $OUTPUT)"
    assert_equals "$(expected_first_install)" "$(calls)" 'the commands, in order (lookup, signature tool, download, check, apt, agent)'
    assert_only_apt_ran_as_root
    assert_no_temp_files 'after a successful install'
    assert_contains "$OUTPUT" 'Replug the Logitech receiver' 'the udev hint'
}

test_the_architecture_comes_from_dpkg() {
    new_case arm64
    export FAKE_ARCH=arm64

    run_install

    assert_equals 0 "$STATUS" "the status (output: $OUTPUT)"
    assert_equals "$(expected_first_install v1.2.3 arm64)" "$(calls)" 'the commands'
}

test_an_unsupported_architecture_installs_nothing() {
    new_case riscv
    export FAKE_ARCH=riscv64

    run_install

    assert_equals 1 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'riscv64' 'the explanation'
    assert_equals '' "$(calls)" 'commands run for an unsupported architecture'
}

test_a_pinned_version_skips_the_release_lookup() {
    local spelling
    for spelling in 0.9.0 v0.9.0; do
        new_case "pinned-$spelling"
        export OPENLOGI_VERSION="$spelling"

        run_install

        assert_equals 0 "$STATUS" "the status for $spelling (output: $OUTPUT)"
        assert_equals "$(expected_first_install v0.9.0 | grep -v 'releases/latest$')" "$(calls)" "the commands for $spelling"
    done
}

test_an_unexpected_release_tag_is_refused_before_anything_runs() {
    local label
    for label in no-tag path command; do
        new_case "tag-$label"
        case "$label" in
            no-tag) export FAKE_LATEST_URL="$RELEASES" ;;
            path) export OPENLOGI_VERSION='1.2.3/../../evil' ;;
            command) export OPENLOGI_VERSION='1.2.3;touch-evil' ;;
        esac

        run_install

        assert_equals 1 "$STATUS" "the status for $label"
        assert_contains "$OUTPUT" 'Refusing the OpenLogi release tag' "the explanation for $label"
        assert_not_contains "$(calls)" 'releases/download' "a download for $label"
        assert_equals '' "$(cat "$SUDO_LOG")" "root commands for $label"
    done
}

test_a_failed_release_lookup_installs_nothing() {
    new_case lookup-fails
    export FAKE_CURL_FAIL_ON=releases/latest

    run_install

    assert_equals 1 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'Could not resolve the latest OpenLogi release' 'the explanation'
    assert_not_contains "$(calls)" 'releases/download' 'a download without a release'
    assert_equals '' "$(cat "$SUDO_LOG")" 'root commands'
}

test_a_missing_signature_tool_stops_before_any_download() {
    new_case no-minisign
    export FAKE_NO_MINISIGN=1

    run_install

    assert_equals 1 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'minisign could not be installed' 'the explanation'
    assert_equals "curl $RELEASES/latest
apt-get -y install minisign" "$(calls)" 'the commands: no package may be fetched that cannot be checked'
}

test_a_failed_download_installs_nothing() {
    local failing
    for failing in .deb .minisig; do
        new_case "download-fails-$failing"
        export FAKE_CURL_FAIL_ON="$failing"

        run_install

        assert_equals 1 "$STATUS" "the status when the $failing download fails"
        assert_contains "$OUTPUT" 'was not installed' "the explanation for $failing"
        assert_not_contains "$(calls)" 'minisign -P' "a signature check after a failed $failing download"
        assert_not_contains "$(calls)" 'apt-get install' "an install after a failed $failing download"
        assert_not_contains "$(calls)" 'systemctl' "an agent start after a failed $failing download"
        assert_no_temp_files "after a failed $failing download"
    done
}

test_a_failed_signature_check_installs_nothing() {
    new_case signature-fails
    export FAKE_MINISIGN_FAIL=1

    run_install

    assert_equals 1 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'was not installed' 'the explanation'
    assert_contains "$(calls)" "minisign -P $VENDOR_KEY" 'the signature check'
    assert_not_contains "$(calls)" 'apt-get install' 'an unverified package reaching apt'
    assert_not_contains "$(calls)" 'systemctl' 'an agent start after a failed signature check'
    assert_no_temp_files 'after a failed signature check'
}

test_a_failed_apt_install_is_reported_and_cleaned_up() {
    new_case apt-fails
    export FAKE_DEB_INSTALL_FAIL=1

    run_install

    assert_equals 1 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'was not installed' 'the explanation'
    assert_not_contains "$(calls)" 'systemctl' 'starting an agent that is not installed'
    assert_no_temp_files 'after a failed apt install'
}

test_a_failed_agent_start_does_not_fail_the_install() {
    new_case agent-fails
    export FAKE_SYSTEMCTL_FAIL=1

    run_install

    assert_equals 0 "$STATUS" "the status (output: $OUTPUT)"
    assert_contains "$OUTPUT" 'systemctl --user enable --now openlogi-agent.service' 'the command to run by hand'
}

###############################################################
# => openlogi_uninstall
###############################################################

test_uninstall_stops_the_agent_and_purges_the_package() {
    new_case uninstall

    openlogi_uninstall >/dev/null 2>&1 || fail 'the uninstall failed'

    assert_equals 'systemctl --user disable --now openlogi-agent.service
apt-get remove --purge -y openlogi' "$(cat "$CALLS")" 'the commands, in order'
}

test_uninstall_goes_on_when_the_agent_or_apt_fail() {
    new_case uninstall-failures
    export FAKE_SYSTEMCTL_FAIL=1 FAKE_APT_REMOVE_FAIL=1

    openlogi_uninstall >/dev/null 2>&1 || fail 'a failing systemctl or apt must not fail the uninstall'

    assert_contains "$(cat "$CALLS")" 'apt-get remove --purge -y openlogi' 'the removal, even after the agent failed to stop'
}

###############################################################
# => How the Ubuntu profile uses the step
###############################################################

test_ubuntu_runs_the_step_without_aborting_provisioning() {
    local file="$BOOTSTRAP_DIR/ubuntu"

    grep -Eq '^[[:space:]]*openlogi_install \|\|$' "$file" ||
        fail "$file must call openlogi_install and not abort when it fails"
    grep -Fq "openlogi_install'" "$file" ||
        fail "$file must tell the user how to rerun the step"
}

test_the_ubuntu_uninstaller_offers_the_step() {
    local file="$BOOTSTRAP_DIR/uninstall_ubuntu"

    grep -Eq '^[[:space:]]*openlogi_uninstall[[:space:]]' "$file" ||
        fail "$file must offer openlogi_uninstall"
}

test_the_audit_maps_the_deb_variable_to_its_package() {
    # tests/test_audit_extra_packages.py makes sure an entry exists; this one checks its value.
    grep -Fq '"openlogi_deb": "openlogi",' "$REPO_ROOT/.local/scripts/audit_installation.py" ||
        fail 'DEB_FILE_PACKAGES must map openlogi_deb to the openlogi package'
}

for test_name in \
    test_an_installed_openlogi_is_left_alone \
    test_a_first_install_checks_the_signature_before_apt_opens_the_file \
    test_the_architecture_comes_from_dpkg \
    test_an_unsupported_architecture_installs_nothing \
    test_a_pinned_version_skips_the_release_lookup \
    test_an_unexpected_release_tag_is_refused_before_anything_runs \
    test_a_failed_release_lookup_installs_nothing \
    test_a_missing_signature_tool_stops_before_any_download \
    test_a_failed_download_installs_nothing \
    test_a_failed_signature_check_installs_nothing \
    test_a_failed_apt_install_is_reported_and_cleaned_up \
    test_a_failed_agent_start_does_not_fail_the_install \
    test_uninstall_stops_the_agent_and_purges_the_package \
    test_uninstall_goes_on_when_the_agent_or_apt_fail \
    test_ubuntu_runs_the_step_without_aborting_provisioning \
    test_the_ubuntu_uninstaller_offers_the_step \
    test_the_audit_maps_the_deb_variable_to_its_package; do
    # A subshell keeps each test's stub overrides and exported variables to itself.
    ( "$test_name" ) || {
        printf 'not ok - %s\n' "$test_name" >&2
        exit 1
    }
    printf 'ok - %s\n' "$test_name"
done

printf 'OpenLogi install tests passed.\n'
