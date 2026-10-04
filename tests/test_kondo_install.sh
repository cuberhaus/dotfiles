#!/usr/bin/env bash
# Hermetic tests for the kondo step in bootstrap/ubuntu_functions: the pinned release, the digest
# check that must pass before sudo installs anything, the failure paths, the uninstall function,
# and how the profiles use them.  Nothing here touches the network or /usr/local/bin: curl, dpkg,
# sudo and install are replaced by functions that act inside a temporary directory and record
# what they were asked to do.  tar is real (curl hands it a genuine archive), and so is the
# sha256sum comparison in the one test that says so.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BOOTSTRAP_DIR="$REPO_ROOT/.local/scripts/bootstrap"
SCRATCH="$(mktemp -d)"
trap 'rm -rf "$SCRATCH"' EXIT

RELEASE=https://github.com/tbillington/kondo/releases/download/v0.9
# The SHA-256 of the release builds.  GitHub shows the same values as the digest of each asset.
# This second copy makes changing a digest in ubuntu_functions a deliberate act that fails this
# test until both agree.
AMD64_ASSET=kondo-x86_64-unknown-linux-gnu.tar.gz
AMD64_DIGEST=56f89890fd4e16cf5e11bb212bb812b60a8319dabfb05fabaf80c6c8b5133b65
ARM64_ASSET=kondo-aarch64-unknown-linux-gnu.tar.gz
ARM64_DIGEST=531cfc381e851300878e20f77ebd58724ddc28ecf5e48bec654f630f0bfc7fb0

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

# Runs the command as the current user, and logs it in SUDO_LOG apart from CALLS, so a test can
# tell the commands that ran as root from the ones that did not.  With FAKE_SUDO_DRY_RUN it only
# logs, for a command (rm -f /usr/local/bin/kondo) that must not touch this machine.
sudo() (
    printf '%s\n' "$*" >> "$SUDO_LOG"
    [ -z "${FAKE_SUDO_DRY_RUN:-}" ] || exit 0
    "$@"
)

dpkg() {
    [ "${1:-}" = --print-architecture ] || return 1
    printf '%s\n' "${FAKE_ARCH:-amd64}"
}

# "Downloads" the case's archive.  A URL that contains FAKE_CURL_FAIL_ON fails like curl --fail
# does on an HTTP error.
curl() {
    local output="" url=""
    while [ $# -gt 0 ]; do
        case "$1" in
            --output) output="$2"; shift ;;
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
    cp "$FIXTURE_ARCHIVE" "$output"
}

# The pinned digests belong to the real release, which this test never downloads, so the stub
# records the line it was asked to check, confirms that the file it names is in the folder the
# step changed into, and answers from FAKE_SHA256_FAIL.  FAKE_REAL_SHA256 runs the real tool.
sha256sum() {
    if [ -n "${FAKE_REAL_SHA256:-}" ]; then
        builtin command sha256sum "$@"
        return
    fi
    local list="${*: -1}" line
    line="$(cat "$list")" || return 1
    record "sha256sum $line"
    [ -f "${line##*  }" ] || return 1
    [ -z "${FAKE_SHA256_FAIL:-}" ]
}

# The step runs this under sudo to put the program in /usr/local/bin; here it lands in the
# case's own root folder.
install() {
    record "install $*"
    local destination="${*: -1}"
    mkdir -p "$ROOT${destination%/*}"
    builtin command install "${@:1:$#-1}" "$ROOT$destination"
}

# `command -v kondo` answers from FAKE_INSTALLED instead of this machine's PATH.
command() {
    if [ "${1:-}" = -v ] && [ "${2:-}" = kondo ]; then
        [ -n "${FAKE_INSTALLED:-}" ]
        return
    fi
    builtin command "$@"
}

# The archive of a real release holds one executable called kondo and nothing else.  This one
# prints a marker, so a test can tell that the file it unpacked is the file that was installed.
make_archive() {
    mkdir -p "$CASE/archive-source"
    printf '#!/bin/sh\necho "fake kondo"\n' > "$CASE/archive-source/kondo"
    chmod 755 "$CASE/archive-source/kondo"
    FIXTURE_ARCHIVE="$CASE/archive.tar.gz"
    tar -czf "$FIXTURE_ARCHIVE" -C "$CASE/archive-source" kondo
}

new_case() {
    CASE="$SCRATCH/$1"
    ROOT="$CASE/root"
    mkdir -p "$CASE/tmp" "$CASE/home" "$ROOT"
    export TMPDIR="$CASE/tmp"
    export HOME="$CASE/home"
    CALLS="$CASE/calls.log"
    SUDO_LOG="$CASE/sudo.log"
    : > "$CALLS"
    : > "$SUDO_LOG"
    make_archive
    unset FAKE_ARCH FAKE_INSTALLED FAKE_CURL_FAIL_ON FAKE_SHA256_FAIL FAKE_REAL_SHA256 \
        FAKE_SUDO_DRY_RUN
}

# Runs the step the way the bootstrap does (its failure is tested, not fatal): OUTPUT holds
# what it printed and STATUS how it ended.  The stubs write their logs to files, so the
# command substitution does not hide them.
run_install() {
    STATUS=0
    OUTPUT="$(kondo_install 2>&1)" || STATUS=$?
}

# The recorded calls, with the random work directory the step creates written as <work>.
calls() { sed "s|$TMPDIR/[^/]*/|<work>/|" "$CALLS"; }

# The calls of a complete first install of asset $1 (default amd64) with the digest $2.
expected_first_install() {
    local asset="${1:-$AMD64_ASSET}" digest="${2:-$AMD64_DIGEST}"
    printf '%s\n' \
        "curl $RELEASE/$asset" \
        "sha256sum $digest  $asset" \
        'install -m 0755 <work>/kondo /usr/local/bin/kondo'
}

assert_only_install_ran_as_root() {
    local line
    [ -s "$SUDO_LOG" ] || fail 'sudo was never used, so nothing could have been installed'
    while IFS= read -r line; do
        case "$line" in
            install\ *) ;;
            *) fail "sudo ran [$line]; only install needs root" ;;
        esac
    done < "$SUDO_LOG"
}

assert_nothing_installed() {
    [ ! -e "$ROOT/usr/local/bin/kondo" ] || fail "$1: kondo was installed"
    assert_equals '' "$(cat "$SUDO_LOG")" "$1: root commands"
}

assert_no_temp_files() {
    [ -z "$(find "$TMPDIR" -mindepth 1)" ] || fail "$1: the work directory was left in $TMPDIR"
}

###############################################################
# => kondo_install
###############################################################

test_an_installed_kondo_is_left_alone() {
    new_case installed
    export FAKE_INSTALLED=1

    run_install

    assert_equals 0 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'already installed' 'the explanation'
    assert_equals '' "$(calls)" 'commands run on a machine that has kondo'
}

test_a_first_install_checks_the_digest_before_sudo_installs_anything() {
    new_case first-install

    run_install

    assert_equals 0 "$STATUS" "the status (output: $OUTPUT)"
    assert_equals "$(expected_first_install)" "$(calls)" 'the commands, in order (download, digest, install)'
    assert_only_install_ran_as_root
    [ -x "$ROOT/usr/local/bin/kondo" ] || fail 'kondo is not executable in /usr/local/bin'
    [ -n "$(find "$ROOT/usr/local/bin/kondo" -perm 755)" ] || fail 'kondo does not have mode 0755'
    assert_equals 'fake kondo' "$("$ROOT/usr/local/bin/kondo")" 'the installed program (the unpacked file)'
    assert_no_temp_files 'after a successful install'
    assert_contains "$OUTPUT" 'kondo 0.9 installed' 'the confirmation'
}

test_arm64_gets_its_own_build_and_digest() {
    new_case arm64
    export FAKE_ARCH=arm64

    run_install

    assert_equals 0 "$STATUS" "the status (output: $OUTPUT)"
    assert_equals "$(expected_first_install "$ARM64_ASSET" "$ARM64_DIGEST")" "$(calls)" 'the commands'
}

test_an_unsupported_architecture_installs_nothing() {
    new_case riscv
    export FAKE_ARCH=riscv64
    # Without nounset, so that only the step's own refusal stops it, not an unset variable.
    set +u

    run_install

    assert_equals 1 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'riscv64' 'the explanation'
    assert_equals '' "$(calls)" 'commands run for an unsupported architecture'
    assert_nothing_installed 'on an unsupported architecture'
}

test_a_failed_download_installs_nothing() {
    new_case download-fails
    export FAKE_CURL_FAIL_ON=.tar.gz

    run_install

    assert_equals 1 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'was not installed' 'the explanation'
    assert_not_contains "$(calls)" 'sha256sum' 'a digest check without a download'
    assert_nothing_installed 'after a failed download'
    assert_no_temp_files 'after a failed download'
}

test_a_failed_digest_check_installs_nothing() {
    new_case digest-fails
    export FAKE_SHA256_FAIL=1

    run_install

    assert_equals 1 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'was not installed' 'the explanation'
    assert_contains "$(calls)" "sha256sum $AMD64_DIGEST  $AMD64_ASSET" 'the digest check'
    assert_not_contains "$(calls)" 'install -m' 'an unverified program reaching /usr/local/bin'
    assert_nothing_installed 'after a failed digest check'
    assert_no_temp_files 'after a failed digest check'
}

test_the_real_sha256sum_refuses_a_download_that_does_not_match_the_pin() {
    new_case real-digest
    export FAKE_REAL_SHA256=1

    run_install

    assert_equals 1 "$STATUS" 'the status'
    # FAILED means sha256sum understood the line the step wrote and compared the digests; a
    # malformed line would be reported as "no properly formatted" instead.
    assert_contains "$OUTPUT" "$AMD64_ASSET: FAILED" 'the verdict of the real tool'
    assert_not_contains "$OUTPUT" 'no properly formatted' 'a checksum line the real tool cannot read'
    assert_nothing_installed 'after the real tool refused the download'
    assert_no_temp_files 'after the real tool refused the download'
}

###############################################################
# => kondo_uninstall
###############################################################

test_uninstall_removes_only_the_installed_program() {
    new_case uninstall
    export FAKE_SUDO_DRY_RUN=1

    kondo_uninstall >/dev/null 2>&1 || fail 'the uninstall failed'

    assert_equals 'rm -f /usr/local/bin/kondo' "$(cat "$SUDO_LOG")" 'the root commands'
}

###############################################################
# => How the profiles use the step
###############################################################

test_ubuntu_runs_the_step_without_aborting_provisioning() {
    local file="$BOOTSTRAP_DIR/ubuntu"

    grep -Eq '^[[:space:]]*kondo_install \|\|$' "$file" ||
        fail "$file must call kondo_install and not abort when it fails"
    grep -Fq "kondo_install'" "$file" ||
        fail "$file must tell the user how to rerun the step"
}

test_the_ubuntu_uninstaller_offers_the_step() {
    local file="$BOOTSTRAP_DIR/uninstall_ubuntu"

    grep -Eq '^[[:space:]]*kondo_uninstall[[:space:]]' "$file" ||
        fail "$file must offer kondo_uninstall"
}

test_arch_and_mac_install_kondo_from_their_package_managers() {
    local file
    for file in arch_functions mac_functions; do
        grep -Eq '^[[:space:]]+kondo([[:space:]]|$)' "$BOOTSTRAP_DIR/$file" ||
            fail "$file must install kondo"
    done
    grep -Eq '(^|[[:space:]])kondo([[:space:]]|$)' "$BOOTSTRAP_DIR/uninstall_arch_functions" ||
        fail 'uninstall_arch_functions must remove kondo'
    grep -Eq '(^|[[:space:]])kondo([[:space:]]|$)' "$BOOTSTRAP_DIR/uninstall_mac_functions" ||
        fail 'uninstall_mac_functions must remove kondo'
}

for test_name in \
    test_an_installed_kondo_is_left_alone \
    test_a_first_install_checks_the_digest_before_sudo_installs_anything \
    test_arm64_gets_its_own_build_and_digest \
    test_an_unsupported_architecture_installs_nothing \
    test_a_failed_download_installs_nothing \
    test_a_failed_digest_check_installs_nothing \
    test_the_real_sha256sum_refuses_a_download_that_does_not_match_the_pin \
    test_uninstall_removes_only_the_installed_program \
    test_ubuntu_runs_the_step_without_aborting_provisioning \
    test_the_ubuntu_uninstaller_offers_the_step \
    test_arch_and_mac_install_kondo_from_their_package_managers; do
    # A subshell keeps each test's stub overrides and exported variables to itself.
    ( "$test_name" ) || {
        printf 'not ok - %s\n' "$test_name" >&2
        exit 1
    }
    printf 'ok - %s\n' "$test_name"
done

printf 'kondo install tests passed.\n'
