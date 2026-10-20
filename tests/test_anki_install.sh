#!/usr/bin/env bash
# Hermetic tests for the Anki step in bootstrap/base_functions (anki_install and anki_uninstall):
# the pinned release, the digest check that must pass before unpacking, the launcher and icon
# placement, the failure paths, the uninstall function, and how the profiles use them.  Nothing
# here touches the network or the real home folder: curl, dpkg, xdg-mime, and
# update-desktop-database are replaced by functions that act inside a temporary directory and
# record what they were asked to do.  tar and zstd are real (curl hands them a genuine .tar.zst
# archive), and so is the sha256sum comparison in the test that says so.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BOOTSTRAP_DIR="$REPO_ROOT/.local/scripts/bootstrap"
SCRATCH="$(mktemp -d)"
trap 'rm -rf "$SCRATCH"' EXIT

VERSION="26.09.3"
RELEASE="https://github.com/ankitects/anki/releases/download/${VERSION}"
# The SHA-256 of the release builds.  GitHub shows the same values as the digest of each asset.
# This second copy makes changing a digest in base_functions a deliberate act that fails this
# test until both agree.
AMD64_ASSET="anki-${VERSION}-linux-x86_64.tar.zst"
AMD64_DIGEST="d70e01971a7aa190b189927573ee643f2525ff840eafaecc7a8f8cde505650b3"
ARM64_ASSET="anki-${VERSION}-linux-aarch64.tar.zst"
ARM64_DIGEST="0b586efdb0e3582bee3b9d08b9fa57df681fb97a8066d76a6743e1bc3fc331eb"

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

###############################################################
# => Stubs: the commands the step runs that could leave the sandbox
###############################################################

record() { printf '%s\n' "$*" >> "$CALLS"; }

dpkg() {
    [ "${1:-}" = --print-architecture ] || return 1
    printf '%s\n' "${FAKE_ARCH:-amd64}"
}

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

xdg-mime() {
    record "xdg-mime $*"
}

update-desktop-database() {
    record "update-desktop-database $*"
}

command() {
    if [ "${1:-}" = -v ] && [ "${2:-}" = anki ]; then
        [ -n "${FAKE_INSTALLED:-}" ]
        return
    fi
    if [ "${1:-}" = -v ] && [ -n "${FAKE_MISSING_TOOL:-}" ] && [ "${2:-}" = "$FAKE_MISSING_TOOL" ]; then
        return 1
    fi
    builtin command "$@"
}

make_archive() {
    local bundle="$CASE/archive-source/anki-26.9.3-linux-x86_64.tar"
    mkdir -p "$bundle/app" "$bundle/app_packages" "$bundle/python"
    printf '#!/bin/sh\necho "fake anki %s"\n' "$VERSION" > "$bundle/anki"
    chmod 755 "$bundle/anki"
    printf '#!/bin/sh\n' > "$bundle/uninstall.sh"
    chmod 755 "$bundle/uninstall.sh"
    cat > "$bundle/anki.desktop" <<'DESKTOP'
[Desktop Entry]
Name=Anki
Exec=anki %f
TryExec=anki
Icon=anki
Type=Application
DESKTOP
    printf 'fake-png\n' > "$bundle/anki.png"
    printf 'fake-xpm\n' > "$bundle/anki.xpm"
    printf '<mime-info/>\n' > "$bundle/anki.xml"
    printf '.TH ANKI 1\n' > "$bundle/anki.1"
    FIXTURE_ARCHIVE="$CASE/archive.tar.zst"
    tar --zstd -cf "$FIXTURE_ARCHIVE" -C "$CASE/archive-source" "anki-26.9.3-linux-x86_64.tar"
}

new_case() {
    CASE="$SCRATCH/$1"
    mkdir -p "$CASE/home"
    export HOME="$CASE/home"
    unset XDG_DATA_HOME
    CALLS="$CASE/calls.log"
    : > "$CALLS"
    make_archive
    unset FAKE_ARCH FAKE_INSTALLED FAKE_CURL_FAIL_ON FAKE_SHA256_FAIL FAKE_REAL_SHA256 \
        FAKE_MISSING_TOOL
}

run_install() {
    STATUS=0
    OUTPUT="$(anki_install 2>&1)" || STATUS=$?
}

calls() { cat "$CALLS"; }

assert_nothing_installed() {
    assert_no_file "$HOME/.local/share/anki" "$1: program directory"
    assert_no_file "$HOME/.local/bin/anki" "$1: binary link"
    assert_no_file "$HOME/.local/share/applications/anki.desktop" "$1: desktop entry"
}

assert_no_partial_dir() {
    assert_no_file "$HOME/.local/share/anki.partial" "$1: partial work directory"
}

###############################################################
# => anki_install
###############################################################

test_an_installed_anki_is_left_alone() {
    new_case installed
    export FAKE_INSTALLED=1

    run_install

    assert_equals 0 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'already installed' 'the explanation'
    assert_equals '' "$(calls)" 'commands run on a machine that has anki'
}

test_a_first_install_checks_the_digest_and_installs_under_local() {
    new_case first-install

    run_install

    assert_equals 0 "$STATUS" "the status (output: $OUTPUT)"
    assert_contains "$(calls)" "curl $RELEASE/$AMD64_ASSET" 'the download URL'
    assert_contains "$(calls)" "sha256sum $AMD64_DIGEST  $AMD64_ASSET" 'the digest check'
    assert_equals "$HOME/.local/share/anki/anki" "$(readlink "$HOME/.local/bin/anki")" \
        '.local/bin/anki points into .local/share/anki'
    assert_equals "fake anki $VERSION" "$("$HOME/.local/bin/anki")" 'the unpacked executable runs'
    assert_file "$HOME/.local/share/anki/uninstall.sh" 'upstream uninstall.sh kept for audit app_location'
    assert_file "$HOME/.local/share/applications/anki.desktop" 'desktop launcher installed'
    assert_contains "$(cat "$HOME/.local/share/applications/anki.desktop")" \
        "Exec=$HOME/.local/bin/anki %f" 'desktop launcher Exec uses .local/bin/anki'
    assert_contains "$(cat "$HOME/.local/share/applications/anki.desktop")" \
        "TryExec=$HOME/.local/bin/anki" 'desktop launcher TryExec uses .local/bin/anki'
    assert_contains "$(cat "$HOME/.local/share/applications/anki.desktop")" \
        "Icon=$HOME/.local/share/anki/anki.png" 'desktop launcher Icon points inside .local/share/anki'
    assert_no_partial_dir 'after a successful install'
    assert_contains "$OUTPUT" "Anki $VERSION installed" 'the confirmation'
}

test_arm64_gets_its_own_build_and_digest() {
    new_case arm64
    export FAKE_ARCH=arm64

    run_install

    assert_equals 0 "$STATUS" "the status (output: $OUTPUT)"
    assert_contains "$(calls)" "curl $RELEASE/$ARM64_ASSET" 'the arm64 download URL'
    assert_contains "$(calls)" "sha256sum $ARM64_DIGEST  $ARM64_ASSET" 'the arm64 digest check'
}

test_an_unsupported_architecture_installs_nothing() {
    new_case riscv
    export FAKE_ARCH=riscv64
    set +u

    run_install

    assert_equals 1 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'riscv64' 'the explanation'
    assert_equals '' "$(calls)" 'commands run for an unsupported architecture'
    assert_nothing_installed 'on an unsupported architecture'
}

test_a_missing_tool_installs_nothing() {
    new_case missing-zstd
    export FAKE_MISSING_TOOL=zstd

    run_install

    assert_equals 1 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'Anki needs zstd to install' 'the explanation'
    assert_equals '' "$(calls)" 'commands run when zstd is missing'
    assert_nothing_installed 'when zstd is missing'
}

test_a_failed_download_installs_nothing() {
    new_case download-fails
    export FAKE_CURL_FAIL_ON=.tar.zst

    run_install

    assert_equals 1 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'was not installed' 'the explanation'
    assert_not_contains "$(calls)" 'sha256sum' 'a digest check without a download'
    assert_nothing_installed 'after a failed download'
    assert_no_partial_dir 'after a failed download'
}

test_a_failed_digest_check_installs_nothing() {
    new_case digest-fails
    export FAKE_SHA256_FAIL=1

    run_install

    assert_equals 1 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'was not installed' 'the explanation'
    assert_contains "$(calls)" "sha256sum $AMD64_DIGEST  $AMD64_ASSET" 'the digest check'
    assert_nothing_installed 'after a failed digest check'
    assert_no_partial_dir 'after a failed digest check'
}

test_the_real_sha256sum_refuses_a_download_that_does_not_match_the_pin() {
    new_case real-digest
    export FAKE_REAL_SHA256=1

    run_install

    assert_equals 1 "$STATUS" 'the status'
    assert_contains "$OUTPUT" "$AMD64_ASSET: FAILED" 'the verdict of the real tool'
    assert_not_contains "$OUTPUT" 'no properly formatted' 'a checksum line the real tool cannot read'
    assert_nothing_installed 'after the real tool refused the download'
    assert_no_partial_dir 'after the real tool refused the download'
}

###############################################################
# => anki_uninstall
###############################################################

test_uninstall_removes_the_program_launcher_and_link_but_keeps_decks() {
    new_case uninstall
    run_install
    assert_equals 0 "$STATUS" 'install succeeded before uninstall'
    mkdir -p "$HOME/.local/share/Anki2/User 1"
    printf 'collection\n' > "$HOME/.local/share/Anki2/User 1/collection.anki2"

    anki_uninstall

    assert_nothing_installed 'after anki_uninstall'
    assert_file "$HOME/.local/share/Anki2/User 1/collection.anki2" 'user decks in Anki2 stay untouched'
}

test_uninstall_keeps_a_command_it_did_not_install() {
    new_case uninstall-foreign
    run_install
    printf '#!/bin/sh\n' > "$CASE/foreign-anki"
    ln -sf "$CASE/foreign-anki" "$HOME/.local/bin/anki"

    anki_uninstall

    assert_equals "$CASE/foreign-anki" "$(readlink "$HOME/.local/bin/anki")" \
        'a symlink pointing outside ~/.local/share/anki stays'
    assert_no_file "$HOME/.local/share/anki" 'the program directory is still removed'
}

test_uninstall_is_a_noop_when_nothing_is_installed() {
    new_case uninstall-empty
    anki_uninstall || fail "uninstall returned $? on a machine without Anki"
}

###############################################################
# => How the profiles use the step
###############################################################

test_ubuntu_and_work_run_the_step_without_aborting_provisioning() {
    local file
    for file in "$BOOTSTRAP_DIR/ubuntu" "$BOOTSTRAP_DIR/work"; do
        grep -Eq '^[[:space:]]*anki_install \|\|$' "$file" ||
            fail "$file must call anki_install and not abort when it fails"
        grep -Fq "anki_install'" "$file" ||
            fail "$file must tell the user how to rerun the step"
    done
}

test_the_ubuntu_and_work_uninstallers_offer_the_step() {
    local file
    for file in "$BOOTSTRAP_DIR/uninstall_ubuntu" "$BOOTSTRAP_DIR/uninstall_work"; do
        grep -Eq '^[[:space:]]*anki_uninstall[[:space:]]' "$file" ||
            fail "$file must offer anki_uninstall"
    done
}

test_mac_installs_and_removes_the_anki_cask() {
    grep -Eq '^[[:space:]]+anki([[:space:]]|$)' "$BOOTSTRAP_DIR/mac_functions" ||
        fail 'mac_functions must install the anki cask'
    grep -Eq '(^|[[:space:]])anki([[:space:]]|$)' "$BOOTSTRAP_DIR/uninstall_mac_functions" ||
        fail 'uninstall_mac_functions must remove the anki cask'
}

for test_name in \
    test_an_installed_anki_is_left_alone \
    test_a_first_install_checks_the_digest_and_installs_under_local \
    test_arm64_gets_its_own_build_and_digest \
    test_an_unsupported_architecture_installs_nothing \
    test_a_missing_tool_installs_nothing \
    test_a_failed_download_installs_nothing \
    test_a_failed_digest_check_installs_nothing \
    test_the_real_sha256sum_refuses_a_download_that_does_not_match_the_pin \
    test_uninstall_removes_the_program_launcher_and_link_but_keeps_decks \
    test_uninstall_keeps_a_command_it_did_not_install \
    test_uninstall_is_a_noop_when_nothing_is_installed \
    test_ubuntu_and_work_run_the_step_without_aborting_provisioning \
    test_the_ubuntu_and_work_uninstallers_offer_the_step \
    test_mac_installs_and_removes_the_anki_cask; do
    ( "$test_name" ) || {
        printf 'not ok - %s\n' "$test_name" >&2
        exit 1
    }
    printf 'ok - %s\n' "$test_name"
done

printf 'anki install tests passed.\n'
