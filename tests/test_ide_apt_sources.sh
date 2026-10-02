#!/usr/bin/env bash
# Hermetic tests for the IDE helpers in bootstrap/base_functions: the deb822 apt
# sources of VS Code, Cursor and Antigravity, the repair step, and the Cursor
# AppImage download.  Nothing here touches /etc, the network, or the real apt:
# sudo, curl, gpg, dpkg and apt-get are replaced by functions that act inside a
# temporary directory and record what they were asked to do.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BOOTSTRAP_DIR="$REPO_ROOT/.local/scripts/bootstrap"
SCRATCH="$(mktemp -d)"
RM_COMMAND="$(command -v rm)"
trap '"$RM_COMMAND" -rf "$SCRATCH"' EXIT
export TMPDIR="$SCRATCH"

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

# shellcheck source=/dev/null
source "$BOOTSTRAP_DIR/base_functions"

###############################################################
# => Stubs: the only commands the helpers run that could leave the sandbox
###############################################################

record() { printf '%s\n' "$*" >> "$CALLS"; }

record_args() {
    local out="" argument
    for argument in "$@"; do
        out+="[$argument]"
    done
    printf '%s\n' "$out" >> "$CALLS"
}

# Run the command as the current user; every path the helpers write is a sandbox path.
sudo() { "$@"; }

apt-get() { record "apt-get $*"; }

# A reply to `dpkg -s` says "installed" only for the names in FAKE_INSTALLED.
dpkg() {
    case "${1:-}" in
        --print-architecture) printf 'amd64\n' ;;
        -s)
            case " ${FAKE_INSTALLED:-} " in
                *" $2 "*) printf 'Package: %s\nStatus: install ok installed\n' "$2" ;;
                *) return 1 ;;
            esac
            ;;
        *) return 1 ;;
    esac
}

curl() {
    local output="" url="" reply
    local default_reply='{"downloadUrl":"https://downloads.example/cursor.AppImage"}'
    while [ $# -gt 0 ]; do
        case "$1" in
            -o) output="$2"; shift ;;
            -*) ;;
            *) url="$1" ;;
        esac
        shift
    done
    record "curl $url"
    [ -z "${FAKE_CURL_FAIL:-}" ] || return 22
    case "$url" in
        *api/download*)
            reply="${FAKE_API_REPLY:-$default_reply}"
            printf '%s' "$reply"
            ;;
        https://downloads.example/cursor.AppImage)
            truncate -s "${FAKE_APPIMAGE_BYTES:-2097152}" "$output"
            ;;
        *) printf 'fake key for %s\n' "$url" > "$output" ;;
    esac
}

gpg() {
    local output="" input=""
    while [ $# -gt 0 ]; do
        case "$1" in
            -o) output="$2"; shift ;;
            --dearmor|--yes) ;;
            *) input="$1" ;;
        esac
        shift
    done
    record "gpg $output"
    [ -z "${FAKE_GPG_FAIL:-}" ] || return 2
    if [ -n "${FAKE_GPG_EMPTY:-}" ]; then
        : > "$output"
    else
        cat "$input" > "$output"
    fi
}

new_case() {
    CASE="$SCRATCH/$1"
    mkdir -p "$CASE/sources.d" "$CASE/keyrings" "$CASE/home"
    export APT_SOURCES_DIR="$CASE/sources.d"
    export HOME="$CASE/home"
    CALLS="$CASE/calls.log"
    : > "$CALLS"
    unset APT_SOURCES_DRY_RUN FAKE_CURL_FAIL FAKE_GPG_FAIL FAKE_GPG_EMPTY \
        FAKE_INSTALLED FAKE_API_REPLY FAKE_APPIMAGE_BYTES FAKE_FAIL_VENDOR
    APT_SOURCES_CHANGED=false
}

count_lines() {
    local count
    count="$(grep -cFx -- "$1" "$CALLS" || true)"
    printf '%s\n' "$count"
}

sandbox_fingerprint() {
    find "$CASE/sources.d" "$CASE/keyrings" -type f -exec sha256sum {} + | sort
}

ensure_demo() {
    apt_vendor_source_ensure demo https://repo.example/apt stable main amd64 \
        https://repo.example/key.asc "$CASE/keyrings/demo.gpg"
}

###############################################################
# => apt_vendor_source_ensure
###############################################################

test_source_is_written_in_deb822_format() {
    new_case deb822
    local expected actual

    ensure_demo || fail 'a fresh source must be written'

    expected="### THIS FILE IS AUTOMATICALLY CONFIGURED ###
# You may comment out this entry, but any other modifications may be lost.
Types: deb
URIs: https://repo.example/apt
Suites: stable
Components: main
Architectures: amd64
Signed-By: $CASE/keyrings/demo.gpg"
    actual="$(cat "$APT_SOURCES_DIR/demo.sources")"
    assert_equals "$expected" "$actual" 'the source file content'
    [ -s "$CASE/keyrings/demo.gpg" ] || fail 'the signing key was not installed'
    assert_equals true "$APT_SOURCES_CHANGED" 'a new source must flag an apt refresh'
    assert_equals 1 "$(count_lines 'curl https://repo.example/key.asc')" 'the key download count'
    assert_equals 0 "$(count_lines 'apt-get update')" 'the helper alone must not refresh apt'
}

test_architectures_line_is_omitted_when_empty() {
    new_case no-architectures

    apt_vendor_source_ensure demo https://repo.example/apt stable main "" \
        https://repo.example/key.asc "$CASE/keyrings/demo.gpg" || fail 'the source was not written'

    if grep -q '^Architectures:' "$APT_SOURCES_DIR/demo.sources"; then
        fail 'an empty architecture list must not produce an Architectures line'
    fi
    grep -Fq 'Components: main' "$APT_SOURCES_DIR/demo.sources" || fail 'the components line is missing'
}

test_components_line_is_omitted_for_a_flat_repository() {
    new_case flat-repository
    local expected

    apt_vendor_source_ensure flat https://repo.example/deb/amd64 / "" "" \
        https://repo.example/key.asc "$CASE/keyrings/flat.gpg" || fail 'the flat source was not written'

    # A flat repository's suite ends in "/" and apt rejects it next to a Components line.
    expected="### THIS FILE IS AUTOMATICALLY CONFIGURED ###
# You may comment out this entry, but any other modifications may be lost.
Types: deb
URIs: https://repo.example/deb/amd64
Suites: /
Signed-By: $CASE/keyrings/flat.gpg"
    assert_equals "$expected" "$(cat "$APT_SOURCES_DIR/flat.sources")" 'the flat source file content'
}

test_second_run_changes_nothing() {
    new_case idempotent
    local before after

    ensure_demo || fail 'the first run failed'
    before="$(sandbox_fingerprint)"
    : > "$CALLS"
    APT_SOURCES_CHANGED=false

    ensure_demo || fail 'the second run failed'

    after="$(sandbox_fingerprint)"
    assert_equals "$before" "$after" 'files after the second run'
    assert_equals false "$APT_SOURCES_CHANGED" 'an up-to-date source must not flag a refresh'
    assert_equals 0 "$(grep -c . "$CALLS" || true)" 'commands run by the second run'
}

test_a_disabled_source_is_enabled_again() {
    new_case disabled
    local expected

    ensure_demo || fail 'the first run failed'
    expected="$(cat "$APT_SOURCES_DIR/demo.sources")"
    printf 'Enabled: no\n' >> "$APT_SOURCES_DIR/demo.sources"
    : > "$CALLS"
    APT_SOURCES_CHANGED=false

    ensure_demo || fail 'repairing a disabled source failed'

    assert_equals "$expected" "$(cat "$APT_SOURCES_DIR/demo.sources")" 'the repaired source content'
    assert_equals true "$APT_SOURCES_CHANGED" 'repairing a source must flag a refresh'
    assert_equals 0 "$(count_lines 'curl https://repo.example/key.asc')" 'an installed key must not be downloaded again'
}

test_a_legacy_list_is_retired_to_a_backup() {
    new_case legacy
    printf 'deb https://repo.example/apt stable main\n' > "$APT_SOURCES_DIR/demo.list"

    ensure_demo || fail 'migrating a legacy list failed'

    [ ! -e "$APT_SOURCES_DIR/demo.list" ] || fail 'the legacy list must no longer be read by apt'
    grep -Fqx 'deb https://repo.example/apt stable main' "$APT_SOURCES_DIR/demo.list.bak" ||
        fail 'the legacy list must be kept as a backup'
    [ -s "$APT_SOURCES_DIR/demo.sources" ] || fail 'the deb822 source was not written'
    assert_equals true "$APT_SOURCES_CHANGED" 'retiring a legacy list must flag a refresh'
}

test_a_failed_key_download_writes_no_source() {
    new_case key-download
    local output status=0
    export FAKE_CURL_FAIL=1

    output="$(ensure_demo 2>&1)" || status=$?

    [ "$status" -ne 0 ] || fail 'a failed key download must fail the helper'
    assert_contains "$output" 'Could not download the demo apt signing key' 'the error message'
    [ ! -e "$APT_SOURCES_DIR/demo.sources" ] || fail 'a source must not be written without its key'
    if compgen -G "$SCRATCH/tmp.*" >/dev/null; then fail 'a temporary key file was left behind'; fi
}

test_a_failed_key_install_writes_no_source() {
    new_case key-install
    local output status=0
    export FAKE_GPG_FAIL=1

    output="$(ensure_demo 2>&1)" || status=$?

    [ "$status" -ne 0 ] || fail 'a failed key install must fail the helper'
    assert_contains "$output" 'Could not install the demo apt signing key' 'the error message'
    [ ! -e "$APT_SOURCES_DIR/demo.sources" ] || fail 'a source must not be written without its key'
    if compgen -G "$SCRATCH/tmp.*" >/dev/null; then fail 'a temporary key file was left behind'; fi
}

test_an_empty_keyring_is_rejected() {
    new_case empty-key
    local status=0
    export FAKE_GPG_EMPTY=1

    ensure_demo >/dev/null 2>&1 || status=$?

    [ "$status" -ne 0 ] || fail 'an empty keyring must fail the helper'
    [ ! -e "$APT_SOURCES_DIR/demo.sources" ] || fail 'a source must not be written with an empty keyring'
}

test_dry_run_changes_nothing() {
    new_case dry-run
    local before after output
    printf 'deb https://repo.example/apt stable main\n' > "$APT_SOURCES_DIR/demo.list"
    printf 'Types: deb\nEnabled: no\n' > "$APT_SOURCES_DIR/demo.sources"
    before="$(sandbox_fingerprint)"
    export APT_SOURCES_DRY_RUN=true

    # Not captured with $(...): that subshell would hide APT_SOURCES_CHANGED.
    ensure_demo > "$CASE/dry-run.out" || fail 'a dry run must succeed'
    output="$(cat "$CASE/dry-run.out")"

    after="$(sandbox_fingerprint)"
    assert_equals "$before" "$after" 'files after a dry run'
    assert_equals 0 "$(grep -c . "$CALLS" || true)" 'commands run by a dry run (no download, no gpg)'
    assert_contains "$output" '[dry-run] would download https://repo.example/key.asc' 'the key preview'
    assert_contains "$output" "[dry-run] would write $APT_SOURCES_DIR/demo.sources" 'the source preview'
    assert_contains "$output" 'URIs: https://repo.example/apt' 'the previewed content'
    assert_contains "$output" "[dry-run] would run: sudo mv -f $APT_SOURCES_DIR/demo.list" 'the retirement preview'
    assert_equals true "$APT_SOURCES_CHANGED" 'a dry run still reports that a refresh would be needed'
}

###############################################################
# => ide_apt_sources_ensure / ide_apt_sources_repair
# (apt_vendor_source_ensure is replaced so the real keyring paths stay untouched)
###############################################################

stub_vendor_source() {
    apt_vendor_source_ensure() {
        record_args source "$@"
        APT_SOURCES_CHANGED=true
        [ "$1" != "${FAKE_FAIL_VENDOR:-}" ]
    }
}

test_presets_name_the_vendor_repositories() {
    new_case presets
    local expected
    stub_vendor_source

    ide_apt_sources_ensure vscode cursor antigravity || fail 'configuring the IDE sources failed'

    expected='[source][vscode][https://packages.microsoft.com/repos/code][stable][main][amd64][https://packages.microsoft.com/keys/microsoft.asc][/usr/share/keyrings/microsoft.gpg]
[source][cursor][https://downloads.cursor.com/aptrepo][stable][main][amd64,arm64][https://downloads.cursor.com/keys/anysphere.asc][/usr/share/keyrings/anysphere.gpg]
[source][antigravity][https://us-central1-apt.pkg.dev/projects/antigravity-auto-updater-dev/][antigravity-debian][main][][https://us-central1-apt.pkg.dev/doc/repo-signing-key.gpg][/etc/apt/keyrings/antigravity-repo-key.gpg]
apt-get update'
    assert_equals "$expected" "$(cat "$CALLS")" 'the vendor presets and the single apt refresh'
}

test_apt_is_not_refreshed_when_no_source_changed() {
    new_case no-refresh
    apt_vendor_source_ensure() { record_args source "$@"; }

    ide_apt_sources_ensure vscode cursor || fail 'configuring the IDE sources failed'

    assert_equals 0 "$(count_lines 'apt-get update')" 'apt refreshes when nothing changed'
}

test_one_failing_vendor_does_not_skip_the_others() {
    new_case partial-failure
    local status=0
    stub_vendor_source
    export FAKE_FAIL_VENDOR=cursor

    ide_apt_sources_ensure vscode cursor antigravity || status=$?

    [ "$status" -ne 0 ] || fail 'a failing vendor must be reported'
    assert_equals 1 "$(grep -c '^\[source\]\[vscode\]' "$CALLS" || true)" 'vscode attempts'
    assert_equals 1 "$(grep -c '^\[source\]\[antigravity\]' "$CALLS" || true)" 'antigravity attempts after the failure'
    assert_equals 1 "$(count_lines 'apt-get update')" 'the refresh for the sources that did change'
}

test_unknown_ide_is_rejected() {
    new_case unknown
    local output status=0
    stub_vendor_source

    output="$(ide_apt_sources_ensure notepad 2>&1)" || status=$?

    assert_equals 2 "$status" 'the exit status for an unknown IDE'
    assert_contains "$output" 'Unknown IDE apt source: notepad' 'the error message'
    assert_equals 0 "$(count_lines 'apt-get update')" 'apt refreshes for an unknown IDE'
}

test_ide_dry_run_previews_the_apt_refresh() {
    new_case ide-dry-run
    local output
    stub_vendor_source
    export APT_SOURCES_DRY_RUN=true

    output="$(ide_apt_sources_ensure cursor)" || fail 'a dry run must succeed'

    assert_contains "$output" '[dry-run] would run: sudo apt-get update' 'the refresh preview'
    assert_equals 0 "$(count_lines 'apt-get update')" 'a dry run must not refresh apt'
}

test_repair_covers_only_installed_ide_packages() {
    new_case repair-installed
    stub_vendor_source
    export FAKE_INSTALLED='code antigravity'

    ide_apt_sources_repair >/dev/null || fail 'the repair failed'

    assert_equals 1 "$(grep -c '^\[source\]\[vscode\]' "$CALLS" || true)" 'vscode repairs'
    assert_equals 1 "$(grep -c '^\[source\]\[antigravity\]' "$CALLS" || true)" 'antigravity repairs'
    assert_equals 0 "$(grep -c '^\[source\]\[cursor\]' "$CALLS" || true)" 'a not-installed IDE must be left alone'
}

test_repair_refreshes_apt_even_when_sources_are_already_right() {
    new_case repair-refresh
    apt_vendor_source_ensure() { record_args source "$@"; }
    export FAKE_INSTALLED='cursor'

    ide_apt_sources_repair >/dev/null || fail 'the repair failed'

    assert_equals 1 "$(count_lines 'apt-get update')" 'the refresh of stale package lists'
}

test_repair_without_ide_packages_does_nothing() {
    new_case repair-none
    local output
    stub_vendor_source
    export FAKE_INSTALLED=''

    output="$(ide_apt_sources_repair)" || fail 'a repair with nothing to do must succeed'

    assert_contains "$output" 'nothing to repair' 'the explanation'
    assert_equals 0 "$(grep -c . "$CALLS" || true)" 'commands run when nothing is installed'
}

###############################################################
# => cursor_appimage_download
###############################################################

test_cursor_appimage_is_resolved_through_the_api_reply() {
    new_case appimage
    local destination="$CASE/home/cursor.AppImage" size

    cursor_appimage_download "$destination" || fail 'the AppImage download failed'

    size="$(stat -c%s "$destination")"
    [ "$size" -ge 1048576 ] || fail "the AppImage is too small: $size bytes"
    [ ! -e "$destination.part" ] || fail 'the partial file must be renamed'
    assert_equals 1 "$(grep -c '^curl https://cursor.com/api/download' "$CALLS" || true)" 'the API lookup'
    assert_equals 1 "$(count_lines 'curl https://downloads.example/cursor.AppImage')" 'the real download'
}

test_cursor_appimage_rejects_an_api_reply_without_a_url() {
    new_case appimage-no-url
    local destination="$CASE/home/cursor.AppImage" status=0
    export FAKE_API_REPLY='{"version":"9.9.9"}'

    cursor_appimage_download "$destination" >/dev/null 2>&1 || status=$?

    [ "$status" -ne 0 ] || fail 'a reply without downloadUrl must fail'
    [ ! -e "$destination" ] || fail 'nothing may be saved when the URL is unknown'
    assert_equals 0 "$(count_lines 'curl https://downloads.example/cursor.AppImage')" 'downloads without a URL'
}

test_cursor_appimage_rejects_a_truncated_download() {
    new_case appimage-truncated
    local destination="$CASE/home/cursor.AppImage" status=0
    export FAKE_APPIMAGE_BYTES=4096

    cursor_appimage_download "$destination" >/dev/null 2>&1 || status=$?

    [ "$status" -ne 0 ] || fail 'a truncated AppImage must fail'
    [ ! -e "$destination" ] || fail 'a truncated AppImage must not be installed'
    [ ! -e "$destination.part" ] || fail 'a truncated download must be removed'
}

###############################################################
# => The bootstraps use the helpers instead of writing apt repositories by hand
###############################################################

test_bootstraps_use_the_shared_helpers() {
    local work="$BOOTSTRAP_DIR/work_functions"
    local ubuntu="$BOOTSTRAP_DIR/ubuntu_functions"
    local arch="$BOOTSTRAP_DIR/arch_functions"
    local file

    grep -Fq 'ide_apt_sources_ensure vscode cursor antigravity' "$work" ||
        fail 'work must configure the VS Code, Cursor and Antigravity sources'
    grep -Fq 'ide_apt_sources_ensure cursor antigravity' "$ubuntu" ||
        fail 'ubuntu must configure the Cursor and Antigravity sources (VS Code is a snap there)'
    grep -Fq 'cursor_appimage_download' "$arch" ||
        fail 'arch must download the Cursor AppImage through the shared helper'

    for file in "$work" "$ubuntu"; do
        if grep -Eq 'sources\.list\.d/(vscode|antigravity)\.list' "$file"; then
            fail "$file must not write legacy one-line apt sources; a release upgrade disables them"
        fi
    done
    for file in "$work" "$ubuntu" "$arch"; do
        if grep -Fq 'cursor.com/api/download' "$file"; then
            fail "$file must resolve the Cursor download through cursor_appimage_download (the API replies with JSON)"
        fi
    done
}

for test_name in \
    test_source_is_written_in_deb822_format \
    test_architectures_line_is_omitted_when_empty \
    test_components_line_is_omitted_for_a_flat_repository \
    test_second_run_changes_nothing \
    test_a_disabled_source_is_enabled_again \
    test_a_legacy_list_is_retired_to_a_backup \
    test_a_failed_key_download_writes_no_source \
    test_a_failed_key_install_writes_no_source \
    test_an_empty_keyring_is_rejected \
    test_dry_run_changes_nothing \
    test_presets_name_the_vendor_repositories \
    test_apt_is_not_refreshed_when_no_source_changed \
    test_one_failing_vendor_does_not_skip_the_others \
    test_unknown_ide_is_rejected \
    test_ide_dry_run_previews_the_apt_refresh \
    test_repair_covers_only_installed_ide_packages \
    test_repair_refreshes_apt_even_when_sources_are_already_right \
    test_repair_without_ide_packages_does_nothing \
    test_cursor_appimage_is_resolved_through_the_api_reply \
    test_cursor_appimage_rejects_an_api_reply_without_a_url \
    test_cursor_appimage_rejects_a_truncated_download \
    test_bootstraps_use_the_shared_helpers; do
    # A subshell keeps each test's stub overrides and exported variables to itself.
    ( "$test_name" ) || {
        printf 'not ok - %s\n' "$test_name" >&2
        exit 1
    }
    printf 'ok - %s\n' "$test_name"
done

printf 'IDE apt source tests passed.\n'
