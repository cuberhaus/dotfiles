#!/usr/bin/env bash
# Hermetic tests for the LanguageTool bootstrap step (languagetool_install, its Java check and
# langtool_uninstall in bootstrap/base_functions), and for how every profile declares a Java
# runtime and calls the step.  LanguageTool is downloaded instead of tracked because it is
# 240 MB.  Nothing here touches the network or the real home directory: curl and java are
# functions, HOME is a temporary directory, and the sha256sum comparison is real only in the one
# test that says so.  unzip and mv are real (curl hands the step a genuine zip).
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BOOTSTRAP_DIR="$REPO_ROOT/.local/scripts/bootstrap"
SCRATCH="$(mktemp -d)"
trap 'rm -rf "$SCRATCH"' EXIT

VERSION=6.6
URL="https://languagetool.org/download/LanguageTool-$VERSION.zip"
# The SHA-256 of that zip, checked against the Internet Archive's copies of the same file.  This
# second copy makes changing the digest in base_functions a deliberate act that fails this test
# until both agree.
DIGEST=53600506b399bb5ffe1e4c8dec794fd378212f14aaf38ccef9b6f89314d11631

PROFILES=(arch manjaro ubuntu ubuntu_windows mac work gentoo)

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

###############################################################
# => Stubs: everything the step runs that could leave the sandbox
###############################################################

record() { printf '%s\n' "$*" >> "$CALLS"; }

# "Downloads" the case's zip.  FAKE_CURL_FAIL makes it fail like curl --fail does on an HTTP error.
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
    [ -z "${FAKE_CURL_FAIL:-}" ] || return 22
    cp "$FIXTURE_ZIP" "$output"
}

# The pinned digest belongs to the real zip, which this test never downloads, so the stub records
# the line it was asked to check, confirms that the file it names is in the folder the step
# changed into, and answers from FAKE_SHA256_FAIL.  FAKE_REAL_SHA256 runs the real tool.
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

# What `java -version` prints (to stderr, like the real one) and how it exits.  The default is
# the Java of the Ubuntu this repository targets.
java() {
    printf '%s\n' "${FAKE_JAVA_OUTPUT:-openjdk version \"25.0.4.1\" 2026-08-18}" >&2
    return "${FAKE_JAVA_STATUS:-0}"
}

# `command -v NAME` answers from FAKE_NO_* instead of this machine's PATH, for the tools the step
# looks for.  Everything else is the real command.
command() {
    if [ "${1:-}" = -v ]; then
        case "${2:-}" in
            java)  [ -z "${FAKE_NO_JAVA:-}" ]; return ;;
            unzip) [ -z "${FAKE_NO_UNZIP:-}" ]; return ;;
        esac
    fi
    builtin command "$@"
}

# The real zip holds a LanguageTool-<version> folder with the server jar in it.  These jars are
# text, so a test can tell that the file it unpacked is the file that was installed.
make_zip() {
    local source="$CASE/zip-source"
    mkdir -p "$source/LanguageTool-$VERSION"
    printf 'fake commandline\n' > "$source/LanguageTool-$VERSION/languagetool-commandline.jar"
    if [ "${1:-with-server}" = with-server ]; then
        printf 'fake server\n' > "$source/LanguageTool-$VERSION/languagetool-server.jar"
    fi
    FIXTURE_ZIP="$CASE/fixture.zip"
    (cd "$source" && python3 -m zipfile -c "$FIXTURE_ZIP" "LanguageTool-$VERSION")
}

new_case() {
    CASE="$SCRATCH/$1"
    mkdir -p "$CASE/home"
    export HOME="$CASE/home"
    CALLS="$CASE/calls.log"
    : > "$CALLS"
    DICTIONARIES="$HOME/.config/texstudio/dictionaries"
    DEST="$DICTIONARIES/LanguageTool-$VERSION"
    unset FAKE_CURL_FAIL FAKE_SHA256_FAIL FAKE_REAL_SHA256 FAKE_NO_JAVA FAKE_NO_UNZIP \
        FAKE_JAVA_OUTPUT FAKE_JAVA_STATUS
}

# Runs a step the way the bootstrap does (its failure is tested, not fatal): OUTPUT holds what it
# printed and STATUS how it ended.  The stubs write their logs to files, so the command
# substitution does not hide them.
run_step() {
    STATUS=0
    OUTPUT="$("$@" 2>&1)" || STATUS=$?
}

calls() { cat "$CALLS"; }

# A machine that already has LanguageTool, so only the Java check can differ between cases.
install_beforehand() {
    mkdir -p "$DEST"
    printf 'mine\n' > "$DEST/languagetool-server.jar"
}

###############################################################
# => languagetool_install
###############################################################

test_the_pin_matches_this_test() {
    local function_text
    function_text="$(declare -f languagetool_install)"
    assert_contains "$function_text" "version=\"$VERSION\"" 'the pinned version'
    assert_contains "$function_text" "digest=\"$DIGEST\"" 'the pinned digest'
}

test_a_first_install_checks_the_digest_before_unpacking() {
    new_case lt-first
    make_zip

    run_step languagetool_install

    assert_equals 0 "$STATUS" "the status (output: $OUTPUT)"
    assert_equals "curl $URL
sha256sum $DIGEST  LanguageTool-$VERSION.zip" "$(calls)" 'the commands, in order (download, digest)'
    assert_equals 'fake server' "$(cat "$DEST/languagetool-server.jar")" 'the installed jar (the unpacked file)'
    [ ! -e "$DEST.partial" ] || fail 'the work folder was left behind'
    assert_contains "$OUTPUT" "LanguageTool $VERSION installed" 'the confirmation'
    assert_not_contains "$OUTPUT" 'Java' 'a Java warning on a machine that has Java 25'
}

test_an_installed_languagetool_is_left_alone() {
    new_case lt-installed
    make_zip
    install_beforehand

    run_step languagetool_install

    assert_equals 0 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'already installed' 'the explanation'
    assert_equals '' "$(calls)" 'commands run on a machine that has it'
    assert_equals 'mine' "$(cat "$DEST/languagetool-server.jar")" 'the existing jar'
}

test_a_failed_download_installs_nothing() {
    new_case lt-download-fails
    make_zip
    export FAKE_CURL_FAIL=1

    run_step languagetool_install

    assert_equals 1 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'was not installed' 'the warning'
    [ ! -e "$DEST" ] || fail 'the destination exists after a failed download'
    [ ! -e "$DEST.partial" ] || fail 'the work folder was left behind'
}

test_a_failed_digest_check_installs_nothing() {
    new_case lt-digest-fails
    make_zip
    export FAKE_SHA256_FAIL=1

    run_step languagetool_install

    assert_equals 1 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'checksum' 'the warning names the checksum'
    [ ! -e "$DEST" ] || fail 'the destination exists after a failed digest check'
    [ ! -e "$DEST.partial" ] || fail 'the work folder was left behind'
}

test_the_real_sha256sum_refuses_a_download_that_does_not_match_the_pin() {
    new_case lt-real-digest
    make_zip
    export FAKE_REAL_SHA256=1

    run_step languagetool_install

    assert_equals 1 "$STATUS" 'the status'
    [ ! -e "$DEST" ] || fail 'a zip that is not the pinned release was installed'
    [ ! -e "$DEST.partial" ] || fail 'the work folder was left behind'
}

test_a_zip_without_the_server_jar_installs_nothing() {
    new_case lt-no-jar
    make_zip without-server

    run_step languagetool_install

    assert_equals 1 "$STATUS" 'the status'
    [ ! -e "$DEST" ] || fail 'the destination exists though the zip had no server jar'
    [ ! -e "$DEST.partial" ] || fail 'the work folder was left behind'
}

test_an_interrupted_earlier_run_is_cleaned_up() {
    new_case lt-recover
    make_zip
    mkdir -p "$DEST.partial" "$DEST"
    printf 'junk\n' > "$DEST.partial/junk"
    printf 'half\n' > "$DEST/stale"

    run_step languagetool_install

    assert_equals 0 "$STATUS" "the status (output: $OUTPUT)"
    [ -f "$DEST/languagetool-server.jar" ] || fail 'the server jar is missing'
    [ ! -e "$DEST/stale" ] || fail 'the half-installed folder was not replaced'
    [ ! -e "$DEST.partial" ] || fail 'the old work folder was left behind'
}

test_a_missing_unzip_is_reported_before_downloading() {
    new_case lt-no-unzip
    make_zip
    export FAKE_NO_UNZIP=1

    run_step languagetool_install

    assert_equals 1 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'needs unzip' 'the explanation'
    assert_equals '' "$(calls)" 'commands run without unzip'
}

###############################################################
# => The Java check (LanguageTool 6.x needs Java 17 or newer)
###############################################################

# java_case NAME OUTPUT [STATUS]: a fresh case whose java prints OUTPUT, on a machine that
# already has LanguageTool, so the status of the step is the status of the Java check.
java_case() {
    new_case "java-$1"
    install_beforehand
    export FAKE_JAVA_OUTPUT="$2" FAKE_JAVA_STATUS="${3:-0}"
}

test_java_17_is_enough() {
    java_case 17 'openjdk version "17.0.9" 2023-10-17'
    run_step languagetool_install
    assert_equals 0 "$STATUS" "the status (output: $OUTPUT)"
    assert_not_contains "$OUTPUT" 'Java' 'a warning for Java 17'
}

test_java_21_with_a_trailing_runtime_line_is_enough() {
    java_case 21 'openjdk version "21.0.1" 2023-10-17
OpenJDK Runtime Environment (build 21.0.1+12)'
    run_step languagetool_install
    assert_equals 0 "$STATUS" "the status (output: $OUTPUT)"
}

test_java_noise_before_the_version_line_is_ignored() {
    java_case noise 'Picked up _JAVA_OPTIONS: -Xmx1g
openjdk version "21.0.1" 2023-10-17'
    run_step languagetool_install
    assert_equals 0 "$STATUS" "the status (output: $OUTPUT)"
}

test_java_16_is_too_old() {
    java_case 16 'openjdk version "16.0.2" 2021-07-20'
    run_step languagetool_install
    assert_equals 1 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'needs Java 17 or newer, but this machine has Java 16' 'the warning'
}

test_java_11_is_too_old() {
    java_case 11 'openjdk version "11.0.21" 2023-10-17'
    run_step languagetool_install
    assert_equals 1 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'has Java 11' 'the warning names the version found'
    assert_contains "$OUTPUT" 'default-jre-headless' 'the warning says how to get a runtime'
}

test_java_8_reports_itself_as_1_8() {
    java_case 8 'java version "1.8.0_392"'
    run_step languagetool_install
    assert_equals 1 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'has Java 8' 'the 1.8 version number is read as Java 8'
}

test_a_machine_without_java_fails_the_step() {
    java_case none 'unused'
    export FAKE_NO_JAVA=1
    run_step languagetool_install
    assert_equals 1 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'has no working Java' 'the warning'
}

test_the_macos_stub_without_a_jdk_counts_as_no_java() {
    # /usr/bin/java exists on a Mac without a JDK, prints this, and exits with an error.
    java_case macos-stub 'The operation couldn'"'"'t be completed. Unable to locate a Java Runtime.' 1
    run_step languagetool_install
    assert_equals 1 "$STATUS" 'the status'
    assert_contains "$OUTPUT" 'has no working Java' 'the warning'
}

test_a_fresh_install_with_old_java_installs_but_fails_the_step() {
    new_case lt-old-java
    make_zip
    export FAKE_JAVA_OUTPUT='openjdk version "11.0.21" 2023-10-17'

    run_step languagetool_install

    assert_equals 1 "$STATUS" 'the status'
    assert_equals 'fake server' "$(cat "$DEST/languagetool-server.jar")" 'LanguageTool is installed anyway'
    assert_contains "$OUTPUT" 'has Java 11' 'the warning'
}

test_an_installed_languagetool_does_not_skip_the_java_check() {
    java_case rerun 'openjdk version "11.0.21" 2023-10-17'
    run_step languagetool_install
    assert_contains "$OUTPUT" 'already installed' 'the skip message'
    assert_equals 1 "$STATUS" 'the status'
    assert_equals '' "$(calls)" 'commands run on a machine that has it'
}

###############################################################
# => langtool_uninstall
###############################################################

test_uninstall_removes_every_version_and_keeps_the_tracked_dictionaries() {
    new_case lt-uninstall
    mkdir -p "$DICTIONARIES/LanguageTool-5.7" "$DICTIONARIES/LanguageTool-$VERSION" \
        "$DICTIONARIES/LanguageTool-$VERSION.partial" "$DICTIONARIES/ca.3.0.6.oxt"
    : > "$DICTIONARIES/dict-en.oxt"
    : > "$DICTIONARIES/LanguageTool-5.7/languagetool-server.jar"

    run_step langtool_uninstall

    assert_equals 0 "$STATUS" 'the status'
    assert_equals "ca.3.0.6.oxt
dict-en.oxt" "$(ls -A "$DICTIONARIES")" 'what is left'
}

test_uninstall_succeeds_when_there_is_nothing_to_remove() {
    new_case lt-uninstall-empty

    run_step langtool_uninstall

    assert_equals 0 "$STATUS" 'the status'
}

###############################################################
# => How the profiles use the step
###############################################################

test_every_profile_calls_the_step_without_aborting_provisioning() {
    local profile file
    for profile in "${PROFILES[@]}"; do
        file="$BOOTSTRAP_DIR/$profile"
        grep -Eq '^[[:space:]]*(gentoo_apply )?languagetool_install \|\|$' "$file" ||
            fail "$file must call languagetool_install and not abort when it fails"
        grep -Fq "languagetool_install'" "$file" ||
            fail "$file must tell the user how to rerun languagetool_install"
    done
}

test_every_profile_declares_a_java_runtime_and_unzip() {
    local file
    # ubuntu_windows installs with ubuntu_functions and manjaro with arch_functions.
    grep -Eq '^[[:space:]]+jre-openjdk-headless([[:space:]]|$)' "$BOOTSTRAP_DIR/arch_functions" ||
        fail 'arch_functions must install jre-openjdk-headless'
    for file in ubuntu_functions work_functions; do
        grep -Eq '^[[:space:]]+default-jre-headless([[:space:]]|$)' "$BOOTSTRAP_DIR/$file" ||
            fail "$file must install default-jre-headless"
    done
    grep -Eq '^[[:space:]]+temurin([[:space:]]|$)' "$BOOTSTRAP_DIR/mac_functions" ||
        fail 'mac_functions must install the temurin cask'
    grep -Eq '^virtual/jre([[:space:]]|$)' "$BOOTSTRAP_DIR/gentoo.packages" ||
        fail 'gentoo.packages must install virtual/jre'

    # macOS ships unzip.
    for file in arch_functions ubuntu_functions work_functions; do
        grep -Eq '^[[:space:]]+unzip([[:space:]]|$)' "$BOOTSTRAP_DIR/$file" ||
            fail "$file must install unzip for languagetool_install"
    done
    grep -Eq '^app-arch/unzip([[:space:]]|$)' "$BOOTSTRAP_DIR/gentoo.packages" ||
        fail 'gentoo.packages must install app-arch/unzip'
}

test_the_uninstallers_offer_languagetool() {
    local profile file
    for profile in ubuntu arch manjaro mac work; do
        file="$BOOTSTRAP_DIR/uninstall_$profile"
        grep -Eq '^[[:space:]]*langtool_uninstall[[:space:]]' "$file" ||
            fail "$file must offer langtool_uninstall"
    done
}

###############################################################
# => What git tracks
###############################################################

test_languagetool_itself_is_not_tracked() {
    local count
    count="$(command git -C "$REPO_ROOT" ls-files -- .config/texstudio/dictionaries | grep -c 'LanguageTool-' || true)"
    assert_equals 0 "$count" 'tracked files inside a LanguageTool-* folder'
}

for test_name in \
    test_the_pin_matches_this_test \
    test_a_first_install_checks_the_digest_before_unpacking \
    test_an_installed_languagetool_is_left_alone \
    test_a_failed_download_installs_nothing \
    test_a_failed_digest_check_installs_nothing \
    test_the_real_sha256sum_refuses_a_download_that_does_not_match_the_pin \
    test_a_zip_without_the_server_jar_installs_nothing \
    test_an_interrupted_earlier_run_is_cleaned_up \
    test_a_missing_unzip_is_reported_before_downloading \
    test_java_17_is_enough \
    test_java_21_with_a_trailing_runtime_line_is_enough \
    test_java_noise_before_the_version_line_is_ignored \
    test_java_16_is_too_old \
    test_java_11_is_too_old \
    test_java_8_reports_itself_as_1_8 \
    test_a_machine_without_java_fails_the_step \
    test_the_macos_stub_without_a_jdk_counts_as_no_java \
    test_a_fresh_install_with_old_java_installs_but_fails_the_step \
    test_an_installed_languagetool_does_not_skip_the_java_check \
    test_uninstall_removes_every_version_and_keeps_the_tracked_dictionaries \
    test_uninstall_succeeds_when_there_is_nothing_to_remove \
    test_every_profile_calls_the_step_without_aborting_provisioning \
    test_every_profile_declares_a_java_runtime_and_unzip \
    test_the_uninstallers_offer_languagetool \
    test_languagetool_itself_is_not_tracked; do
    # A subshell keeps each test's stub overrides and exported variables to itself.
    ( "$test_name" ) || {
        printf 'not ok - %s\n' "$test_name" >&2
        exit 1
    }
    printf 'ok - %s\n' "$test_name"
done

printf 'LanguageTool install tests passed.\n'
