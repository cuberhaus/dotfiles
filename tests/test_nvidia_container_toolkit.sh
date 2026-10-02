#!/usr/bin/env bash
# Hermetic tests for the NVIDIA Container Toolkit step in bootstrap/base_functions:
# GPU detection, the apt source, the install and uninstall functions, the repair
# step, and how the profiles use them.  Nothing here touches /etc, the network, or the
# real apt: the PCI bus is a fake sysfs tree, and sudo, curl, gpg, dpkg and apt-get are
# replaced by functions that act inside a temporary directory and record what they
# were asked to do.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BOOTSTRAP_DIR="$REPO_ROOT/.local/scripts/bootstrap"
SCRATCH="$(mktemp -d)"
# Resolved here, before the command() stub below exists, so this is the real builtin.
# shellcheck disable=SC2218
RM_COMMAND="$(command -v rm)"
trap '"$RM_COMMAND" -rf "$SCRATCH"' EXIT
export TMPDIR="$SCRATCH"

NVIDIA_VENDOR=0x10de
INTEL_VENDOR=0x8086
AMD_VENDOR=0x1002
PACKAGE=nvidia-container-toolkit
KEY_URL=https://nvidia.github.io/libnvidia-container/gpgkey

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
# => Stubs: the only commands the step runs that could leave the sandbox
###############################################################

record() { printf '%s\n' "$*" >> "$CALLS"; }

# Run the command as the current user.  `sudo env NAME=value command` keeps the
# assignment but drops env itself: env would run the real apt-get, not the stub below.
# The subshell keeps the assignment from leaking into the commands that follow.
# Every call is logged in SUDO_LOG apart from CALLS, so a test can tell the sudo commands
# a step runs from the ones it only prints in a dry run.
sudo() (
    printf '%s\n' "$*" >> "$SUDO_LOG"
    if [ "${1:-}" = env ]; then
        shift
        while [ $# -gt 0 ] && [[ "$1" == *=* ]]; do
            export "${1?}"
            shift
        done
    fi
    "$@"
)

apt-get() {
    record "apt-get $*${DEBIAN_FRONTEND:+ (DEBIAN_FRONTEND=$DEBIAN_FRONTEND)}"
    case "${1:-}" in
        update) [ -z "${FAKE_APT_UPDATE_FAIL:-}" ] ;;
        install) [ -z "${FAKE_APT_INSTALL_FAIL:-}" ] ;;
        remove) [ -z "${FAKE_APT_REMOVE_FAIL:-}" ] ;;
        *) return 0 ;;
    esac
}

# A reply to `dpkg -s` says "installed" only for the names in FAKE_INSTALLED.
dpkg() {
    case "${1:-}" in
        --print-architecture) printf '%s\n' "${FAKE_ARCH:-amd64}" ;;
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
    local output="" url=""
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
    printf 'fake key for %s\n' "$url" > "$output"
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
    cat "$input" > "$output"
}

# `command -v docker` answers from FAKE_DOCKER instead of this machine's PATH.
command() {
    if [ "${1:-}" = -v ] && [ "${2:-}" = docker ]; then
        if [ -n "${FAKE_DOCKER:-}" ]; then
            return 0
        fi
        return 1
    fi
    builtin command "$@"
}

new_case() {
    CASE="$SCRATCH/$1"
    mkdir -p "$CASE/pci" "$CASE/sources.d" "$CASE/keyrings" "$CASE/home"
    export PCI_DEVICES_DIR="$CASE/pci"
    export APT_SOURCES_DIR="$CASE/sources.d"
    export NVIDIA_CONTAINER_TOOLKIT_KEYRING="$CASE/keyrings/nvidia-container-toolkit-keyring.gpg"
    export HOME="$CASE/home"
    CALLS="$CASE/calls.log"
    SUDO_LOG="$CASE/sudo.log"
    : > "$CALLS"
    : > "$SUDO_LOG"
    unset APT_SOURCES_DRY_RUN DEBIAN_FRONTEND FAKE_ARCH FAKE_INSTALLED FAKE_DOCKER \
        FAKE_CURL_FAIL FAKE_APT_UPDATE_FAIL FAKE_APT_INSTALL_FAIL FAKE_APT_REMOVE_FAIL
    APT_SOURCES_CHANGED=false
}

# add_pci_device ADDRESS VENDOR CLASS: one entry of /sys/bus/pci/devices.
add_pci_device() {
    mkdir -p "$PCI_DEVICES_DIR/$1"
    printf '%s\n' "$2" > "$PCI_DEVICES_DIR/$1/vendor"
    printf '%s\n' "$3" > "$PCI_DEVICES_DIR/$1/class"
}

add_nvidia_gpu() { add_pci_device 0000:01:00.0 "$NVIDIA_VENDOR" 0x030000; }

count_lines() {
    local count
    count="$(grep -cFx -- "$1" "$CALLS" || true)"
    printf '%s\n' "$count"
}

sandbox_fingerprint() {
    find "$CASE/sources.d" "$CASE/keyrings" -type f -exec sha256sum {} + | sort
}

source_file() { printf '%s\n' "$APT_SOURCES_DIR/nvidia-container-toolkit.sources"; }

# The source apt must read for a machine of architecture $1.
expected_source() {
    printf '%s\n' \
        '### THIS FILE IS AUTOMATICALLY CONFIGURED ###' \
        '# You may comment out this entry, but any other modifications may be lost.' \
        'Types: deb' \
        "URIs: https://nvidia.github.io/libnvidia-container/stable/deb/$1" \
        'Suites: /' \
        "Signed-By: $NVIDIA_CONTAINER_TOOLKIT_KEYRING"
}

INSTALL_CALL="apt-get install -y $PACKAGE (DEBIAN_FRONTEND=noninteractive)"

###############################################################
# => nvidia_gpu_present
###############################################################

test_a_vga_adapter_is_a_gpu() {
    new_case gpu-vga
    add_pci_device 0000:00:02.0 "$INTEL_VENDOR" 0x060000
    add_nvidia_gpu

    nvidia_gpu_present || fail 'an NVIDIA VGA controller must count as an NVIDIA GPU'
}

test_a_hybrid_laptop_counts_through_its_3d_controller() {
    new_case gpu-hybrid
    # Intel graphics drive the panel; the NVIDIA part is a 3D controller (0x0302)
    # next to its HDMI audio function (0x0403).
    add_pci_device 0000:00:02.0 "$INTEL_VENDOR" 0x030000
    add_pci_device 0000:01:00.0 "$NVIDIA_VENDOR" 0x030200
    add_pci_device 0000:01:00.1 "$NVIDIA_VENDOR" 0x040300

    nvidia_gpu_present || fail 'an NVIDIA 3D controller must count as an NVIDIA GPU'
}

test_the_audio_function_of_a_card_alone_is_not_a_gpu() {
    new_case gpu-audio-only
    add_pci_device 0000:01:00.1 "$NVIDIA_VENDOR" 0x040300

    if nvidia_gpu_present; then fail 'an NVIDIA audio function alone must not count as a GPU'; fi
}

test_other_vendors_graphics_are_not_an_nvidia_gpu() {
    new_case gpu-other-vendors
    add_pci_device 0000:00:02.0 "$INTEL_VENDOR" 0x030000
    add_pci_device 0000:03:00.0 "$AMD_VENDOR" 0x030000
    add_pci_device 0000:04:00.0 "$AMD_VENDOR" 0x030200

    if nvidia_gpu_present; then fail 'Intel and AMD graphics must not count as an NVIDIA GPU'; fi
}

test_a_missing_or_empty_pci_tree_means_no_gpu() {
    new_case gpu-none
    if nvidia_gpu_present; then fail 'an empty PCI tree has no GPU'; fi

    PCI_DEVICES_DIR="$CASE/does-not-exist"
    if nvidia_gpu_present; then fail 'a missing PCI tree has no GPU'; fi
}

test_a_device_without_readable_ids_is_ignored() {
    new_case gpu-unreadable
    mkdir -p "$PCI_DEVICES_DIR/0000:05:00.0"
    printf '%s\n' "$NVIDIA_VENDOR" > "$PCI_DEVICES_DIR/0000:05:00.0/vendor"

    if nvidia_gpu_present; then fail 'a device with no class file must be ignored'; fi
}

###############################################################
# => nvidia_container_toolkit_install
###############################################################

test_machines_without_an_nvidia_gpu_are_skipped() {
    new_case install-no-gpu
    local output
    add_pci_device 0000:00:02.0 "$INTEL_VENDOR" 0x030000

    output="$(nvidia_container_toolkit_install 2>&1)" || fail 'a machine without an NVIDIA GPU must not fail'

    assert_contains "$output" 'No NVIDIA GPU detected' 'the explanation'
    assert_equals 0 "$(grep -c . "$CALLS" || true)" 'commands run on a machine without an NVIDIA GPU'
    assert_equals '' "$(sandbox_fingerprint)" 'files written on a machine without an NVIDIA GPU'
}

test_a_first_install_writes_the_source_refreshes_apt_then_installs() {
    new_case install-first
    local expected_calls
    add_nvidia_gpu

    nvidia_container_toolkit_install >/dev/null 2>&1 || fail 'the first install failed'

    assert_equals "$(expected_source amd64)" "$(cat "$(source_file)")" 'the source file content'
    [ -s "$NVIDIA_CONTAINER_TOOLKIT_KEYRING" ] || fail 'the signing key was not installed'
    expected_calls="curl $KEY_URL
gpg $NVIDIA_CONTAINER_TOOLKIT_KEYRING
apt-get update
$INSTALL_CALL"
    assert_equals "$expected_calls" "$(cat "$CALLS")" 'the commands, in order (key, source, refresh, then install)'
}

test_the_architecture_comes_from_dpkg() {
    new_case install-arm64
    add_nvidia_gpu
    export FAKE_ARCH=arm64

    nvidia_container_toolkit_install >/dev/null 2>&1 || fail 'the install failed'

    assert_equals "$(expected_source arm64)" "$(cat "$(source_file)")" 'the source file content'
}

test_a_second_run_only_asks_apt_to_install() {
    new_case install-second
    local output
    add_nvidia_gpu
    nvidia_container_toolkit_install >/dev/null 2>&1 || fail 'the first install failed'
    : > "$CALLS"
    export FAKE_INSTALLED="$PACKAGE" FAKE_DOCKER=1

    output="$(nvidia_container_toolkit_install 2>&1)" || fail 'the second run failed'

    assert_equals "$INSTALL_CALL" "$(cat "$CALLS")" 'the commands of an up-to-date machine (no key, no refresh)'
    assert_not_contains "$output" 'nvidia-ctk runtime configure' 'the Docker hint for an already installed toolkit'
}

test_a_source_disabled_by_a_release_upgrade_is_rewritten() {
    new_case install-disabled
    local expected_calls
    add_nvidia_gpu
    # What a release upgrade leaves behind: the file, switched off.
    printf '%s\n' 'Types: deb' \
        'URIs: https://nvidia.github.io/libnvidia-container/experimental/deb/' \
        'Suites: /' 'Components:' 'Enabled: no' \
        "Signed-By: $NVIDIA_CONTAINER_TOOLKIT_KEYRING" > "$(source_file)"
    printf 'existing key\n' > "$NVIDIA_CONTAINER_TOOLKIT_KEYRING"
    export FAKE_INSTALLED="$PACKAGE"

    nvidia_container_toolkit_install >/dev/null 2>&1 || fail 'repairing the source failed'

    assert_equals "$(expected_source amd64)" "$(cat "$(source_file)")" 'the repaired source content'
    assert_equals 'existing key' "$(cat "$NVIDIA_CONTAINER_TOOLKIT_KEYRING")" 'an installed key must be kept'
    expected_calls="apt-get update
$INSTALL_CALL"
    assert_equals "$expected_calls" "$(cat "$CALLS")" 'a repaired source needs a refresh, but no new key'
}

test_a_legacy_one_line_source_is_retired() {
    new_case install-legacy
    local legacy="$APT_SOURCES_DIR/nvidia-container-toolkit.list"
    add_nvidia_gpu
    # NVIDIA's own install guide writes this file.
    printf '%s\n' "deb [signed-by=$NVIDIA_CONTAINER_TOOLKIT_KEYRING] https://nvidia.github.io/libnvidia-container/stable/deb/amd64 /" > "$legacy"

    nvidia_container_toolkit_install >/dev/null 2>&1 || fail 'the install failed'

    [ ! -e "$legacy" ] || fail 'the legacy list must no longer be read by apt (two sources for one repository break apt)'
    [ -s "$legacy.bak" ] || fail 'the legacy list must be kept as a backup'
    [ -s "$(source_file)" ] || fail 'the deb822 source was not written'
}

test_a_failed_key_download_installs_nothing() {
    new_case install-key-fails
    local output status=0
    add_nvidia_gpu
    export FAKE_CURL_FAIL=1

    output="$(nvidia_container_toolkit_install 2>&1)" || status=$?

    [ "$status" -ne 0 ] || fail 'a failed key download must fail the step (the profiles turn that into a warning)'
    assert_contains "$output" 'Could not download the nvidia-container-toolkit apt signing key' 'the error message'
    [ ! -e "$(source_file)" ] || fail 'a source must not be written without its key'
    assert_equals 0 "$(grep -c '^apt-get' "$CALLS" || true)" 'apt commands after a failed key download'
    if compgen -G "$SCRATCH/tmp.*" >/dev/null; then fail 'a temporary key file was left behind'; fi
}

test_a_failed_apt_refresh_installs_nothing() {
    new_case install-update-fails
    local status=0
    add_nvidia_gpu
    export FAKE_APT_UPDATE_FAIL=1

    nvidia_container_toolkit_install >/dev/null 2>&1 || status=$?

    [ "$status" -ne 0 ] || fail 'a failed apt refresh must fail the step'
    assert_equals 1 "$(count_lines 'apt-get update')" 'refresh attempts'
    assert_equals 0 "$(count_lines "$INSTALL_CALL")" 'installs after a failed refresh'
}

test_a_failed_package_install_is_reported() {
    new_case install-apt-fails
    local status=0
    add_nvidia_gpu
    export FAKE_APT_INSTALL_FAIL=1 FAKE_DOCKER=1

    nvidia_container_toolkit_install >/dev/null 2>&1 || status=$?

    [ "$status" -ne 0 ] || fail 'a failed package install must fail the step'
}

test_dry_run_changes_nothing() {
    new_case install-dry-run
    local output
    add_nvidia_gpu
    export APT_SOURCES_DRY_RUN=true FAKE_DOCKER=1

    # Not captured with $(...): that subshell would hide the commands it ran.
    nvidia_container_toolkit_install > "$CASE/dry-run.out" 2>&1 || fail 'a dry run must succeed'
    output="$(cat "$CASE/dry-run.out")"

    assert_equals '' "$(sandbox_fingerprint)" 'files written by a dry run'
    assert_equals 0 "$(grep -c . "$CALLS" || true)" 'commands run by a dry run (no download, no gpg, no apt)'
    assert_equals 0 "$(grep -c . "$SUDO_LOG" || true)" 'sudo commands run by a dry run (they are only printed)'
    assert_contains "$output" "[dry-run] would download $KEY_URL" 'the key preview'
    assert_contains "$output" "[dry-run] would write $(source_file)" 'the source preview'
    assert_contains "$output" 'Suites: /' 'the previewed content'
    assert_contains "$output" '[dry-run] would run: sudo apt-get update' 'the refresh preview'
    assert_contains "$output" "[dry-run] would run: sudo env DEBIAN_FRONTEND=noninteractive apt-get install -y $PACKAGE" 'the install preview'
    assert_not_contains "$output" 'nvidia-ctk runtime configure' 'the Docker hint during a dry run'
}

test_the_docker_hint_follows_a_first_install_with_docker() {
    new_case hint-first
    local output
    add_nvidia_gpu
    export FAKE_DOCKER=1

    output="$(nvidia_container_toolkit_install 2>&1)" || fail 'the install failed'

    assert_contains "$output" 'sudo nvidia-ctk runtime configure --runtime=docker && sudo systemctl restart docker' 'the Docker hint'
    # The step installs the package only; it never edits Docker's configuration.
    assert_equals 0 "$(grep -c 'nvidia-ctk' "$CALLS" || true)" 'nvidia-ctk runs'
}

test_no_docker_hint_without_docker() {
    new_case hint-no-docker
    local output
    add_nvidia_gpu

    output="$(nvidia_container_toolkit_install 2>&1)" || fail 'the install failed'

    assert_not_contains "$output" 'nvidia-ctk runtime configure' 'the Docker hint on a machine without Docker'
}

###############################################################
# => nvidia_ctk_uninstall
###############################################################

test_uninstall_removes_the_package_the_source_and_the_key() {
    new_case uninstall
    local legacy="$APT_SOURCES_DIR/nvidia-container-toolkit.list"
    printf 'source\n' > "$(source_file)"
    printf 'legacy\n' > "$legacy"
    printf 'backup\n' > "$legacy.bak"
    printf 'key\n' > "$NVIDIA_CONTAINER_TOOLKIT_KEYRING"
    printf 'docker\n' > "$APT_SOURCES_DIR/docker.sources"
    printf 'docker key\n' > "$CASE/keyrings/docker.gpg"

    nvidia_ctk_uninstall >/dev/null 2>&1 || fail 'the uninstall failed'

    assert_equals 'apt-get remove --purge -y nvidia-container-toolkit' "$(cat "$CALLS")" 'the commands run'
    [ ! -e "$(source_file)" ] || fail 'the apt source must be removed'
    [ ! -e "$legacy" ] || fail 'the legacy apt source must be removed'
    [ ! -e "$legacy.bak" ] || fail 'the retired legacy source must be removed'
    [ ! -e "$NVIDIA_CONTAINER_TOOLKIT_KEYRING" ] || fail 'the signing key must be removed'
    [ -s "$APT_SOURCES_DIR/docker.sources" ] || fail "another vendor's apt source must stay"
    [ -s "$CASE/keyrings/docker.gpg" ] || fail "another vendor's signing key must stay"
}

test_uninstall_cleans_up_even_when_apt_cannot_remove_the_package() {
    new_case uninstall-apt-fails
    printf 'source\n' > "$(source_file)"
    printf 'key\n' > "$NVIDIA_CONTAINER_TOOLKIT_KEYRING"
    export FAKE_APT_REMOVE_FAIL=1

    nvidia_ctk_uninstall >/dev/null 2>&1 || fail 'a package that is not installed must not fail the uninstall'

    [ ! -e "$(source_file)" ] || fail 'the apt source must be removed anyway'
    [ ! -e "$NVIDIA_CONTAINER_TOOLKIT_KEYRING" ] || fail 'the signing key must be removed anyway'
}

###############################################################
# => make repair REPAIR=nvidia-container-toolkit
###############################################################

# The repair script runs as a child process, so it needs real executables where the
# tests above use functions: an apt-get so its precondition holds, and a dpkg.  In a
# dry run neither is ever asked to change anything, and the sudo is there to prove it:
# it fails closed, so a regression can never reach the real sudo from a test (on a
# machine with cached credentials that would rewrite /etc/apt and install packages).
# It also logs each call: the step runs inside `|| return 1` chains, where `set -e` is
# off, so a refused sudo can be swallowed and its exit status alone would prove nothing.
repair_stubs() {
    mkdir -p "$CASE/bin"
    cat > "$CASE/bin/apt-get" <<'EOF'
#!/bin/sh
exit 0
EOF
    cat > "$CASE/bin/dpkg" <<'EOF'
#!/bin/sh
case "$1" in --print-architecture) echo amd64 ;; *) exit 1 ;; esac
EOF
    cat > "$CASE/bin/sudo" <<'EOF'
#!/bin/sh
[ -z "$SUDO_CALL_LOG" ] || echo "sudo $*" >> "$SUDO_CALL_LOG"
echo "unexpected sudo call in the toolkit repair test: sudo $*" >&2
exit 99
EOF
    chmod +x "$CASE/bin/apt-get" "$CASE/bin/dpkg" "$CASE/bin/sudo"
}

run_repair() {
    SUDO_CALL_LOG="$CASE/sudo-calls.log" PATH="$CASE/bin:$PATH" \
        bash "$REPO_ROOT/.local/scripts/repair-installation" nvidia-container-toolkit
}

# A preview, or a machine without the GPU, must never reach sudo.
assert_no_sudo_calls() {
    [ ! -s "$CASE/sudo-calls.log" ] || fail "$1 reached sudo: $(cat "$CASE/sudo-calls.log")"
}

test_the_repair_step_previews_the_changes() {
    new_case repair-preview
    local output
    add_nvidia_gpu
    repair_stubs

    output="$(DRY_RUN=true run_repair 2>&1)" || fail "the repair preview failed: $output"

    assert_contains "$output" "[dry-run] would write $(source_file)" 'the source preview'
    assert_contains "$output" "[dry-run] would run: sudo env DEBIAN_FRONTEND=noninteractive apt-get install -y $PACKAGE" 'the install preview'
    assert_equals '' "$(sandbox_fingerprint)" 'files written by the repair preview'
    assert_no_sudo_calls 'the repair preview'
}

test_the_repair_step_skips_a_machine_without_an_nvidia_gpu() {
    new_case repair-no-gpu
    local output
    add_pci_device 0000:00:02.0 "$INTEL_VENDOR" 0x030000
    repair_stubs

    output="$(DRY_RUN=true run_repair 2>&1)" || fail "the repair failed: $output"

    assert_contains "$output" 'No NVIDIA GPU detected' 'the explanation'
    assert_equals '' "$(sandbox_fingerprint)" 'files written on a machine without an NVIDIA GPU'
    assert_no_sudo_calls 'the repair on a machine without an NVIDIA GPU'
}

test_the_repair_usage_lists_the_step() {
    new_case repair-usage
    local output status=0

    output="$(bash "$REPO_ROOT/.local/scripts/repair-installation" no-such-step 2>&1)" || status=$?

    assert_equals 2 "$status" 'the exit status for an unknown step'
    assert_contains "$output" 'nvidia-container-toolkit' 'the usage text'
    grep -Fq 'nvidia-container-toolkit' "$REPO_ROOT/Makefile" || fail 'make help must list the repair step'
}

###############################################################
# => How the profiles use the step
###############################################################

# Only the work profile runs the step for now.  Giving another profile the step takes one
# call in its entrypoint and one entry in its uninstall checklist; the audit follows by
# itself, because it reads the call.
test_work_runs_the_step_without_aborting_provisioning() {
    local file="$BOOTSTRAP_DIR/work"

    grep -Eq '^[[:space:]]*nvidia_container_toolkit_install \|\|$' "$file" ||
        fail "$file must call nvidia_container_toolkit_install and not abort when it fails"
    grep -Fq 'make repair REPAIR=nvidia-container-toolkit' "$file" ||
        fail "$file must tell the user how to rerun the step"
}

test_profiles_do_not_declare_the_package_themselves() {
    local file
    # A package in a profile's *_functions body is expected on every machine of that
    # profile, GPU or not.  The audit learns of this one through STANDALONE_INSTALLERS.
    # Comments and the `make repair REPAIR=nvidia-container-toolkit` hint may name it.
    for file in "$BOOTSTRAP_DIR/ubuntu_functions" "$BOOTSTRAP_DIR/work_functions" \
        "$BOOTSTRAP_DIR/ubuntu" "$BOOTSTRAP_DIR/work"; do
        if grep -Ev '^[[:space:]]*#' "$file" |
            sed 's/REPAIR=nvidia-container-toolkit//g' |
            grep -Fq 'nvidia-container-toolkit'; then
            fail "$file must not list nvidia-container-toolkit: the audit would expect it on machines without an NVIDIA GPU"
        fi
    done
}

test_the_work_uninstaller_offers_the_step() {
    local file="$BOOTSTRAP_DIR/uninstall_work"

    grep -Eq '^[[:space:]]*nvidia_ctk_uninstall[[:space:]]' "$file" ||
        fail "$file must offer nvidia_ctk_uninstall"
}

test_uninstall_tags_fit_the_checklist() {
    local file tag longest=0
    # whiptail sizes its tag column to the longest tag; more than 21 characters clip the
    # description of the longest rows ("asusctl & asusd (ASUS laptops; keeps /etc/asusd)").
    for file in "$BOOTSTRAP_DIR/uninstall_ubuntu" "$BOOTSTRAP_DIR/uninstall_work"; do
        while IFS= read -r tag; do
            [ "${#tag}" -le "$longest" ] || longest="${#tag}"
            [ "${#tag}" -le 21 ] || fail "$file: the checklist tag [$tag] is too long (21 characters at most)"
        done < <(awk '/^run_selected_uninstalls/{p=1; next} p && NF {print $1} p && !/\\$/{exit}' "$file")
    done
    [ "$longest" -gt 0 ] || fail 'no checklist tag was found'
}

for test_name in \
    test_a_vga_adapter_is_a_gpu \
    test_a_hybrid_laptop_counts_through_its_3d_controller \
    test_the_audio_function_of_a_card_alone_is_not_a_gpu \
    test_other_vendors_graphics_are_not_an_nvidia_gpu \
    test_a_missing_or_empty_pci_tree_means_no_gpu \
    test_a_device_without_readable_ids_is_ignored \
    test_machines_without_an_nvidia_gpu_are_skipped \
    test_a_first_install_writes_the_source_refreshes_apt_then_installs \
    test_the_architecture_comes_from_dpkg \
    test_a_second_run_only_asks_apt_to_install \
    test_a_source_disabled_by_a_release_upgrade_is_rewritten \
    test_a_legacy_one_line_source_is_retired \
    test_a_failed_key_download_installs_nothing \
    test_a_failed_apt_refresh_installs_nothing \
    test_a_failed_package_install_is_reported \
    test_dry_run_changes_nothing \
    test_the_docker_hint_follows_a_first_install_with_docker \
    test_no_docker_hint_without_docker \
    test_uninstall_removes_the_package_the_source_and_the_key \
    test_uninstall_cleans_up_even_when_apt_cannot_remove_the_package \
    test_the_repair_step_previews_the_changes \
    test_the_repair_step_skips_a_machine_without_an_nvidia_gpu \
    test_the_repair_usage_lists_the_step \
    test_work_runs_the_step_without_aborting_provisioning \
    test_profiles_do_not_declare_the_package_themselves \
    test_the_work_uninstaller_offers_the_step \
    test_uninstall_tags_fit_the_checklist; do
    # A subshell keeps each test's stub overrides and exported variables to itself.
    ( "$test_name" ) || {
        printf 'not ok - %s\n' "$test_name" >&2
        exit 1
    }
    printf 'ok - %s\n' "$test_name"
done

printf 'NVIDIA Container Toolkit tests passed.\n'
