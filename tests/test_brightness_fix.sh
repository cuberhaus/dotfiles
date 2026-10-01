#!/usr/bin/env bash
# Hermetic tests for .local/scripts/brightness_fix.sh and its work-bootstrap wiring.
# Fake id, update-grub, and kernelstub binaries plus env-overridden sysfs/DMI
# directories keep every case off the real bootloader and hardware.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT="$REPO_ROOT/.local/scripts/brightness_fix.sh"
SHUTDOWN_SCRIPT="$REPO_ROOT/.local/scripts/permanent_shutdown_fix.sh"
WORK_FUNCTIONS="$REPO_ROOT/.local/scripts/bootstrap/work_functions"
ROOT_DIR="$(mktemp -d)"
trap 'rm -rf "$ROOT_DIR"' EXIT

readonly BASELINE_LINE='GRUB_CMDLINE_LINUX_DEFAULT="nvidia-drm.modeset=1 acpi=force pcie_port_pm=off acpi_osi=Linux"'
readonly NATIVE_LINE='GRUB_CMDLINE_LINUX_DEFAULT="nvidia-drm.modeset=1 acpi=force pcie_port_pm=off acpi_osi=Linux acpi_backlight=native"'
readonly EC_GUID='603E9613-EF25-4338-A3D0-C46177516DB7'

CASE=''
OUTPUT=''
STATUS=0

fail() {
    printf 'FAIL: %s\n' "$1" >&2
    shift
    if [[ $# -gt 0 ]]; then
        printf '%s\n' "$@" >&2
    fi
    exit 1
}

## Write a GRUB defaults file whose command-line variable line is $1. The commented
## and plain GRUB_CMDLINE_LINUX lines are decoys that must never be touched.
set_grub_line() {
    {
        printf '%s\n' '# If you change this file, run update-grub afterwards.' 'GRUB_DEFAULT=0'
        printf '%s\n' '#GRUB_CMDLINE_LINUX_DEFAULT="commented decoy"'
        printf '%s\n' "$1" 'GRUB_CMDLINE_LINUX=""'
    } >"$CASE/grub"
    cp "$CASE/grub" "$CASE/grub.original"
}

## Create a fresh fake machine in $CASE: a verified ROG board running GRUB.
new_case() {
    CASE="$ROOT_DIR/$1"
    mkdir -p "$CASE/bin" "$CASE/dmi" "$CASE/wmi" "$CASE/backlight"
    printf 'ASUSTeK COMPUTER INC.\n' >"$CASE/dmi/board_vendor"
    printf 'G635LX\n' >"$CASE/dmi/board_name"
    : >"$CASE/wmi/$EC_GUID"
    printf 'BOOT_IMAGE=/boot/vmlinuz root=UUID=x ro nvidia-drm.modeset=1\n' >"$CASE/cmdline"
    set_grub_line "$BASELINE_LINE"

    cat >"$CASE/bin/id" <<'EOF'
#!/usr/bin/env bash
if [[ "$1" == '-u' ]]; then
    printf '%s\n' "${FAKE_UID:-0}"
else
    command id "$@"
fi
EOF
    cat >"$CASE/bin/update-grub" <<'EOF'
#!/usr/bin/env bash
printf 'update-grub\n' >>"$FAKE_LOG_DIR/update-grub.log"
[[ "${FAKE_UPDATE_GRUB_FAIL:-false}" != true ]]
EOF
    cat >"$CASE/bin/kernelstub" <<'EOF'
#!/usr/bin/env bash
if [[ "$1" == '-p' ]]; then
    printf '%s\n' "${FAKE_KERNELSTUB_OPTIONS:-kernel options: quiet splash}"
else
    printf '%s\n' "$*" >>"$FAKE_LOG_DIR/kernelstub.log"
fi
EOF
    chmod +x "$CASE/bin/id" "$CASE/bin/update-grub" "$CASE/bin/kernelstub"
}

## Add a fake /sys/class/backlight device: name, type.
add_backlight() {
    mkdir -p "$CASE/backlight/$1"
    printf '%s\n' "$2" >"$CASE/backlight/$1/type"
    printf '50\n' >"$CASE/backlight/$1/brightness"
    printf '100\n' >"$CASE/backlight/$1/max_brightness"
}

## Run the script in $CASE with the given arguments; set OUTPUT and STATUS.
run_fix() {
    STATUS=0
    OUTPUT="$(
        env PATH="$CASE/bin:$PATH" \
            FAKE_LOG_DIR="$CASE" \
            GRUB_DEFAULT_FILE="$CASE/grub" \
            BRIGHTNESS_FIX_BOOTLOADER="${BOOTLOADER_UNDER_TEST:-grub}" \
            BRIGHTNESS_FIX_DMI_DIR="$CASE/dmi" \
            BRIGHTNESS_FIX_WMI_DIR="$CASE/wmi" \
            BRIGHTNESS_FIX_CMDLINE_FILE="$CASE/cmdline" \
            BRIGHTNESS_FIX_BACKLIGHT_DIR="$CASE/backlight" \
            bash "$SCRIPT" "$@" 2>&1
    )" || STATUS=$?
}

assert_status() {
    [[ "$STATUS" -eq "$1" ]] || fail "$2 (exit $STATUS, expected $1)" "$OUTPUT"
}

assert_output() {
    [[ "$OUTPUT" == *"$1"* ]] || fail "$2: output lacks '$1'" "$OUTPUT"
}

assert_grub_line() {
    grep -Fxq -- "$1" "$CASE/grub" ||
        fail "$2: expected '$1'" "$(grep '^GRUB_CMDLINE_LINUX_DEFAULT' "$CASE/grub" || true)"
}

assert_grub_untouched() {
    cmp -s "$CASE/grub" "$CASE/grub.original" || fail "$1: the GRUB file must not change" "$(cat "$CASE/grub")"
}

## Everything except the command-line variable must survive a rewrite verbatim.
assert_other_lines_untouched() {
    diff <(grep -v '^GRUB_CMDLINE_LINUX_DEFAULT=' "$CASE/grub.original") \
        <(grep -v '^GRUB_CMDLINE_LINUX_DEFAULT=' "$CASE/grub") >/dev/null ||
        fail "$1: lines other than GRUB_CMDLINE_LINUX_DEFAULT must be untouched"
}

log_lines() {
    if [[ -f "$CASE/$1.log" ]]; then wc -l <"$CASE/$1.log"; else echo 0; fi
}

assert_log_lines() {
    [[ "$(log_lines "$1")" -eq "$2" ]] ||
        fail "$3: expected $2 line(s) in $1.log" "$(cat "$CASE/$1.log" 2>/dev/null || echo '(no log)')"
}

test_apply_and_revert_round_trip() {
    new_case round-trip

    run_fix
    assert_status 0 'apply on a verified board'
    assert_grub_line "$NATIVE_LINE" 'apply must append the parameter after the existing ones'
    assert_other_lines_untouched 'apply'
    cmp -s "$CASE/grub.bak" "$CASE/grub.original" || fail 'apply must back up the original GRUB file'
    assert_log_lines update-grub 1 'apply must regenerate GRUB once'
    assert_output 'Reboot' 'apply must tell the user to reboot'

    cp "$CASE/grub" "$CASE/grub.applied"
    run_fix
    assert_status 0 'second apply'
    assert_output 'Already configured' 'second apply must be a no-op'
    cmp -s "$CASE/grub" "$CASE/grub.applied" || fail 'second apply must not change the file'
    assert_log_lines update-grub 1 'a no-op apply must not regenerate GRUB'

    run_fix --revert
    assert_status 0 'revert'
    assert_grub_untouched 'revert must restore the original line'
    assert_log_lines update-grub 2 'revert must regenerate GRUB'

    run_fix --revert
    assert_status 0 'second revert'
    assert_output 'Nothing to revert' 'second revert must be a no-op'
    assert_log_lines update-grub 2 'a no-op revert must not regenerate GRUB'
}

test_dry_run_changes_nothing() {
    new_case dry-run

    FAKE_UID=1000 run_fix --dry-run
    assert_status 0 'dry run works without root'
    assert_grub_untouched 'dry run'
    assert_log_lines update-grub 0 'dry run must not regenerate GRUB'
    [[ ! -e "$CASE/grub.bak" ]] || fail 'dry run must not create a backup'
    assert_output '+GRUB_CMDLINE_LINUX_DEFAULT="nvidia-drm.modeset=1 acpi=force pcie_port_pm=off acpi_osi=Linux acpi_backlight=native"' \
        'dry run must show the proposed line'
}

test_changes_require_root() {
    new_case root-required

    FAKE_UID=1000 run_fix
    assert_status 1 'apply without root'
    assert_output 'This script must be run as root.' 'apply without root'
    assert_output 'Re-run it with: sudo ' 'apply without root'
    assert_grub_untouched 'apply without root'

    FAKE_UID=1000 run_fix --revert
    assert_status 1 'revert without root'
    assert_grub_untouched 'revert without root'
}

test_empty_command_line() {
    new_case empty-line
    set_grub_line 'GRUB_CMDLINE_LINUX_DEFAULT=""'

    run_fix
    assert_status 0 'apply to an empty command line'
    assert_grub_line 'GRUB_CMDLINE_LINUX_DEFAULT="acpi_backlight=native"' 'empty line must not gain a leading space'

    run_fix --revert
    assert_grub_line 'GRUB_CMDLINE_LINUX_DEFAULT=""' 'revert must leave an empty command line'
}

test_conflicting_value_needs_force() {
    new_case conflict
    set_grub_line 'GRUB_CMDLINE_LINUX_DEFAULT="quiet acpi_backlight=video splash"'

    run_fix
    assert_status 1 'conflicting acpi_backlight value'
    assert_output 'acpi_backlight=video' 'conflict must name the existing value'
    assert_output '--force' 'conflict must explain how to override'
    assert_grub_untouched 'conflict without --force'
    assert_log_lines update-grub 0 'conflict must not regenerate GRUB'

    run_fix --revert
    assert_status 0 'revert with only a foreign value'
    assert_output 'Nothing to revert' 'a foreign value must not be removed by revert'
    assert_grub_untouched 'revert of a foreign value'

    run_fix --force
    assert_status 0 'forced replacement'
    assert_grub_line 'GRUB_CMDLINE_LINUX_DEFAULT="quiet splash acpi_backlight=native"' '--force must replace the foreign value'
}

test_unverified_hardware_is_skipped() {
    new_case unverified
    printf 'X123\n' >"$CASE/dmi/board_name"

    run_fix
    assert_status 0 'unverified board'
    assert_output 'Skipping' 'unverified board must be skipped'
    assert_grub_untouched 'unverified board'

    run_fix --force
    assert_status 0 'forced apply on an unverified board'
    assert_grub_line "$NATIVE_LINE" '--force must apply to an unverified board'

    printf 'Y456\n' >"$CASE/dmi/board_name"
    run_fix --revert
    assert_status 0 'revert on an unverified board'
    assert_grub_untouched 'revert must not be gated by the hardware check'

    new_case no-ec-interface
    rm "$CASE/wmi/$EC_GUID"
    run_fix
    assert_status 0 'board without the NVIDIA EC interface'
    assert_output 'Skipping' 'a board without the EC interface must be skipped'
    assert_grub_untouched 'board without the EC interface'
}

test_update_grub_failure_restores_the_file() {
    new_case update-grub-failure

    FAKE_UPDATE_GRUB_FAIL=true run_fix
    assert_status 1 'failing update-grub'
    assert_output 'restored' 'failing update-grub must report the restore'
    assert_grub_untouched 'failing update-grub'
}

test_unsupported_grub_formats_are_refused() {
    new_case single-quoted
    set_grub_line "GRUB_CMDLINE_LINUX_DEFAULT='quiet splash'"

    run_fix
    assert_status 1 'single-quoted GRUB line'
    assert_output 'plain double-quoted' 'unsupported format must explain itself'
    assert_grub_untouched 'single-quoted GRUB line'
}

test_kernelstub_path() {
    export BOOTLOADER_UNDER_TEST=kernelstub

    new_case kernelstub-add
    run_fix
    assert_status 0 'kernelstub apply'
    assert_log_lines kernelstub 1 'kernelstub apply'
    grep -qx -- '-a acpi_backlight=native' "$CASE/kernelstub.log" || fail 'kernelstub apply must add the parameter' "$(cat "$CASE/kernelstub.log")"
    assert_log_lines update-grub 0 'kernelstub must not call update-grub'

    new_case kernelstub-present
    FAKE_KERNELSTUB_OPTIONS='kernel options: quiet acpi_backlight=native' run_fix
    assert_output 'Already configured' 'kernelstub apply when present'
    assert_log_lines kernelstub 0 'kernelstub apply when present'

    new_case kernelstub-revert
    FAKE_KERNELSTUB_OPTIONS='kernel options: quiet acpi_backlight=native' run_fix --revert
    assert_status 0 'kernelstub revert'
    grep -qx -- '-d acpi_backlight=native' "$CASE/kernelstub.log" || fail 'kernelstub revert must delete the parameter'

    new_case kernelstub-conflict
    FAKE_KERNELSTUB_OPTIONS='kernel options: quiet acpi_backlight=video' run_fix
    assert_status 1 'kernelstub conflict'
    assert_log_lines kernelstub 0 'kernelstub conflict without --force'
    FAKE_KERNELSTUB_OPTIONS='kernel options: quiet acpi_backlight=video' run_fix --force
    assert_status 0 'kernelstub forced replacement'
    [[ "$(cat "$CASE/kernelstub.log")" == $'-d acpi_backlight=video\n-a acpi_backlight=native' ]] ||
        fail 'kernelstub --force must delete the foreign value before adding ours' "$(cat "$CASE/kernelstub.log")"

    new_case kernelstub-dry-run
    run_fix --dry-run
    assert_status 0 'kernelstub dry run'
    assert_output 'would run: kernelstub -a acpi_backlight=native' 'kernelstub dry run'
    assert_log_lines kernelstub 0 'kernelstub dry run'

    unset BOOTLOADER_UNDER_TEST
}

test_status_report() {
    new_case status-broken
    add_backlight nvidia_wmi_ec_backlight firmware
    run_fix --status
    assert_status 0 'status'
    assert_output 'verified affected board' 'status must recognise the board'
    assert_output 'non-functional firmware backlight' 'status must flag the ghost backlight'
    assert_grub_untouched 'status'

    new_case status-pending
    add_backlight nvidia_wmi_ec_backlight firmware
    set_grub_line "$NATIVE_LINE"
    run_fix --status
    assert_output 'configured but not active yet' 'status must report a pending reboot'

    new_case status-working
    add_backlight nvidia_0 raw
    set_grub_line "$NATIVE_LINE"
    printf 'ro acpi_backlight=native\n' >"$CASE/cmdline"
    run_fix --status
    assert_output 'a native backlight is registered' 'status must confirm the native backlight'

    new_case status-no-device
    add_backlight nvidia_wmi_ec_backlight firmware
    set_grub_line "$NATIVE_LINE"
    printf 'ro acpi_backlight=native\n' >"$CASE/cmdline"
    run_fix --status
    assert_output 'no native backlight appeared' 'status must flag a parameter without a native device'

    new_case status-other-machine
    rm "$CASE/wmi/$EC_GUID"
    run_fix --status
    assert_output 'Nothing to do on this machine' 'status on unrelated hardware'

    run_fix --status --dry-run
    assert_status 2 '--status with --dry-run'
    run_fix --revert --status
    assert_status 2 '--revert with --status'
    run_fix --bogus
    assert_status 2 'unknown option'
}

test_work_bootstrap_wiring() {
    local log="$ROOT_DIR/sudo.log"

    (
        # shellcheck source=/dev/null
        source "$WORK_FUNCTIONS"
        sudo() { printf '%s\n' "$*" >"$log"; }

        DOTFILES_ROOT="$ROOT_DIR/checkout" brightness_fix
        grep -qx "bash $ROOT_DIR/checkout/.local/scripts/brightness_fix.sh" "$log" ||
            fail 'brightness_fix must run the script from DOTFILES_ROOT'

        unset DOTFILES_ROOT
        HOME="$ROOT_DIR/home" brightness_fix
        grep -qx "bash $ROOT_DIR/home/.local/scripts/brightness_fix.sh" "$log" ||
            fail 'brightness_fix must fall back to HOME'
    )
}

## The two kernel-parameter fixes share one GRUB line, so they must coexist in both orders.
test_coexists_with_the_shutdown_fix() {
    local shutdown_output="$ROOT_DIR/shutdown.out"

    run_shutdown_fix() {
        env PATH="$CASE/bin:$PATH" FAKE_LOG_DIR="$CASE" GRUB_DEFAULT_FILE="$CASE/grub" \
            SHUTDOWN_FIX_BOOTLOADER=grub bash "$SHUTDOWN_SCRIPT" >"$shutdown_output" 2>&1
    }

    new_case coexist-existing
    set_grub_line "$NATIVE_LINE"
    run_shutdown_fix
    grep -q 'already applied, skipping' "$shutdown_output" ||
        fail 'the shutdown fix must recognise its parameters before our appended one' "$(cat "$shutdown_output")"
    assert_grub_untouched 'shutdown fix after the brightness fix'

    new_case coexist-fresh
    set_grub_line 'GRUB_CMDLINE_LINUX_DEFAULT="quiet splash"'
    run_shutdown_fix
    run_fix
    assert_status 0 'bootstrap order: shutdown fix, then brightness fix'
    assert_grub_line "$NATIVE_LINE" 'bootstrap order must keep both sets of parameters'
}

test_apply_and_revert_round_trip
test_dry_run_changes_nothing
test_changes_require_root
test_empty_command_line
test_conflicting_value_needs_force
test_unverified_hardware_is_skipped
test_update_grub_failure_restores_the_file
test_unsupported_grub_formats_are_refused
test_kernelstub_path
test_status_report
test_work_bootstrap_wiring
test_coexists_with_the_shutdown_fix

printf 'PASS: brightness_fix applies, reverts, gates, and coexists with the shutdown fix\n'
