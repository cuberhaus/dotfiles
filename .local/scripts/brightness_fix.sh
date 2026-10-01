#!/usr/bin/env bash
# Restore internal-panel brightness control on ASUS ROG laptops whose NVIDIA GPU
# drives the panel (GPU MUX "Ultimate" mode). The firmware still advertises its
# embedded-controller backlight (nvidia_wmi_ec_backlight), which accepts values
# but moves nothing. Adding acpi_backlight=native to the kernel command line makes
# the kernel skip it so the NVIDIA driver registers its own backlight (nvidia_0).
# Supports GRUB and Pop!_OS (kernelstub). Idempotent; reversible with --revert.
# Evidence, verification, and rollback: docs/ROG-BRIGHTNESS-DIAGNOSIS.md
set -euo pipefail

readonly PARAMETER_NAME='acpi_backlight'
readonly PARAMETER="${PARAMETER_NAME}=native"
readonly GRUB_VARIABLE='GRUB_CMDLINE_LINUX_DEFAULT'
readonly GRUB_LINE_PATTERN='^GRUB_CMDLINE_LINUX_DEFAULT="(.*)"[[:space:]]*$'
readonly NVIDIA_EC_BACKLIGHT_GUID='603E9613-EF25-4338-A3D0-C46177516DB7'
# DMI board names confirmed to expose a non-functional nvidia_wmi_ec_backlight.
# Add a board only after verifying it (see the diagnosis document), or pass --force.
readonly AFFECTED_BOARDS=('G635LX')

readonly GRUB_DEFAULT_FILE="${GRUB_DEFAULT_FILE:-/etc/default/grub}"
readonly BOOTLOADER="${BRIGHTNESS_FIX_BOOTLOADER:-auto}"
readonly DMI_DIR="${BRIGHTNESS_FIX_DMI_DIR:-/sys/class/dmi/id}"
readonly WMI_DEVICES_DIR="${BRIGHTNESS_FIX_WMI_DIR:-/sys/bus/wmi/devices}"
readonly BACKLIGHT_DIR="${BRIGHTNESS_FIX_BACKLIGHT_DIR:-/sys/class/backlight}"
readonly KERNEL_CMDLINE_FILE="${BRIGHTNESS_FIX_CMDLINE_FILE:-/proc/cmdline}"

MODE=apply
DRY_RUN=false
FORCE=false
CHANGED=false
RERUN_COMMAND=''
WORK_DIR=''
HAS_PARAMETER=false
CONFLICTING_PARAMETER=''
REWRITTEN_CMDLINE=''

info() { printf '\033[1;34m[INFO]\033[0m  %s\n' "$*"; }
success() { printf '\033[1;32m[ OK ]\033[0m  %s\n' "$*"; }
warn() { printf '\033[1;33m[WARN]\033[0m  %s\n' "$*"; }
error() { printf '\033[1;31m[ERROR]\033[0m %s\n' "$*" >&2; }

usage() {
    cat <<EOF
Usage: ${0##*/} [--dry-run] [--revert] [--force]
       ${0##*/} --status

Make the internal panel brightness controllable on ASUS ROG laptops whose NVIDIA
GPU drives the panel, by adding $PARAMETER to the kernel command line.

Options:
  --dry-run   Show the change without writing anything (no root needed).
  --revert    Remove $PARAMETER again.
  --force     Apply to a board that is not on the verified list, and replace a
              different $PARAMETER_NAME= value.
  --status    Report hardware, boot options, and backlight devices (read-only).
  -h, --help  Show this help.

Applying or reverting needs root and a reboot.
See docs/ROG-BRIGHTNESS-DIAGNOSIS.md for the evidence and the rollback steps.
EOF
}

set_mode() {
    if [[ "$MODE" != apply && "$MODE" != "$1" ]]; then
        error "Choose only one of --revert or --status."
        exit 2
    fi
    MODE="$1"
}

parse_args() {
    local argument
    for argument in "$@"; do
        case "$argument" in
            --dry-run) DRY_RUN=true ;;
            --revert) set_mode revert ;;
            --status) set_mode status ;;
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
    if [[ "$MODE" == status && ("$DRY_RUN" == true || "$FORCE" == true) ]]; then
        error "--status is read-only and cannot be combined with --dry-run or --force."
        exit 2
    fi
}

require_root() {
    if [[ "$(id -u)" -ne 0 ]]; then
        error "This script must be run as root."
        error "Re-run it with: $RERUN_COMMAND"
        exit 1
    fi
}

## Print the first line of a file, or nothing when it is unreadable.
read_value() {
    local value=''
    if [[ -r "$1" ]]; then
        IFS= read -r value <"$1" || true
    fi
    printf '%s' "$value"
}

## Succeed on a verified board that exposes the NVIDIA EC backlight interface.
is_affected_hardware() {
    local board candidate
    [[ -e "$WMI_DEVICES_DIR/$NVIDIA_EC_BACKLIGHT_GUID" ]] || return 1
    [[ "$(read_value "$DMI_DIR/board_vendor")" == ASUS* ]] || return 1
    board="$(read_value "$DMI_DIR/board_name")"
    for candidate in "${AFFECTED_BOARDS[@]}"; do
        [[ "$board" == "$candidate" ]] && return 0
    done
    return 1
}

## Record in HAS_PARAMETER and CONFLICTING_PARAMETER what the command line $1
## says about acpi_backlight: our value, or a different one.
scan_cmdline() {
    local token
    local -a words=()
    HAS_PARAMETER=false
    CONFLICTING_PARAMETER=''
    read -r -a words <<<"$1"
    for token in "${words[@]}"; do
        case "$token" in
            "$PARAMETER") HAS_PARAMETER=true ;;
            "$PARAMETER_NAME="*) CONFLICTING_PARAMETER="$token" ;;
        esac
    done
}

## Compute the command line for $MODE from $1 into REWRITTEN_CMDLINE.
## Return 0 when it changed, 1 when it is already in the requested state, and
## 2 when a different acpi_backlight value is present and --force was not given.
rewrite_cmdline() {
    local token
    local -a words=() kept=()

    scan_cmdline "$1"
    if [[ "$MODE" == revert ]]; then
        [[ "$HAS_PARAMETER" == true ]] || return 1
    else
        if [[ -n "$CONFLICTING_PARAMETER" && "$FORCE" != true ]]; then
            return 2
        fi
        if [[ "$HAS_PARAMETER" == true && -z "$CONFLICTING_PARAMETER" ]]; then
            return 1
        fi
    fi

    read -r -a words <<<"$1"
    for token in "${words[@]}"; do
        if [[ "$token" == "$PARAMETER" ]]; then
            continue
        elif [[ "$MODE" == apply && "$token" == "$PARAMETER_NAME="* ]]; then
            continue
        fi
        kept+=("$token")
    done
    if [[ "$MODE" == apply ]]; then
        kept+=("$PARAMETER")
    fi
    REWRITTEN_CMDLINE="${kept[*]}"
}

detect_bootloader() {
    case "$BOOTLOADER" in
        kernelstub | grub)
            printf '%s\n' "$BOOTLOADER"
            ;;
        auto)
            if command -v kernelstub >/dev/null 2>&1 && [[ -e /etc/kernelstub/configuration ]]; then
                printf '%s\n' kernelstub
            elif [[ -f "$GRUB_DEFAULT_FILE" ]] && command -v update-grub >/dev/null 2>&1; then
                printf '%s\n' grub
            else
                error "Unable to detect a supported bootloader (Pop!_OS kernelstub, or GRUB with update-grub)."
                return 1
            fi
            ;;
        *)
            error "BRIGHTNESS_FIX_BOOTLOADER must be auto, kernelstub, or grub."
            return 2
            ;;
    esac
}

## Print the value of the effective (last) GRUB_CMDLINE_LINUX_DEFAULT="..." line.
## Fail when the variable is missing or is not a plain double-quoted string.
grub_current_cmdline() {
    local line value='' found=false
    while IFS= read -r line || [[ -n "$line" ]]; do
        if [[ "$line" =~ $GRUB_LINE_PATTERN ]]; then
            value="${BASH_REMATCH[1]}"
            found=true
        elif [[ "$line" == "$GRUB_VARIABLE="* ]]; then
            found=false
            break
        fi
    done <"$GRUB_DEFAULT_FILE"
    if [[ "$found" != true ]]; then
        error "$GRUB_DEFAULT_FILE has no plain double-quoted $GRUB_VARIABLE=\"...\" line."
        error "Add $PARAMETER to it by hand, then run update-grub."
        return 1
    fi
    printf '%s\n' "$value"
}

## Print the options the bootloader passes to the next boot.
configured_cmdline() {
    case "$1" in
        grub) grub_current_cmdline ;;
        kernelstub) kernelstub -p | tr '\n\t' '  ' ;;
        *) return 1 ;;
    esac
}

## Print $GRUB_DEFAULT_FILE with its effective GRUB_CMDLINE_LINUX_DEFAULT set to $1.
grub_render() {
    local line number=0 last=0
    while IFS= read -r line || [[ -n "$line" ]]; do
        number=$((number + 1))
        if [[ "$line" =~ $GRUB_LINE_PATTERN ]]; then
            last=$number
        fi
    done <"$GRUB_DEFAULT_FILE"

    number=0
    while IFS= read -r line || [[ -n "$line" ]]; do
        number=$((number + 1))
        if [[ "$number" -eq "$last" ]]; then
            printf '%s="%s"\n' "$GRUB_VARIABLE" "$1"
        else
            printf '%s\n' "$line"
        fi
    done <"$GRUB_DEFAULT_FILE"
}

write_grub() {
    local candidate="$WORK_DIR/grub.new"
    local backup="${GRUB_DEFAULT_FILE}.bak"

    grub_render "$REWRITTEN_CMDLINE" >"$candidate"
    if [[ "$DRY_RUN" == true ]]; then
        info "Dry run - $GRUB_DEFAULT_FILE would change as follows (nothing is written):"
        diff -u --label "$GRUB_DEFAULT_FILE" --label "$GRUB_DEFAULT_FILE (proposed)" \
            "$GRUB_DEFAULT_FILE" "$candidate" || true
        info "Dry run - update-grub would then regenerate the GRUB configuration."
        return 0
    fi

    info "Backing up $GRUB_DEFAULT_FILE to $backup..."
    cp --backup=numbered "$GRUB_DEFAULT_FILE" "$backup"
    cat "$candidate" >"$GRUB_DEFAULT_FILE"
    info "Running update-grub..."
    if ! update-grub; then
        cat "$backup" >"$GRUB_DEFAULT_FILE"
        error "update-grub failed; restored $GRUB_DEFAULT_FILE from $backup."
        return 1
    fi
    CHANGED=true
}

## Run the command, or only print it during a dry run.
run_or_show() {
    if [[ "$DRY_RUN" == true ]]; then
        info "Dry run - would run: $*"
    else
        "$@"
    fi
}

write_kernelstub() {
    if [[ "$MODE" == revert ]]; then
        run_or_show kernelstub -d "$PARAMETER"
    else
        if [[ -n "$CONFLICTING_PARAMETER" ]]; then
            run_or_show kernelstub -d "$CONFLICTING_PARAMETER"
        fi
        run_or_show kernelstub -a "$PARAMETER"
    fi
    if [[ "$DRY_RUN" != true ]]; then
        CHANGED=true
    fi
}

## Say whether the running kernel already carries the parameter.
note_running_kernel() {
    scan_cmdline "$(read_value "$KERNEL_CMDLINE_FILE")"
    if [[ "$HAS_PARAMETER" == true ]]; then
        info "It is already active in the running kernel."
    else
        info "It is not active in the running kernel yet - reboot to apply it."
    fi
}

apply_change() {
    local bootloader="$1" current rc=0

    if ! current="$(configured_cmdline "$bootloader")"; then
        error "Cannot read the current $bootloader boot options."
        return 1
    fi
    rewrite_cmdline "$current" || rc=$?
    case "$rc" in
        0) ;;
        1)
            if [[ "$MODE" == revert ]]; then
                info "Nothing to revert: the boot options do not contain $PARAMETER."
            else
                success "Already configured: the boot options contain $PARAMETER."
                note_running_kernel
            fi
            return 0
            ;;
        *)
            error "The boot options already set a different value: $CONFLICTING_PARAMETER"
            error "Remove it yourself, or re-run with --force to replace it with $PARAMETER."
            return 1
            ;;
    esac

    case "$bootloader" in
        grub) write_grub ;;
        kernelstub) write_kernelstub ;;
    esac
}

print_next_steps() {
    echo "--------------------------------------------------------"
    if [[ "$MODE" == revert ]]; then
        success "Removed $PARAMETER."
        info "Reboot to return to the firmware backlight (nvidia_wmi_ec_backlight)."
    else
        success "Added $PARAMETER."
        info "Reboot, then check the result with: $0 --status"
        info "Expected: the parameter is active and an nvidia_0 backlight device exists."
        info "If the built-in panel still ignores the brightness keys, undo with: sudo $0 --revert"
    fi
}

## Print one aligned "label value" line.
field() { printf '  %-25s %s\n' "$1" "$2"; }

status_report() {
    local bootloader='' configured boot_state='unknown' configured_active=false running_active
    local hardware device type native_device=false device_count=0

    if is_affected_hardware; then
        hardware='verified affected board'
    elif [[ -e "$WMI_DEVICES_DIR/$NVIDIA_EC_BACKLIGHT_GUID" ]]; then
        hardware='has the NVIDIA EC backlight but is not on the verified list'
    else
        hardware='no NVIDIA EC backlight (this fix does not apply)'
    fi
    info "Hardware and boot options"
    field 'Board:' "$(read_value "$DMI_DIR/board_vendor") $(read_value "$DMI_DIR/board_name") - $hardware"

    if bootloader="$(detect_bootloader 2>/dev/null)" &&
        configured="$(configured_cmdline "$bootloader" 2>/dev/null)"; then
        scan_cmdline "$configured"
        configured_active="$HAS_PARAMETER"
        if [[ -n "$CONFLICTING_PARAMETER" ]]; then
            boot_state="a different value is set: $CONFLICTING_PARAMETER"
        elif [[ "$HAS_PARAMETER" == true ]]; then
            boot_state="$PARAMETER is configured"
        else
            boot_state="$PARAMETER is not configured"
        fi
    fi
    field 'Bootloader:' "${bootloader:-not detected}"
    field 'Boot options:' "$boot_state"

    scan_cmdline "$(read_value "$KERNEL_CMDLINE_FILE")"
    running_active="$HAS_PARAMETER"
    if [[ "$running_active" == true ]]; then
        field 'Running kernel:' "$PARAMETER is active"
    else
        field 'Running kernel:' "$PARAMETER is not active"
    fi

    info "Backlight devices"
    for device in "$BACKLIGHT_DIR"/*; do
        [[ -d "$device" ]] || continue
        type="$(read_value "$device/type")"
        field "${device##*/}" "$type, brightness $(read_value "$device/brightness")/$(read_value "$device/max_brightness")"
        device_count=$((device_count + 1))
        if [[ "$type" != firmware ]]; then
            native_device=true
        fi
    done
    if [[ "$device_count" -eq 0 ]]; then
        field '(none)' 'no backlight device is registered'
    fi

    info "Verdict"
    if [[ "$running_active" == true && "$native_device" == true ]]; then
        success "$PARAMETER is active and a native backlight is registered. Test the brightness keys and the GNOME slider."
        info "If the panel still does not respond, undo with: sudo $0 --revert"
    elif [[ "$running_active" == true ]]; then
        warn "$PARAMETER is active but no native backlight appeared. Undo with: sudo $0 --revert"
    elif [[ "$configured_active" == true ]]; then
        warn "$PARAMETER is configured but not active yet. Reboot to apply it."
    elif is_affected_hardware; then
        warn "This board uses the non-functional firmware backlight. Fix it with: sudo $0"
    elif [[ -e "$WMI_DEVICES_DIR/$NVIDIA_EC_BACKLIGHT_GUID" ]]; then
        info "Unverified board: confirm the symptom in docs/ROG-BRIGHTNESS-DIAGNOSIS.md, then apply with --force."
    else
        info "Nothing to do on this machine."
    fi
}

main() {
    local bootloader
    RERUN_COMMAND="sudo $0${*:+ $*}"
    parse_args "$@"

    if [[ "$MODE" == status ]]; then
        status_report
        return 0
    fi

    if [[ "$MODE" == apply && "$FORCE" != true ]] && ! is_affected_hardware; then
        info "Skipping: this is not a verified ASUS ROG board with the NVIDIA EC backlight."
        info "Inspect it with '$0 --status', or pass --force to apply anyway."
        return 0
    fi

    if [[ "$DRY_RUN" != true ]]; then
        require_root
    fi
    bootloader="$(detect_bootloader)"
    WORK_DIR="$(mktemp -d)"
    trap 'rm -rf "$WORK_DIR"' EXIT

    info "Using $bootloader to ${MODE/apply/add} $PARAMETER."
    apply_change "$bootloader"
    if [[ "$CHANGED" == true ]]; then
        print_next_steps
    fi
}

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
    main "$@"
fi
