#!/usr/bin/env bash
# Apply kernel parameters intended to address systems that do not shut down
# cleanly. Supports Pop!_OS (kernelstub/systemd-boot) and GRUB installations.
set -euo pipefail

readonly EXPECTED_PARAMETERS=(
    'nvidia-drm.modeset=1'
    'acpi=force'
    'pcie_port_pm=off'
    'acpi_osi=Linux'
)
readonly GRUB_DEFAULT_FILE="${GRUB_DEFAULT_FILE:-/etc/default/grub}"
readonly GRUB_LINE_PATTERN='^GRUB_CMDLINE_LINUX_DEFAULT="(.*)"[[:space:]]*$'
readonly BOOTLOADER="${SHUTDOWN_FIX_BOOTLOADER:-auto}"

print_parameters() {
    local IFS=' '
    printf '%s\n' "${EXPECTED_PARAMETERS[*]}"
}

require_root() {
    if [[ "$(id -u)" -ne 0 ]]; then
        echo "This script must be run as root." >&2
        echo "Re-run it with: sudo $0" >&2
        exit 1
    fi
}

apply_kernelstub_fix() {
    local parameter
    local current_options

    echo "Configuring kernelstub parameters for Pop!_OS/systemd-boot..."
    current_options="$(kernelstub -p | tr '\n\t' '  ')"
    for parameter in "${EXPECTED_PARAMETERS[@]}"; do
        case " $current_options " in
            *" $parameter "*) echo "Kernel parameter already present: $parameter" ;;
            *)
                echo "Adding kernel parameter: $parameter"
                kernelstub -a "$parameter"
                ;;
        esac
    done

    # Match the existing GRUB behaviour: show shutdown messages instead of a splash screen.
    for parameter in quiet splash; do
        case " $current_options " in
            *" $parameter "*)
                echo "Removing kernel parameter: $parameter"
                kernelstub -d "$parameter"
                ;;
        esac
    done

    echo "Verification - Current kernelstub options:"
    kernelstub -p
}

grub_current_cmdline() {
    local line value='' found=false
    while IFS= read -r line || [[ -n "$line" ]]; do
        if [[ "$line" =~ $GRUB_LINE_PATTERN ]]; then
            value="${BASH_REMATCH[1]}"
            found=true
        elif [[ "$line" == GRUB_CMDLINE_LINUX_DEFAULT=* ]]; then
            found=false
            break
        fi
    done <"$GRUB_DEFAULT_FILE"
    if [[ "$found" != true ]]; then
        echo "$GRUB_DEFAULT_FILE has no plain double-quoted GRUB_CMDLINE_LINUX_DEFAULT=\"...\" line." >&2
        return 1
    fi
    printf '%s\n' "$value"
}

rewrite_grub_cmdline() {
    local current="$1" token parameter
    local -a words=() kept=()
    read -r -a words <<<"$current"
    for token in "${words[@]}"; do
        case "$token" in
            quiet | splash) continue ;;
        esac
        kept+=("$token")
    done
    for parameter in "${EXPECTED_PARAMETERS[@]}"; do
        case " ${kept[*]} " in
            *" $parameter "*) ;;
            *) kept+=("$parameter") ;;
        esac
    done
    printf '%s\n' "${kept[*]}"
}

apply_grub_fix() {
    local current rewritten line number=0 last=0 tmp_file
    current="$(grub_current_cmdline)"
    rewritten="$(rewrite_grub_cmdline "$current")"

    if [[ "$current" == "$rewritten" ]]; then
        echo "GRUB shutdown fix already applied, skipping."
        return
    fi

    echo "Backing up $GRUB_DEFAULT_FILE to ${GRUB_DEFAULT_FILE}.bak..."
    cp --backup=numbered "$GRUB_DEFAULT_FILE" "${GRUB_DEFAULT_FILE}.bak"
    echo "Configuring GRUB parameters..."
    echo "Setting GRUB_CMDLINE_LINUX_DEFAULT to '$rewritten'"

    while IFS= read -r line || [[ -n "$line" ]]; do
        number=$((number + 1))
        if [[ "$line" =~ $GRUB_LINE_PATTERN ]]; then
            last=$number
        fi
    done <"$GRUB_DEFAULT_FILE"

    tmp_file="$(mktemp)"
    number=0
    while IFS= read -r line || [[ -n "$line" ]]; do
        number=$((number + 1))
        if [[ "$number" -eq "$last" ]]; then
            printf 'GRUB_CMDLINE_LINUX_DEFAULT="%s"\n' "$rewritten"
        else
            printf '%s\n' "$line"
        fi
    done <"$GRUB_DEFAULT_FILE" >"$tmp_file"
    cat "$tmp_file" >"$GRUB_DEFAULT_FILE"
    rm -f "$tmp_file"

    echo "Verification - Current GRUB line:"
    grep '^GRUB_CMDLINE_LINUX_DEFAULT=' "$GRUB_DEFAULT_FILE"
    echo "Running update-grub to apply changes..."
    update-grub
}

detect_bootloader() {
    case "$BOOTLOADER" in
        kernelstub|grub)
            printf '%s\n' "$BOOTLOADER"
            ;;
        auto)
            if command -v kernelstub >/dev/null 2>&1 && [[ -e /etc/kernelstub/configuration ]]; then
                printf '%s\n' kernelstub
            elif [[ -f "$GRUB_DEFAULT_FILE" ]] && command -v update-grub >/dev/null 2>&1; then
                printf '%s\n' grub
            else
                echo "Unable to detect a supported bootloader." >&2
                echo "Expected Pop!_OS kernelstub or GRUB with update-grub." >&2
                exit 1
            fi
            ;;
        *)
            echo "SHUTDOWN_FIX_BOOTLOADER must be auto, kernelstub, or grub." >&2
            exit 2
            ;;
    esac
}

main() {
    local bootloader
    require_root
    bootloader="$(detect_bootloader)"

    echo "Starting shutdown fix application using $bootloader..."
    case "$bootloader" in
        kernelstub) apply_kernelstub_fix ;;
        grub) apply_grub_fix ;;
    esac

    echo "--------------------------------------------------------"
    echo "Fix applied successfully."
    echo "Configured parameters: $(print_parameters)"
    echo "NOTE: 'quiet splash' has been removed when present."
    echo "Please reboot your system to verify."
}

main "$@"
