#!/usr/bin/env bash
# Set the keyboard backlight of an ASUS laptop to a rainbow with asusctl. It acts only
# when asusctl is installed, asusd answers on D-Bus, and every keyboard lighting device
# that asusd exports lists the effect as supported. A machine without asusctl, without a
# lighting device, or whose devices lack the effect is skipped; asusctl without an
# answering asusd is a failure, because the daemon should be running. It needs no sudo:
# asusd accepts D-Bus calls from the adm, sudo, users, and wheel groups.
# Rationale, trade-offs, and recovery: docs/ASUSCTL.md
set -euo pipefail

# The effect that the bootstraps apply. Edit these three lines to change the machine's
# default, or pass --effect, --speed, and --direction for a single run.
EFFECT=rainbow-wave
SPEED=med
DIRECTION=right

# Test seams: the two programs and the waits can be replaced so the tests never touch
# the real keyboard. The retries cover asusd starting up right after it was installed.
readonly ASUSCTL_PROGRAM="${ASUSCTL_LIGHTING_ASUSCTL:-asusctl}"
readonly BUSCTL_PROGRAM="${ASUSCTL_LIGHTING_BUSCTL:-busctl}"
readonly WAIT_RETRIES="${ASUSCTL_LIGHTING_RETRIES:-15}"
readonly WAIT_INTERVAL="${ASUSCTL_LIGHTING_INTERVAL:-1}"

# asusd exports one xyz.ljones.Aura object per lighting device under DEVICE_PREFIX; the
# AniMe display sits beside them without that interface. LedModeData is
# (uu(yyy)(yyy)ss): mode, zone, two RGB colours, speed, direction. Both the mode numbers
# below and this layout come from the release pinned in asusctl_install.sh (AuraModeNum
# and AuraEffect in rog-aura/src/builtin_modes.rs): check them when bumping the pin.
readonly DAEMON_BUS_NAME='xyz.ljones.Asusd'
readonly DEVICE_PREFIX='/xyz/ljones/aura/'
readonly AURA_INTERFACE='xyz.ljones.Aura'
readonly MODE_DATA_SIGNATURE='(uu(yyy)(yyy)ss)'
# asusd may refuse to read the current effect for a moment after changing it.
readonly VERIFY_ATTEMPTS=3

DRY_RUN=false
DIRECTION_GIVEN=false
MODE_NUMBER=''
EFFECT_COMMAND=()
DEVICES=()

info() { printf '\033[1;34m[INFO]\033[0m  %s\n' "$*"; }
success() { printf '\033[1;32m[ OK ]\033[0m  %s\n' "$*"; }
warn() { printf '\033[1;33m[WARN]\033[0m  %s\n' "$*"; }
error() { printf '\033[1;31m[ERROR]\033[0m %s\n' "$*" >&2; }

## Announce a step that a dry run does not perform.
plan() { info "Dry run - would $*"; }

usage() {
    cat <<EOF
Usage: ${0##*/} [--effect EFFECT] [--speed SPEED] [--direction DIRECTION] [--dry-run]

Set the keyboard backlight to a rainbow through asusctl. Nothing happens unless asusctl
is installed and asusd reports that every keyboard lighting device supports the effect.

Options:
  --effect EFFECT        rainbow-wave sweeps the colours across the keys; rainbow-cycle
                         fades the whole keyboard through them (now: $EFFECT).
  --speed SPEED          low, med, or high (now: $SPEED).
  --direction DIRECTION  up, down, left, or right; rainbow-wave only (now: $DIRECTION).
  --dry-run              Show what would be done without changing the lighting.
  -h, --help             Show this help.

No sudo is needed. See docs/ASUSCTL.md for the design and the recovery steps.
EOF
}

## Report a command-line mistake and stop.
usage_error() {
    error "$1"
    usage >&2
    exit 2
}

## Succeed when $2 is an accepted value of the setting named $1.
valid_value() {
    case "$1:$2" in
        effect:rainbow-wave | effect:rainbow-cycle) ;;
        speed:low | speed:med | speed:high) ;;
        direction:up | direction:down | direction:left | direction:right) ;;
        *) return 1 ;;
    esac
}

## Print the AuraModeNum that asusd uses for an effect name.
mode_number() {
    case "$1" in
        rainbow-cycle) printf '2' ;;
        rainbow-wave) printf '3' ;;
    esac
}

parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            --dry-run) DRY_RUN=true ;;
            --effect | --speed | --direction)
                [[ $# -ge 2 ]] || usage_error "$1 needs a value."
                case "$1" in
                    --effect) EFFECT="$2" ;;
                    --speed) SPEED="$2" ;;
                    --direction)
                        DIRECTION="$2"
                        DIRECTION_GIVEN=true
                        ;;
                esac
                shift
                ;;
            -h | --help)
                usage
                exit 0
                ;;
            *) usage_error "Unknown option: $1" ;;
        esac
        shift
    done

    valid_value effect "$EFFECT" || usage_error "Invalid effect: $EFFECT (use rainbow-wave or rainbow-cycle)."
    valid_value speed "$SPEED" || usage_error "Invalid speed: $SPEED (use low, med, or high)."
    valid_value direction "$DIRECTION" || usage_error "Invalid direction: $DIRECTION (use up, down, left, or right)."
    if [[ "$EFFECT" == rainbow-cycle && "$DIRECTION_GIVEN" == true ]]; then
        usage_error "--direction only applies to rainbow-wave."
    fi

    MODE_NUMBER="$(mode_number "$EFFECT")"
}

## Fill EFFECT_COMMAND with the asusctl command that sets the requested effect.
build_effect_command() {
    EFFECT_COMMAND=("$ASUSCTL_PROGRAM" aura effect "$EFFECT")
    if [[ "$EFFECT" == rainbow-wave ]]; then
        EFFECT_COMMAND+=(--direction "$DIRECTION")
    fi
    EFFECT_COMMAND+=(--speed "$SPEED")
}

## Describe the requested effect for messages.
describe_effect() {
    if [[ "$EFFECT" == rainbow-wave ]]; then
        printf '%s (speed %s, direction %s)' "$EFFECT" "$SPEED" "$DIRECTION"
    else
        printf '%s (speed %s)' "$EFFECT" "$SPEED"
    fi
}

## Print an Aura property of a device as busctl shows it ("au 3 0 1 2", "u 3", ...);
## fail when the object has no such property, which is how a non-lighting object shows.
read_property() {
    "$BUSCTL_PROGRAM" --system get-property "$DAEMON_BUS_NAME" "$1" "$AURA_INTERFACE" "$2" 2>/dev/null
}

## Succeed while asusd owns its bus name and answers a request.
daemon_answers() {
    "$BUSCTL_PROGRAM" --system --list tree "$DAEMON_BUS_NAME" >/dev/null 2>&1
}

## Print the object path of every keyboard lighting device that asusd exports.
lighting_devices() {
    local object
    while IFS= read -r object; do
        [[ "$object" == "$DEVICE_PREFIX"* ]] || continue
        if read_property "$object" SupportedBasicModes >/dev/null; then
            printf '%s\n' "$object"
        fi
    done < <("$BUSCTL_PROGRAM" --system --list tree "$DAEMON_BUS_NAME" 2>/dev/null || true)
}

## Fill DEVICES, waiting for asusd to export its devices when it was only just started.
wait_for_devices() {
    local attempt
    for ((attempt = 0; attempt <= WAIT_RETRIES; attempt++)); do
        mapfile -t DEVICES < <(lighting_devices)
        if [[ "${#DEVICES[@]}" -gt 0 ]]; then
            return 0
        fi
        if [[ "$attempt" -lt "$WAIT_RETRIES" ]]; then
            sleep "$WAIT_INTERVAL"
        fi
    done
    return 1
}

## Succeed when the device lists the requested effect among its supported modes.
supports_effect() {
    local value mode
    local -a modes=()
    value="$(read_property "$1" SupportedBasicModes)" || return 1
    [[ "$value" == 'au '* ]] || return 1
    read -r -a modes <<<"${value#au }"
    # The first element is the number of modes that follow.
    for mode in "${modes[@]:1}"; do
        if [[ "$mode" == "$MODE_NUMBER" ]]; then
            return 0
        fi
    done
    return 1
}

## Succeed when the device already shows the requested effect. An unreadable or
## unfamiliar reply counts as "not applied", so it is never mistaken for success.
effect_applied() {
    local value speed direction
    local -a fields=()
    value="$(read_property "$1" LedModeData)" || return 1
    read -r -a fields <<<"$value"
    # signature mode zone r g b r g b "speed" "direction"
    [[ "${#fields[@]}" -eq 11 && "${fields[0]}" == "$MODE_DATA_SIGNATURE" ]] || return 1
    [[ "${fields[1]}" == "$MODE_NUMBER" ]] || return 1
    speed="${fields[9]//\"/}"
    direction="${fields[10]//\"/}"
    [[ "${speed,,}" == "$SPEED" ]] || return 1
    if [[ "$EFFECT" == rainbow-wave && "${direction,,}" != "$DIRECTION" ]]; then
        return 1
    fi
}

## Print the name of the first device that does not show the effect yet, if any.
first_pending_device() {
    local device
    for device in "${DEVICES[@]}"; do
        if ! effect_applied "$device"; then
            printf '%s' "${device#"$DEVICE_PREFIX"}"
            return 0
        fi
    done
    return 1
}

## Wait for every device to report the effect; print the first one that does not.
verify_effect() {
    local attempt pending=''
    for ((attempt = 1; attempt <= VERIFY_ATTEMPTS; attempt++)); do
        if ! pending="$(first_pending_device)"; then
            return 0
        fi
        if [[ "$attempt" -lt "$VERIFY_ATTEMPTS" ]]; then
            sleep "$WAIT_INTERVAL"
        fi
    done
    printf '%s' "$pending"
    return 1
}

main() {
    local device pending output
    local -a unsupported=()

    parse_args "$@"
    build_effect_command

    if ! command -v "$ASUSCTL_PROGRAM" >/dev/null 2>&1; then
        info "Skipping keyboard lighting: asusctl is not installed."
        return 0
    fi
    if ! command -v "$BUSCTL_PROGRAM" >/dev/null 2>&1; then
        error "busctl (part of systemd) is needed to read what the keyboard supports."
        return 1
    fi

    if ! wait_for_devices; then
        if ! daemon_answers; then
            error "asusd is not answering on D-Bus, so the keyboard lighting cannot be set. Check: systemctl status asusd.service"
            return 1
        fi
        info "Skipping keyboard lighting: asusd reports no keyboard lighting device."
        return 0
    fi

    # asusctl applies an effect to every lighting device and stops at the first error, so
    # an effect that only some of them support would be left half applied.
    for device in "${DEVICES[@]}"; do
        if ! supports_effect "$device"; then
            unsupported+=("${device#"$DEVICE_PREFIX"}")
        fi
    done
    if [[ "${#unsupported[@]}" -gt 0 ]]; then
        info "Skipping keyboard lighting: asusd lists no $EFFECT on ${unsupported[*]}, and asusctl applies an effect to every lighting device."
        return 0
    fi

    if ! first_pending_device >/dev/null; then
        success "Keyboard lighting is already $(describe_effect)."
        return 0
    fi
    if [[ "$DRY_RUN" == true ]]; then
        plan "run: asusctl ${EFFECT_COMMAND[*]:1}"
        return 0
    fi

    if ! output="$("${EFFECT_COMMAND[@]}" 2>&1)"; then
        error "asusctl could not set the keyboard lighting to $(describe_effect):"
        printf '%s\n' "$output" >&2
        return 1
    fi
    if ! pending="$(verify_effect)"; then
        error "asusctl accepted the command but ${pending} does not report $(describe_effect)."
        return 1
    fi
    success "Keyboard lighting set to $(describe_effect)."
}

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
    main "$@"
fi
