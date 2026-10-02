#!/usr/bin/env bash
# Hermetic tests for .local/scripts/asusctl_lighting.sh and its bootstrap/repair wiring.
# A fake busctl serves asusd's Aura properties in the exact text that the real busctl
# printed on a ROG Strix SCAR 16 (G635LX), and a fake asusctl changes that state the way
# the real CLI does: it applies an effect to every lighting device and stops at the
# first refusal. Nothing touches the real keyboard, the real daemon, or the real asusctl.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT="$REPO_ROOT/.local/scripts/asusctl_lighting.sh"
BASE_FUNCTIONS="$REPO_ROOT/.local/scripts/bootstrap/base_functions"
REPAIR_SCRIPT="$REPO_ROOT/.local/scripts/repair-installation"
TEST_ROOT="$(mktemp -d)"
FAKE_BIN="$TEST_ROOT/fake-bin"
trap 'rm -rf "$TEST_ROOT"' EXIT

# The modes that the G635LX keyboard reports (AuraModeNum has no 9).
readonly G635LX_MODES='0 1 2 3 4 5 6 7 8 10 11 12'
readonly WAVE_COMMAND='aura effect rainbow-wave --direction right --speed med'

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

## Write the fake busctl and asusctl. Both keep their state in $FAKE_LOG_DIR, which each
## test case sets to its own directory.
write_fakes() {
    mkdir -p "$FAKE_BIN"
    cat >"$FAKE_BIN/busctl" <<'EOF'
#!/usr/bin/env bash
set -u
printf '%s\n' "$*" >>"$FAKE_LOG_DIR/busctl.log"
if [[ -e "$FAKE_LOG_DIR/asusd-down" ]]; then
    echo 'Failed to introspect object / of service xyz.ljones.Asusd: The name xyz.ljones.Asusd was not provided by any .service files' >&2
    exit 1
fi
case "$1 $2 $3" in
    '--system --list tree')
        count=0
        if [[ -f "$FAKE_LOG_DIR/tree.count" ]]; then count="$(<"$FAKE_LOG_DIR/tree.count")"; fi
        count=$((count + 1))
        echo "$count" >"$FAKE_LOG_DIR/tree.count"
        echo /xyz/ljones
        # A daemon that was only just started has not exported its devices yet.
        if ((count > ${FAKE_DEVICES_AFTER:-0})); then
            echo /xyz/ljones/aura
            for dir in "$FAKE_LOG_DIR"/aura/*/; do
                if [[ -d "$dir" ]]; then echo "/xyz/ljones/aura/$(basename "$dir")"; fi
            done
            echo /xyz/ljones/aura/anime
        fi
        ;;
    '--system get-property xyz.ljones.Asusd')
        object="$4" interface="$5" property="$6"
        dir="$FAKE_LOG_DIR/aura/${object#/xyz/ljones/aura/}"
        # Only lighting devices implement xyz.ljones.Aura; the AniMe display and the
        # parent object do not.
        if [[ "$interface" != xyz.ljones.Aura || "$object" != /xyz/ljones/aura/* || ! -d "$dir" ]]; then
            echo "Failed to get property $property on interface $interface: Unknown interface '$interface'" >&2
            exit 1
        fi
        case "$property" in
            SupportedBasicModes)
                read -r -a modes <"$dir/modes"
                echo "au ${#modes[@]} ${modes[*]}"
                ;;
            LedModeData)
                reads=0
                if [[ -f "$FAKE_LOG_DIR/mode-data.reads" ]]; then reads="$(<"$FAKE_LOG_DIR/mode-data.reads")"; fi
                reads=$((reads + 1))
                echo "$reads" >"$FAKE_LOG_DIR/mode-data.reads"
                # asusd reads this property with try_lock, so it can refuse for a moment.
                if ((reads <= ${FAKE_MODE_DATA_FAILS:-0})); then
                    echo "Failed to get property LedModeData on interface xyz.ljones.Aura: Aura control couldn't lock self" >&2
                    exit 1
                fi
                if [[ -n "${FAKE_OLD_LAYOUT:-}" ]]; then
                    echo '(uu(yyy)ss) 3 0 166 0 0 "Med" "Right"'
                    exit 0
                fi
                echo "(uu(yyy)(yyy)ss) $(<"$dir/mode") 0 $(<"$dir/colour") 0 0 0 \"$(<"$dir/speed")\" \"$(<"$dir/direction")\""
                ;;
            *) exit 1 ;;
        esac
        ;;
    *) exit 1 ;;
esac
EOF
    cat >"$FAKE_BIN/asusctl" <<'EOF'
#!/usr/bin/env bash
set -u
printf '%s\n' "$*" >>"$FAKE_LOG_DIR/asusctl.log"
if [[ -n "${FAKE_ASUSCTL_FAIL:-}" ]]; then
    echo "$FAKE_ASUSCTL_FAIL" >&2
    exit 1
fi
[[ "${1:-} ${2:-}" == 'aura effect' ]] || exit 0
effect="$3"
shift 3
speed='' direction=''
while [[ $# -gt 0 ]]; do
    case "$1" in
        --speed) speed="$2"; shift ;;
        --direction) direction="$2"; shift ;;
    esac
    shift
done
case "$effect" in
    rainbow-cycle) mode=2 ;;
    rainbow-wave) mode=3 ;;
    *) exit 1 ;;
esac
# The call is acknowledged but nothing changes.
if [[ -n "${FAKE_ASUSCTL_NOOP:-}" ]]; then exit 0; fi
# Like the real CLI: every lighting device in path order, stopping at the first refusal.
for dir in "$FAKE_LOG_DIR"/aura/*/; do
    [[ -d "$dir" ]] || continue
    if [[ -n "${FAKE_ASUSCTL_ONLY:-}" && "$(basename "$dir")" != "$FAKE_ASUSCTL_ONLY" ]]; then continue; fi
    read -r -a modes <"$dir/modes"
    if [[ " ${modes[*]} " != *" $mode "* ]]; then
        echo 'Error: The Aura effect is not supported' >&2
        exit 1
    fi
    echo "$mode" >"$dir/mode"
    # The CLI leaves the first colour at its default red, and rainbow-cycle has no
    # direction, so the daemon keeps the default one.
    echo '166 0 0' >"$dir/colour"
    echo "${speed^}" >"$dir/speed"
    direction="${direction:-right}"
    echo "${direction^}" >"$dir/direction"
done
EOF
    chmod +x "$FAKE_BIN/busctl" "$FAKE_BIN/asusctl"
}

## Add the lighting device $1 that reports the modes $2 and starts on static red.
add_device() {
    local dir="$CASE/aura/$1"
    mkdir -p "$dir"
    printf '%s\n' "$2" >"$dir/modes"
    printf '0\n' >"$dir/mode"
    printf '166 0 0\n' >"$dir/colour"
    printf 'Med\n' >"$dir/speed"
    printf 'Right\n' >"$dir/direction"
}

## A fresh machine in $CASE: asusd is running and exports the keyboard of a G635LX
## next to its AniMe display, which is not a lighting device.
new_case() {
    CASE="$TEST_ROOT/$1"
    mkdir -p "$CASE/aura"
    add_device 19b6_3_4 "$G635LX_MODES"
}

## Run the script in $CASE with the given arguments; set OUTPUT and STATUS. LIGHTING_*
## variables set in front of the call replace the fakes and the wait.
run_lighting() {
    STATUS=0
    OUTPUT="$(
        env FAKE_LOG_DIR="$CASE" \
            ASUSCTL_LIGHTING_ASUSCTL="${LIGHTING_ASUSCTL:-$FAKE_BIN/asusctl}" \
            ASUSCTL_LIGHTING_BUSCTL="${LIGHTING_BUSCTL:-$FAKE_BIN/busctl}" \
            ASUSCTL_LIGHTING_RETRIES="${LIGHTING_RETRIES:-0}" \
            ASUSCTL_LIGHTING_INTERVAL=0 \
            bash "$SCRIPT" "$@" 2>&1
    )" || STATUS=$?
}

assert_status() {
    [[ "$STATUS" -eq "$1" ]] || fail "$2 (exit $STATUS, expected $1)" "$OUTPUT"
}

assert_output() {
    [[ "$OUTPUT" == *"$1"* ]] || fail "$2: output lacks '$1'" "$OUTPUT"
}

refute_output() {
    [[ "$OUTPUT" != *"$1"* ]] || fail "$2: output must not contain '$1'" "$OUTPUT"
}

## Assert that the asusctl calls of this case are exactly $1, one per line.
assert_asusctl_calls() {
    local actual=''
    if [[ -f "$CASE/asusctl.log" ]]; then
        actual="$(<"$CASE/asusctl.log")"
    fi
    [[ "$actual" == "$1" ]] ||
        fail "$2: unexpected asusctl calls" "expected: ${1:-(none)}" "actual:   ${actual:-(none)}" "$OUTPUT"
}

## Print "mode speed direction" of a device, for example "3 Med Right".
device_state() {
    printf '%s %s %s' "$(<"$CASE/aura/$1/mode")" "$(<"$CASE/aura/$1/speed")" "$(<"$CASE/aura/$1/direction")"
}

assert_state() {
    [[ "$(device_state "$1")" == "$2" ]] || fail "$3: $1 is '$(device_state "$1")', expected '$2'" "$OUTPUT"
}

assert_nothing_queried() {
    [[ ! -e "$CASE/busctl.log" && ! -e "$CASE/asusctl.log" ]] ||
        fail "$1: nothing may be queried or changed" "$(cat "$CASE/busctl.log" "$CASE/asusctl.log" 2>/dev/null)"
}

test_skipped_without_the_tools() {
    new_case no-asusctl
    LIGHTING_ASUSCTL="$CASE/missing/asusctl" run_lighting
    assert_status 0 'asusctl is not installed'
    assert_output 'Skipping keyboard lighting: asusctl is not installed.' 'asusctl is not installed'
    assert_nothing_queried 'asusctl is not installed'

    new_case no-busctl
    LIGHTING_BUSCTL="$CASE/missing/busctl" run_lighting
    assert_status 1 'busctl is missing'
    assert_output 'busctl (part of systemd) is needed' 'busctl is missing'
    assert_asusctl_calls '' 'busctl is missing'
}

test_daemon_problems() {
    new_case daemon-down
    touch "$CASE/asusd-down"
    run_lighting
    assert_status 1 'asusd is not running'
    assert_output 'asusd is not answering on D-Bus' 'asusd is not running'
    assert_output 'systemctl status asusd.service' 'the message names the check to run'
    assert_asusctl_calls '' 'asusd is not running'

    # The AniMe display is exported beside the lighting devices but is not one.
    new_case no-lighting-device
    rm -r "$CASE/aura/19b6_3_4"
    run_lighting
    assert_status 0 'asusd exports no lighting device'
    assert_output 'asusd reports no keyboard lighting device' 'asusd exports no lighting device'
    assert_asusctl_calls '' 'asusd exports no lighting device'
}

test_waits_for_a_daemon_that_just_started() {
    new_case slow-start
    FAKE_DEVICES_AFTER=3 LIGHTING_RETRIES=5 run_lighting
    assert_status 0 'the device appears on the fourth poll'
    assert_output 'Keyboard lighting set to rainbow-wave' 'the device appears on the fourth poll'
    [[ "$(<"$CASE/tree.count")" -eq 4 ]] ||
        fail 'polling must stop as soon as the device appears' "tree requests: $(<"$CASE/tree.count")"

    # One try plus the retries, then one request that tells "no device" from "no daemon".
    new_case slow-start-gives-up
    FAKE_DEVICES_AFTER=9 LIGHTING_RETRIES=1 run_lighting
    assert_status 0 'the device never appears'
    assert_output 'asusd reports no keyboard lighting device' 'the device never appears'
    [[ "$(<"$CASE/tree.count")" -eq 3 ]] ||
        fail 'the wait must be bounded by the retries' "tree requests: $(<"$CASE/tree.count")"
    assert_asusctl_calls '' 'the device never appears'
}

test_unsupported_effects_are_skipped() {
    new_case unsupported-wave
    printf '0 1 2 4\n' >"$CASE/aura/19b6_3_4/modes"
    run_lighting
    assert_status 0 'rainbow-wave is not supported'
    assert_output 'asusd lists no rainbow-wave on 19b6_3_4' 'rainbow-wave is not supported'
    assert_asusctl_calls '' 'rainbow-wave is not supported'
    assert_state 19b6_3_4 '0 Med Right' 'rainbow-wave is not supported'
    run_lighting --effect rainbow-cycle
    assert_asusctl_calls 'aura effect rainbow-cycle --speed med' 'rainbow-cycle is still supported'

    new_case unsupported-both
    printf '0 1 4\n' >"$CASE/aura/19b6_3_4/modes"
    run_lighting
    assert_output 'asusd lists no rainbow-wave' 'no rainbow effect at all'
    run_lighting --effect rainbow-cycle
    assert_status 0 'no rainbow effect at all'
    assert_output 'asusd lists no rainbow-cycle' 'no rainbow effect at all'
    assert_asusctl_calls '' 'no rainbow effect at all'

    # asusctl applies an effect to every device and stops at the first refusal, so it
    # must not run when any device lacks the effect.
    new_case mixed-devices
    add_device 1866_3_5 '0 1 4'
    run_lighting
    assert_status 0 'one of two devices lacks the effect'
    assert_output 'asusd lists no rainbow-wave on 1866_3_5' 'one of two devices lacks the effect'
    refute_output '19b6_3_4' 'the supported device is not blamed'
    assert_asusctl_calls '' 'one of two devices lacks the effect'
    assert_state 19b6_3_4 '0 Med Right' 'one of two devices lacks the effect'

    # Devices are listed in path order, so 1866_3_5 above comes before the keyboard.
    # The refusal must be noticed just as well when the device sorts after it.
    new_case mixed-devices-last
    add_device 1c1c_3_6 '0 1 4'
    run_lighting
    assert_status 0 'the last of two devices lacks the effect'
    assert_output 'asusd lists no rainbow-wave on 1c1c_3_6' 'the last of two devices lacks the effect'
    refute_output '19b6_3_4' 'the supported device is not blamed'
    assert_asusctl_calls '' 'the last of two devices lacks the effect'
    assert_state 19b6_3_4 '0 Med Right' 'the last of two devices lacks the effect'
}

test_applies_and_stays_applied() {
    new_case apply
    run_lighting
    assert_status 0 'first run'
    assert_output 'Keyboard lighting set to rainbow-wave (speed med, direction right).' 'first run'
    assert_asusctl_calls "$WAVE_COMMAND" 'first run'
    assert_state 19b6_3_4 '3 Med Right' 'first run'

    run_lighting
    assert_status 0 'second run'
    assert_output 'Keyboard lighting is already rainbow-wave (speed med, direction right).' 'second run'
    assert_asusctl_calls "$WAVE_COMMAND" 'a second run must not call asusctl again'

    new_case two-devices
    add_device 1866_3_5 "$G635LX_MODES"
    run_lighting
    assert_status 0 'two supporting devices'
    assert_asusctl_calls "$WAVE_COMMAND" 'two supporting devices share one call'
    assert_state 19b6_3_4 '3 Med Right' 'two supporting devices'
    assert_state 1866_3_5 '3 Med Right' 'two supporting devices'
}

test_changes_made_by_hand_are_reapplied() {
    local expected
    new_case drift
    run_lighting
    printf 'Low\n' >"$CASE/aura/19b6_3_4/speed"
    run_lighting
    assert_status 0 'speed changed by hand'
    assert_output 'set to rainbow-wave' 'speed changed by hand'
    assert_state 19b6_3_4 '3 Med Right' 'speed changed by hand'

    printf 'Left\n' >"$CASE/aura/19b6_3_4/direction"
    run_lighting
    assert_state 19b6_3_4 '3 Med Right' 'direction changed by hand'

    printf '0\n' >"$CASE/aura/19b6_3_4/mode"
    run_lighting
    assert_state 19b6_3_4 '3 Med Right' 'another effect picked by hand'

    expected="$(printf '%s\n%s\n%s\n%s' "$WAVE_COMMAND" "$WAVE_COMMAND" "$WAVE_COMMAND" "$WAVE_COMMAND")"
    assert_asusctl_calls "$expected" 'one call for the first run and one for each change'
}

test_options() {
    new_case cycle
    run_lighting --effect rainbow-cycle --speed high
    assert_status 0 'rainbow-cycle'
    assert_output 'set to rainbow-cycle (speed high).' 'rainbow-cycle'
    assert_asusctl_calls 'aura effect rainbow-cycle --speed high' 'rainbow-cycle has no direction'
    assert_state 19b6_3_4 '2 High Right' 'rainbow-cycle'
    # The direction is not part of rainbow-cycle, so a different one still counts as applied.
    printf 'Left\n' >"$CASE/aura/19b6_3_4/direction"
    run_lighting --effect rainbow-cycle --speed high
    assert_output 'already rainbow-cycle (speed high)' 'rainbow-cycle ignores the direction'
    assert_asusctl_calls 'aura effect rainbow-cycle --speed high' 'rainbow-cycle ignores the direction'

    new_case direction
    run_lighting --direction up --speed low
    assert_status 0 'direction and speed'
    assert_asusctl_calls 'aura effect rainbow-wave --direction up --speed low' 'direction and speed'
    assert_state 19b6_3_4 '3 Low Up' 'direction and speed'
}

test_dry_run_changes_nothing() {
    new_case dry-run
    run_lighting --dry-run
    assert_status 0 'dry run'
    assert_output "Dry run - would run: asusctl $WAVE_COMMAND" 'dry run'
    assert_asusctl_calls '' 'dry run'
    assert_state 19b6_3_4 '0 Med Right' 'dry run'

    run_lighting
    run_lighting --dry-run
    assert_output 'already rainbow-wave' 'dry run on an applied keyboard'
    refute_output 'Dry run' 'dry run on an applied keyboard'
}

test_failures_are_reported() {
    new_case command-fails
    FAKE_ASUSCTL_FAIL='Error: Permission denied' run_lighting
    assert_status 1 'asusctl fails'
    assert_output 'asusctl could not set the keyboard lighting to rainbow-wave (speed med, direction right):' 'asusctl fails'
    assert_output 'Permission denied' 'the asusctl message is shown'
    # The failure itself is the report; the check that follows must not add a second one.
    refute_output 'accepted the command' 'asusctl fails'
    assert_state 19b6_3_4 '0 Med Right' 'asusctl fails'

    new_case change-ignored
    FAKE_ASUSCTL_NOOP=1 run_lighting
    assert_status 1 'the daemon ignores the call'
    assert_output '19b6_3_4 does not report rainbow-wave (speed med, direction right)' 'the daemon ignores the call'

    new_case one-device-unchanged
    add_device 1866_3_5 "$G635LX_MODES"
    FAKE_ASUSCTL_ONLY=19b6_3_4 run_lighting
    assert_status 1 'the second device keeps its effect'
    assert_output '1866_3_5 does not report rainbow-wave' 'the second device keeps its effect'

    # Every device is verified, not only the first one in path order.
    new_case last-device-unchanged
    add_device 1c1c_3_6 "$G635LX_MODES"
    FAKE_ASUSCTL_ONLY=19b6_3_4 run_lighting
    assert_status 1 'the last device keeps its effect'
    assert_output '1c1c_3_6 does not report rainbow-wave' 'the last device keeps its effect'
    refute_output '19b6_3_4 does not report' 'the changed device is not blamed'

    # An unfamiliar LedModeData layout must never be mistaken for success.
    new_case unfamiliar-layout
    FAKE_OLD_LAYOUT=1 run_lighting
    assert_status 1 'unfamiliar property layout'
    assert_output 'does not report rainbow-wave' 'unfamiliar property layout'
    assert_asusctl_calls "$WAVE_COMMAND" 'unfamiliar property layout is applied once'
}

test_verification_tolerates_a_busy_daemon() {
    # Read 1 is the check before the change, read 2 the first check after it.
    new_case busy-daemon
    FAKE_MODE_DATA_FAILS=2 run_lighting
    assert_status 0 'two refused reads'
    assert_output 'Keyboard lighting set to rainbow-wave' 'two refused reads'

    new_case stuck-daemon
    FAKE_MODE_DATA_FAILS=4 run_lighting
    assert_status 1 'reads refused for good'
    assert_output '19b6_3_4 does not report rainbow-wave' 'reads refused for good'
}

assert_usage_error() {
    local message="$1"
    shift
    new_case "usage-$RANDOM"
    run_lighting "$@"
    assert_status 2 "usage error for: $*"
    assert_output "$message" "usage error for: $*"
    assert_output 'Usage: asusctl_lighting.sh' "usage error for: $*"
    assert_nothing_queried "usage error for: $*"
}

test_usage() {
    assert_usage_error 'Unknown option: --bogus' --bogus
    assert_usage_error '--effect needs a value.' --effect
    assert_usage_error '--speed needs a value.' --speed
    assert_usage_error '--direction needs a value.' --direction
    assert_usage_error 'Invalid effect: stars' --effect stars
    assert_usage_error 'Invalid speed: fast' --speed fast
    assert_usage_error 'Invalid direction: sideways' --direction sideways
    assert_usage_error '--direction only applies to rainbow-wave.' --effect rainbow-cycle --direction up

    new_case help
    run_lighting --help
    assert_status 0 '--help'
    assert_output '--effect EFFECT' '--help'
    assert_output '(now: rainbow-wave)' '--help'
    assert_nothing_queried '--help'
}

write_recording_script() {
    cat >"$1" <<'EOF'
#!/usr/bin/env bash
printf '%s args=%s\n' "$0" "$*" >>"$WRAPPER_LOG"
EOF
}

test_bootstrap_and_repair_wiring() {
    local checkout="$TEST_ROOT/checkout" home="$TEST_ROOT/wrapper-home" repo="$TEST_ROOT/repair-repo"
    local log="$TEST_ROOT/wrapper.log" usage_text profile file install_line lighting_line

    mkdir -p "$checkout/.local/scripts" "$home/.local/scripts" "$repo/.local/scripts"
    write_recording_script "$checkout/.local/scripts/asusctl_lighting.sh"
    write_recording_script "$home/.local/scripts/asusctl_lighting.sh"
    write_recording_script "$repo/.local/scripts/asusctl_lighting.sh"
    write_recording_script "$repo/.local/scripts/asusctl_install.sh"
    cp "$REPAIR_SCRIPT" "$repo/.local/scripts/repair-installation"
    : >"$log"

    (
        # shellcheck source=/dev/null
        source "$BASE_FUNCTIONS"
        export WRAPPER_LOG="$log"
        unset DOTFILES_ROOT
        DOTFILES_ROOT="$checkout" asusctl_lighting
        HOME="$home" asusctl_lighting
    ) || fail 'asusctl_lighting must run the script'
    [[ "$(<"$log")" == "$checkout/.local/scripts/asusctl_lighting.sh args="$'\n'"$home/.local/scripts/asusctl_lighting.sh args=" ]] ||
        fail 'asusctl_lighting must run the script of the checkout and fall back to HOME' "$(<"$log")"

    # A failing script must reach the bootstrap, which warns and carries on.
    printf '#!/usr/bin/env bash\nexit 1\n' >"$checkout/.local/scripts/asusctl_lighting.sh"
    if (
        # shellcheck source=/dev/null
        source "$BASE_FUNCTIONS"
        DOTFILES_ROOT="$checkout" asusctl_lighting
    ); then
        fail 'asusctl_lighting must report a failing script'
    fi

    : >"$log"
    WRAPPER_LOG="$log" DRY_RUN=true bash "$repo/.local/scripts/repair-installation" asusctl-lighting auto
    WRAPPER_LOG="$log" bash "$repo/.local/scripts/repair-installation" asusctl-lighting auto
    [[ "$(<"$log")" == "$repo/.local/scripts/asusctl_lighting.sh args=--dry-run"$'\n'"$repo/.local/scripts/asusctl_lighting.sh args=" ]] ||
        fail 'make repair REPAIR=asusctl-lighting must run only the lighting script and honour DRY_RUN' "$(<"$log")"
    usage_text="$(bash "$repo/.local/scripts/repair-installation" bogus 2>&1 || true)"
    [[ "$usage_text" == *'asusctl|asusctl-lighting'* ]] ||
        fail 'the repair usage text must list asusctl-lighting' "$usage_text"

    for profile in work ubuntu; do
        file="$REPO_ROOT/.local/scripts/bootstrap/$profile"
        install_line="$(grep -n '^ *asusctl_install ||' "$file" | cut -d: -f1)"
        lighting_line="$(grep -n '^ *asusctl_lighting ||' "$file" | cut -d: -f1)"
        [[ -n "$install_line" && -n "$lighting_line" && "$lighting_line" -gt "$install_line" ]] ||
            fail "the $profile bootstrap must set the keyboard lighting after it installs asusctl, without aborting on failure"
        grep -Fq 'make repair REPAIR=asusctl-lighting' "$file" ||
            fail "the $profile bootstrap must name the repair command"
    done
    grep -Fq 'bash tests/test_asusctl_lighting.sh' "$REPO_ROOT/Makefile" ||
        fail 'make test must run this suite'
}

write_fakes

test_skipped_without_the_tools
test_daemon_problems
test_waits_for_a_daemon_that_just_started
test_unsupported_effects_are_skipped
test_applies_and_stays_applied
test_changes_made_by_hand_are_reapplied
test_options
test_dry_run_changes_nothing
test_failures_are_reported
test_verification_tolerates_a_busy_daemon
test_usage
test_bootstrap_and_repair_wiring

printf 'PASS: asusctl_lighting gates on asusctl and the keyboard, applies once, verifies, and is wired into the bootstraps\n'
