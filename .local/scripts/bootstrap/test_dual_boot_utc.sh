#!/usr/bin/env bash
# shellcheck disable=SC2317
set -euo pipefail

script_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$script_dir/base_functions"

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

run_success_case() (
    local command_log
    command_log="$(mktemp)"
    trap 'rm -f "$command_log"' EXIT

    is_wsl() { return 1; }
    is_systemd_booted() { return 0; }
    timedatectl() {
        printf 'timedatectl %s\n' "$*" >> "$command_log"
        case "$*" in
            'show --property=NTPSynchronized --value') printf 'yes\n' ;;
            'show --property=LocalRTC --value') printf 'no\n' ;;
        esac
    }
    sudo() {
        printf 'sudo %s\n' "$*" >> "$command_log"
        "$@"
    }

    parse_bootstrap_args --dual-boot-utc
    configure_dual_boot_utc_rtc

    local expected
    expected="$(mktemp)"
    trap 'rm -f "$command_log" "$expected"' EXIT
    printf '%s\n' \
        'sudo timedatectl set-ntp true' \
        'timedatectl set-ntp true' \
        'timedatectl show --property=NTPSynchronized --value' \
        'sudo timedatectl set-local-rtc 0' \
        'timedatectl set-local-rtc 0' \
        'timedatectl show --property=LocalRTC --value' > "$expected"
    diff -u "$expected" "$command_log" || fail 'UTC RTC commands ran in the wrong order'
)

run_wsl_case() (
    is_wsl() { return 0; }
    is_systemd_booted() { return 0; }
    sudo() { fail 'sudo must not run under WSL'; }

    parse_bootstrap_args --dual-boot-utc
    if configure_dual_boot_utc_rtc 2>/dev/null; then
        fail 'WSL must reject --dual-boot-utc'
    fi
)

run_sync_timeout_case() (
    is_wsl() { return 1; }
    is_systemd_booted() { return 0; }
    timedatectl() {
        case "$*" in
            'show --property=NTPSynchronized --value') printf 'no\n' ;;
            'set-local-rtc 0') fail 'RTC must not change before NTP synchronizes' ;;
        esac
    }
    sudo() { "$@"; }
    sleep() { :; }

    DUAL_BOOT_NTP_ATTEMPTS=2
    parse_bootstrap_args --dual-boot-utc
    if configure_dual_boot_utc_rtc 2>/dev/null; then
        fail 'NTP timeout must abort the UTC RTC migration'
    fi
)

run_entrypoint_contract_case() {
    local entrypoint
    local parse_line
    local configure_line
    local update_line
    for entrypoint in ubuntu arch manjaro work; do
        grep -Fq 'parse_bootstrap_args "$@"' "$script_dir/$entrypoint" ||
            fail "$entrypoint does not parse bootstrap arguments"
        parse_line="$(grep -nF 'parse_bootstrap_args "$@"' "$script_dir/$entrypoint" | cut -d: -f1)"
        configure_line="$(grep -nF 'configure_dual_boot_utc_rtc' "$script_dir/$entrypoint" | cut -d: -f1)"
        update_line="$(grep -nF 'info "Updating system..."' "$script_dir/$entrypoint" | cut -d: -f1)"
        if (( parse_line >= configure_line || configure_line >= update_line )); then
            fail "$entrypoint must parse and configure UTC RTC before package updates"
        fi
    done
}

run_success_case
run_wsl_case
run_sync_timeout_case
run_entrypoint_contract_case
printf 'PASS: dual-boot UTC RTC helper\n'