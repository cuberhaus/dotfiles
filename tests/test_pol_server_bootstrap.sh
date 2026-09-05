#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BOOTSTRAP="$REPO_ROOT/server/pol-server/bootstrap"
INSTALLER="$REPO_ROOT/server/pol-server/install-autonomy"
HARDWARE="$REPO_ROOT/server/pol-server/hardware-qualification"
MAINTENANCE="$REPO_ROOT/server/pol-server/maintenance-access"
CASE_DIR="$(mktemp -d)"
FAKE_ROOT="$CASE_DIR/root"
FAKE_BIN="$CASE_DIR/bin"
EVENT_LOG="$CASE_DIR/events.log"
PACKAGE_STATE="$CASE_DIR/packages"
SYSTEMD_ENABLED="$CASE_DIR/systemd-enabled"
SYSTEMD_ACTIVE="$CASE_DIR/systemd-active"
SYSTEMD_MASKED="$CASE_DIR/systemd-masked"
ORIGINAL_PATH="$PATH"
trap 'rm -rf "$CASE_DIR"' EXIT

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

assert_file_contains() {
    local file="$1"
    local expected="$2"
    grep -Fq -- "$expected" "$file" || fail "Expected $file to contain: $expected"
}

[ -x "$BOOTSTRAP" ] || fail 'pol-server bootstrap must exist and be executable'
[ -x "$HARDWARE" ] || fail 'pol-server hardware qualification command must exist and be executable'
[ -x "$MAINTENANCE" ] || fail 'pol-server maintenance access command must exist and be executable'
[ -x "$REPO_ROOT/server/pol-server/deploy" ] || fail 'pol-server deploy must exist and be executable'
[ -x "$INSTALLER" ] || fail 'pol-server autonomy installer must exist and be executable'
grep -Fq 'bootstrap-pol-server:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose bootstrap-pol-server'
grep -Fq 'audit-pol-server:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose audit-pol-server'
grep -Fq 'enroll-pol-server:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose enroll-pol-server'
grep -Fq 'enroll-pol-server-maintenance:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose temporary maintenance enrollment'
grep -Fq 'revoke-pol-server-maintenance:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose maintenance revocation'
grep -Fq 'audit-pol-server-hardware:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose audit-pol-server-hardware'
grep -Fq 'start-pol-server-smart-long-kingston:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose the Kingston SMART test'
grep -Fq 'start-pol-server-smart-long-micron:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose the Micron SMART test'

mkdir -p "$FAKE_ROOT/etc" "$FAKE_BIN"
: > "$EVENT_LOG"
: > "$PACKAGE_STATE"
: > "$SYSTEMD_ENABLED"
: > "$SYSTEMD_ACTIVE"
: > "$SYSTEMD_MASKED"

cat > "$FAKE_ROOT/etc/os-release" <<'EOF'
ID=debian
VERSION_ID="13"
EOF

cat > "$FAKE_BIN/dpkg-query" <<'EOF'
#!/usr/bin/env bash
package="${@: -1}"
if grep -Fxq -- "$package" "$PACKAGE_STATE"; then
    printf 'installed\n'
else
    exit 1
fi
EOF

cat > "$FAKE_BIN/apt-get" <<'EOF'
#!/usr/bin/env bash
printf 'apt-get:%s\n' "$*" >> "$EVENT_LOG"
if [ "${1:-}" = install ]; then
    shift
    for argument in "$@"; do
        case "$argument" in
            -*) ;;
            *) grep -Fxq -- "$argument" "$PACKAGE_STATE" || printf '%s\n' "$argument" >> "$PACKAGE_STATE" ;;
        esac
    done
    printf '%s\n' smbd.service nmbd.service >> "$SYSTEMD_ENABLED"
    printf '%s\n' smbd.service nmbd.service >> "$SYSTEMD_ACTIVE"
fi
EOF

cat > "$FAKE_BIN/systemctl" <<'EOF'
#!/usr/bin/env bash
command_name="${1:-}"
shift || true
case "$command_name" in
    --failed) ;;
    show)
        if [ "$*" = 'fstrim.timer -p LastTriggerUSec --value' ]; then
            printf 'Sat 2026-09-05 13:12:33 CEST\n'
        else
            exit 2
        fi
        ;;
    is-enabled)
        if grep -Fxq -- "${1:-}" "$SYSTEMD_MASKED"; then
            printf 'masked\n'
        elif grep -Fxq -- "${1:-}" "$SYSTEMD_ENABLED"; then
            printf 'enabled\n'
        else
            printf 'disabled\n'
            exit 1
        fi
        ;;
    is-active)
        if grep -Fxq -- "${1:-}" "$SYSTEMD_ACTIVE"; then
            printf 'active\n'
        else
            printf 'inactive\n'
            exit 1
        fi
        ;;
    mask)
        printf 'systemctl:mask %s\n' "$*" >> "$EVENT_LOG"
        for unit in "$@"; do
            grep -Fxq -- "$unit" "$SYSTEMD_MASKED" || printf '%s\n' "$unit" >> "$SYSTEMD_MASKED"
        done
        ;;
    enable)
        [ "${1:-}" = --now ] && shift
        printf 'systemctl:enable --now %s\n' "$*" >> "$EVENT_LOG"
        for unit in "$@"; do
            grep -Fxq -- "$unit" "$SYSTEMD_ENABLED" || printf '%s\n' "$unit" >> "$SYSTEMD_ENABLED"
            grep -Fxq -- "$unit" "$SYSTEMD_ACTIVE" || printf '%s\n' "$unit" >> "$SYSTEMD_ACTIVE"
        done
        ;;
    disable)
        [ "${1:-}" = --now ] && shift
        printf 'systemctl:disable --now %s\n' "$*" >> "$EVENT_LOG"
        for unit in "$@"; do
            grep -Fxv -- "$unit" "$SYSTEMD_ENABLED" > "$SYSTEMD_ENABLED.tmp" || true
            mv "$SYSTEMD_ENABLED.tmp" "$SYSTEMD_ENABLED"
            grep -Fxv -- "$unit" "$SYSTEMD_ACTIVE" > "$SYSTEMD_ACTIVE.tmp" || true
            mv "$SYSTEMD_ACTIVE.tmp" "$SYSTEMD_ACTIVE"
        done
        ;;
    daemon-reload|restart|reload)
        printf 'systemctl:%s %s\n' "$command_name" "$*" >> "$EVENT_LOG"
        ;;
    *)
        printf 'Unexpected systemctl call: %s %s\n' "$command_name" "$*" >&2
        exit 2
        ;;
esac
EOF

cat > "$FAKE_BIN/hostname" <<'EOF'
#!/usr/bin/env bash
printf 'pol-server\n'
EOF

cat > "$FAKE_BIN/timedatectl" <<'EOF'
#!/usr/bin/env bash
case "$*" in
    'show -p Timezone --value') printf 'Europe/Madrid\n' ;;
    'show -p NTPSynchronized --value') printf '%s\n' "${FAKE_NTP_SYNCHRONIZED:-yes}" ;;
    *) exit 2 ;;
esac
EOF

cat > "$FAKE_BIN/localectl" <<'EOF'
#!/usr/bin/env bash
printf '   System Locale: LANG=es_ES.UTF-8\n'
EOF

cat > "$FAKE_BIN/sshd" <<'EOF'
#!/usr/bin/env bash
printf 'sshd:%s\n' "$*" >> "$EVENT_LOG"
EOF

cat > "$FAKE_BIN/visudo" <<'EOF'
#!/usr/bin/env bash
printf 'visudo:%s\n' "$*" >> "$EVENT_LOG"
EOF

cat > "$FAKE_BIN/date" <<'EOF'
#!/usr/bin/env bash
printf '20260905210000Z\n'
EOF

chmod +x "$FAKE_BIN/dpkg-query" "$FAKE_BIN/apt-get" "$FAKE_BIN/systemctl" "$FAKE_BIN/hostname" \
    "$FAKE_BIN/timedatectl" "$FAKE_BIN/localectl" "$FAKE_BIN/sshd" "$FAKE_BIN/visudo" "$FAKE_BIN/date"
export EVENT_LOG PACKAGE_STATE SYSTEMD_ENABLED SYSTEMD_ACTIVE SYSTEMD_MASKED
export PATH="$FAKE_BIN:$ORIGINAL_PATH"
export POL_SERVER_ROOT="$FAKE_ROOT"
export POL_SERVER_ALLOW_UNPRIVILEGED=true

"$INSTALLER"

assert_file_contains "$FAKE_ROOT/usr/local/sbin/pol-server-bootstrap" '/usr/local/lib/cuberhaus/pol-server/bootstrap'
assert_file_contains "$FAKE_ROOT/usr/local/sbin/pol-server-hardware" '/usr/local/lib/cuberhaus/pol-server/hardware-qualification'
assert_file_contains "$FAKE_ROOT/usr/local/sbin/pol-server-maintenance" '/usr/local/lib/cuberhaus/pol-server/maintenance-access'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-bootstrap --apply'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-hardware --report'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-hardware --start-smart-long kingston'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-hardware --start-smart-long micron'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-maintenance --revoke'
assert_file_contains "$EVENT_LOG" 'visudo:-cf'
[ -x "$FAKE_ROOT/usr/local/lib/cuberhaus/pol-server/bootstrap" ] ||
    fail 'installer must deploy an executable root-owned bootstrap bundle'
[ -x "$FAKE_ROOT/usr/local/lib/cuberhaus/pol-server/hardware-qualification" ] ||
    fail 'installer must deploy the executable hardware qualification command'

"$INSTALLER" --temporary-full-access
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-maintenance" \
    'pol ALL=(ALL:ALL) NOTAFTER=20260905210000Z NOPASSWD: ALL'
"$MAINTENANCE" --revoke
[ ! -e "$FAKE_ROOT/etc/sudoers.d/pol-server-maintenance" ] ||
    fail 'maintenance revocation must remove the temporary sudoers grant'

"$BOOTSTRAP" --apply

assert_file_contains "$EVENT_LOG" 'apt-get:update'
for unit in sleep.target suspend.target hibernate.target hybrid-sleep.target; do
    assert_file_contains "$EVENT_LOG" "systemctl:mask $unit"
done
assert_file_contains "$EVENT_LOG" 'systemctl:disable --now smbd.service'
assert_file_contains "$EVENT_LOG" 'systemctl:disable --now nmbd.service'
assert_file_contains "$FAKE_ROOT/etc/systemd/logind.conf.d/90-pol-server.conf" 'HandleLidSwitch=ignore'
assert_file_contains "$FAKE_ROOT/etc/apt/apt.conf.d/52pol-server-periodic" 'APT::Periodic::Unattended-Upgrade "1";'
assert_file_contains "$FAKE_ROOT/etc/ssh/sshd_config.d/10-pol-server.conf" 'PasswordAuthentication no'
assert_file_contains "$FAKE_ROOT/home/pol/.ssh/authorized_keys" 'dotfiles-client@pol-server'
for package in ca-certificates curl openssh-server restic samba smartmontools ufw unattended-upgrades; do
    grep -Fxq -- "$package" "$PACKAGE_STATE" || fail "Expected package to be installed: $package"
done

: > "$EVENT_LOG"
"$BOOTSTRAP" --apply
[ ! -s "$EVENT_LOG" ] || fail 'second bootstrap apply must not make converged-state changes'

check_output="$("$BOOTSTRAP" --check)"
grep -Fq 'Hostname current: pol-server' <<< "$check_output" || fail 'baseline must audit the hostname'
grep -Fq 'Timezone current: Europe/Madrid' <<< "$check_output" || fail 'baseline must audit the timezone'
grep -Fq 'System locale current: LANG=es_ES.UTF-8' <<< "$check_output" || fail 'baseline must audit the locale'
grep -Fq 'System clock synchronized' <<< "$check_output" || fail 'baseline must audit time synchronization'
grep -Fq 'No failed systemd units' <<< "$check_output" || fail 'baseline must audit failed units'
grep -Fq 'fstrim.timer last trigger: Sat 2026-09-05 13:12:33 CEST' <<< "$check_output" ||
    fail 'baseline must report the latest fstrim timer trigger'

export FAKE_NTP_SYNCHRONIZED=no
if "$BOOTSTRAP" --check >/dev/null 2>&1; then
    fail 'bootstrap check must report unsynchronized time'
fi
unset FAKE_NTP_SYNCHRONIZED

rm "$FAKE_ROOT/etc/systemd/logind.conf.d/90-pol-server.conf"
if "$BOOTSTRAP" --check >/dev/null 2>&1; then
    fail 'bootstrap check must report configuration drift'
fi

printf 'pol-server bootstrap tests passed.\n'
