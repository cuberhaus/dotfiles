#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BOOTSTRAP="$REPO_ROOT/server/pol-server/bootstrap"
DEPLOY="$REPO_ROOT/server/pol-server/deploy"
INSTALLER="$REPO_ROOT/server/pol-server/install-autonomy"
HARDWARE="$REPO_ROOT/server/pol-server/hardware-qualification"
STORAGE="$REPO_ROOT/server/pol-server/storage-setup"
SAMBA="$REPO_ROOT/server/pol-server/samba-setup"
MAINTENANCE="$REPO_ROOT/server/pol-server/maintenance-access"
GITHUB_MIRROR="$REPO_ROOT/server/pol-server/github-mirror"
RSS_EMAIL="$REPO_ROOT/server/pol-server/rss-email"
CASE_DIR="$(mktemp -d)"
FAKE_ROOT="$CASE_DIR/root"
FAKE_BIN="$CASE_DIR/bin"
EVENT_LOG="$CASE_DIR/events.log"
PACKAGE_STATE="$CASE_DIR/packages"
SYSTEMD_ENABLED="$CASE_DIR/systemd-enabled"
SYSTEMD_ACTIVE="$CASE_DIR/systemd-active"
SYSTEMD_MASKED="$CASE_DIR/systemd-masked"
SAMBA_PASSWORD_STATE="$CASE_DIR/samba-password"
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
[ -x "$STORAGE" ] || fail 'pol-server storage setup command must exist and be executable'
[ -x "$SAMBA" ] || fail 'pol-server Samba setup command must exist and be executable'
[ -x "$MAINTENANCE" ] || fail 'pol-server maintenance access command must exist and be executable'
[ -x "$GITHUB_MIRROR" ] || fail 'pol-server GitHub mirror command must exist and be executable'
[ -x "$RSS_EMAIL" ] || fail 'pol-server RSS email command must exist and be executable'
[ -x "$DEPLOY" ] || fail 'pol-server deploy must exist and be executable'
[ -x "$INSTALLER" ] || fail 'pol-server autonomy installer must exist and be executable'
grep -Eq '^[[:space:]]*rss-email[[:space:]]*\\$' "$DEPLOY" ||
    fail 'pol-server deploy must transfer the RSS email command'
grep -Eq '^[[:space:]]*storage-setup[[:space:]]*\\$' "$DEPLOY" ||
    fail 'pol-server deploy must transfer the storage setup command'
grep -Eq '^[[:space:]]*pol-server-storage[[:space:]]*\\$' "$DEPLOY" ||
    fail 'pol-server deploy must transfer the storage launcher'
grep -Eq '^[[:space:]]*samba-setup[[:space:]]*\\$' "$DEPLOY" ||
    fail 'pol-server deploy must transfer the Samba setup command'
grep -Eq '^[[:space:]]*pol-server-samba[[:space:]]*\\$' "$DEPLOY" ||
    fail 'pol-server deploy must transfer the Samba launcher'
grep -Fq 'bootstrap-pol-server:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose bootstrap-pol-server'
grep -Fq 'audit-pol-server:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose audit-pol-server'
grep -Fq 'enroll-pol-server:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose enroll-pol-server'
grep -Fq 'enroll-pol-server-maintenance:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose temporary maintenance enrollment'
grep -Fq 'revoke-pol-server-maintenance:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose maintenance revocation'
grep -Fq 'reboot-pol-server:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose remote reboot verification'
grep -Fq 'upgrade-pol-server:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose Debian upgrades'
grep -Fq "'sudo -n systemctl reboot'" "$REPO_ROOT/server/pol-server/deploy" ||
    fail 'remote reboot must be noninteractive and limited to the maintenance window'
grep -Fq "'sudo -n /usr/local/sbin/pol-server-bootstrap --check'" "$DEPLOY" ||
    fail 'remote baseline audit must inspect root-only credential state'
grep -Fq 'DEBIAN_FRONTEND=noninteractive apt-get full-upgrade -y' "$REPO_ROOT/server/pol-server/deploy" ||
    fail 'remote upgrades must use noninteractive Debian package handling'
grep -Fq 'audit-pol-server-hardware:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose audit-pol-server-hardware'
grep -Fq 'start-pol-server-smart-long-kingston:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose the Kingston SMART test'
grep -Fq 'start-pol-server-smart-long-micron:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose the Micron SMART test'
grep -Fq 'audit-pol-server-wd-backup:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose the WD backup disk audit'
grep -Fq 'start-pol-server-smart-long-wd-backup:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose the WD backup disk SMART test'
grep -Fq 'test-pol-server-thermals:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose the bounded thermal test'
grep -Fq 'audit-pol-server-storage:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose the storage preflight'
grep -Fq 'prepare-pol-server-storage:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose the guarded storage migration'
grep -Fq 'audit-pol-server-samba:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose the Samba audit'
grep -Fq 'configure-pol-server-samba:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose Samba staging'
grep -Fq 'set-pol-server-samba-password:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose interactive Samba activation'
grep -Fq 'configure-pol-server-github-mirrors:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose GitHub mirror authentication'
grep -Fq 'sync-pol-server-github-mirrors:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose manual GitHub mirror sync'
grep -Fq 'audit-pol-server-github-mirrors:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose GitHub mirror checks'
grep -Fq 'configure-pol-server-rss-email:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose RSS email configuration'
grep -Fq 'run-pol-server-rss-email:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose a manual RSS email run'
grep -Fq 'test-pol-server-rss-email:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose an RSS delivery test'
grep -Fq 'email-pol-server-wd-report:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose the WD report email'
grep -Fq "'sudo -n /usr/local/sbin/pol-server-rss-email --test-email'" "$DEPLOY" ||
    fail 'remote RSS delivery test must use its narrow root command'
grep -Fq "'sudo -n /usr/local/sbin/pol-server-rss-email --send-wd-report'" "$DEPLOY" ||
    fail 'remote WD report email must use its narrow root command'
grep -Fq "'sudo -n /usr/local/sbin/pol-server-storage --check'" "$DEPLOY" ||
    fail 'remote storage preflight must use its narrow root command'
grep -Fq "'sudo -n /usr/local/sbin/pol-server-storage --apply ERASE-KINGSTON-SA400S37960G'" "$DEPLOY" ||
    fail 'remote storage migration must use its exact destructive command'
grep -Fq "'sudo -n /usr/local/sbin/pol-server-samba --check'" "$DEPLOY" ||
    fail 'remote Samba audit must use its narrow root command'
grep -Fq "'sudo -n /usr/local/sbin/pol-server-samba --apply'" "$DEPLOY" ||
    fail 'remote Samba staging must use its exact root command'
grep -Fq "'sudo -n /usr/local/sbin/pol-server-samba --set-password'" "$DEPLOY" ||
    fail 'remote Samba password setup must use its exact root command'

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

cat > "$FAKE_BIN/pdbedit" <<'EOF'
#!/usr/bin/env bash
[ "$*" = '-L -u pol-files' ] && [ -f "$SAMBA_PASSWORD_STATE" ]
EOF

cat > "$FAKE_BIN/date" <<'EOF'
#!/usr/bin/env bash
printf '20260905210000Z\n'
EOF

chmod +x "$FAKE_BIN/dpkg-query" "$FAKE_BIN/apt-get" "$FAKE_BIN/systemctl" "$FAKE_BIN/hostname" \
    "$FAKE_BIN/timedatectl" "$FAKE_BIN/localectl" "$FAKE_BIN/sshd" "$FAKE_BIN/visudo" "$FAKE_BIN/pdbedit" "$FAKE_BIN/date"
export EVENT_LOG PACKAGE_STATE SYSTEMD_ENABLED SYSTEMD_ACTIVE SYSTEMD_MASKED SAMBA_PASSWORD_STATE
export PATH="$FAKE_BIN:$ORIGINAL_PATH"
export POL_SERVER_ROOT="$FAKE_ROOT"
export POL_SERVER_ALLOW_UNPRIVILEGED=true

"$INSTALLER"

assert_file_contains "$FAKE_ROOT/usr/local/sbin/pol-server-bootstrap" '/usr/local/lib/cuberhaus/pol-server/bootstrap'
assert_file_contains "$FAKE_ROOT/usr/local/sbin/pol-server-hardware" '/usr/local/lib/cuberhaus/pol-server/hardware-qualification'
assert_file_contains "$FAKE_ROOT/usr/local/sbin/pol-server-storage" '/usr/local/lib/cuberhaus/pol-server/storage-setup'
assert_file_contains "$FAKE_ROOT/usr/local/sbin/pol-server-samba" '/usr/local/lib/cuberhaus/pol-server/samba-setup'
assert_file_contains "$FAKE_ROOT/usr/local/sbin/pol-server-maintenance" '/usr/local/lib/cuberhaus/pol-server/maintenance-access'
assert_file_contains "$FAKE_ROOT/usr/local/sbin/pol-server-github-mirror" '/usr/local/lib/cuberhaus/pol-server/github-mirror'
assert_file_contains "$FAKE_ROOT/usr/local/sbin/pol-server-rss-email" '/usr/local/lib/cuberhaus/pol-server/rss-email'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-bootstrap --apply'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-bootstrap --check'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-hardware --report'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-hardware --report-disk wd-backup'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-hardware --start-smart-long kingston'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-hardware --start-smart-long micron'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-hardware --start-smart-long wd-backup'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-hardware --thermal-load'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-storage --check'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-storage --apply ERASE-KINGSTON-SA400S37960G'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-samba --check'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-samba --apply'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-samba --set-password'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-maintenance --revoke'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-github-mirror --configure-token'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-github-mirror --sync'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-github-mirror --check'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-rss-email --configure'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-rss-email --test-email'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/local/sbin/pol-server-rss-email --send-wd-report'
assert_file_contains "$FAKE_ROOT/etc/sudoers.d/pol-server-bootstrap" 'NOPASSWD: /usr/bin/systemctl start pol-server-rss-email.service'
assert_file_contains "$EVENT_LOG" 'visudo:-cf'
[ -x "$FAKE_ROOT/usr/local/lib/cuberhaus/pol-server/bootstrap" ] ||
    fail 'installer must deploy an executable root-owned bootstrap bundle'
[ -x "$FAKE_ROOT/usr/local/lib/cuberhaus/pol-server/hardware-qualification" ] ||
    fail 'installer must deploy the executable hardware qualification command'
[ -x "$FAKE_ROOT/usr/local/lib/cuberhaus/pol-server/storage-setup" ] ||
    fail 'installer must deploy the executable storage setup command'
[ -x "$FAKE_ROOT/usr/local/lib/cuberhaus/pol-server/samba-setup" ] ||
    fail 'installer must deploy the executable Samba setup command'
[ -x "$FAKE_ROOT/usr/local/lib/cuberhaus/pol-server/github-mirror" ] ||
    fail 'installer must deploy the executable GitHub mirror command'
[ -x "$FAKE_ROOT/usr/local/lib/cuberhaus/pol-server/rss-email" ] ||
    fail 'installer must deploy the executable RSS email command'
assert_file_contains "$FAKE_ROOT/usr/local/lib/cuberhaus/pol-server/etc/systemd/system/pol-server-github-mirror.service" 'DynamicUser=yes'
assert_file_contains "$FAKE_ROOT/usr/local/lib/cuberhaus/pol-server/etc/systemd/system/pol-server-github-mirror.service" 'LoadCredential=github-token:/etc/cuberhaus/github-mirror-token'
assert_file_contains "$FAKE_ROOT/usr/local/lib/cuberhaus/pol-server/etc/systemd/system/pol-server-github-mirror.timer" 'OnCalendar=*-*-* 03:30:00'
assert_file_contains "$FAKE_ROOT/usr/local/lib/cuberhaus/pol-server/etc/systemd/system/pol-server-github-mirror.timer" 'Persistent=true'
assert_file_contains "$FAKE_ROOT/usr/local/lib/cuberhaus/pol-server/etc/systemd/system/pol-server-rss-email.service" 'DynamicUser=yes'
assert_file_contains "$FAKE_ROOT/usr/local/lib/cuberhaus/pol-server/etc/systemd/system/pol-server-rss-email.service" 'LoadCredential=rss-email.json:/etc/cuberhaus/rss-email.json'
assert_file_contains "$FAKE_ROOT/usr/local/lib/cuberhaus/pol-server/etc/systemd/system/pol-server-rss-email.timer" 'OnCalendar=*-*-* 08:00:00'
assert_file_contains "$FAKE_ROOT/usr/local/lib/cuberhaus/pol-server/etc/systemd/system/pol-server-rss-email.timer" 'OnCalendar=*-*-* 20:00:00'
assert_file_contains "$FAKE_ROOT/usr/local/lib/cuberhaus/pol-server/etc/systemd/system/pol-server-rss-email.timer" 'Persistent=true'

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
for package in ca-certificates curl git git-lfs jq openssh-server python3 restic samba smbclient smartmontools stress-ng ufw unattended-upgrades; do
    grep -Fxq -- "$package" "$PACKAGE_STATE" || fail "Expected package to be installed: $package"
done

install -D -m 0600 /dev/null "$FAKE_ROOT/etc/cuberhaus/github-mirror-token"
install -D -m 0600 /dev/null "$FAKE_ROOT/etc/cuberhaus/rss-email.json"
"$BOOTSTRAP" --apply
assert_file_contains "$EVENT_LOG" 'systemctl:enable --now pol-server-github-mirror.timer'
assert_file_contains "$EVENT_LOG" 'systemctl:enable --now pol-server-rss-email.timer'

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

install -D -m 0644 "$REPO_ROOT/server/pol-server/etc/samba/smb.conf" \
    "$FAKE_ROOT/etc/samba/smb.conf"
touch "$SAMBA_PASSWORD_STATE"
: > "$EVENT_LOG"
"$BOOTSTRAP" --apply
assert_file_contains "$EVENT_LOG" 'systemctl:enable --now smbd.service'
if grep -Fq 'systemctl:enable --now nmbd.service' "$EVENT_LOG"; then
    fail 'configured baseline must not enable legacy NetBIOS discovery'
fi
configured_output="$($BOOTSTRAP --check)"
grep -Fq 'Configured Samba service active and enabled: smbd.service' <<< "$configured_output" ||
    fail 'baseline must accept active Samba after tracked configuration and password enrollment'
grep -Fq 'Legacy NetBIOS service inactive and disabled: nmbd.service' <<< "$configured_output" ||
    fail 'baseline must require nmbd to remain inactive after Samba configuration'

rm "$FAKE_ROOT/etc/systemd/logind.conf.d/90-pol-server.conf"
if "$BOOTSTRAP" --check >/dev/null 2>&1; then
    fail 'bootstrap check must report configuration drift'
fi

printf 'pol-server bootstrap tests passed.\n'
