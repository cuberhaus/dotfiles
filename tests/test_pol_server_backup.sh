#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BACKUP="$REPO_ROOT/server/pol-server/backup-setup"
CASE_DIR="$(mktemp -d)"
FAKE_ROOT="$CASE_DIR/root"
FAKE_BIN="$CASE_DIR/bin"
EVENT_LOG="$CASE_DIR/events.log"
MOUNT_STATE="$CASE_DIR/mounted"
RESTIC_SNAPSHOT_STATE="$CASE_DIR/restic-snapshot"
trap 'rm -rf "$CASE_DIR"' EXIT

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

mkdir -p "$FAKE_ROOT/etc" "$FAKE_BIN"
: > "$FAKE_ROOT/etc/fstab"
: > "$EVENT_LOG"

cat > "$FAKE_BIN/lsblk" <<'EOF'
#!/usr/bin/env bash
case "$*" in
    '-dn -o PATH,MODEL')
        printf '/dev/sdz WDC WD15SMRW-11YNDS0\n'
        ;;
    '-nrpo PATH,TYPE,FSTYPE,LABEL,UUID /dev/sdz')
        printf '/dev/sdz disk   \n'
        printf '/dev/sdz1 part vfat EFI 67E3-17ED\n'
        printf '/dev/sdz2 part exfat Pol-HDD 5EEB-7DF6\n'
        ;;
    *)
        printf 'Unexpected lsblk call: %s\n' "$*" >&2
        exit 2
        ;;
esac
EOF

cat > "$FAKE_BIN/blockdev" <<'EOF'
#!/usr/bin/env bash
[ "$*" = '--getsize64 /dev/sdz' ] || exit 2
printf '1500267937792\n'
EOF

cat > "$FAKE_BIN/findmnt" <<'EOF'
#!/usr/bin/env bash
case "$*" in
    '-n -o SOURCE,FSTYPE --mountpoint '*)
        if [ -f "$MOUNT_STATE" ]; then
            printf '/dev/sdz2 exfat\n'
        else
            exit 1
        fi
        ;;
    '-n -o TARGET --source /dev/sdz2')
        [ -n "${FAKE_OTHER_MOUNT:-}" ] || exit 1
        printf '%s\n' "$FAKE_OTHER_MOUNT"
        ;;
    *) exit 2 ;;
esac
EOF

cat > "$FAKE_BIN/mount" <<'EOF'
#!/usr/bin/env bash
printf 'mount:%s\n' "$*" >> "$EVENT_LOG"
touch "$MOUNT_STATE"
EOF

cat > "$FAKE_BIN/restic" <<'EOF'
#!/usr/bin/env bash
[ -n "${RESTIC_REPOSITORY:-}" ] || exit 91
[ -r "${RESTIC_PASSWORD_FILE:-}" ] || exit 92
[ "$(cat "$RESTIC_PASSWORD_FILE")" = 'test-restic-password-1234567890' ] || exit 93
printf 'restic:repository=%s password-file=%s command=%s\n' \
    "$RESTIC_REPOSITORY" "$RESTIC_PASSWORD_FILE" "$*" >> "$EVENT_LOG"
case "${1:-}" in
    init)
        mkdir -p "$RESTIC_REPOSITORY"
        printf 'repository-version\n' > "$RESTIC_REPOSITORY/config"
        ;;
    snapshots|check) ;;
    backup) touch "$RESTIC_SNAPSHOT_STATE" ;;
    restore)
        [ -f "$RESTIC_SNAPSHOT_STATE" ] || exit 94
        while (( $# > 0 )); do
            if [ "$1" = --target ]; then
                restore_target="$2"
                break
            fi
            shift
        done
        restored_probe="$restore_target$FAKE_BACKUP_SOURCE/.cuberhaus-backup-probe"
        mkdir -p "$(dirname "$restored_probe")"
        cp "$FAKE_BACKUP_SOURCE/.cuberhaus-backup-probe" "$restored_probe"
        ;;
    *) exit 95 ;;
esac
EOF

cat > "$FAKE_BIN/systemctl" <<'EOF'
#!/usr/bin/env bash
case "${1:-}" in
    enable)
        printf 'systemctl:%s\n' "$*" >> "$EVENT_LOG"
        ;;
    is-enabled|is-active)
        if [ "${FAKE_BACKUP_TIMER_ACTIVE:-false}" = true ]; then
            printf '%s\n' "${1#is-}d"
        else
            exit 1
        fi
        ;;
    *)
        printf 'systemctl:%s\n' "$*" >> "$EVENT_LOG"
        ;;
esac
EOF

for command_name in parted mkfs mkfs.exfat; do
    cat > "$FAKE_BIN/$command_name" <<'EOF'
#!/usr/bin/env bash
printf 'destructive:%s %s\n' "${0##*/}" "$*" >> "$EVENT_LOG"
exit 99
EOF
done

chmod +x "$FAKE_BIN"/*
export EVENT_LOG MOUNT_STATE RESTIC_SNAPSHOT_STATE

[ -x "$BACKUP" ] || fail 'pol-server backup setup command must exist and be executable'

check_output="$(
    PATH="$FAKE_BIN:$PATH" \
        POL_SERVER_ALLOW_UNPRIVILEGED=true \
        POL_SERVER_ROOT="$FAKE_ROOT" \
        "$BACKUP" --check
)"
grep -Fq 'Backup disk: /dev/sdz (WDC WD15SMRW-11YNDS0)' <<< "$check_output" ||
    fail 'check must report the model-pinned WD disk'
grep -Fq 'Backup partition: /dev/sdz2 (exfat, UUID 5EEB-7DF6)' <<< "$check_output" ||
    fail 'check must report the exact existing exFAT partition'
grep -Fq 'Backup state: available, unconfigured' <<< "$check_output" ||
    fail 'check must report an untouched eligible destination'

export FAKE_OTHER_MOUNT=/media/existing
if PATH="$FAKE_BIN:$PATH" \
        POL_SERVER_ALLOW_UNPRIVILEGED=true \
        POL_SERVER_ROOT="$FAKE_ROOT" \
        "$BACKUP" --prepare >/dev/null 2>&1; then
    fail 'prepare must reject the WD partition when it is already mounted elsewhere'
fi
unset FAKE_OTHER_MOUNT

PATH="$FAKE_BIN:$PATH" \
    POL_SERVER_ALLOW_UNPRIVILEGED=true \
    POL_SERVER_ROOT="$FAKE_ROOT" \
    "$BACKUP" --prepare >/dev/null

expected_fstab='UUID=5EEB-7DF6 /mnt/pol-server-backup exfat defaults,nofail,x-systemd.automount,x-systemd.idle-timeout=10min,x-systemd.device-timeout=30s,nosuid,nodev,noexec,uid=0,gid=0,fmask=0077,dmask=0077 0 0'
grep -Fqx "$expected_fstab" "$FAKE_ROOT/etc/fstab" ||
    fail 'prepare must persist the existing exFAT partition by UUID'
grep -Fqx "mount:$FAKE_ROOT/mnt/pol-server-backup" "$EVENT_LOG" ||
    fail 'prepare must mount only through the fixed backup mountpoint'
[ -d "$FAKE_ROOT/mnt/pol-server-backup/pol-server-restic" ] ||
    fail 'prepare must create only the dedicated restic repository directory'
[ -f "$FAKE_ROOT/srv/storage/.cuberhaus-backup-probe" ] ||
    fail 'prepare must create a stable restore-test probe in the backup source'
if grep -Fq 'destructive:' "$EVENT_LOG"; then
    fail 'prepare must never partition or format the WD disk'
fi

PATH="$FAKE_BIN:$PATH" \
    POL_SERVER_ALLOW_UNPRIVILEGED=true \
    POL_SERVER_ROOT="$FAKE_ROOT" \
    "$BACKUP" --prepare >/dev/null
[ "$(grep -Fxc "$expected_fstab" "$FAKE_ROOT/etc/fstab")" -eq 1 ] ||
    fail 'prepare must keep the UUID mount entry idempotent'

prepared_output="$(
    PATH="$FAKE_BIN:$PATH" \
        POL_SERVER_ALLOW_UNPRIVILEGED=true \
        POL_SERVER_ROOT="$FAKE_ROOT" \
        "$BACKUP" --check
)"
grep -Fq 'Backup state: prepared, repository not initialized' <<< "$prepared_output" ||
    fail 'check must recognize a mounted but uninitialized destination'

export FAKE_BACKUP_SOURCE="$FAKE_ROOT/srv/storage"
printf 'existing unrelated content\n' > "$FAKE_ROOT/mnt/pol-server-backup/pol-server-restic/existing.txt"
if printf '%s\n%s\n' 'test-restic-password-1234567890' 'test-restic-password-1234567890' |
        PATH="$FAKE_BIN:$PATH" \
            POL_SERVER_ALLOW_UNPRIVILEGED=true \
            POL_SERVER_ROOT="$FAKE_ROOT" \
            "$BACKUP" --configure-password-stdin >/dev/null 2>&1; then
    fail 'configuration must refuse a non-empty directory that is not a restic repository'
fi
rm "$FAKE_ROOT/mnt/pol-server-backup/pol-server-restic/existing.txt"

printf '%s\n%s\n' 'test-restic-password-1234567890' 'test-restic-password-1234567890' |
    PATH="$FAKE_BIN:$PATH" \
        POL_SERVER_ALLOW_UNPRIVILEGED=true \
        POL_SERVER_ROOT="$FAKE_ROOT" \
        "$BACKUP" --configure-password-stdin >/dev/null

credential="$FAKE_ROOT/etc/cuberhaus/restic-password"
[ "$(stat -c %a "$credential")" = 600 ] ||
    fail 'the restic credential must be root-only'
grep -Fq "restic:repository=$FAKE_ROOT/mnt/pol-server-backup/pol-server-restic password-file=" "$EVENT_LOG" ||
    fail 'repository initialization must use environment-based restic configuration'
grep -Fq 'command=init' "$EVENT_LOG" ||
    fail 'configuration must initialize the dedicated restic repository'
grep -Fq 'systemctl:enable --now pol-server-backup.timer' "$EVENT_LOG" ||
    fail 'configuration must enable the daily backup timer only after initialization'
if grep -Fq 'test-restic-password-1234567890' "$EVENT_LOG"; then
    fail 'the restic password must not appear in command logs'
fi

PATH="$FAKE_BIN:$PATH" \
    POL_SERVER_ALLOW_UNPRIVILEGED=true \
    POL_SERVER_ROOT="$FAKE_ROOT" \
    "$BACKUP" --backup >/dev/null
grep -Fq "command=backup --one-file-system --exclude-caches $FAKE_ROOT/srv/storage" "$EVENT_LOG" ||
    fail 'backup must cover the complete storage tree without crossing filesystems'
[ -s "$FAKE_ROOT/var/lib/cuberhaus-backup/last-success" ] ||
    fail 'a successful backup must publish a freshness marker'

PATH="$FAKE_BIN:$PATH" \
    POL_SERVER_ALLOW_UNPRIVILEGED=true \
    POL_SERVER_ROOT="$FAKE_ROOT" \
    "$BACKUP" --check-repository >/dev/null
grep -Eq 'command=check --read-data-subset=[0-9]+/12' "$EVENT_LOG" ||
    fail 'repository checks must rotate through twelve data subsets'
[ -s "$FAKE_ROOT/var/lib/cuberhaus-backup/last-check" ] ||
    fail 'a successful repository check must publish a freshness marker'

PATH="$FAKE_BIN:$PATH" \
    POL_SERVER_ALLOW_UNPRIVILEGED=true \
    POL_SERVER_ROOT="$FAKE_ROOT" \
    "$BACKUP" --restore-test >/dev/null
grep -Fq 'command=restore latest --target ' "$EVENT_LOG" ||
    fail 'restore acceptance must restore the latest snapshot to a separate path'

export FAKE_BACKUP_TIMER_ACTIVE=true
configured_output="$(
    PATH="$FAKE_BIN:$PATH" \
        POL_SERVER_ALLOW_UNPRIVILEGED=true \
        POL_SERVER_ROOT="$FAKE_ROOT" \
        "$BACKUP" --check
)"
grep -Fq 'Backup state: configured' <<< "$configured_output" ||
    fail 'check must recognize the initialized repository and active timer'

printf 'pol-server backup setup tests passed.\n'