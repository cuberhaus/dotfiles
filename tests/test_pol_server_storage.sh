#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
STORAGE="$REPO_ROOT/server/pol-server/storage-setup"
CASE_DIR="$(mktemp -d)"
FAKE_BIN="$CASE_DIR/bin"
EVENT_LOG="$CASE_DIR/events.log"
FSTAB="$CASE_DIR/fstab"
STORAGE_ROOT="$CASE_DIR/storage"
ORIGINAL_PATH="$PATH"
trap 'rm -rf "$CASE_DIR"' EXIT

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

mkdir -p "$FAKE_BIN"
: > "$EVENT_LOG"
printf '# test fstab\n' > "$FSTAB"

cat > "$FAKE_BIN/lsblk" <<'EOF'
#!/usr/bin/env bash
case "$*" in
    '-dn -o PATH,MODEL')
        printf '%s\n' \
            "/dev/sda ${FAKE_KINGSTON_MODEL:-KINGSTON SA400S37960G}" \
            '/dev/sdb Micron_1100_MTFDDAV256TBN' \
            '/dev/sdc WDC WD15SMRW-11YNDS0'
        ;;
    '-nr -o MOUNTPOINTS /dev/sda')
        if [ "${FAKE_STORAGE_CONFIGURED:-false}" = true ]; then
            printf '%s\n' "$POL_SERVER_STORAGE_ROOT"
        else
            printf '%s' "${FAKE_KINGSTON_MOUNTS:-}"
        fi
        ;;
    '-nrpo PATH,TYPE,FSTYPE,LABEL /dev/sda')
        if [ "${FAKE_STORAGE_CONFIGURED:-false}" = true ]; then
            printf '%s\n' \
                '/dev/sda disk' \
                '/dev/sda1 part ext4 nas-data'
        else
            printf '%s\n' \
                '/dev/sda disk' \
                '/dev/sda1 part' \
                '/dev/sda2 part ntfs'
        fi
        ;;
    '-nrpo PATH,TYPE /dev/sda')
        printf '%s\n' \
            '/dev/sda disk' \
            '/dev/sda1 part'
        ;;
    '-dn -o LABEL /dev/sda1') printf 'nas-data\n' ;;
    *)
        printf 'Unexpected lsblk call: %s\n' "$*" >&2
        exit 2
        ;;
esac
EOF

cat > "$FAKE_BIN/blockdev" <<'EOF'
#!/usr/bin/env bash
[ "$*" = '--getsize64 /dev/sda' ] || exit 2
printf '%s\n' "${FAKE_KINGSTON_SIZE:-960197124096}"
EOF

cat > "$FAKE_BIN/findmnt" <<'EOF'
#!/usr/bin/env bash
case "$*" in
    '-n -o SOURCE /') printf '%s\n' "${FAKE_ROOT_SOURCE:-/dev/sdb2}" ;;
    '-n -o SOURCE,FSTYPE --target '*) printf '/dev/sda1 ext4\n' ;;
    *) exit 2 ;;
esac
EOF

cat > "$FAKE_BIN/parted" <<'EOF'
#!/usr/bin/env bash
printf 'parted:%s\n' "$*" >> "$EVENT_LOG"
EOF

cat > "$FAKE_BIN/udevadm" <<'EOF'
#!/usr/bin/env bash
printf 'udevadm:%s\n' "$*" >> "$EVENT_LOG"
EOF

cat > "$FAKE_BIN/mkfs.ext4" <<'EOF'
#!/usr/bin/env bash
printf 'mkfs.ext4:%s\n' "$*" >> "$EVENT_LOG"
EOF

cat > "$FAKE_BIN/blkid" <<'EOF'
#!/usr/bin/env bash
[ "$*" = '-s UUID -o value /dev/sda1' ] || exit 2
printf '11111111-2222-3333-4444-555555555555\n'
EOF

cat > "$FAKE_BIN/getent" <<'EOF'
#!/usr/bin/env bash
exit 1
EOF

cat > "$FAKE_BIN/groupadd" <<'EOF'
#!/usr/bin/env bash
printf 'groupadd:%s\n' "$*" >> "$EVENT_LOG"
EOF

cat > "$FAKE_BIN/install" <<'EOF'
#!/usr/bin/env bash
printf 'install:%s\n' "$*" >> "$EVENT_LOG"
for argument in "$@"; do
    [[ "$argument" == /* ]] && mkdir -p "$argument"
done
EOF

cat > "$FAKE_BIN/mount" <<'EOF'
#!/usr/bin/env bash
printf 'mount:%s\n' "$*" >> "$EVENT_LOG"
EOF

chmod +x "$FAKE_BIN"/*
export PATH="$FAKE_BIN:$ORIGINAL_PATH"
export POL_SERVER_ALLOW_UNPRIVILEGED=true
export POL_SERVER_FSTAB="$FSTAB"
export POL_SERVER_STORAGE_ROOT="$STORAGE_ROOT"
export EVENT_LOG

[ -x "$STORAGE" ] || fail 'storage setup command must exist and be executable'

report="$($STORAGE --check)"
grep -Fq 'Kingston target: /dev/sda (KINGSTON SA400S37960G)' <<< "$report" ||
    fail 'check must identify the model-matched Kingston target'
grep -Fq 'Mount state: unmounted' <<< "$report" ||
    fail 'check must require the Kingston to be unmounted'
grep -Fq 'Legacy filesystem: ntfs' <<< "$report" ||
    fail 'check must report the existing NTFS filesystem'

if "$STORAGE" --apply WRONG-TOKEN >/dev/null 2>&1; then
    fail 'apply must reject an incorrect destructive confirmation token'
fi

export FAKE_KINGSTON_MOUNTS=/mnt/legacy
if "$STORAGE" --apply ERASE-KINGSTON-SA400S37960G >/dev/null 2>&1; then
    fail 'apply must reject a mounted Kingston target'
fi
unset FAKE_KINGSTON_MOUNTS

export FAKE_KINGSTON_SIZE=500000000000
if "$STORAGE" --check >/dev/null 2>&1; then
    fail 'check must reject an unexpected Kingston capacity'
fi
unset FAKE_KINGSTON_SIZE

export FAKE_ROOT_SOURCE=/dev/sda2
if "$STORAGE" --apply ERASE-KINGSTON-SA400S37960G >/dev/null 2>&1; then
    fail 'apply must reject a Kingston target containing the root filesystem'
fi
unset FAKE_ROOT_SOURCE

"$STORAGE" --apply ERASE-KINGSTON-SA400S37960G >/dev/null
grep -Fqx 'parted:--script /dev/sda mklabel gpt mkpart primary ext4 1MiB 100%' "$EVENT_LOG" ||
    fail 'apply must create one GPT partition only on the model-matched Kingston'
grep -Fqx 'mkfs.ext4:-F -L nas-data /dev/sda1' "$EVENT_LOG" ||
    fail 'apply must format the new partition as ext4 with the nas-data label'
grep -Fqx 'UUID=11111111-2222-3333-4444-555555555555 /srv/storage ext4 defaults,nosuid,nodev 0 2' "$FSTAB" ||
    fail 'apply must persist the storage mount by filesystem UUID'
grep -Fqx "mount:$STORAGE_ROOT" "$EVENT_LOG" ||
    fail 'apply must mount the data filesystem through its fstab target'
grep -Fqx 'groupadd:--system nasusers' "$EVENT_LOG" ||
    fail 'apply must create the dedicated NAS access group'

export FAKE_STORAGE_CONFIGURED=true
configured_report="$($STORAGE --check)"
grep -Fq 'Storage state: configured' <<< "$configured_report" ||
    fail 'check must recognize the configured nas-data filesystem'
grep -Fq 'Filesystem UUID: 11111111-2222-3333-4444-555555555555' <<< "$configured_report" ||
    fail 'check must report the configured filesystem UUID'

printf 'pol-server storage setup tests passed.\n'