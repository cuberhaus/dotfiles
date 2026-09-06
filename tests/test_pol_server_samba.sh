#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SAMBA="$REPO_ROOT/server/pol-server/samba-setup"
CASE_DIR="$(mktemp -d)"
FAKE_BIN="$CASE_DIR/bin"
STORAGE_ROOT="$CASE_DIR/storage"
SMB_CONF="$CASE_DIR/smb.conf"
EVENT_LOG="$CASE_DIR/events.log"
ACCOUNT_STATE="$CASE_DIR/account-created"
PASSWORD_STATE="$CASE_DIR/password-created"
SERVICE_STATE="$CASE_DIR/service-active"
ORIGINAL_PATH="$PATH"
trap 'rm -rf "$CASE_DIR"' EXIT

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

mkdir -p "$FAKE_BIN" "$STORAGE_ROOT"/{shared,incoming,private,immich,appdata}
chmod 0755 "$STORAGE_ROOT"
: > "$EVENT_LOG"

cat > "$FAKE_BIN/findmnt" <<'EOF'
#!/usr/bin/env bash
case "$*" in
    '-n -o SOURCE,FSTYPE --target '*) printf '/dev/disk/by-uuid/test ext4\n' ;;
    *) exit 2 ;;
esac
EOF

cat > "$FAKE_BIN/getent" <<'EOF'
#!/usr/bin/env bash
[ "$*" = 'group nasusers' ] && printf 'nasusers:x:999:\n' && exit 0
[ "$*" = 'passwd pol-files' ] && [ -f "$ACCOUNT_STATE" ] &&
    printf 'pol-files:x:998:999::/nonexistent:/usr/sbin/nologin\n' && exit 0
exit 2
EOF

cat > "$FAKE_BIN/pdbedit" <<'EOF'
#!/usr/bin/env bash
[ "$*" = '-L -u pol-files' ] && [ -f "$PASSWORD_STATE" ] &&
    printf 'pol-files:998:\n' && exit 0
exit 1
EOF

cat > "$FAKE_BIN/systemctl" <<'EOF'
#!/usr/bin/env bash
case "$*" in
    'is-active smbd.service')
        [ -f "$SERVICE_STATE" ] && printf 'active\n' || printf 'inactive\n'
        ;;
    'is-enabled smbd.service')
        [ -f "$SERVICE_STATE" ] && printf 'enabled\n' || printf 'disabled\n'
        ;;
    'is-active nmbd.service') printf 'inactive\n' ;;
    'is-enabled nmbd.service') printf 'disabled\n' ;;
    'disable --now smbd.service')
        printf 'systemctl:%s\n' "$*" >> "$EVENT_LOG"
        rm -f "$SERVICE_STATE"
        ;;
    'disable --now nmbd.service')
        printf 'systemctl:%s\n' "$*" >> "$EVENT_LOG"
        ;;
    'enable --now smbd.service')
        printf 'systemctl:%s\n' "$*" >> "$EVENT_LOG"
        touch "$SERVICE_STATE"
        ;;
    *) exit 2 ;;
esac
EOF

cat > "$FAKE_BIN/smbpasswd" <<'EOF'
#!/usr/bin/env bash
printf 'smbpasswd:%s\n' "$*" >> "$EVENT_LOG"
case "$*" in
    '-a pol-files') ;;
    '-s -a pol-files')
        IFS= read -r first_password
        IFS= read -r second_password
        [ -n "$first_password" ] && [ "$first_password" = "$second_password" ] || exit 2
        ;;
    *) exit 2 ;;
esac
touch "$PASSWORD_STATE"
EOF

cat > "$FAKE_BIN/useradd" <<'EOF'
#!/usr/bin/env bash
printf 'useradd:%s\n' "$*" >> "$EVENT_LOG"
touch "$ACCOUNT_STATE"
EOF

for command in chown chmod; do
    cat > "$FAKE_BIN/$command" <<'EOF'
#!/usr/bin/env bash
printf '%s:%s\n' "${0##*/}" "$*" >> "$EVENT_LOG"
EOF
done

cat > "$FAKE_BIN/install" <<'EOF'
#!/usr/bin/env bash
printf 'install:%s\n' "$*" >> "$EVENT_LOG"
target="${@: -1}"
source="${@: -2:1}"
mkdir -p "$(dirname "$target")"
cp "$source" "$target"
EOF

cat > "$FAKE_BIN/testparm" <<'EOF'
#!/usr/bin/env bash
printf 'testparm:%s\n' "$*" >> "$EVENT_LOG"
[ "$1" = -s ] && [ "$2" = "$POL_SERVER_SAMBA_CONF" ]
EOF

chmod +x "$FAKE_BIN"/*
export PATH="$FAKE_BIN:$ORIGINAL_PATH"
export POL_SERVER_ALLOW_UNPRIVILEGED=true
export POL_SERVER_SAMBA_CONF="$SMB_CONF"
export POL_SERVER_STORAGE_ROOT="$STORAGE_ROOT"
export ACCOUNT_STATE EVENT_LOG PASSWORD_STATE SERVICE_STATE

[ -x "$SAMBA" ] || fail 'Samba setup command must exist and be executable'

report="$($SAMBA --check)"
grep -Fq 'Storage state: ready (ext4)' <<< "$report" ||
    fail 'check must require the ext4 storage mount'
grep -Fq 'Samba state: unconfigured' <<< "$report" ||
    fail 'check must distinguish an unconfigured server'
grep -Fq 'Account state: missing (pol-files)' <<< "$report" ||
    fail 'check must report the dedicated Samba account state'
grep -Fq 'Service state: inactive and disabled' <<< "$report" ||
    fail 'check must report the inactive Samba service'

"$SAMBA" --apply >/dev/null
grep -Fqx 'useradd:--system --no-create-home --home-dir /nonexistent --shell /usr/sbin/nologin --gid nasusers pol-files' "$EVENT_LOG" ||
    fail 'apply must create a dedicated non-login Samba identity'
grep -Fqx "chmod:0755 $STORAGE_ROOT" "$EVENT_LOG" ||
    fail 'apply must make the mounted storage root traversable to share users'
grep -Fqx "chown:root:nasusers $STORAGE_ROOT/shared $STORAGE_ROOT/incoming" "$EVENT_LOG" ||
    fail 'apply must assign shared directories to the NAS group'
grep -Fqx "chmod:2770 $STORAGE_ROOT/shared $STORAGE_ROOT/incoming" "$EVENT_LOG" ||
    fail 'apply must preserve group inheritance on shared directories'
grep -Fqx "chown:pol-files:nasusers $STORAGE_ROOT/private" "$EVENT_LOG" ||
    fail 'apply must make the private directory user-owned'
grep -Fqx "chmod:0700 $STORAGE_ROOT/private" "$EVENT_LOG" ||
    fail 'apply must restrict the private directory to pol-files'
grep -Fqx "testparm:-s $SMB_CONF" "$EVENT_LOG" ||
    fail 'apply must validate the installed Samba configuration'
grep -Fqx 'systemctl:disable --now smbd.service' "$EVENT_LOG" ||
    fail 'apply must leave smbd inactive until password enrollment'
grep -Fqx 'systemctl:disable --now nmbd.service' "$EVENT_LOG" ||
    fail 'apply must keep legacy NetBIOS discovery disabled'
grep -Fq 'server min protocol = SMB2_10' "$SMB_CONF" ||
    fail 'configuration must disable SMB1'
grep -Fq 'hosts allow = 127.0.0.1 192.168.1.0/24' "$SMB_CONF" ||
    fail 'configuration must allow only localhost and the trusted LAN'
grep -Fq 'valid users = pol-files' "$SMB_CONF" ||
    fail 'the private share must allow only the dedicated account'

"$SAMBA" --set-password >/dev/null
grep -Fqx 'smbpasswd:-a pol-files' "$EVENT_LOG" ||
    fail 'password enrollment must remain interactive without a password argument'
grep -Fqx 'systemctl:enable --now smbd.service' "$EVENT_LOG" ||
    fail 'password enrollment must activate smbd only after account enrollment'
if grep -Fq 'systemctl:enable --now nmbd.service' "$EVENT_LOG"; then
    fail 'password enrollment must not enable legacy NetBIOS discovery'
fi

configured_report="$($SAMBA --check)"
grep -Fq 'Samba state: configured' <<< "$configured_report" ||
    fail 'check must recognize the tracked, validated Samba configuration'
grep -Fq 'Account state: enrolled (pol-files)' <<< "$configured_report" ||
    fail 'check must verify the Samba password database entry'
grep -Fq 'Service state: active and enabled' <<< "$configured_report" ||
    fail 'check must verify the active smbd service'

/usr/bin/chmod 0700 "$STORAGE_ROOT"
blocked_report="$("$SAMBA" --check)"
grep -Fq 'Samba state: configuration drift' <<< "$blocked_report" ||
    fail 'check must reject configured Samba when the storage root blocks traversal'
grep -Fq 'Storage root state: blocked (0700)' <<< "$blocked_report" ||
    fail 'check must report the mounted storage root mode'
/usr/bin/chmod 0755 "$STORAGE_ROOT"

rm -f "$PASSWORD_STATE" "$SERVICE_STATE"
printf 'generated-test-secret\ngenerated-test-secret\n' |
    "$SAMBA" --set-password-stdin >/dev/null
grep -Fqx 'smbpasswd:-s -a pol-files' "$EVENT_LOG" ||
    fail 'stdin enrollment must use smbpasswd silent mode without a password argument'
stdin_report="$($SAMBA --check)"
grep -Fq 'Samba state: configured' <<< "$stdin_report" ||
    fail 'stdin password enrollment must activate the same configured state'

printf 'pol-server Samba setup tests passed.\n'