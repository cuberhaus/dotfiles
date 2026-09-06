#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BOOTSTRAP="$REPO_ROOT/server/pol-server/full-bootstrap"
MAKEFILE="$REPO_ROOT/Makefile"

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

assert_file_contains() {
    local file="$1"
    local expected="$2"
    grep -Fq -- "$expected" "$file" || fail "Expected $file to contain: $expected"
}

assert_file_excludes() {
    local file="$1"
    local unexpected="$2"
    ! grep -Fq -- "$unexpected" "$file" || fail "Expected $file to exclude: $unexpected"
}

[ -x "$BOOTSTRAP" ] || fail 'full pol-server bootstrap must exist and be executable'
assert_file_contains "$MAKEFILE" 'bootstrap-pol-server-full:'
assert_file_contains "$MAKEFILE" 'bash tests/test_pol_server_full_bootstrap.sh'

for mode in \
    --revoke-maintenance \
    --check \
    --hardware-report \
    --check-storage \
    --prepare-storage \
    --check-samba \
    --test-samba-access \
    --check-backup \
    --run-backup \
    --check-backup-repository \
    --restore-test-backup \
    --check-github-mirrors \
    --configure-rss-email \
    --test-rss-email \
    --email-wd-report \
    --check-immich \
    --backup-immich \
    --check-monitoring; do
    assert_file_contains "$BOOTSTRAP" "$mode"
done
assert_file_contains "$BOOTSTRAP" 'wireguard-bootstrap'
assert_file_contains "$BOOTSTRAP" 'make -C'
assert_file_contains "$BOOTSTRAP" 'RESTORE-WD-IMMICH'
assert_file_contains "$BOOTSTRAP" 'sudo -n true'
assert_file_excludes "$BOOTSTRAP" "if confirm 'Configure or refresh RSS email"
assert_file_contains "$BOOTSTRAP" 'Did the RSS delivery test and WD SMART report arrive?'
assert_file_contains "$BOOTSTRAP" 'Service active and enabled: pol-server-rss-email.timer'

CASE_DIR="$(mktemp -d)"
FAKE_BIN="$CASE_DIR/bin"
DEPLOY_LOG="$CASE_DIR/deploy.log"
WIREGUARD_LOG="$CASE_DIR/wireguard.log"
STATE_FILE="$CASE_DIR/state/pol-server.state"
trap 'rm -rf "$CASE_DIR"' EXIT
mkdir -p "$FAKE_BIN"
: > "$DEPLOY_LOG"
: > "$WIREGUARD_LOG"

cat > "$FAKE_BIN/fake-deploy" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$DEPLOY_LOG"
mode="${*: -1}"
[ "${FAKE_DEPLOY_FAILURE:-}" != "$mode" ] || exit 1
case "$mode" in
    --check)
        printf '%s\n' '[INFO] Service active and enabled: pol-server-rss-email.timer'
        ;;
    --check-samba)
        printf '%s\n' 'Samba state: configured' 'Account state: enrolled (pol-files)' \
            'Service state: active and enabled' 'NetBIOS state: inactive and disabled'
        ;;
    --check-backup)
        printf '%s\n' 'Backup state: configured'
        ;;
esac
EOF

cat > "$FAKE_BIN/fake-wireguard-bootstrap" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$WIREGUARD_LOG"
EOF

cat > "$FAKE_BIN/ssh" <<'EOF'
#!/usr/bin/env bash
remote_command="${*: -1}"
printf 'ssh:%s\n' "$remote_command" >> "$DEPLOY_LOG"
case "$remote_command" in
    'test "$(hostname)" = pol-server') exit 0 ;;
    'sudo -n true') exit 1 ;;
    *) exit 2 ;;
esac
EOF

chmod +x "$FAKE_BIN/fake-deploy" "$FAKE_BIN/fake-wireguard-bootstrap" "$FAKE_BIN/ssh"
export DEPLOY_LOG WIREGUARD_LOG
export POL_SERVER_DEPLOY="$FAKE_BIN/fake-deploy"
export POL_SERVER_WIREGUARD_BOOTSTRAP="$FAKE_BIN/fake-wireguard-bootstrap"
export POL_SERVER_BOOTSTRAP_STATE="$STATE_FILE"
export POL_SERVER_ALLOW_NONINTERACTIVE=true
export POL_SERVER_SKIP_LOCAL_CHECK=true
export PATH="$FAKE_BIN:$PATH"

printf 'y\nn\nn\nn\ny\ny\n' | "$BOOTSTRAP" --host fake-nas --run >/dev/null
for mode in \
    --revoke-maintenance \
    --enroll-maintenance \
    --apply \
    --check \
    --hardware-report \
    --check-storage \
    --check-samba \
    --test-samba-access \
    --check-backup \
    --run-backup \
    --check-backup-repository \
    --restore-test-backup \
    --check-github-mirrors \
    --configure-rss-email \
    --test-rss-email \
    --email-wd-report \
    --check-immich \
    --backup-immich \
    --check-monitoring \
    --check-wireguard; do
    grep -Fq -- "--host fake-nas $mode" "$DEPLOY_LOG" ||
        fail "full bootstrap must run tracked mode: $mode"
done
grep -Fq -- '--host fake-nas' "$WIREGUARD_LOG" ||
    fail 'full bootstrap must compose the tracked WireGuard bootstrap'
grep -Fxq hardware-qualified "$STATE_FILE" || fail 'hardware acceptance must be resumable'
grep -Fxq rss-email-accepted "$STATE_FILE" || fail 'RSS email acceptance must be resumable'
grep -Fxq immich-library-accepted "$STATE_FILE" || fail 'Immich acceptance must be resumable'
grep -Fxq wireguard-mobile-accepted "$STATE_FILE" || fail 'WireGuard acceptance must be resumable'
[ "$(tail -n 1 "$DEPLOY_LOG")" = 'ssh:sudo -n true' ] ||
    fail 'full bootstrap must finish by proving broad maintenance is closed'

printf 'n\n' | "$BOOTSTRAP" --host fake-nas --run >/dev/null
[ "$(grep -Fc -- '--host fake-nas --configure-rss-email' "$DEPLOY_LOG")" -eq 1 ] ||
    fail 'a resumed full bootstrap must not replace an accepted RSS credential'
[ "$(grep -Fc -- '--host fake-nas --test-rss-email' "$DEPLOY_LOG")" -eq 2 ] ||
    fail 'a resumed full bootstrap must revalidate RSS delivery'

: > "$DEPLOY_LOG"
export FAKE_DEPLOY_FAILURE=--run-backup
if printf 'n\n' | "$BOOTSTRAP" --host fake-nas --run >/dev/null 2>&1; then
    fail 'full bootstrap must fail when backup acceptance fails'
fi
unset FAKE_DEPLOY_FAILURE
[ "$(tail -n 1 "$DEPLOY_LOG")" = '--host fake-nas --revoke-maintenance' ] ||
    fail 'failed full bootstrap must revoke temporary maintenance through its exit trap'

printf 'pol-server full bootstrap contract passed.\n'