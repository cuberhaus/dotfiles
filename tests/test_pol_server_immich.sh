#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
IMMICH="$REPO_ROOT/server/pol-server/immich-setup"
COMPOSE="$REPO_ROOT/server/pol-server/etc/immich/compose.yaml"
UNIT="$REPO_ROOT/server/pol-server/etc/systemd/system/pol-server-immich.service"
CASE_DIR="$(mktemp -d)"
FAKE_ROOT="$CASE_DIR/root"
FAKE_BIN="$CASE_DIR/bin"
EVENT_LOG="$CASE_DIR/events.log"
PACKAGE_STATE="$CASE_DIR/packages"
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

[ -x "$IMMICH" ] || fail 'pol-server Immich setup command must exist and be executable'
[ -r "$COMPOSE" ] || fail 'tracked Immich Compose configuration must exist'
[ -r "$UNIT" ] || fail 'tracked Immich systemd service must exist'

assert_file_contains "$COMPOSE" "image: ghcr.io/immich-app/immich-server:\${IMMICH_VERSION}"
assert_file_contains "$COMPOSE" "image: ghcr.io/immich-app/immich-machine-learning:\${IMMICH_VERSION}"
assert_file_contains "$COMPOSE" "- '\${IMMICH_BIND_ADDRESS}:2283:2283'"
assert_file_contains "$COMPOSE" "- \${UPLOAD_LOCATION}:/data"
assert_file_contains "$COMPOSE" "- \${DB_DATA_LOCATION}:/var/lib/postgresql/data"
if grep -Eq "(^|[[:space:]])- ['\"]?2283:2283" "$COMPOSE"; then
    fail 'Immich must not publish its port on every host interface'
fi
assert_file_contains "$UNIT" 'RequiresMountsFor=/srv/storage'
assert_file_contains "$UNIT" 'After=docker.service network-online.target'

mkdir -p \
    "$FAKE_ROOT/etc" \
    "$FAKE_ROOT/etc/cuberhaus" \
    "$FAKE_ROOT/etc/systemd/system" \
    "$FAKE_ROOT/srv/storage/immich" \
    "$FAKE_ROOT/var/lib/immich" \
    "$FAKE_BIN"
printf 'ID=debian\nVERSION_ID="13"\nVERSION_CODENAME=trixie\n' > "$FAKE_ROOT/etc/os-release"
: > "$EVENT_LOG"
: > "$PACKAGE_STATE"

cat > "$FAKE_BIN/dpkg-query" <<'EOF'
#!/usr/bin/env bash
package="${@: -1}"
if grep -Fxq -- "$package" "$PACKAGE_STATE"; then
    printf 'installed\n'
else
    exit 1
fi
EOF

cat > "$FAKE_BIN/docker" <<'EOF'
#!/usr/bin/env bash
printf 'docker:%s\n' "$*" >> "$EVENT_LOG"
case "$*" in
    'compose version') printf 'Docker Compose version v2.39.2\n' ;;
    'info') ;;
    *'compose '*' config --quiet') ;;
    *'compose '*' up -d --wait --wait-timeout 300') ;;
    *'compose '*' pull') ;;
    *'compose '*' down') ;;
    *'compose '*' up -d database') ;;
    *'compose '*' stop immich-server') ;;
    *'compose '*' start immich-server') ;;
    *'compose '*' exec -T database pg_dump '*)
        [ "${FAKE_PG_DUMP_FAIL:-false}" != true ] || exit 1
        printf '%s\n' 'CREATE TABLE assets (id integer);'
        ;;
    *'compose '*' exec -T database psql '*) cat >/dev/null ;;
    *'compose '*' ps --status running --services')
        printf '%s\n' immich-server immich-machine-learning redis database
        ;;
    "inspect --format {{.State.Health.Status}} immich_postgres") printf 'healthy\n' ;;
esac
EOF

cat > "$FAKE_BIN/systemctl" <<'EOF'
#!/usr/bin/env bash
printf 'systemctl:%s\n' "$*" >> "$EVENT_LOG"
case "$*" in
    'is-enabled pol-server-immich.service') printf 'enabled\n' ;;
    'is-active pol-server-immich.service') printf 'active\n' ;;
esac
EOF

cat > "$FAKE_BIN/apt-get" <<'EOF'
#!/usr/bin/env bash
printf 'apt-get:%s\n' "$*" >> "$EVENT_LOG"
if [ "${1:-}" = install ]; then
    shift
    for argument in "$@"; do
        case "$argument" in
            ca-certificates|curl|docker-ce|docker-ce-cli|containerd.io|docker-buildx-plugin|docker-compose-plugin)
                grep -Fxq -- "$argument" "$PACKAGE_STATE" || printf '%s\n' "$argument" >> "$PACKAGE_STATE"
                ;;
        esac
    done
fi
EOF

cat > "$FAKE_BIN/dpkg" <<'EOF'
#!/usr/bin/env bash
[ "$*" = '--print-architecture' ] || exit 2
printf 'amd64\n'
EOF

cat > "$FAKE_BIN/id" <<'EOF'
#!/usr/bin/env bash
if [ "$*" = '-nG pol' ]; then
    printf 'pol sudo nasusers\n'
else
    /usr/bin/id "$@"
fi
EOF

cat > "$FAKE_BIN/ss" <<'EOF'
#!/usr/bin/env bash
printf 'LISTEN 0 4096 192.168.1.34:2283 0.0.0.0:*\n'
EOF

cat > "$FAKE_BIN/curl" <<'EOF'
#!/usr/bin/env bash
printf 'curl:%s\n' "$*" >> "$EVENT_LOG"
case "$*" in
    *'http://192.168.1.34:2283/api/server/ping') printf '{"res":"pong"}\n' ;;
    *'https://download.docker.com/linux/debian/gpg'*)
        while (( $# > 0 )); do
            if [ "$1" = -o ]; then
                printf '%s\n' 'fake Docker signing key' > "$2"
                exit 0
            fi
            shift
        done
        exit 2
        ;;
    *) exit 22 ;;
esac
EOF

chmod +x "$FAKE_BIN"/*
export EVENT_LOG PACKAGE_STATE

printf 'docker.io\n' > "$PACKAGE_STATE"
if PATH="$FAKE_BIN:$PATH" \
    POL_SERVER_ALLOW_UNPRIVILEGED=true \
    POL_SERVER_ROOT="$FAKE_ROOT" \
    "$IMMICH" --install >/dev/null 2>&1; then
    fail 'install must refuse a conflicting distribution Docker package'
fi

: > "$PACKAGE_STATE"
PATH="$FAKE_BIN:$PATH" \
    POL_SERVER_ALLOW_UNPRIVILEGED=true \
    POL_SERVER_ROOT="$FAKE_ROOT" \
    "$IMMICH" --install >/dev/null

docker_source="$FAKE_ROOT/etc/apt/sources.list.d/docker.sources"
docker_key="$FAKE_ROOT/etc/apt/keyrings/docker.asc"
[ -r "$docker_source" ] || fail 'install must configure the official Docker apt source'
[ -r "$docker_key" ] || fail 'install must configure the official Docker signing key'
assert_file_contains "$docker_source" 'URIs: https://download.docker.com/linux/debian'
assert_file_contains "$docker_source" 'Suites: trixie'
assert_file_contains "$docker_source" 'Architectures: amd64'
for package in docker-ce docker-ce-cli containerd.io docker-buildx-plugin docker-compose-plugin; do
    grep -Fxq -- "$package" "$PACKAGE_STATE" || fail "install must add Docker package: $package"
done
grep -Fq 'systemctl:enable --now docker.service' "$EVENT_LOG" ||
    fail 'install must enable and start Docker'

PATH="$FAKE_BIN:$PATH" \
    POL_SERVER_ALLOW_UNPRIVILEGED=true \
    POL_SERVER_ROOT="$FAKE_ROOT" \
    "$IMMICH" --configure >/dev/null

environment_file="$FAKE_ROOT/etc/cuberhaus/immich.env"
[ -r "$environment_file" ] || fail 'configure must create the root-only Immich environment'
[ "$(stat -c %a "$environment_file")" = 600 ] || fail 'Immich environment must be mode 0600'
assert_file_contains "$environment_file" 'UPLOAD_LOCATION=/srv/storage/immich'
assert_file_contains "$environment_file" 'DB_DATA_LOCATION=/var/lib/immich/postgres'
assert_file_contains "$environment_file" 'IMMICH_VERSION=v3.1.0'
assert_file_contains "$environment_file" 'IMMICH_BIND_ADDRESS=192.168.1.34'
if grep -Fq 'DB_PASSWORD=postgres' "$environment_file"; then
    fail 'configure must replace the default database password'
fi
[ -d "$FAKE_ROOT/var/lib/immich/postgres" ] || fail 'configure must create the Micron database directory'
grep -Fq 'systemctl:enable --now pol-server-immich.service' "$EVENT_LOG" ||
    fail 'configure must enable and start the boot-managed Immich service'

check_output="$(
    PATH="$FAKE_BIN:$PATH" \
        POL_SERVER_ALLOW_UNPRIVILEGED=true \
        POL_SERVER_ROOT="$FAKE_ROOT" \
        "$IMMICH" --check
)"
grep -Fq 'Immich version: v3.1.0' <<< "$check_output" || fail 'check must report the pinned release'
grep -Fq 'Immich state: configured and healthy' <<< "$check_output" || fail 'check must verify the running stack'
grep -Fq 'Immich URL: http://192.168.1.34:2283' <<< "$check_output" || fail 'check must report the LAN-only URL'

: > "$EVENT_LOG"
PATH="$FAKE_BIN:$PATH" \
    POL_SERVER_ALLOW_UNPRIVILEGED=true \
    POL_SERVER_ROOT="$FAKE_ROOT" \
    "$IMMICH" --backup >/dev/null
dump_path="$(find "$FAKE_ROOT/srv/storage/immich/backups" -type f -name 'immich-db-backup-*.sql.gz' -print -quit)"
[ -n "$dump_path" ] || fail 'backup must create a compressed database dump in the Immich media tree'
gzip -t "$dump_path" || fail 'backup must create a valid gzip stream'
stop_line="$(grep -n -F ' stop immich-server' "$EVENT_LOG" | head -1 | cut -d: -f1)"
dump_line="$(grep -n -F ' exec -T database pg_dump ' "$EVENT_LOG" | head -1 | cut -d: -f1)"
start_line="$(grep -n -F ' start immich-server' "$EVENT_LOG" | head -1 | cut -d: -f1)"
if [ -z "$stop_line" ] || [ -z "$dump_line" ] || [ -z "$start_line" ] ||
    (( stop_line >= dump_line || dump_line >= start_line )); then
    fail 'backup must stop the server, dump PostgreSQL, then restart the server'
fi

: > "$EVENT_LOG"
export FAKE_PG_DUMP_FAIL=true
if PATH="$FAKE_BIN:$PATH" \
    POL_SERVER_ALLOW_UNPRIVILEGED=true \
    POL_SERVER_ROOT="$FAKE_ROOT" \
    "$IMMICH" --backup >/dev/null 2>&1; then
    fail 'backup must fail when pg_dump fails'
fi
unset FAKE_PG_DUMP_FAIL
grep -Fq ' start immich-server' "$EVENT_LOG" ||
    fail 'backup must restart the server after a failed database dump'

backup_command="$FAKE_ROOT/usr/local/lib/cuberhaus/pol-server/backup-setup"
install -d -m 0755 "$(dirname "$backup_command")"
cat > "$backup_command" <<'EOF'
#!/usr/bin/env bash
printf 'backup:%s\n' "$*" >> "$EVENT_LOG"
case "${1:-}" in
    --check) printf 'Backup state: configured\n' ;;
    --backup) [ "${FAKE_RESTIC_BACKUP_FAIL:-false}" != true ] ;;
    *) exit 2 ;;
esac
EOF
chmod +x "$backup_command"

: > "$EVENT_LOG"
export FAKE_RESTIC_BACKUP_FAIL=true
if PATH="$FAKE_BIN:$PATH" \
    POL_SERVER_ALLOW_UNPRIVILEGED=true \
    POL_SERVER_ROOT="$FAKE_ROOT" \
    "$IMMICH" --upgrade >/dev/null 2>&1; then
    fail 'upgrade must fail when the required Restic backup fails'
fi
unset FAKE_RESTIC_BACKUP_FAIL
if grep -Fq ' pull' "$EVENT_LOG"; then
    fail 'upgrade must not pull or restart containers after a failed backup'
fi

: > "$EVENT_LOG"
PATH="$FAKE_BIN:$PATH" \
    POL_SERVER_ALLOW_UNPRIVILEGED=true \
    POL_SERVER_ROOT="$FAKE_ROOT" \
    "$IMMICH" --upgrade >/dev/null
backup_line="$(grep -n -F 'backup:--backup' "$EVENT_LOG" | head -1 | cut -d: -f1)"
pull_line="$(grep -n -F ' pull' "$EVENT_LOG" | head -1 | cut -d: -f1)"
if [ -z "$backup_line" ] || [ -z "$pull_line" ] || (( backup_line >= pull_line )); then
    fail 'upgrade must complete the Restic backup before pulling images'
fi

recovery_library="$FAKE_ROOT/mnt/pol-server-backup/Immich/library"
for folder in library upload profile backups thumbs encoded-video; do
    mkdir -p "$recovery_library/$folder"
    : > "$recovery_library/$folder/.immich"
done
printf 'recovered photo\n' > "$recovery_library/library/photo.jpg"
printf 'CREATE TABLE recovered (id integer);\n' |
    gzip -c > "$recovery_library/backups/immich-db-backup-20260905T120000Z.sql.gz"

dry_run_output="$(
    PATH="$FAKE_BIN:$PATH" \
        POL_SERVER_ALLOW_UNPRIVILEGED=true \
        POL_SERVER_ROOT="$FAKE_ROOT" \
        "$IMMICH" --restore-existing
)"
grep -Fq 'Immich restore plan:' <<< "$dry_run_output" || fail 'restore must print its validated plan'
grep -Fq 'Dry run only.' <<< "$dry_run_output" || fail 'restore must default to a dry run'
[ ! -e "$FAKE_ROOT/srv/storage/immich/library/photo.jpg" ] || fail 'restore dry-run must not copy media'

mkdir -p "$FAKE_ROOT/srv/storage/immich/upload"
printf 'do not overwrite\n' > "$FAKE_ROOT/srv/storage/immich/upload/existing.jpg"
if PATH="$FAKE_BIN:$PATH" \
    POL_SERVER_ALLOW_UNPRIVILEGED=true \
    POL_SERVER_ROOT="$FAKE_ROOT" \
    "$IMMICH" --restore-existing-apply RESTORE-WD-IMMICH >/dev/null 2>&1; then
    fail 'restore must refuse a target containing user media'
fi
rm "$FAKE_ROOT/srv/storage/immich/upload/existing.jpg"

: > "$EVENT_LOG"
PATH="$FAKE_BIN:$PATH" \
    POL_SERVER_ALLOW_UNPRIVILEGED=true \
    POL_SERVER_ROOT="$FAKE_ROOT" \
    "$IMMICH" --restore-existing-apply RESTORE-WD-IMMICH >/dev/null
[ -r "$FAKE_ROOT/srv/storage/immich/library/photo.jpg" ] || fail 'restore apply must copy recovered media'
[ -r "$recovery_library/library/photo.jpg" ] || fail 'restore apply must preserve the WD recovery source'
find "$FAKE_ROOT/srv/storage" -maxdepth 1 -type d -name 'immich.pre-restore-*' -print -quit |
    grep -q . || fail 'restore apply must retain the prior media tree for rollback'
find "$FAKE_ROOT/var/lib/immich" -maxdepth 1 -type d -name 'postgres.pre-restore-*' -print -quit |
    grep -q . || fail 'restore apply must retain the prior database for rollback'
grep -Fq ' exec -T database psql ' "$EVENT_LOG" || fail 'restore apply must import the validated database dump'

printf 'pol-server Immich setup tests passed.\n'
