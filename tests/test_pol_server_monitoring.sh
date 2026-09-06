#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
MONITORING="$REPO_ROOT/server/pol-server/monitoring-setup"
CONFIG="$REPO_ROOT/server/pol-server/etc/netdata/netdata.conf"
CASE_DIR="$(mktemp -d)"
FAKE_ROOT="$CASE_DIR/root"
FAKE_BIN="$CASE_DIR/bin"
EVENT_LOG="$CASE_DIR/events.log"
PACKAGE_STATE="$CASE_DIR/packages"
SERVICE_STATE="$CASE_DIR/services"
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

[ -x "$MONITORING" ] || fail 'pol-server monitoring setup must exist and be executable'
[ -r "$CONFIG" ] || fail 'tracked Netdata configuration must exist'
assert_file_contains "$CONFIG" 'bind to = 127.0.0.1 192.168.1.34'
assert_file_contains "$CONFIG" 'default port = 19999'
assert_file_contains "$CONFIG" 'db = dbengine'
assert_file_contains "$CONFIG" 'dbengine tier 0 retention size = 2GiB'
assert_file_contains "$CONFIG" 'dbengine tier 0 retention time = 3mo'
if grep -Eq 'bind to[[:space:]]*=[[:space:]]*(\*|0\.0\.0\.0|::)' "$CONFIG"; then
    fail 'Netdata must not bind to a wildcard address'
fi
if grep -Fq '/var/run/docker.sock' "$CONFIG"; then
    fail 'Netdata must not receive Docker socket access'
fi

mkdir -p "$FAKE_ROOT/etc" "$FAKE_BIN"
: > "$EVENT_LOG"
: > "$PACKAGE_STATE"
: > "$SERVICE_STATE"

cat > "$FAKE_ROOT/etc/os-release" <<'EOF'
ID=debian
VERSION_ID="13"
EOF

cat > "$FAKE_BIN/dpkg-query" <<'EOF'
#!/usr/bin/env bash
package="${@: -1}"
grep -Fxq -- "$package" "$PACKAGE_STATE" || exit 1
if [[ "$*" == *'${Version}'* ]]; then
    printf '1.47.5-2\n'
else
    printf 'installed\n'
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
            */netdata-repo_5-5+debian13_all.deb)
                grep -Fxq netdata-repo "$PACKAGE_STATE" || printf '%s\n' netdata-repo >> "$PACKAGE_STATE"
                ;;
            *) grep -Fxq -- "$argument" "$PACKAGE_STATE" || printf '%s\n' "$argument" >> "$PACKAGE_STATE" ;;
        esac
    done
    [ "${FAKE_APT_FAILURE:-}" != install ] || exit 100
fi
[ "${FAKE_APT_FAILURE:-}" != update ] || exit 100
EOF

cat > "$FAKE_BIN/systemctl" <<'EOF'
#!/usr/bin/env bash
command_name="${1:-}"
shift || true
case "$command_name" in
    enable)
        [ "${1:-}" = --now ] && shift
        printf 'systemctl:enable --now %s\n' "$*" >> "$EVENT_LOG"
        printf '%s\n' "$@" > "$SERVICE_STATE"
        ;;
    is-enabled|is-active)
        grep -Fxq -- "${1:-}" "$SERVICE_STATE"
        ;;
    restart)
        printf 'systemctl:restart %s\n' "$*" >> "$EVENT_LOG"
        [ "${FAKE_RESTART_FAILURE:-false}" = false ]
        ;;
    disable)
        [ "${1:-}" = --now ] && shift
        printf 'systemctl:disable --now %s\n' "$*" >> "$EVENT_LOG"
        : > "$SERVICE_STATE"
        ;;
    *)
        printf 'Unexpected systemctl call: %s %s\n' "$command_name" "$*" >&2
        exit 2
        ;;
esac
EOF

cat > "$FAKE_BIN/ss" <<'EOF'
#!/usr/bin/env bash
owner=netdata
[ "${FAKE_FOREIGN_OWNER:-false}" = false ] || owner=python3
printf 'LISTEN 0 4096 192.168.1.34:19999 0.0.0.0:* users:(("%s",pid=42,fd=7))\n' "$owner"
printf 'LISTEN 0 4096 127.0.0.1:19999 0.0.0.0:* users:(("%s",pid=42,fd=8))\n' "$owner"
if [ "${FAKE_PUBLIC_LISTENER:-false}" = true ]; then
    printf 'LISTEN 0 4096 0.0.0.0:19999 0.0.0.0:* users:(("netdata",pid=42,fd=9))\n'
fi
EOF

cat > "$FAKE_BIN/curl" <<'EOF'
#!/usr/bin/env bash
[ "${FAKE_API_FAILURE:-false}" = false ] || exit 22
printf 'curl:%s\n' "$*" >> "$EVENT_LOG"
case "$*" in
    *netdata-repo_5-5+debian13_all.deb*)
        while [ "$#" -gt 0 ]; do
            if [ "$1" = -o ]; then
                printf 'pinned Netdata repository package\n' > "$2"
                exit
            fi
            shift
        done
        exit 2
        ;;
    */api/v1/info)
        printf '{"version":"v2.11.0","mirrored_hosts_status":[{"hostname":"pol-server","reachable":true}]}\n'
        ;;
    */netdata.conf) cat "$FAKE_ROOT/etc/netdata/netdata.conf" ;;
    *) exit 22 ;;
esac
EOF

cat > "$FAKE_BIN/sha256sum" <<'EOF'
#!/usr/bin/env bash
printf 'sha256sum:%s\n' "$*" >> "$EVENT_LOG"
cat >/dev/null
EOF

cat > "$FAKE_BIN/dpkg-deb" <<'EOF'
#!/usr/bin/env bash
printf 'dpkg-deb:%s\n' "$*" >> "$EVENT_LOG"
case "${3:-}" in
    Package) printf 'netdata-repo\n' ;;
    Version) printf '5-5\n' ;;
    Architecture) printf 'all\n' ;;
    *) exit 2 ;;
esac
EOF

chmod +x "$FAKE_BIN/dpkg-query" "$FAKE_BIN/apt-get" "$FAKE_BIN/systemctl" \
    "$FAKE_BIN/ss" "$FAKE_BIN/curl" "$FAKE_BIN/sha256sum" "$FAKE_BIN/dpkg-deb"
export EVENT_LOG PACKAGE_STATE SERVICE_STATE FAKE_ROOT
export PATH="$FAKE_BIN:$ORIGINAL_PATH"
export POL_SERVER_ROOT="$FAKE_ROOT"
export POL_SERVER_ALLOW_UNPRIVILEGED=true

"$MONITORING" --install

grep -Fxq netdata "$PACKAGE_STATE" || fail 'Netdata package must be installed'
grep -Fxq netdata-repo "$PACKAGE_STATE" || fail 'official Netdata repository package must be installed'
assert_file_contains "$EVENT_LOG" 'curl:-fsSL -o '
assert_file_contains "$EVENT_LOG" 'netdata-repo_5-5+debian13_all.deb'
assert_file_contains "$EVENT_LOG" 'sha256sum:-c -'
assert_file_contains "$EVENT_LOG" 'dpkg-deb:-f '
assert_file_contains "$EVENT_LOG" 'apt-get:install -y -o Dpkg::Options::=--force-confold netdata'
assert_file_contains "$EVENT_LOG" 'systemctl:enable --now netdata.service'
assert_file_contains "$FAKE_ROOT/etc/netdata/netdata.conf" 'bind to = 127.0.0.1 192.168.1.34'
assert_file_contains "$FAKE_ROOT/etc/netdata/netdata.conf" 'dbengine tier 0 retention size = 2GiB'
assert_file_contains "$FAKE_ROOT/etc/netdata/netdata.conf" 'dbengine tier 0 retention time = 3mo'
[ "$(stat -c '%a' "$FAKE_ROOT/etc/netdata/netdata.conf")" = 644 ] ||
    fail 'Netdata configuration must use mode 0644'
[ -e "$FAKE_ROOT/etc/netdata/.opt-out-from-anonymous-statistics" ] ||
    fail 'Netdata anonymous telemetry opt-out marker must be installed'
[ "$(stat -c '%a' "$FAKE_ROOT/etc/netdata/.opt-out-from-anonymous-statistics")" = 644 ] ||
    fail 'Netdata telemetry opt-out marker must use mode 0644'

: > "$EVENT_LOG"
"$MONITORING" --install
[ ! -s "$EVENT_LOG" ] || fail 'second monitoring install must not change converged state'

check_output="$("$MONITORING" --check)"
grep -Fq 'Netdata package installed' <<< "$check_output" || fail 'check must report package state'
grep -Fq 'Netdata repository package installed' <<< "$check_output" ||
    fail 'check must report repository package state'
grep -Fq '1.47.5-2' <<< "$check_output" || fail 'check must report the package version'
grep -Fq 'Netdata configuration current' <<< "$check_output" || fail 'check must report configuration state'
grep -Fq 'Netdata service active and enabled' <<< "$check_output" || fail 'check must report service state'
grep -Fq 'Netdata API healthy on loopback' <<< "$check_output" || fail 'check must query the local API'
grep -Fq 'Netdata listener restricted to loopback and 192.168.1.34:19999' <<< "$check_output" ||
    fail 'check must reject public listeners'
grep -Fq 'Netdata effective access and retention settings verified' <<< "$check_output" ||
    fail 'check must verify the effective Netdata configuration'

export FAKE_PUBLIC_LISTENER=true
if "$MONITORING" --check >/dev/null 2>&1; then
    fail 'check must reject a wildcard listener'
fi
unset FAKE_PUBLIC_LISTENER

export FAKE_FOREIGN_OWNER=true
if "$MONITORING" --check >/dev/null 2>&1; then
    fail 'check must reject listeners owned by another process'
fi
unset FAKE_FOREIGN_OWNER

export FAKE_API_FAILURE=true
if "$MONITORING" --check >/dev/null 2>&1; then
    fail 'check must report an unhealthy local API'
fi
unset FAKE_API_FAILURE

sed -i 's/192\.168\.1\.34/0.0.0.0/' "$FAKE_ROOT/etc/netdata/netdata.conf"
if "$MONITORING" --check >/dev/null 2>&1; then
    fail 'check must report configuration drift'
fi

rm -rf "$FAKE_ROOT/etc/netdata"
: > "$PACKAGE_STATE"
: > "$SERVICE_STATE"
: > "$EVENT_LOG"
export FAKE_APT_FAILURE=install
if "$MONITORING" --install >/dev/null 2>&1; then
    fail 'a failed package install must fail the monitoring setup'
fi
unset FAKE_APT_FAILURE
[ ! -e "$FAKE_ROOT/etc/netdata/netdata.conf" ] ||
    fail 'failed package install must remove a newly staged configuration'
[ ! -e "$FAKE_ROOT/etc/netdata/.opt-out-from-anonymous-statistics" ] ||
    fail 'failed package install must remove a newly staged telemetry marker'
assert_file_contains "$EVENT_LOG" 'systemctl:disable --now netdata.service'

mkdir -p "$FAKE_ROOT/etc/netdata"
printf 'original local configuration\n' > "$FAKE_ROOT/etc/netdata/netdata.conf"
printf 'netdata\n' > "$PACKAGE_STATE"
printf 'netdata.service\n' > "$SERVICE_STATE"
: > "$EVENT_LOG"
export FAKE_RESTART_FAILURE=true
if "$MONITORING" --install >/dev/null 2>&1; then
    fail 'a failed Netdata restart must fail the monitoring setup'
fi
unset FAKE_RESTART_FAILURE
assert_file_contains "$FAKE_ROOT/etc/netdata/netdata.conf" 'original local configuration'
[ ! -e "$FAKE_ROOT/etc/netdata/.opt-out-from-anonymous-statistics" ] ||
    fail 'failed restart must remove a newly staged telemetry marker'

printf 'pol-server monitoring tests passed.\n'