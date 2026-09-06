#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WIREGUARD="$REPO_ROOT/server/pol-server/wireguard-setup"
DEPLOY="$REPO_ROOT/server/pol-server/deploy"
BOOTSTRAP="$REPO_ROOT/server/pol-server/wireguard-bootstrap"
FIREWALL="$REPO_ROOT/server/pol-server/etc/nftables.d/pol-server-wireguard.nft"
DDNS_SERVICE="$REPO_ROOT/server/pol-server/etc/systemd/system/pol-server-duckdns.service"
DDNS_TIMER="$REPO_ROOT/server/pol-server/etc/systemd/system/pol-server-duckdns.timer"
CASE_DIR="$(mktemp -d)"
FAKE_ROOT="$CASE_DIR/root"
FAKE_BIN="$CASE_DIR/bin"
EVENT_LOG="$CASE_DIR/events.log"
PACKAGE_STATE="$CASE_DIR/packages"
ENABLED_STATE="$CASE_DIR/enabled"
ACTIVE_STATE="$CASE_DIR/active"
NFT_STATE="$CASE_DIR/nft-applied"
CURL_CONFIG="$CASE_DIR/curl-config"
WG_GENKEY_STATE="$CASE_DIR/wg-genkey-count"
ORIGINAL_PATH="$PATH"
SERVER_PRIVATE_KEY="$(printf '%*s' 32 '' | tr ' ' a | base64 -w0)"
SERVER_PUBLIC_KEY="$(printf '%*s' 32 '' | tr ' ' b | base64 -w0)"
PEER_PUBLIC_KEY="$(printf '%*s' 32 '' | tr ' ' c | base64 -w0)"
SECOND_PEER_PUBLIC_KEY="$(printf '%*s' 32 '' | tr ' ' d | base64 -w0)"
CLIENT_PRIVATE_KEY="$(printf '%*s' 32 '' | tr ' ' e | base64 -w0)"
DUCKDNS_TEST_VALUE=duckdns-test-value
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

[ -x "$WIREGUARD" ] || fail 'pol-server WireGuard setup must exist and be executable'
[ -x "$BOOTSTRAP" ] || fail 'interactive WireGuard bootstrap must exist and be executable'
[ -r "$FIREWALL" ] || fail 'tracked WireGuard peer firewall must exist'
[ -r "$DDNS_SERVICE" ] || fail 'tracked DuckDNS service must exist'
[ -r "$DDNS_TIMER" ] || fail 'tracked DuckDNS timer must exist'

assert_file_contains "$FIREWALL" 'iifname "wg0"'
assert_file_contains "$FIREWALL" 'ip saddr 10.77.0.2'
assert_file_contains "$FIREWALL" 'ip daddr 192.168.1.34'
assert_file_contains "$FIREWALL" 'tcp dport { 2283, 19999 } accept'
assert_file_contains "$FIREWALL" 'iifname "wg0" drop'
assert_file_contains "$DDNS_SERVICE" 'LoadCredential=duckdns-token:/etc/cuberhaus/duckdns-token'
assert_file_contains "$DDNS_TIMER" 'OnUnitActiveSec=5min'
assert_file_contains "$BOOTSTRAP" '--enroll-maintenance'
assert_file_contains "$BOOTSTRAP" '--revoke-maintenance'
assert_file_contains "$BOOTSTRAP" '--install-wireguard'
assert_file_contains "$BOOTSTRAP" '--generate-wireguard-client'
assert_file_contains "$BOOTSTRAP" '--check-wireguard'
assert_file_contains "$BOOTSTRAP" '--revoke-wireguard-client'
assert_file_contains "$BOOTSTRAP" 'sudo apt-get install -y'
if grep -Eq 'tcp dport (22|445)' "$FIREWALL"; then
    fail 'tracked WireGuard policy must not allow SSH or SMB'
fi

mkdir -p "$FAKE_ROOT/etc" "$FAKE_BIN"
: > "$EVENT_LOG"
: > "$PACKAGE_STATE"
: > "$ENABLED_STATE"
: > "$ACTIVE_STATE"
: > "$WG_GENKEY_STATE"

cat > "$FAKE_ROOT/etc/os-release" <<'EOF'
ID=debian
VERSION_ID="13"
EOF

cat > "$FAKE_BIN/dpkg-query" <<'EOF'
#!/usr/bin/env bash
package="${@: -1}"
grep -Fxq -- "$package" "$PACKAGE_STATE" || exit 1
printf 'installed\n'
EOF

cat > "$FAKE_BIN/apt-get" <<'EOF'
#!/usr/bin/env bash
printf 'apt-get:%s\n' "$*" >> "$EVENT_LOG"
[ "${FAKE_APT_FAILURE:-}" != "${1:-}" ] || exit 100
if [ "${1:-}" = install ]; then
    shift
    for argument in "$@"; do
        case "$argument" in
            -*) ;;
            *) grep -Fxq -- "$argument" "$PACKAGE_STATE" || printf '%s\n' "$argument" >> "$PACKAGE_STATE" ;;
        esac
    done
fi
EOF

cat > "$FAKE_BIN/wg" <<'EOF'
#!/usr/bin/env bash
case "${1:-}" in
    genkey)
        count="$(wc -l < "$WG_GENKEY_STATE")"
        printf 'generated\n' >> "$WG_GENKEY_STATE"
        if [ "$count" = 0 ]; then
            printf '%s\n' "$SERVER_PRIVATE_KEY"
        else
            printf '%s\n' "$CLIENT_PRIVATE_KEY"
        fi
        ;;
    pubkey)
        private_key="$(cat)"
        if [ "$private_key" = "$SERVER_PRIVATE_KEY" ]; then
            printf '%s\n' "$SERVER_PUBLIC_KEY"
        elif [ "$private_key" = "$CLIENT_PRIVATE_KEY" ]; then
            printf '%s\n' "$PEER_PUBLIC_KEY"
        else
            exit 1
        fi
        ;;
    show)
        case "${2:-}:${3:-}" in
            wg0:listen-port) printf '51820\n' ;;
            wg0:peers)
                awk '/^\[Peer\]/{ peer=1 } peer && /^PublicKey = /{ print $3 }' \
                    "$FAKE_ROOT/etc/wireguard/wg0.conf"
                ;;
            *) exit 2 ;;
        esac
        ;;
    *) exit 2 ;;
esac
EOF

cat > "$FAKE_BIN/ssh" <<'EOF'
#!/usr/bin/env bash
remote_command="${*: -1}"
case "$remote_command" in
    'sudo -n /usr/local/sbin/pol-server-wireguard --server-public-key')
        "$WIREGUARD" --server-public-key
        ;;
    'sudo -n /usr/local/sbin/pol-server-wireguard --enroll-pol-iphone-stdin')
        [ "${FAKE_SSH_ENROLL_FAILURE:-false}" = false ] || exit 1
        "$WIREGUARD" --enroll-pol-iphone-stdin
        ;;
    *)
        printf 'Unexpected SSH command: %s\n' "$remote_command" >&2
        exit 2
        ;;
esac
EOF

cat > "$FAKE_BIN/qrencode" <<'EOF'
#!/usr/bin/env bash
printf 'qrencode must not run during noninteractive tests\n' >&2
exit 2
EOF

cat > "$FAKE_BIN/nft" <<'EOF'
#!/usr/bin/env bash
case "$*" in
    'list table inet pol_server_wireguard')
        [ -e "$NFT_STATE" ] || exit 1
        cat "$FAKE_ROOT/etc/nftables.d/pol-server-wireguard.nft"
        ;;
    '--check --file '*)
        printf 'nft:check %s\n' "$*" >> "$EVENT_LOG"
        [ "${FAKE_NFT_FAILURE:-false}" = false ]
        ;;
    '--file '*)
        printf 'nft:apply %s\n' "$*" >> "$EVENT_LOG"
        [ "${FAKE_NFT_FAILURE:-false}" = false ] || exit 1
        touch "$NFT_STATE"
        ;;
    *) exit 2 ;;
esac
EOF

cat > "$FAKE_BIN/curl" <<'EOF'
#!/usr/bin/env bash
printf 'curl:%s\n' "$*" >> "$EVENT_LOG"
cat > "$CURL_CONFIG"
[ "${FAKE_DUCKDNS_FAILURE:-false}" = false ] || exit 22
printf 'OK\n'
EOF

cat > "$FAKE_BIN/sysctl" <<'EOF'
#!/usr/bin/env bash
printf 'sysctl:%s\n' "$*" >> "$EVENT_LOG"
EOF

cat > "$FAKE_BIN/systemctl" <<'EOF'
#!/usr/bin/env bash
command_name="${1:-}"
shift || true
mark_state() {
    local state_file="$1" unit="$2"
    grep -Fxq -- "$unit" "$state_file" || printf '%s\n' "$unit" >> "$state_file"
}
case "$command_name" in
    daemon-reload)
        printf 'systemctl:daemon-reload\n' >> "$EVENT_LOG"
        ;;
    enable)
        [ "${1:-}" = --now ] && shift
        printf 'systemctl:enable --now %s\n' "$*" >> "$EVENT_LOG"
        for unit in "$@"; do
            mark_state "$ENABLED_STATE" "$unit"
            mark_state "$ACTIVE_STATE" "$unit"
            if [ "$unit" = pol-server-wireguard-firewall.service ]; then
                "$WIREGUARD" --apply-firewall
            fi
        done
        ;;
    is-enabled)
        grep -Fxq -- "${1:-}" "$ENABLED_STATE"
        ;;
    is-active)
        grep -Fxq -- "${1:-}" "$ACTIVE_STATE"
        ;;
    reload)
        printf 'systemctl:reload %s\n' "$*" >> "$EVENT_LOG"
        [ "${FAKE_RELOAD_FAILURE:-false}" = false ] || exit 1
        [ "${1:-}" != pol-server-wireguard-firewall.service ] || "$WIREGUARD" --apply-firewall
        ;;
    restart)
        printf 'systemctl:restart %s\n' "$*" >> "$EVENT_LOG"
        [ "${FAKE_RESTART_FAILURE:-false}" = false ]
        ;;
    start)
        printf 'systemctl:start %s\n' "$*" >> "$EVENT_LOG"
        [ "${FAKE_DUCKDNS_FAILURE:-false}" = false ] || exit 1
        if [ "${1:-}" = pol-server-duckdns.service ]; then
            CREDENTIALS_DIRECTORY="$FAKE_ROOT/etc/cuberhaus" "$WIREGUARD" --update-duckdns
        fi
        ;;
    *)
        printf 'Unexpected systemctl call: %s %s\n' "$command_name" "$*" >&2
        exit 2
        ;;
esac
EOF

chmod +x "$FAKE_BIN/dpkg-query" "$FAKE_BIN/apt-get" "$FAKE_BIN/wg" \
    "$FAKE_BIN/nft" "$FAKE_BIN/curl" "$FAKE_BIN/sysctl" "$FAKE_BIN/systemctl" \
    "$FAKE_BIN/ssh" "$FAKE_BIN/qrencode"
export EVENT_LOG PACKAGE_STATE ENABLED_STATE ACTIVE_STATE NFT_STATE CURL_CONFIG
export WG_GENKEY_STATE SERVER_PRIVATE_KEY SERVER_PUBLIC_KEY PEER_PUBLIC_KEY
export SECOND_PEER_PUBLIC_KEY CLIENT_PRIVATE_KEY FAKE_ROOT WIREGUARD
export PATH="$FAKE_BIN:$ORIGINAL_PATH"
export POL_SERVER_ROOT="$FAKE_ROOT"
export POL_SERVER_ALLOW_UNPRIVILEGED=true

export FAKE_APT_FAILURE=install
if "$WIREGUARD" --install >/dev/null 2>&1; then
    fail 'failed package installation must fail WireGuard setup'
fi
unset FAKE_APT_FAILURE
[ ! -e "$FAKE_ROOT/etc/wireguard/wg0.conf" ] ||
    fail 'failed package installation must not stage a WireGuard configuration'

"$WIREGUARD" --install

grep -Fxq wireguard-tools "$PACKAGE_STATE" || fail 'wireguard-tools must be installed'
grep -Fxq nftables "$PACKAGE_STATE" || fail 'nftables must be installed'
assert_file_contains "$EVENT_LOG" 'apt-get:update'
assert_file_contains "$EVENT_LOG" 'apt-get:install -y wireguard-tools nftables'
assert_file_contains "$EVENT_LOG" 'sysctl:--system'
assert_file_contains "$EVENT_LOG" 'systemctl:enable --now pol-server-wireguard-firewall.service'
assert_file_contains "$EVENT_LOG" 'systemctl:enable --now wg-quick@wg0.service'
assert_file_contains "$EVENT_LOG" 'nft:check --check --file'
assert_file_contains "$EVENT_LOG" 'nft:apply --file'
assert_file_contains "$FAKE_ROOT/etc/wireguard/wg0.conf" 'Address = 10.77.0.1/24'
assert_file_contains "$FAKE_ROOT/etc/wireguard/wg0.conf" 'ListenPort = 51820'
if grep -Fq '[Peer]' "$FAKE_ROOT/etc/wireguard/wg0.conf"; then
    fail 'initial WireGuard installation must not invent a peer key'
fi
[ "$(stat -c '%a' "$FAKE_ROOT/etc/wireguard/wg0.conf")" = 600 ] ||
    fail 'WireGuard configuration must use mode 0600'
[ "$(stat -c '%a' "$FAKE_ROOT/etc/cuberhaus/wireguard-server-private-key")" = 600 ] ||
    fail 'WireGuard server private key must use mode 0600'
[ "$(stat -c '%a' "$FAKE_ROOT/etc/cuberhaus/wireguard-server-public-key")" = 644 ] ||
    fail 'WireGuard server public key must use mode 0644'

: > "$EVENT_LOG"
"$WIREGUARD" --install
[ ! -s "$EVENT_LOG" ] || fail 'second WireGuard install must not change converged state'

if printf 'not-a-key\n' | "$WIREGUARD" --enroll-pol-iphone-stdin >/dev/null 2>&1; then
    fail 'peer enrollment must reject malformed public keys'
fi
[ ! -e "$FAKE_ROOT/etc/cuberhaus/wireguard-pol-iphone-public-key" ] ||
    fail 'invalid peer enrollment must not persist state'

: > "$EVENT_LOG"
printf '%s\n' "$PEER_PUBLIC_KEY" | "$WIREGUARD" --enroll-pol-iphone-stdin
assert_file_contains "$FAKE_ROOT/etc/wireguard/wg0.conf" '# pol-iphone'
assert_file_contains "$FAKE_ROOT/etc/wireguard/wg0.conf" 'AllowedIPs = 10.77.0.2/32'
assert_file_contains "$EVENT_LOG" 'systemctl:restart wg-quick@wg0.service'
[ "$(stat -c '%a' "$FAKE_ROOT/etc/cuberhaus/wireguard-pol-iphone-public-key")" = 600 ] ||
    fail 'fixed peer state must use mode 0600'

check_output="$("$WIREGUARD" --check)"
grep -Fq 'WireGuard tools installed' <<< "$check_output" || fail 'check must report package state'
grep -Fq 'private key redacted' <<< "$check_output" || fail 'check must explicitly redact private keys'
grep -Fq 'allows only Immich and Netdata' <<< "$check_output" || fail 'check must report the peer policy'
grep -Fq 'listening on UDP 51820' <<< "$check_output" || fail 'check must report the listener'
grep -Fq 'Peer pol-iphone enrolled at 10.77.0.2/32' <<< "$check_output" ||
    fail 'check must report the fixed peer'
if grep -Fq "$SERVER_PRIVATE_KEY" <<< "$check_output"; then
    fail 'check must never print the server private key'
fi

original_config="$(cat "$FAKE_ROOT/etc/wireguard/wg0.conf")"
export FAKE_RESTART_FAILURE=true
if printf '%s\n' "$SECOND_PEER_PUBLIC_KEY" |
    "$WIREGUARD" --enroll-pol-iphone-stdin >/dev/null 2>&1; then
    fail 'a failed WireGuard restart must fail peer enrollment'
fi
unset FAKE_RESTART_FAILURE
[ "$(cat "$FAKE_ROOT/etc/cuberhaus/wireguard-pol-iphone-public-key")" = "$PEER_PUBLIC_KEY" ] ||
    fail 'failed peer enrollment must restore the prior peer key'
[ "$(cat "$FAKE_ROOT/etc/wireguard/wg0.conf")" = "$original_config" ] ||
    fail 'failed peer enrollment must restore the prior configuration'

"$WIREGUARD" --revoke-pol-iphone
[ ! -e "$FAKE_ROOT/etc/cuberhaus/wireguard-pol-iphone-public-key" ] ||
    fail 'peer revocation must remove peer state'
if grep -Fq '[Peer]' "$FAKE_ROOT/etc/wireguard/wg0.conf"; then
    fail 'peer revocation must remove the peer from wg0'
fi
"$WIREGUARD" --revoke-pol-iphone
printf '%s\n' "$PEER_PUBLIC_KEY" | "$WIREGUARD" --enroll-pol-iphone-stdin >/dev/null

: > "$EVENT_LOG"
printf '%s\n' "$DUCKDNS_TEST_VALUE" | "$WIREGUARD" --configure-duckdns-token
[ "$(stat -c '%a' "$FAKE_ROOT/etc/cuberhaus/duckdns-token")" = 600 ] ||
    fail 'DuckDNS token must use mode 0600'
assert_file_contains "$EVENT_LOG" 'systemctl:start pol-server-duckdns.service'
assert_file_contains "$EVENT_LOG" 'systemctl:enable --now pol-server-duckdns.timer'
assert_file_contains "$CURL_CONFIG" 'domains=pol-home-nas'
assert_file_contains "$CURL_CONFIG" "token=$DUCKDNS_TEST_VALUE"
if grep -Fq "$DUCKDNS_TEST_VALUE" "$EVENT_LOG"; then
    fail 'DuckDNS token must not appear in command arguments or logs'
fi

export FAKE_DUCKDNS_FAILURE=true
if printf '%s\n' abcdefabcdefabcdefabcdefabcdefab |
    "$WIREGUARD" --configure-duckdns-token >/dev/null 2>&1; then
    fail 'failed DuckDNS validation must fail credential enrollment'
fi
unset FAKE_DUCKDNS_FAILURE
[ "$(cat "$FAKE_ROOT/etc/cuberhaus/duckdns-token")" = "$DUCKDNS_TEST_VALUE" ] ||
    fail 'failed DuckDNS validation must restore the previous credential'

check_output="$("$WIREGUARD" --check)"
grep -Fq 'DuckDNS fallback timer active; credential redacted' <<< "$check_output" ||
    fail 'check must report configured DuckDNS without printing its credential'

export HOME="$CASE_DIR/home"
export XDG_CONFIG_HOME="$HOME/.config"
client_output="$("$DEPLOY" --host fake-nas --generate-wireguard-client)"
client_config="$HOME/.config/cuberhaus/secrets/pol-server-wireguard-pol-iphone.conf"
[ "$(stat -c '%a' "$client_config")" = 600 ] || fail 'local client profile must use mode 0600'
assert_file_contains "$client_config" "PrivateKey = $CLIENT_PRIVATE_KEY"
assert_file_contains "$client_config" "PublicKey = $SERVER_PUBLIC_KEY"
assert_file_contains "$client_config" 'Endpoint = pol-home-nas.duckdns.org:51820'
assert_file_contains "$client_config" 'AllowedIPs = 192.168.1.34/32'
assert_file_contains "$client_config" 'PersistentKeepalive = 25'
grep -Fq 'mode 0600' <<< "$client_output" || fail 'client generation must report private storage'
if grep -Fq "$CLIENT_PRIVATE_KEY" <<< "$client_output"; then
    fail 'client generation must never print the private key'
fi

original_client_config="$(cat "$client_config")"
export FAKE_SSH_ENROLL_FAILURE=true
if "$DEPLOY" --host fake-nas --generate-wireguard-client >/dev/null 2>&1; then
    fail 'failed remote peer enrollment must fail local client generation'
fi
unset FAKE_SSH_ENROLL_FAILURE
[ "$(cat "$client_config")" = "$original_client_config" ] ||
    fail 'failed peer enrollment must preserve the prior local client profile'

if "$DEPLOY" --host fake-nas --show-wireguard-qr >/dev/null 2>&1; then
    fail 'QR display must refuse noninteractive output'
fi
[ "$(wc -l < "$WG_GENKEY_STATE")" = 2 ] ||
    fail 'rerunning client enrollment must reuse rather than rotate the existing private key'

ORCHESTRATOR_LOG="$CASE_DIR/orchestrator.log"
cat > "$FAKE_BIN/fake-deploy" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$ORCHESTRATOR_LOG"
mode="${*: -1}"
[ "${FAKE_DEPLOY_FAILURE:-}" != "$mode" ]
EOF
chmod +x "$FAKE_BIN/fake-deploy"
export ORCHESTRATOR_LOG
export POL_SERVER_DEPLOY="$FAKE_BIN/fake-deploy"
export POL_SERVER_ALLOW_NONINTERACTIVE=true

printf 'y\nn\ny\ny\ny\ny\ny\ny\ny\ny\ny\n' | "$BOOTSTRAP" --host fake-nas >/dev/null
for mode in \
    --check \
    --check-immich \
    --check-monitoring \
    --enroll-maintenance \
    --install-wireguard \
    --configure-duckdns \
    --generate-wireguard-client \
    --show-wireguard-qr \
    --check-wireguard \
    --revoke-wireguard-client \
    --revoke-maintenance; do
    grep -Fq -- "--host fake-nas $mode" "$ORCHESTRATOR_LOG" ||
        fail "interactive bootstrap must run tracked mode: $mode"
done
[ "$(tail -n 1 "$ORCHESTRATOR_LOG")" = '--host fake-nas --revoke-maintenance' ] ||
    fail 'successful bootstrap must finish by revoking temporary maintenance'

: > "$ORCHESTRATOR_LOG"
export FAKE_DEPLOY_FAILURE=--install-wireguard
if "$BOOTSTRAP" --host fake-nas </dev/null >/dev/null 2>&1; then
    fail 'interactive bootstrap must fail when WireGuard deployment fails'
fi
unset FAKE_DEPLOY_FAILURE
[ "$(tail -n 1 "$ORCHESTRATOR_LOG")" = '--host fake-nas --revoke-maintenance' ] ||
    fail 'failed bootstrap must revoke temporary maintenance through its exit trap'

printf 'pol-server WireGuard contract passed.\n'