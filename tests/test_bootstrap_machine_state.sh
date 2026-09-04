#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CASE_DIR="$(mktemp -d)"
EVENT_LOG="$CASE_DIR/events.log"
FAKE_BIN="$CASE_DIR/bin"
trap 'rm -rf "$CASE_DIR"' EXIT

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

export HOME="$CASE_DIR/home"
export DOTFILES_ROOT="$REPO_ROOT"
export EVENT_LOG
mkdir -p "$HOME" "$FAKE_BIN"

source "$REPO_ROOT/.local/scripts/bootstrap/base_functions"
source "$REPO_ROOT/.local/scripts/bootstrap/arch_functions"
source "$REPO_ROOT/.local/scripts/bootstrap/ubuntu_functions"

info() { :; }
sudo() { printf 'sudo:%s\n' "$*" >> "$EVENT_LOG"; }
record_package() { printf 'package:%s\n' "$*" >> "$EVENT_LOG"; }
trackpad_scrolling() { printf 'trackpad\n' >> "$EVENT_LOG"; }
laptop-detect() { [ "${FAKE_IS_LAPTOP:-false}" = true ]; }
pac=record_package

FAKE_IS_LAPTOP=false
laptop_install
[ ! -s "$EVENT_LOG" ] || fail 'desktop machines must skip laptop configuration'

FAKE_IS_LAPTOP=true
laptop_install
expected_laptop=$'package:tlp\nsudo:systemctl enable --now tlp.service\ntrackpad'
[ "$(cat "$EVENT_LOG")" = "$expected_laptop" ] ||
    fail 'laptop configuration did not converge the expected state'

: > "$EVENT_LOG"
cat > "$FAKE_BIN/systemctl" <<'EOF'
#!/usr/bin/env bash
printf 'systemctl:%s\n' "$*" >> "$EVENT_LOG"
if [ "${FAKE_NTPD_PRESENT:-false}" = true ]; then
    printf 'ntpd.service enabled\n'
fi
EOF
chmod +x "$FAKE_BIN/systemctl"
export PATH="$FAKE_BIN:$PATH"
export FAKE_NTPD_PRESENT=true
configure_arch_services
grep -Fxq 'sudo:systemctl disable --now ntpd.service' "$EVENT_LOG" ||
    fail 'an installed ntpd service must be disabled'

: > "$EVENT_LOG"
export FAKE_NTPD_PRESENT=false
configure_arch_services
if grep -Fq 'disable --now ntpd.service' "$EVENT_LOG"; then
    fail 'an absent ntpd service must not be disabled'
fi

: > "$EVENT_LOG"
id() {
    case "$1" in
        -un) printf 'bootstrap-user\n' ;;
        -nG) printf '%s\n' "$FAKE_GROUPS" ;;
        *) return 2 ;;
    esac
}
FAKE_GROUPS=libvirt
ensure_virtualization_groups
[ "$(cat "$EVENT_LOG")" = 'sudo:usermod -aG kvm bootstrap-user' ] ||
    fail 'only missing virtualization group membership should be added'

: > "$EVENT_LOG"
FAKE_GROUPS='libvirt kvm'
ensure_virtualization_groups
[ ! -s "$EVENT_LOG" ] ||
    fail 'existing virtualization group membership must not be changed'

printf 'Bootstrap machine-state tests passed.\n'