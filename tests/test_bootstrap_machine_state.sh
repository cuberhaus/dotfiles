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

# The inotify helper writes through `sudo tee` and `sudo sysctl -w`. Run the real
# `tee` against a temporary drop-in and only record `sysctl`, so no test can touch
# the host's kernel limits or /etc.
INOTIFY_SYSCTL_CONF="$CASE_DIR/99-inotify.conf"
export INOTIFY_SYSCTL_CONF
warn() { printf 'warn:%s\n' "$*" >> "$EVENT_LOG"; }
sudo() {
    printf 'sudo:%s\n' "$*" >> "$EVENT_LOG"
    case "${1:-}" in
        tee) shift; tee "$@" > /dev/null ;;
        sysctl) [ "${FAKE_SYSCTL_FAIL:-false}" != true ] ;;
    esac
}
running_inotify_watch_limit() { printf '%s\n' "$FAKE_RUNNING_LIMIT"; }
reset_inotify_case() {
    : > "$EVENT_LOG"
    rm -f "$INOTIFY_SYSCTL_CONF"
}

reset_inotify_case
FAKE_RUNNING_LIMIT=65536
configure_inotify_watches
grep -Fxq 'fs.inotify.max_user_watches=524288' "$INOTIFY_SYSCTL_CONF" ||
    fail 'a fresh machine must persist the inotify watch limit'
grep -Fxq 'sudo:sysctl -w fs.inotify.max_user_watches=524288' "$EVENT_LOG" ||
    fail 'a lower running inotify limit must be raised immediately'

FAKE_RUNNING_LIMIT=524288
: > "$EVENT_LOG"
configure_inotify_watches
[ ! -s "$EVENT_LOG" ] ||
    fail 'a converged inotify watch limit must not be reapplied'
[ "$(grep -c '^fs.inotify.max_user_watches=' "$INOTIFY_SYSCTL_CONF")" -eq 1 ] ||
    fail 'the persisted inotify watch limit must not be duplicated'

reset_inotify_case
printf 'fs.inotify.max_user_watches = 1048576\n' > "$INOTIFY_SYSCTL_CONF"
FAKE_RUNNING_LIMIT=1048576
configure_inotify_watches
[ ! -s "$EVENT_LOG" ] ||
    fail 'a higher inotify watch limit must never be lowered'

reset_inotify_case
printf '# local tuning\nfs.inotify.max_user_watches=65536\n' > "$INOTIFY_SYSCTL_CONF"
FAKE_RUNNING_LIMIT=65536
configure_inotify_watches
[ "$(tail -n 1 "$INOTIFY_SYSCTL_CONF")" = 'fs.inotify.max_user_watches=524288' ] ||
    fail 'a lower persisted limit must be superseded by a later assignment'
grep -Fxq '# local tuning' "$INOTIFY_SYSCTL_CONF" ||
    fail 'existing lines in the drop-in must be preserved'

reset_inotify_case
printf 'fs.inotify.max_user_watches=524288\n' > "$INOTIFY_SYSCTL_CONF"
FAKE_RUNNING_LIMIT=65536
configure_inotify_watches
[ "$(cat "$EVENT_LOG")" = 'sudo:sysctl -w fs.inotify.max_user_watches=524288' ] ||
    fail 'a persisted but not yet running limit must only be applied, not rewritten'

reset_inotify_case
FAKE_RUNNING_LIMIT=65536
INOTIFY_MAX_USER_WATCHES=1048576 configure_inotify_watches
grep -Fxq 'fs.inotify.max_user_watches=1048576' "$INOTIFY_SYSCTL_CONF" ||
    fail 'INOTIFY_MAX_USER_WATCHES must select the persisted limit'

reset_inotify_case
if INOTIFY_MAX_USER_WATCHES='lots' configure_inotify_watches 2> /dev/null; then
    fail 'a non-numeric inotify limit must be rejected'
fi
[ ! -s "$EVENT_LOG" ] && [ ! -e "$INOTIFY_SYSCTL_CONF" ] ||
    fail 'a rejected inotify limit must not change the machine'

reset_inotify_case
FAKE_RUNNING_LIMIT=65536
FAKE_SYSCTL_FAIL=true configure_inotify_watches ||
    fail 'a failed live sysctl write must not abort the bootstrap'
grep -q '^warn:' "$EVENT_LOG" ||
    fail 'a failed live sysctl write must warn the user'
grep -Fxq 'fs.inotify.max_user_watches=524288' "$INOTIFY_SYSCTL_CONF" ||
    fail 'the limit must stay persisted when the live write fails'

reset_inotify_case
uname() { printf 'Darwin\n'; }
configure_inotify_watches
unset -f uname
[ ! -s "$EVENT_LOG" ] && [ ! -e "$INOTIFY_SYSCTL_CONF" ] ||
    fail 'non-Linux systems have no inotify limit to configure'

for entrypoint in ubuntu arch manjaro work; do
    grep -Eq '^[[:space:]]*configure_inotify_watches$' \
        "$REPO_ROOT/.local/scripts/bootstrap/$entrypoint" ||
        fail "the $entrypoint bootstrap must configure the inotify watch limit"
done

printf 'Bootstrap machine-state tests passed.\n'