#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT="$REPO_ROOT/.local/scripts/bin/changeBrightness"

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

CASE_DIR="$(mktemp -d)"
trap 'rm -rf "$CASE_DIR"' EXIT

cat > "$CASE_DIR/brightnessctl" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$BRIGHTNESSCTL_LOG"
if [ "${1:-}" = -m ]; then
    printf '%s\n' 'nvidia_wmi_ec_backlight,backlight,21,100,21%'
fi
EOF
chmod +x "$CASE_DIR/brightnessctl"

cat > "$CASE_DIR/dunstify" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$DUNSTIFY_LOG"
EOF
chmod +x "$CASE_DIR/dunstify"

export BRIGHTNESSCTL_LOG="$CASE_DIR/brightnessctl.log"
export DUNSTIFY_LOG="$CASE_DIR/dunstify.log"
export PATH="$CASE_DIR:$PATH"

"$SCRIPT" 5
grep -Fxq 'set 5%+' "$BRIGHTNESSCTL_LOG" \
    || fail 'positive changes must use brightnessctl increase syntax'
grep -Fq 'Brightness: 21%' "$DUNSTIFY_LOG" \
    || fail 'brightness notification must report the current value'

: > "$BRIGHTNESSCTL_LOG"
"$SCRIPT" -5
grep -Fxq 'set 5%-' "$BRIGHTNESSCTL_LOG" \
    || fail 'negative changes must use brightnessctl decrease syntax'

printf 'PASS: brightness control uses brightnessctl for both directions\n'