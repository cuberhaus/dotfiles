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

# Help answers on stdout with exit 0; a missing step is a usage error on stderr with exit 2. Neither
# may touch the backlight.
: > "$BRIGHTNESSCTL_LOG"
"$SCRIPT" -h > "$CASE_DIR/help.out" 2> "$CASE_DIR/help.err" \
    || fail '-h must exit 0'
grep -Fq 'Usage: changeBrightness STEP' "$CASE_DIR/help.out" \
    || fail '-h must print the usage on stdout'
[ ! -s "$CASE_DIR/help.err" ] || fail '-h must not write to stderr'

status=0
"$SCRIPT" > "$CASE_DIR/usage.out" 2> "$CASE_DIR/usage.err" || status=$?
[ "$status" -eq 2 ] || fail "a call without a step must exit 2, got $status"
grep -Fq 'Usage: changeBrightness STEP' "$CASE_DIR/usage.err" \
    || fail 'a call without a step must print the usage on stderr'
[ ! -s "$CASE_DIR/usage.out" ] || fail 'a call without a step must keep stdout empty'
[ ! -s "$BRIGHTNESSCTL_LOG" ] || fail 'help and usage errors must not touch the backlight'

printf 'PASS: brightness control uses brightnessctl for both directions\n'