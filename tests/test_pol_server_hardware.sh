#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
HARDWARE="$REPO_ROOT/server/pol-server/hardware-qualification"
CASE_DIR="$(mktemp -d)"
FAKE_BIN="$CASE_DIR/bin"
EVENT_LOG="$CASE_DIR/events.log"
ORIGINAL_PATH="$PATH"
trap 'rm -rf "$CASE_DIR"' EXIT

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

[ -x "$HARDWARE" ] || fail 'hardware qualification command must exist and be executable'
grep -Fq 'audit-pol-server-hardware:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose the redacted hardware audit'
grep -Fq 'start-pol-server-smart-long-kingston:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose the Kingston long test'
grep -Fq 'start-pol-server-smart-long-micron:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose the Micron long test'
grep -Fq 'test-pol-server-thermals:' "$REPO_ROOT/Makefile" || fail 'Makefile must expose the bounded thermal test'

mkdir -p "$FAKE_BIN"
: > "$EVENT_LOG"

cat > "$FAKE_BIN/lsblk" <<'EOF'
#!/usr/bin/env bash
case "$*" in
    '-dn -o PATH,MODEL')
        printf '%s\n' \
            '/dev/sda KINGSTON SA400S37960G' \
            '/dev/sdb Micron_1100_MTFDDAV256TBN'
        ;;
    '-nr -o MOUNTPOINTS /dev/sda') ;;
    '-nr -o MOUNTPOINTS /dev/sdb') printf '/\n/boot/efi\n[SWAP]\n' ;;
    *)
        printf 'Unexpected lsblk call: %s\n' "$*" >&2
        exit 2
        ;;
esac
EOF

cat > "$FAKE_BIN/blockdev" <<'EOF'
#!/usr/bin/env bash
case "${2:-}" in
    /dev/sda) printf '960197124096\n' ;;
    /dev/sdb) printf '256060514304\n' ;;
    *) exit 2 ;;
esac
EOF

cat > "$FAKE_BIN/smartctl" <<'EOF'
#!/usr/bin/env bash
printf 'smartctl:%s\n' "$*" >> "$EVENT_LOG"
if [ "${1:-}" = -t ]; then
    printf 'Please wait 120 minutes for test to complete.\n'
    exit 0
fi
cat <<'REPORT'
Serial Number: SECRET-SERIAL-MUST-NOT-LEAK
Self-test execution status:      (   0) The previous self-test routine completed
                                        without error or no self-test has ever been run.
SMART overall-health self-assessment test result: PASSED
  5 Reallocated_Sector_Ct   0x0033   100   100   010    Pre-fail  Always       -       0
199 UDMA_CRC_Error_Count    0x003e   100   100   000    Old_age   Always       -       37
# 1  Extended offline    Completed without error       00%      1000         -
REPORT
EOF

cat > "$FAKE_BIN/systemctl" <<'EOF'
#!/usr/bin/env bash
[ "$*" = '--failed --no-legend' ] || exit 2
EOF

cat > "$FAKE_BIN/stress-ng" <<'EOF'
#!/usr/bin/env bash
printf 'stress-ng:%s\n' "$*" >> "$EVENT_LOG"
EOF

cat > "$FAKE_BIN/findmnt" <<'EOF'
#!/usr/bin/env bash
printf '/dev/sdb2\n'
EOF

chmod +x "$FAKE_BIN"/*
export EVENT_LOG
export PATH="$FAKE_BIN:$ORIGINAL_PATH"
export POL_SERVER_ALLOW_UNPRIVILEGED=true
export POL_SERVER_BATTERY_ROOT="$CASE_DIR/no-battery"

report="$($HARDWARE --report)"
grep -Fq 'smartctl:-c -H -A -l selftest /dev/sda' "$EVENT_LOG" || fail 'report must request SMART execution status'
grep -Fq 'KINGSTON SA400S37960G: /dev/sda' <<< "$report" || fail 'report must identify the Kingston by model'
grep -Fq 'Micron_1100_MTFDDAV256TBN: /dev/sdb' <<< "$report" || fail 'report must identify the Micron by model'
grep -Fq 'UDMA_CRC_Error_Count' <<< "$report" || fail 'report must include relevant SMART counters'
if grep -Fq 'SECRET-SERIAL-MUST-NOT-LEAK' <<< "$report"; then
    fail 'report must redact disk serial numbers'
fi

: > "$EVENT_LOG"
"$HARDWARE" --start-smart-long kingston >/dev/null
grep -Fqx 'smartctl:-t long /dev/sda' "$EVENT_LOG" || fail 'Kingston test must target only the model-matched disk'
if grep -Fq '/dev/sdb' "$EVENT_LOG"; then
    fail 'Kingston test must not target the Micron disk'
fi

"$HARDWARE" --thermal-load >/dev/null
grep -Fqx 'stress-ng:--cpu 2 --cpu-load 60 --timeout 2m --metrics-brief --thermalstat 10' "$EVENT_LOG" ||
    fail 'thermal test must retain its fixed worker, load, duration, and telemetry limits'

if "$HARDWARE" --start-smart-long unknown >/dev/null 2>&1; then
    fail 'unknown disk aliases must be rejected'
fi

printf 'pol-server hardware qualification tests passed.\n'