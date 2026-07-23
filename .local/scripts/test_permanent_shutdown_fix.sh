#!/usr/bin/env bash
# Hermetic regression test for the Pop!_OS/kernelstub shutdown-fix path.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly SCRIPT_DIR
readonly SCRIPT="$SCRIPT_DIR/permanent_shutdown_fix.sh"
test_dir="$(mktemp -d)"
trap 'rm -rf "$test_dir"' EXIT

mkdir -p "$test_dir/bin"
cat >"$test_dir/bin/id" <<'EOF'
#!/usr/bin/env bash
if [[ "$1" == '-u' ]]; then
    printf '%s\n' "${SHUTDOWN_FIX_TEST_UID:-0}"
else
    command id "$@"
fi
EOF
cat >"$test_dir/bin/kernelstub" <<EOF
#!/usr/bin/env bash
if [[ "\$1" == '-p' ]]; then
    echo 'kernel options: quiet splash'
else
    printf '%s\n' "\$*" >>'$test_dir/kernelstub.log'
fi
EOF
chmod +x "$test_dir/bin/id" "$test_dir/bin/kernelstub"

PATH="$test_dir/bin:$PATH" SHUTDOWN_FIX_BOOTLOADER=kernelstub bash "$SCRIPT" >"$test_dir/output"

for parameter in nvidia-drm.modeset=1 acpi=force pcie_port_pm=off acpi_osi=Linux; do
    grep -qx -- "-a $parameter" "$test_dir/kernelstub.log"
done
grep -qx -- '-d quiet' "$test_dir/kernelstub.log"
grep -qx -- '-d splash' "$test_dir/kernelstub.log"
grep -q 'Fix applied successfully.' "$test_dir/output"

if PATH="$test_dir/bin:$PATH" SHUTDOWN_FIX_TEST_UID=1000 SHUTDOWN_FIX_BOOTLOADER=kernelstub bash "$SCRIPT" >"$test_dir/non_root_output" 2>"$test_dir/non_root_error"; then
    echo "Expected non-root execution to fail." >&2
    exit 1
fi
grep -q 'This script must be run as root.' "$test_dir/non_root_error"
grep -q "Re-run it with: sudo $SCRIPT" "$test_dir/non_root_error"

source "$SCRIPT_DIR/bootstrap/work_functions"
sudo() {
    printf '%s\n' "$*" >"$test_dir/sudo.log"
}
HOME="$test_dir/home" shutdown_fix
grep -qx "bash $test_dir/home/.local/scripts/permanent_shutdown_fix.sh" "$test_dir/sudo.log"

echo "permanent_shutdown_fix kernelstub test passed."
