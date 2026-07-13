#!/usr/bin/env bash
# Hermetic regression test for the Pop!_OS/kernelstub shutdown-fix path.
set -euo pipefail

readonly SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
readonly SCRIPT="$SCRIPT_DIR/permanent_shutdown_fix.sh"
test_dir="$(mktemp -d)"
trap 'rm -rf "$test_dir"' EXIT

mkdir -p "$test_dir/bin"
cat >"$test_dir/bin/sudo" <<'EOF'
#!/usr/bin/env bash
exec "$@"
EOF
cat >"$test_dir/bin/kernelstub" <<EOF
#!/usr/bin/env bash
if [[ "\$1" == '-p' ]]; then
    echo 'kernel options: quiet splash'
else
    printf '%s\n' "\$*" >>'$test_dir/kernelstub.log'
fi
EOF
chmod +x "$test_dir/bin/sudo" "$test_dir/bin/kernelstub"

PATH="$test_dir/bin:$PATH" SHUTDOWN_FIX_BOOTLOADER=kernelstub "$SCRIPT" >"$test_dir/output"

for parameter in nvidia-drm.modeset=1 acpi=force pcie_port_pm=off acpi_osi=Linux; do
    grep -qx -- "-a $parameter" "$test_dir/kernelstub.log"
done
grep -qx -- '-d quiet' "$test_dir/kernelstub.log"
grep -qx -- '-d splash' "$test_dir/kernelstub.log"
grep -q 'Fix applied successfully.' "$test_dir/output"

echo "permanent_shutdown_fix kernelstub test passed."