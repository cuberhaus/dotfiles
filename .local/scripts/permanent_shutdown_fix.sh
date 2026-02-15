#!/bin/bash
set -e

# Description:
# This script applies the fix for the PC not shutting down properly.
# 1. It adds 'acpi=force', 'nvidia-drm.modeset=1', 'pcie_port_pm=off' and 'acpi_osi=Linux' to handle power management and graphics drivers.
# 2. It REMOVES 'quiet splash' to enable VERBOSE LOGGING during shutdown.
#    This ensures you can see the commands executing while the system shuts down.
# Idempotent: skips if GRUB already has the expected parameters.

EXPECTED='nvidia-drm.modeset=1 acpi=force pcie_port_pm=off acpi_osi=Linux'

if grep -qF "$EXPECTED" /etc/default/grub 2>/dev/null; then
    echo "GRUB shutdown fix already applied, skipping."
    exit 0
fi

echo "Starting shutdown fix application..."

# 1. Backup existing GRUB config
echo "Backing up /etc/default/grub to /etc/default/grub.bak.$(date +%F_%T)..."
sudo cp --no-clobber /etc/default/grub "/etc/default/grub.bak.$(date +%F_%T)" || echo "Backup already exists for this second, skipping copy."

# 2. Apply the GRUB configuration
# We explicitly overwrite the line to ensure 'quiet splash' is GONE and our parameters are PRESENT.
echo "Configuring GRUB parameters..."
echo "Setting GRUB_CMDLINE_LINUX_DEFAULT to '$EXPECTED'"
sudo sed -i "s/^GRUB_CMDLINE_LINUX_DEFAULT=.*/GRUB_CMDLINE_LINUX_DEFAULT=\"$EXPECTED\"/" /etc/default/grub

# 3. Verify the change
echo "Verification - Current GRUB Line:"
grep "GRUB_CMDLINE_LINUX_DEFAULT" /etc/default/grub

# 4. Update GRUB to apply changes to the bootloader
echo "Running update-grub to apply changes..."
sudo update-grub

echo "--------------------------------------------------------"
echo "Fix applied successfully."
echo "NOTE: 'quiet splash' has been removed."
echo "You will now see text scrolling (commands executing) during boot and shutdown."
echo "Please reboot your system to verify."
