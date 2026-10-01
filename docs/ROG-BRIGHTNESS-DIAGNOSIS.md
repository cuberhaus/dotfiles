# ROG Internal-Panel Brightness Diagnosis

## Summary

On the ASUS ROG Strix SCAR 16 (G635LX) running in GPU MUX "Ultimate" (dGPU)
mode, neither the Fn brightness keys nor the GNOME slider changed the
built-in panel, while external monitors responded normally. Every software
layer reported success; the panel itself ignored the value.

The only backlight device the kernel registers is the firmware's
embedded-controller (EC) interface, `nvidia_wmi_ec_backlight`. It accepts and
reads back values but does not drive this panel. The kernel's automatic
selection prefers that interface over a native one, so the NVIDIA driver never
registers its own backlight.

The mitigation is the kernel parameter `acpi_backlight=native`, applied by
`.local/scripts/brightness_fix.sh` and run by the `work` bootstrap. It was
confirmed on this machine on 2026-10-01: after the reboot the kernel registers
the native `nvidia_0` backlight and brightness control works on the built-in
panel. See [Verification](#verification).

## Environment

- ASUS ROG Strix SCAR 16 G635LX (`board_name` `G635LX`), BIOS `G635LX.338`.
- Ubuntu 26.04.1, kernel `7.0.0-34-generic`, GNOME 50 on Wayland (mutter 50.1).
- Intel Arrow Lake iGPU plus NVIDIA RTX 5090 Mobile, NVIDIA open kernel module
  `595.91.07`.
- GPU MUX in dGPU mode: `/sys/devices/platform/asus-nb-wmi/gpu_mux_mode` is `0`
  and `dgpu_disable` is `0`. The panel is connector `card2-eDP-1` on the
  `nvidia` DRM device, and `i915` logs `failed to retrieve link info, disabling
  eDP`.
- Kernel command line from the shutdown fix:
  `nvidia-drm.modeset=1 acpi=force pcie_port_pm=off acpi_osi=Linux`.

## Confirmed failure path

1. The firmware exposes the NVIDIA EC backlight WMI interface (GUID
   `603E9613-EF25-4338-A3D0-C46177516DB7`, present under
   `/sys/bus/wmi/devices/`). The kernel's backlight-type detection
   (`drivers/acpi/video_detect.c`) prefers it over native drivers unless an
   `acpi_backlight=` command-line value is given, so `nvidia-wmi-ec-backlight`
   registers `/sys/class/backlight/nvidia_wmi_ec_backlight` (type `firmware`,
   maximum `100`) as the only backlight device.
2. Because the kernel picked the EC interface, `acpi_video_backlight_use_native()`
   is false, so the NVIDIA driver's registration (`nvkms_register_backlight` in
   `nvidia-modeset`) skips its native `nvidia_0` device and logs
   `nvidia-modeset: ACPI reported no NVIDIA native backlight available;
   attempting to use ACPI backlight.`
3. GNOME 50 sends the slider and Fn-key requests through mutter's
   `org.gnome.Mutter.DisplayConfig.SetBacklight`, then logind `SetBrightness`,
   then that sysfs file. `brightnessctl` and `changeBrightness` write the same
   file, so they fail identically. External monitors use DDC/CI and are
   unaffected.
4. The EC stores whatever it is given (`actual_brightness` follows
   `brightness`), but the panel does not react. A controlled 90%, 5%, 90% sweep
   through mutter showed no visible change on the built-in panel. At the time of
   writing the EC reported `1/100` while the screen was fully readable, so the
   stored value is unrelated to what the panel shows.

## Hypotheses tested

| Hypothesis | Result |
| --- | --- |
| Missing permissions on the sysfs file | Rejected: the `90-brightnessctl` udev rule and `video` group membership are in place, and writes succeed. |
| GNOME or mutter ignores the request | Rejected: mutter accepts `SetBacklight` and forwards it to logind and sysfs. |
| The Intel iGPU should control the panel | Rejected: in dGPU mode `i915` disables the eDP link and the panel belongs to the NVIDIA device. |
| The firmware EC interface is a ghost that the panel ignores | Confirmed: the sweep showed no change, and selecting the native backlight instead fixed it (see [Verification](#verification)). |

Anecdotal forum reports describe the same symptom and the same parameter on
other ASUS laptops in dGPU mode. They are context, not evidence for this
machine.

## Fix

Add `acpi_backlight=native` to the kernel command line. A command-line value
outranks the automatic detection, so the kernel stops selecting the EC
interface, `nvidia_wmi_ec_backlight` is no longer registered, and the NVIDIA
driver is expected to register its native `nvidia_0` backlight, which GNOME,
`brightnessctl`, and the Fn keys then use. This was derived from reading the
kernel and driver sources and then confirmed on the hardware (see
[Verification](#verification)).

```bash
.local/scripts/brightness_fix.sh --status    # read-only report
.local/scripts/brightness_fix.sh --dry-run   # preview the change, no root
sudo .local/scripts/brightness_fix.sh        # apply, then reboot
```

The script supports GRUB (`/etc/default/grub` plus `update-grub`) and Pop!_OS
`kernelstub`. It is idempotent, backs up `/etc/default/grub` to `.bak`,
restores it if `update-grub` fails, and refuses to overwrite a different
`acpi_backlight=` value unless `--force` is given. It only acts on boards listed
in `AFFECTED_BOARDS` that also expose the EC interface; other machines are
skipped, and `--force` overrides that after you confirm the same symptom.

`make bootstrap-work` runs it right after the shutdown fix. The order matters:
on a fresh install the shutdown fix rewrites the whole GRUB command line, while
this fix appends to it. If the shutdown fix runs again later, it still
recognises its own parameters in front of the appended one and skips.

## Verification

After the reboot:

```bash
.local/scripts/brightness_fix.sh --status
brightnessctl -l              # expect an nvidia_0 device of class backlight
brightnessctl set 50%         # the built-in panel should visibly change
```

Then use the Fn keys and the GNOME slider.

| Observation | Meaning | Action |
| --- | --- | --- |
| `nvidia_0` exists and the panel changes | The fix works | Record it below. |
| The parameter is active but no `nvidia_0` appears | NVIDIA offers no native backlight for this panel in this mode | Revert and see the next section. |
| `nvidia_0` exists but the panel does not change | The native path is also a ghost | Revert and see the next section. |
| The built-in panel is unusably dark | The native device started at a low level | Use the external monitor or a TTY, run `brightnessctl -d nvidia_0 set 50%`, or revert. |

Outcome on this machine (2026-10-01, first boot after applying the fix):
confirmed working. The read-only checks showed:

- `--status` reported `acpi_backlight=native` configured and active.
- `nvidia_wmi_ec_backlight` was gone; `nvidia_0` (type `raw`, maximum `100`)
  was the only backlight device, and `brightnessctl -l` listed it.
- The `ACPI reported no NVIDIA native backlight available` kernel message from
  the previous boot no longer appeared.
- `systemd-backlight@backlight:nvidia_0.service` was active, so the level is
  saved and restored for the new device.
- Brightness control on the built-in panel was confirmed by the user.

## Rollback

```bash
sudo .local/scripts/brightness_fix.sh --revert    # then reboot
```

The previous file is also at `/etc/default/grub.bak`; copy it back and run
`sudo update-grub` for a manual rollback. `GRUB_TIMEOUT=0` hides the GRUB menu
on this machine, so recover through the working external monitor or a TTY
rather than by editing the command line at boot.

## If it does not work

- Switch the GPU mode to Hybrid in the BIOS (untested here). The Intel iGPU
  then drives the panel, so `intel_backlight` should be used instead of the
  firmware EC device. This costs battery life and the MUX direct path.
- In Hybrid mode a DPCD-capable eDP panel may also need
  `i915.enable_dpcd_backlight=1`. That is untested here.
- Do not drop `acpi_osi=Linux` to change the firmware's answer: the shutdown fix
  depends on it.

## Adding another board

Confirm the same symptom (the EC device exists, accepts values, and the panel
ignores them), try `--force`, and add the DMI `board_name` to `AFFECTED_BOARDS`
in `.local/scripts/brightness_fix.sh` once it works. The hermetic test is
`bash tests/test_brightness_fix.sh`.
