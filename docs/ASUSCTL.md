# asusctl on ASUS laptops

## Summary

ASUS ships no Linux software for its laptops. The community project
[ASUS Linux](https://asus-linux.org/guides/asusctl-install/) provides
`asusctl` (command line) and `asusd` (system daemon) for the parts of the
hardware that the kernel exposes but a desktop does not manage: firmware
platform profiles, fan curves, the battery charge limit, and keyboard lighting.

`.local/scripts/asusctl_install.sh` installs a pinned release (6.3.8) on
supported ASUS laptops and skips every other machine, so profiles can call it
unconditionally. The `work` bootstrap calls it after the brightness fix and the
`ubuntu` bootstrap after the display configuration, through `asusctl_install`
in `bootstrap/base_functions`. A failure only warns and names the repair
command. The `arch`, `manjaro`, `mac`, and `ubuntu-windows` profiles do not
call it: the installer needs apt, and the ASUS Linux guide documents its own
package repository for Arch.

```bash
.local/scripts/asusctl_install.sh --status      # read-only report
.local/scripts/asusctl_install.sh --dry-run     # preview every step, no sudo
.local/scripts/asusctl_install.sh               # install or repair
.local/scripts/asusctl_install.sh --uninstall   # remove exactly what it added
make repair REPAIR=asusctl DRY_RUN=true         # the same preview through make
make repair REPAIR=asusctl                      # the same install through make
```

Run it as your normal user, not as root: it builds third-party code as you and
calls `sudo` only to install. `--unattended` (what the bootstrap passes in
unattended mode) uses `sudo -n` and stops with an explanation when credentials
are not cached, so authorize `sudo -v` first.

`.local/scripts/asusctl_lighting.sh` sets the keyboard backlight to a rainbow.
The `work` and `ubuntu` bootstraps run it right after the installer, through
`asusctl_lighting` in `bootstrap/base_functions`. It skips machines without
`asusctl` and keyboards that lack the effect (see
[Keyboard lighting](#keyboard-lighting)).

## Which machines

The installer acts only when all of these hold, and prints the first one that
fails when it skips:

| Check | Reason |
| --- | --- |
| DMI vendor starts with `ASUS` | Other vendors have no ASUS firmware interfaces. |
| DMI product family is a TUF, ROG, Zephyrus, Strix, Vivobook, ASUSLaptop, Zenbook, ProArt, TX Air, TX Gaming, or EXPERTBOOK family | These are the families the udev rule of the pinned release starts `asusd` for. |
| Running kernel is 6.19 or newer | The documented minimum; older kernels lack the drivers. |
| The `asus-nb-wmi` driver is bound | udev starts `asusd` when this driver binds, so the daemon would never run without it. |

The family list mirrors `data/asusd.rules` of the pinned release. Update both
when you change the pin (see [Updating the pinned release](#updating-the-pinned-release)).
`--force` skips the hardware and kernel checks, for example to try an unlisted
model.

## What the installer does

1. Checks the machine (above) and refuses to run as root.
2. Installs only the missing build dependencies with apt: `ca-certificates`,
   `git`, `make`, `build-essential`, `pkg-config`, `cargo`, `libudev-dev`,
   `libusb-1.0-0-dev`, `libclang-dev`, and the runtime `libusb-1.0-0`.
   `libclang-dev` is needed because a git dependency of the daemon generates
   bindings while it builds. The packages stay after an uninstall; they are
   ordinary development packages.
3. Clones the pinned tag from `https://gitlab.com/asus-linux/asusctl.git` and
   aborts unless `HEAD` is the pinned commit, so a moved tag or a tampered
   mirror never reaches the build.
4. Builds `asusctl`, `asusd`, and `asus-shutdown` with
   `cargo build --release --locked` as your user, in a temporary directory
   below `~/.cache` that is removed afterwards. It needs Rust 1.82 or newer
   (Ubuntu 26.04 ships 1.93). The graphical `rog-control-center` is not built.
5. Installs into a staging directory with upstream's own Makefile targets
   (`install-asusd`, `install-asus-shutdown`, `install-asusctl`,
   `install-data-asusd`, `prefix=/usr`) and refuses to continue unless every
   required file exists, nothing lands outside `/usr`, and no binary misses a
   shared library.
6. Copies the files into place with `sudo`, preserving modes, and records them
   in `/var/lib/cuberhaus/asusctl/manifest` (57 files for 6.3.8) together with
   `version` (the release and commit).
7. Reloads systemd and udev, restarts `asusd.service`, and enables
   `asus-shutdown.service`.
8. Applies the power-profile policy below.

Rerunning is safe. When the pinned release is installed and every recorded file
exists, it only makes sure `asusd` runs and the policy holds. An upgrade removes
the files that the previous manifest listed and the new release no longer
ships; files you added yourself are never touched.

It leaves another installation alone: if `/usr/bin/asusd`, `/usr/bin/asusctl`,
the same names under `/usr/local/bin`, or `/etc/systemd/system/asusd.service`
exist without this script's manifest, it skips. `--force` replaces such files,
except files that dpkg owns: remove that package instead. Do not combine this
installation with the Homebrew casks (below).

## Install route and trade-offs

The guide lists Debian-based distributions as unsupported because their kernels
are often older than 6.19, and says that on distributions it does not package
for, `asusctl` must be built from the source in the project's repository. For
Ubuntu 26.04 it documents Homebrew.

| Route | Advantages | Disadvantages |
| --- | --- | --- |
| Homebrew casks from `ublue-os/homebrew-tap`, the route the ASUS Linux guide documents for Ubuntu 26.04 | Prebuilt, so there is no compile and no build packages. | Needs Homebrew on the machine. The cask `asusctl-linux` (6.3.8 when checked on 2026-10-02) downloads a prebuilt Ubuntu 22.04 tarball from a personal GitHub repository, `daegalus/linux-app-builds`, verified only by the checksum in the cask. Its `sudo` postflight writes below `/opt/ublue-asusctl`, `/etc/systemd/system`, `/etc/udev/rules.d`, `/etc/dbus-1/system.d`, and `/etc/asusd`, and rewrites the unit's `ExecStart`. |
| Pinned source build (chosen) | The code comes from the project's own repository at a commit that is verified. Files land in the standard places under `/usr`, are recorded, and are removed exactly. No Homebrew. | The first build takes about 90 seconds of compiling (89 seconds measured in an ubuntu:26.04 container on this machine) plus downloads, needs the development packages, and needs network access to GitLab, GitHub (cargo also fetches the git repositories that the graphical app depends on, even though it is not built), and crates.io. |

The dependencies are locked by the release's `Cargo.lock` (`--locked`), so a
build uses the crate versions that upstream tested. The trust placed in
upstream's repository is the same as running its own `make install`.

## Power profiles and power-profiles-daemon

`asusd` manages the firmware platform profile and the CPU energy preference
itself. By default it switches them when the power source changes (AC selects
Performance, battery selects Quiet) and applies that choice every time the
daemon starts. GNOME's Power Mode is `power-profiles-daemon` (or `tuned`), which
controls the same files. The ASUS Linux guide warns that running both can cause
races and offers two options: disable the other daemon, or keep it and switch
off `asusd`'s profile management with three settings.

The installer takes the second option, because it keeps GNOME's Power Mode and
changes nothing outside `asusd`. While `power-profiles-daemon.service` or
`tuned.service` is running, it turns off these three D-Bus properties of
`xyz.ljones.Asusd` (object `/xyz/ljones`, interface `xyz.ljones.Platform`):
`ChangePlatformProfileOnAc`, `ChangePlatformProfileOnBattery`, and
`PlatformProfileLinkedEpp`. `asusd` stores the values in its own configuration,
`/etc/asusd/asusd.ron`, so they survive reboots; setting them through D-Bus
avoids editing a file that does not exist until the daemon first runs. The
installer never stops, disables, or masks the other daemon, and it does not
touch the settings when no other daemon runs.

The first start of `asusd` can change the firmware profile once, before the
settings take effect. The installer compares the profile before and after and
tells you when that happened (choose your Power Mode again in GNOME Settings or
with `powerprofilesctl set balanced`).

To let `asusd` manage profiles instead, as the guide's first option:

```bash
sudo systemctl disable --now power-profiles-daemon.service
for property in ChangePlatformProfileOnAc ChangePlatformProfileOnBattery PlatformProfileLinkedEpp; do
    sudo busctl --system set-property xyz.ljones.Asusd /xyz/ljones xyz.ljones.Platform "$property" b true
done
```

GNOME's Power Mode then disappears; use `asusctl profile` instead. The installer
leaves the settings alone from then on because no other daemon is running.

`asusd` also stops `nvidia-powerd` while on battery by default
(`disable_nvidia_powerd_on_battery` in `/etc/asusd/asusd.ron`). The installer
does not change that default.

## Keyboard lighting

`.local/scripts/asusctl_lighting.sh` sets the keyboard backlight to the
`rainbow-wave` effect, at medium speed, moving right. It needs no `sudo`: the
D-Bus policy of `asusd` lets your own user read and change the keyboard. It does
not set the brightness, and it leaves the AniMe Matrix display alone, because
that is a separate device (`xyz.ljones.Anime`) and not an Aura lighting device.

```bash
.local/scripts/asusctl_lighting.sh --dry-run        # checks everything, changes nothing
.local/scripts/asusctl_lighting.sh                  # apply the effect, or confirm it
.local/scripts/asusctl_lighting.sh --effect rainbow-cycle --speed high
make repair REPAIR=asusctl-lighting DRY_RUN=true    # the same preview through make
make repair REPAIR=asusctl-lighting                 # the same step through make
```

A skip prints its reason and exits successfully; a failure exits non-zero. The
script goes through these cases in order:

| Situation | Result |
| --- | --- |
| `asusctl` is not installed | Skips. |
| `busctl` is missing | Fails. `busctl` (part of systemd) is how the script reads what the keyboard supports. |
| `asusd` answers but exports no lighting device (an object with the `xyz.ljones.Aura` interface) | Skips: this model has no Aura keyboard. |
| `asusd` does not answer on the system bus | Fails and points to `systemctl status asusd.service`, because the daemon should be running once `asusctl` is installed. The script polls for up to 15 seconds first, which covers a daemon that has just started after a fresh install. |
| A lighting device does not list the effect in `SupportedBasicModes` | Skips and names the device. `asusctl aura effect` applies an effect to every lighting device and stops at the first one that refuses it, so running it would leave the devices half changed. |
| Every device already shows the effect, speed, and direction | Reports that and changes nothing. |
| Anything else | Runs `asusctl aura effect rainbow-wave --direction right --speed med`, then reads the result back from every device. |

The read-back uses the daemon's `LedModeData` property (`(uu(yyy)(yyy)ss)`: mode,
zone, two colours, speed, direction). It reads every device again, up to three
times one second apart, until all of them report the effect, because `asusd` can
refuse a read for a moment after a change. A device that does not report the
effect, or a property layout the script does not recognize, counts as a failure:
an answer the script cannot read is never taken for success.

The daemon stores the effect (in `/etc/asusd/aura_19b6.ron` on the G635LX; the
name carries the keyboard's USB product ID) and restores it at every boot, so the
step only has to succeed once per machine. A failure in a bootstrap only warns and
names the repair command. Rerunning the bootstrap or the repair applies the
configured effect again whenever the keyboard shows a different one, so it
replaces an effect that you chose by hand. Edit `EFFECT`, `SPEED`, and `DIRECTION`
at the top of the script to change what every machine gets, or pass `--effect`,
`--speed`, and `--direction` for a single run (`rainbow-cycle` has no direction).
The script accepts only `rainbow-wave` and `rainbow-cycle`, because it needs the
daemon's mode number for an effect to check support and confirm the result; use
`asusctl aura effect` directly for anything else.

The step never sets the brightness, but `asusd` raises a backlight that is `Off`
to `Med` whenever it applies an effect, so a keyboard that you had switched off
lights up the first time the step changes it. To go back to the daemon's default,
a static red, run `asusctl aura effect static -c a60000`.

To check the keyboard by hand (the device name differs per model; list the names
with `busctl --system --list tree xyz.ljones.Asusd`):

```bash
.local/scripts/asusctl_lighting.sh --dry-run
busctl --system get-property xyz.ljones.Asusd /xyz/ljones/aura/19b6_3_4 xyz.ljones.Aura LedModeData
# (uu(yyy)(yyy)ss) 3 0 166 0 0 0 0 0 "Med" "Right"   -> mode 3 is rainbow-wave
```

The mode numbers are `0` static, `1` breathe, `2` rainbow-cycle, `3` rainbow-wave,
and `4` and above the other effects. On 2026-10-02 the dry run above, against the
daemon of the G635LX, reported the keyboard as already showing `rainbow-wave`
(speed med, direction right).

## Not installed

- `rog-control-center`, the graphical front end. It needs more build
  dependencies and is optional; everything it shows is also reachable through
  `asusctl`.
- `asusd-user`, the per-user daemon that lets applications create AniMe Matrix
  sequences. The system daemon already drives this laptop's AniMe Matrix display
  (upstream lists the `G635L` board as an AniMe model, and `asusd` exports
  `/xyz/ljones/aura/anime` here), so its built-in animations and `asusctl anime`
  (images, GIFs, brightness, power saving) work without it.
- GPU switching tools. The ASUS Linux guide tells you to remove
  distribution-provided graphics switching such as `supergfxd` and `envycontrol`
  before setting up `asusctl`, and calls `supergfxctl` deprecated (its
  experimental replacement is Cardwire). The installer neither installs nor
  changes any of them; on this laptop the MUX mode is a firmware setting
  (`gpu_mux_mode`).

## Day-to-day

The installer does not set personal preferences. After installing, these are
the usual first steps:

```bash
asusctl info                 # model and the features the daemon found
asusctl profile list         # firmware platform profiles
asusctl battery limit 80     # stop charging at 80%, kept across reboots
```

A lower charge limit reduces battery wear on a laptop that is mostly plugged in.
Features depend on the model and the kernel, so `asusctl info` is the reference
for what works. On the G635LX with kernel 7.0 the PPT and TGP power limits fail
with `ENODEV`, because the kernel's per-board power-limit table has no entry for
this board; the entry first appears upstream in Linux v7.3-rc1.

## Verification

```bash
.local/scripts/asusctl_install.sh --status
systemctl status asusd.service asus-shutdown.service
journalctl -u asusd.service -n 50 --no-pager
busctl --system get-property xyz.ljones.Asusd /xyz/ljones xyz.ljones.Platform ChangePlatformProfileOnAc
powerprofilesctl get
```

`--status` reports the hardware checks, the installed version against the pin,
the state of `asusd.service`, and who owns the Power Mode. The property should
read `b false` while `power-profiles-daemon` runs, and the verdict at the end
tells you what to do when anything is off.

The installer was exercised on 2026-10-02 in a disposable ubuntu:26.04
container with the real script: status, dry run, installation as a non-root user
through `sudo`, an idempotent rerun, uninstall, and reinstall. The container has
no systemd, so service activation was skipped there. The installed units
passed `systemd-analyze verify`, the rule passed `udevadm verify`, the D-Bus
policy parsed as XML, and none of the three binaries missed a shared library.
The container had no system D-Bus either, so `asusd` itself stopped at the
missing bus socket; running it against the real hardware is the first
verification on a laptop.

## Uninstall

```bash
.local/scripts/asusctl_install.sh --uninstall --dry-run
.local/scripts/asusctl_install.sh --uninstall           # keeps /etc/asusd
.local/scripts/asusctl_install.sh --uninstall --purge   # also deletes /etc/asusd
```

It stops and disables the services, removes exactly the files in the manifest,
removes the state directory, reloads systemd and udev, and leaves files that you
added. `/etc/asusd` holds the daemon's settings (for example the battery charge
limit), so it stays unless you pass `--purge`. The `work` and `ubuntu`
uninstallers offer the same removal as an unchecked item.

## Updating the pinned release

1. Choose a release tag and read its commit:
   `git ls-remote https://gitlab.com/asus-linux/asusctl.git 'refs/tags/<tag>' 'refs/tags/<tag>^{}'`.
   For an annotated tag the second line (the peeled commit) is the one to pin.
2. Change `VERSION` and `COMMIT` at the top of `.local/scripts/asusctl_install.sh`.
3. In a checkout of the new tag, compare the families in `data/asusd.rules`
   with `is_supported_family`, `rust-version` in `Cargo.toml` with `MIN_RUST`,
   the install targets in the Makefile with `stage_files`, and the kernel
   requirement in the ASUS Linux guide with `MIN_KERNEL`. Update the file count
   in this document if the manifest size changes.
4. Compare `AuraModeNum` in `rog-aura/src/builtin_modes.rs` with `mode_number`,
   and the type of the `LedModeData` property (`asusd/src/aura_laptop/trait_impls.rs`,
   shown by `busctl introspect`) with `MODE_DATA_SIGNATURE`, in
   `.local/scripts/asusctl_lighting.sh`. If upstream renumbers the modes or changes
   the layout, the script would misread what a keyboard supports or has applied.
5. Run `bash tests/test_asusctl_install.sh` and
   `bash tests/test_asusctl_lighting.sh`, then repeat the real run in a
   disposable container. The hardware checks accept the variables
   `ASUSCTL_INSTALL_DMI_DIR`, `ASUSCTL_INSTALL_PLATFORM_DIR`, and
   `ASUSCTL_INSTALL_OSRELEASE_FILE`, which the test suite also uses to describe a
   machine without owning one.

## Tests

`bash tests/test_asusctl_install.sh` runs the installer against a fake machine:
temporary system directories, fake `sudo`, `apt-get`, `cargo`, `systemctl`, and
`busctl`, and a local git repository standing in for upstream. It covers the
hardware gates, the pin check, staging, unattended mode, the profile policy,
upgrades and stale files, uninstall, status, and the bootstrap and repair
wiring. `bash tests/test_bootstrap_work.sh` covers the position of the steps in
the `work` bootstrap and that a failure does not stop provisioning.

`bash tests/test_asusctl_lighting.sh` runs the lighting script against a fake
`busctl` and `asusctl` that keep per-device state, so it never touches a real
keyboard. It covers every case in the table above (including devices that sort
before and after the keyboard), the wait for a daemon that has just started,
repeat runs, effects changed by hand, the options, the dry run, failures that
only the read-back can see, a busy daemon, and the wiring into the bootstraps,
the repair step, and `make test`.
