# .local directory

Everything under `.local/` follows the
[XDG Base Directory](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
convention and is symlinked into `$HOME/.local/` by GNU Stow.

## Directory layout

```text
.local/
├── etc/                        # Miscellaneous config snippets
│   ├── launch.json             # VSCode-style debug launch config
│   ├── tasks.json              # VSCode-style task definitions
│   └── .ycm_extra_conf.py     # YouCompleteMe C/C++ flags
│
├── scripts/                    # Shell scripts and automation
│   ├── bin/                    # User scripts added to $PATH
│   │   ├── changeBrightness   # Brightness control (used by i3/xmonad)
│   │   ├── changeVolume       # Volume control with notification
│   │   ├── cleanup            # Free disk space; asks before deleting each unused Docker volume
│   │   ├── clone-all          # Clone all repos from a GitHub user
│   │   ├── git-recurse        # Run git commands across multiple repos
│   │   ├── logout-all         # Sign out of browsers, editors and CLIs (--close-apps quits them first); --audit lists stored credentials
│   │   ├── match-monitor-scales # Give every monitor the same scale on GNOME (stops the Chromium fullscreen wiggle)
│   │   ├── vault-secret       # Access SOPS-encrypted vault credentials
│   │   ├── program            # Launch-or-focus helper for scratchpads
│   │   ├── prompt             # Custom prompt helper
│   │   └── yolo               # Stage everything, commit and push with no review
│   │
│   ├── bootstrap/              # OS-specific bootstrap scripts
│   │   ├── arch               # Arch Linux bootstrap entrypoint
│   │   ├── manjaro            # Manjaro bootstrap entrypoint
│   │   ├── ubuntu             # Ubuntu bootstrap entrypoint
│   │   ├── mac                # macOS bootstrap entrypoint
│   │   ├── gentoo             # Gentoo bootstrap entrypoint (experimental)
│   │   ├── base_functions     # Shared helpers (logging, $DOTFILES, prep)
│   │   ├── arch_functions     # Arch/Manjaro package lists & installers
│   │   ├── ubuntu_functions   # Ubuntu package lists & installers
│   │   ├── mac_functions      # macOS (Homebrew) package lists & installers
│   │   ├── gentoo_functions   # Gentoo (Portage, OpenRC/systemd) helpers
│   │   ├── gentoo.packages    # Gentoo package manifest: plain atoms in sections
│   │   └── xterm-256color-italic.terminfo
│   ├── automation/             # Scheduled package updates and workspace pulls
│   ├── audit_installation.py   # Read-only installation alignment report
│   │   ├── install             # Installs systemd timers or launchd agents
│   │   ├── system-maintenance  # Root apt/pacman upgrades (Linux)
│   │   ├── user-package-maintenance # Homebrew/yay upgrades
│   │   └── workspace-pull      # Safe recursive pull of ~/cuberhaus
│   │
│   ├── cinnamon_path/          # Scripts added to $PATH on Cinnamon DE
│   │   ├── cinnamon_load_config
│   │   ├── cinnamon_dump_config
│   │   ├── light-theme
│   │   └── dark-theme
│   │
│   ├── gnome_path/             # Scripts added to $PATH on GNOME DE
│   │   ├── gnome_load_config
│   │   └── gnome_dump_config
│   │
│   ├── hooks/                  # Git hooks
│   │   └── pre-commit         # Runs shellcheck on staged shell scripts
│   │
│   ├── asusctl_install.sh      # Builds and installs the pinned asusctl/asusd release on supported ASUS laptops
│   ├── asusctl_lighting.sh     # Sets the keyboard backlight to a rainbow through asusctl when the keyboard supports it
│   ├── brightness_fix.sh       # Selects the native NVIDIA backlight on ASUS ROG laptops via kernelstub or GRUB
│   ├── lint.sh                 # Lint all tracked shell scripts with shellcheck
│   ├── match_monitor_scales.py # Python module behind match-monitor-scales: plans and applies equal monitor scales through mutter and gdctl
│   ├── permanent_shutdown_fix.sh # Applies shutdown kernel parameters via kernelstub or GRUB
│   ├── toggle_theme            # Switch between light/dark themes
│   ├── ycm.sh                  # Builds or verifies YouCompleteMe's compiled core and bundled clangd
│   └── ...                     # Other utility scripts
│
├── share/                      # XDG data files
│   ├── fonts/                  # Nerd Fonts (SourceCodePro, UbuntuMono, etc.)
│   ├── icons/                  # Avatar and hicolor icon overrides
│   ├── nvim/                   # Neovim data
│   └── xfce4/                  # XFCE4 terminal color schemes
│
└── xdg/
    └── wallpapers/             # Bundled wallpapers
```

## How scripts are loaded

- **`bin/`** is added to `$PATH` by `.zshenv` for Zsh. pipx applications in
  `$HOME/.local/bin` are exposed by `.zshenv` for Zsh and `.bashrc` for Bash;
  bootstrap scripts do not let `pipx ensurepath` rewrite startup files.
- **`vault-secret`** opens a dynamic credential selector for the Obsidian
  vault's SOPS store. Use `vault-secret list`, `vault-secret <entry>`, or
  `vault-secret edit` for direct operations; set `VAULT_ROOT` when the vault is
  outside its standard checkout locations. `vault-secret key-wrap` keeps the
  age identity passphrase-protected (`keys.txt.gpg`, unlocked through
  gpg-agent) instead of a plain `keys.txt` that anyone with sudo can read, and
  `vault-secret key-status` shows where the identity comes from and whether it
  opens the store.
- **`cinnamon_path/`** and **`gnome_path/`** are conditionally added to
  `$PATH` based on the `$DESKTOP_SESSION` environment variable (see `.zshenv`).
- **`bootstrap/`** scripts are run via `make bootstrap-<os>` (see the root
  Makefile) and are **not** on `$PATH`.
- **`automation/`** contains shared jobs invoked by `systemd` on Linux and
  `launchd` on macOS. `make install-automations` installs or refreshes their
  native scheduler definitions; `make uninstall-automations-dry-run` previews
  removal and `make uninstall-automations` disables/removes them.
- **`audit_installation.py`** statically reads the selected bootstrap instead
  of sourcing it, then reports checkout, Stow, package, and scheduler drift via
  `make audit-installation`. Set `PROFILE=<name>` to override auto-detection,
  which reads `DOTFILES_PROFILE` (recorded by `bootstrap/work`, whose `DISTRO`
  must stay `ubuntu`), then `DISTRO`, then the operating system. It also
  reports packages that are installed but declared nowhere as `[NOTICE]`
  findings, which never change the exit code, and the apps that came with their
  own installer (the launchers in `~/.local/share/applications` that start a
  program from the home folder and that the bootstrap does not declare), with
  a `Fix:` command under an app when the place of its program proves what
  belongs to it (an AppImage, an app folder in an install prefix, a Qt
  installer's folder) and none otherwise; `--list-extra` prints them as
  `manager:name` and `app:name`.

## Bootstrap flow

Most bootstrap entrypoints follow this pattern:

1. Source `base_functions` (logging, `$DOTFILES` auto-detection, common prep).
2. Source the OS-specific `*_functions` file (package lists, installers).
3. Run system update.
4. Call installer functions in dependency order, converging machine-specific
  state from hardware, service, and group checks.
5. Switch default shell to zsh.
6. Install package-maintenance and workspace-pull schedules through the root
  Makefile target.

The work bootstrap mirrors all terminal output to a persistent per-run log at
`${XDG_STATE_HOME:-$HOME/.local/state}/cuberhaus/bootstrap/work-<UTC timestamp>-<PID>.log`.
The log path is printed when the bootstrap starts. Its source-safe `work_main`
entrypoint is exercised with destructive stages replaced by test doubles in
`tests/test_bootstrap_work.sh`. Bootstrap Make targets are unattended by
default; authorize `sudo` first with `sudo -v`, and the entrypoint then uses
noninteractive sudo and apt behavior. Pass `BOOTSTRAP_ARGS=` to a direct target
to restore interactive prompts.
After provisioning, the shared `bootstrap-workspace` Make target clones the
authenticated private workspace repository when absent, restores its pinned
skills by default, retries an incomplete initial restore, and runs its Linux
sync script.

The Gentoo bootstrap deviates from this pattern on purpose: it is experimental
and has never run on a real machine, so every change goes through one dry-run
chokepoint (`make bootstrap-gentoo-dry-run`), packages come from the
`gentoo.packages` manifest rather than inline lists, and it is excluded from
`make audit-installation` and profile detection until proven. Its assumptions,
privileges, recovery steps, and unsupported choices are in
[docs/GENTOO-BOOTSTRAP.md](../docs/GENTOO-BOOTSTRAP.md).

See the root [README](../README.md) for quick-start instructions.

## IDE update channels

On the `ubuntu` and `work` profiles, Cursor and Antigravity (and VS Code on
`work`) update through their vendors' apt repositories. `ide_apt_sources_ensure`
in `bootstrap/base_functions` registers each repository as a deb822 `.sources`
file, and only after the signing key is installed and verified, so a failed key
download never leaves a source that would break `apt-get update` for the whole
machine. VS Code on `ubuntu` is a snap, and `arch` keeps the Cursor AppImage
because Arch has no apt. `cursor_is_installed` accepts either form, so a machine
that already has the AppImage does not get a second copy. Antigravity's package
does not register its own repository (checked in 1.23.2), so the file the
bootstrap writes is its only update path.

An Ubuntu release upgrade disables third-party apt sources, and the weekly
`apt-get full-upgrade` then skips these packages without reporting anything.
`make audit-installation` reports that state per installed IDE package under
"IDE update channels". `make repair REPAIR=ide-repos` re-enables the sources for
the installed IDE packages and refreshes the package lists. Add `DRY_RUN=true`
to print the `sudo` commands without running them; run the real repair from a
terminal that can ask for the `sudo` password.

A vendor repository can trail the vendor's in-app updater by a few days, so a
newer candidate is reported as a warning, not as drift.

Run the hermetic test with `bash tests/test_ide_apt_sources.sh`.

## NVIDIA Container Toolkit

On the `work` profile, `nvidia_container_toolkit_install` (in
`bootstrap/base_functions`) installs `nvidia-container-toolkit` from NVIDIA's
apt repository when the machine has an NVIDIA GPU, and skips every other
machine. It looks for a PCI display device from NVIDIA (vendor `0x10de`, class
`0x03xxxx`) in sysfs, so it works before the driver has loaded; the card's HDMI
audio function alone does not count. It runs after `docker_install`, and a
failure only warns, because the repository needs the network.

The repository is registered like the IDE ones, through `apt_vendor_source_ensure`:
a deb822 `.sources` file, written only after NVIDIA's signing key is installed.
It is a flat repository, so the file has `Suites: /` and no `Components:` line.
A release upgrade leaves that file `Enabled: no`, after which the weekly
full-upgrade skips the toolkit silently; the step rewrites the file and retires
a one-line `nvidia-container-toolkit.list` that NVIDIA's own guide writes, because
two sources for one repository break `apt-get update`.

The step installs the package and nothing more. Pointing Docker at the NVIDIA
runtime edits `/etc/docker/daemon.json` and restarts the daemon, so run
`sudo nvidia-ctk runtime configure --runtime=docker && sudo systemctl restart docker`
yourself; the step prints that command after a first install on a machine that
has Docker.

`make repair REPAIR=nvidia-container-toolkit` repeats the step on any machine
with apt. Add `DRY_RUN=true` to print what it would write and run, without root;
run the real repair from a terminal that can ask for the `sudo` password. The
uninstall checklist of `work` offers `nvidia_ctk_uninstall`, which removes the
package, the apt source, and the key but leaves `daemon.json` alone.

Only `work` calls the step for now. To give another profile the step, add the
same `nvidia_container_toolkit_install ||` call with its warning to that
profile's entrypoint and `nvidia_ctk_uninstall` to its uninstall checklist;
`make audit-installation` then declares the package for that profile by itself.
On a profile that does not call the step, the audit lists an installed toolkit
under "Packages installed outside the bootstrap".

Run the hermetic test with `bash tests/test_nvidia_container_toolkit.sh`.

## OpenLogi

On the `ubuntu` profile, `openlogi_install` (in `bootstrap/ubuntu_functions`)
installs [OpenLogi](https://github.com/AprilNEA/OpenLogi), a Linux replacement
for Logitech Options+ that pairs and configures Bolt and Unifying receivers (it
cannot pair Lightspeed ones). It runs after `ubuntu_install`, and a failure only
warns, because the package comes from the network.

OpenLogi has no apt repository. It ships `.deb` files on GitHub releases, each
signed with minisign. The step resolves the latest stable release (or
`OPENLOGI_VERSION`, for example `0.8.11`), downloads the `.deb` for the
machine's architecture (`amd64` or `arm64`) and its `.minisig` into a temporary
directory, and checks the signature against the vendor's key before `apt-get`
opens the file. A refused release tag, a failed download, or a bad signature
installs nothing and removes the directory. `minisign` comes from Ubuntu's
universe repository (24.04 and later). 22.04 does not package it, and there the
step stops with a warning before it downloads anything.

The key is pinned in `ubuntu_functions`. It is the trust anchor in the vendor's
`packaging/linux/install.sh`, so check it there before changing it;
`tests/test_openlogi_install.sh` holds a second copy so that a change is
deliberate.

The package ships a systemd user unit for the agent that owns the receiver and
a udev rule. The step starts the agent with
`systemctl --user enable --now openlogi-agent.service`, and prints that command
when it cannot (over SSH, for example). Replug the receiver after a first
install so the udev rule applies. Only one program can own a receiver, so stop
Solaar first if you used it. OpenLogi's per-application profiles work on X11
and XWayland only.

The uninstall checklist of `ubuntu` offers `openlogi_uninstall`, which stops the
agent and purges the package. apt leaves your own settings in your home folder
alone, and `minisign` stays. `make audit-installation` declares `minisign`
through the profile and `openlogi` through `DEB_FILE_PACKAGES`. There is no
`make repair` step: to repeat the step, run the command that its warning prints.

Run the hermetic test with `bash tests/test_openlogi_install.sh`.

## kondo

[kondo](https://github.com/tbillington/kondo) deletes the folders that a project
can rebuild (`node_modules`, `target`, `build`, `.venv`, and so on) to free disk
space. It asks before it cleans each project it finds (`--all` skips the
question), deletes for good, and does not ask Git, so a tracked `build/` folder
goes too. `kondo --dry-run DIR` only lists what it would clean, and
`--older 3M` leaves alone the projects that changed in the last three months.

The `arch`, `manjaro`, and `mac` profiles install it from the package manager
(`kondo` in `bootstrap/arch_functions` and `bootstrap/mac_functions`). Ubuntu
has no package, so on the `ubuntu` profile `kondo_install` (in
`bootstrap/ubuntu_functions`) downloads the pinned release for the machine's
architecture (`amd64` or `arm64`) into a temporary directory, checks it against
the SHA-256 pinned in the function, and only then lets `sudo install` copy the
program to `/usr/local/bin/kondo`. A failed download or a digest that does not
match installs nothing and removes the directory. The step runs after
`openlogi_install`, and a failure only warns, because the file comes from the
network.

Upstream publishes no checksum file, so the digests in the function are the
trust anchor. GitHub shows the same value as the digest of each release asset:

```sh
gh api repos/tbillington/kondo/releases/tags/vX.Y.Z --jq '.assets[] | {name, digest}'
```

To update kondo, change `version` and both digests in `kondo_install` together,
and the copies in `tests/test_kondo_install.sh` with them; the test fails until
they agree. The `ubuntu-windows`, `work`, and `gentoo` profiles do not install
kondo.

The uninstall checklist of `ubuntu` offers `kondo_uninstall`, which removes
`/usr/local/bin/kondo`. There is no `make repair` step: to repeat the install,
run the command that its warning prints.

Run the hermetic test with `bash tests/test_kondo_install.sh`.

## Video editing: OpenShot, Blender, and DaVinci Resolve

Three editors for three jobs: OpenShot for quick cuts, Blender for 3D (and its
video sequencer), and DaVinci Resolve for colour work and heavier editing.

### OpenShot and Blender

On `ubuntu` and `work` both are snaps, one line each in `snaps_install`
(`bootstrap/ubuntu_functions`) and `gui_apps_install` (`bootstrap/work_functions`):
`openshot-qt` (strictly confined) and `blender --classic`. `arch` and `manjaro`
install `openshot` and `blender` with pacman, and `mac` installs the
`openshot-video-editor` and `blender` casks. The WSL profile has no desktop to
run them on and installs neither.

The OpenShot snap is the choice over the alternatives, measured on Ubuntu 26.04:

| Option | Why not |
| --- | --- |
| Stable PPA (`ppa:openshot.developers/ppa`) | Installs about 70 packages (OpenCV, GDAL, HDF5 and their dependencies; roughly 148 MiB to download and 412 MiB on disk), ships `python3-openshot` as a daily build, and an Ubuntu release upgrade disables third-party sources |
| Ubuntu archive package | 3.4, two releases behind the snap (4.0) |
| AppImage | Updates by hand, and reportedly no GPU acceleration |
| Flatpak | Lags behind the snap, and this repository does not use Flatpak |

The snap bundles an FFmpeg with the NVENC encoders (`h264_nvenc`, `hevc_nvenc`,
`av1_nvenc`), and snapd mounts the host's NVIDIA libraries into the snap, so GPU
export should work; confirm it once with a test export. The snap's publisher is
not verified by Canonical (`snap info openshot-qt`). Blender's classic snap is the
upstream build; the Ubuntu archive one is a distro build that may lack Cycles GPU
support. If the snap cannot read an external drive, run
`sudo snap connect openshot-qt:removable-media`.

The `uninstall` checklists remove the snaps with the other snaps (`ubuntu`) or the
GUI apps (`work`), and the pacman and cask lists with their profile.

### DaVinci Resolve

Blackmagic Design offers the download only behind a registration form, so nothing
here can fetch it. You download the free Linux ZIP once
(`DaVinci_Resolve_<version>_Linux.zip`, from the
[support page](https://www.blackmagicdesign.com/support/family/davinci-resolve-and-fusion))
into `~/Downloads` (or your XDG download folder, or your home folder), and
`davinci_resolve_install` (in `bootstrap/base_functions`, running
`.local/scripts/davinci_resolve_install.sh`) does the rest. Only the `ubuntu` and
`work` entrypoints call it, after `gui_apps_install` on `work` and before
`ai_tools_install` on `ubuntu`, non-fatally as `davinci_resolve_install ||`: a
missing ZIP is the normal state of a new machine, so it only warns. The `arch`
profile does not install it because the `davinci-resolve` AUR package needs the
same ZIP placed by hand, and Homebrew has no cask.

The script stops quietly when the machine has no NVIDIA GPU (the same sysfs check
as the NVIDIA Container Toolkit; `--force` overrides it) or already has
`/opt/resolve/bin/resolve`. Otherwise, in this order:

1. Picks the newest `DaVinci_Resolve_<version>_Linux.zip` or `.run` in the search
   folders (an unpacked `.run` beats its ZIP; versions compare by number, so
   `21.10` is newer than `21.1.1`), or the file named by `--installer FILE`. The
   Studio edition has a different file name, so pass it with `--installer`.
2. Checks about 12 GiB of free space where Resolve and the unpacked ZIP go.
3. Unpacks the ZIP into a temporary folder and finds the `.run` inside it, before
   anything on the system changes, because a truncated 3 GB download is the likeliest
   failure. The folder is removed afterwards; the ZIP stays.
4. Installs the libraries Resolve needs with `apt-get`. The list is the
   `PREREQUISITE_PACKAGES` array in the script: every package that the `AppRun` of
   Resolve 21.1.1 checks on Ubuntu (`check_ubuntu_package_deps`), under the `t64`
   names of Ubuntu 24.04 and later, plus `unzip` and `libfuse2t64`. Only the missing
   ones are installed. It is the whole vendor list rather than what `ldd` reports,
   because Resolve's Qt xcb plugin and `libQt5XcbQpa.so.5` do not link
   `libxcb-damage0`, which Blackmagic requires and the first version of the script
   left out.
5. Writes `/etc/udev/rules.d/75-davincipanel.rules` with only the Blackmagic USB rule
   (`SUBSYSTEM=="usb", ATTRS{idVendor}=="1edb", MODE="0666"`), before Blackmagic's
   installer runs. Resolve's `post_install.sh` writes three rules files to
   `/usr/lib/udev/rules.d` (`75-davincipanel.rules`, `75-davincikb.rules`,
   `75-sdx.rules`). The first ends in
   `KERNEL=="hidraw*", SUBSYSTEM=="hidraw", MODE="0777", GROUP="resolve"`, which is not
   limited to Blackmagic hardware: it makes every raw HID device (touchpad, keyboard
   interfaces, security keys) writable by every local process. udev lets a file in
   `/etc/udev/rules.d` replace the file of the same name in `/usr/lib/udev/rules.d`, so
   the override keeps the panel rule and drops the broad one. Because it exists before
   the installer runs, no device is exposed in between, and a reinstall or upgrade
   cannot bring the rule back. The other two rules name the vendor ID of Blackmagic's
   keyboard and the Feitian dongle (`096e`) of Resolve Studio, and stay. An existing
   file or link of that name (yours, or a link to `/dev/null`) is never overwritten, and
   if the file cannot be written the installation stops instead of installing the rule
   it was meant to prevent. `--keep-vendor-udev-rules` skips the step if you own a
   Blackmagic panel and want the vendor's file exactly as shipped. To undo it, delete
   the override file.
6. Runs Blackmagic's installer as `sudo env SKIP_PACKAGE_CHECK=1 ./DaVinci_Resolve_<version>_Linux.run -i`.
   It asks questions (the license, then "Do you wish to continue?"), so the step needs a
   terminal and refuses `--unattended` runs instead of hanging.
7. Moves the glib libraries that Resolve bundles (`libglib-2.0`, `libgobject-2.0`,
   `libgio-2.0`, `libgmodule-2.0`) into `/opt/resolve/libs/not_used`, so Resolve uses
   the newer system ones. Bundled copies that are older than the system's end in
   `symbol lookup error`. To undo it: `sudo mv /opt/resolve/libs/not_used/* /opt/resolve/libs/`.
8. Runs `ldd` on the program and on its Qt platform plugin and lists every library
   that is still not found, with the way to find its package (`apt-file search`).
9. Lists every `/dev/hidraw*` device that any user can write to, which should be none.
   If one is, it says how to find the udev rule that does it. A device that was
   exposed before the rule was fixed keeps its mode until it is replugged or the
   machine reboots, because udev sets permissions when a device appears.

`make repair REPAIR=davinci-resolve` repeats it, and `DRY_RUN=true` shows the plan
without `sudo` or unpacking anything. `make audit-installation` declares the
prerequisite libraries for the profiles that call the step (`STANDALONE_INSTALLERS`
in `audit_installation.py`); it does not check Resolve itself.

Things to know before the first start:

- The free edition on Linux does not decode H.264/H.265 video or AAC audio. Convert
  such clips first, for example
  `ffmpeg -i clip.mp4 -c:v dnxhd -profile:v dnxhr_hq -pix_fmt yuv422p -c:a pcm_s16le clip.mov`.
- If the window does not open on Wayland, start it through XWayland:
  `QT_QPA_PLATFORM=xcb /opt/resolve/bin/resolve`.
- The interface looks tiny on a HiDPI panel until you set **UI Display Scale**
  (Preferences, User, UI Settings; 100, 150, 200, or "follow system"). Resolve stores it as
  `<DisplayScale>` in `~/.local/share/DaVinciResolve/configs/config.user.xml` and sets Qt's
  scale from it at start-up, so `QT_SCALE_FACTOR` in the environment or in a launcher does
  nothing (measured on 21.1.1: 1.33 and 2 both left `resolve_graphics_log.txt` at
  `DPR=1.0`). Restart Resolve after changing it. On the G635LX under GNOME 50 the
  `xwayland-native-scaling` feature of mutter is on, so an X11 program sees the 133% panel
  as 3840x2400 (2x GNOME's 1920x1200), and 200% is the value that makes the interface the
  size of the rest of the desktop (the log then reads `1920x1200, DPR=2.0`).
- That file is tracked in this repository as
  `.local/share/DaVinciResolve/configs/config.user.xml` and Stow links it, so the scale
  follows the checkout to a new machine. It is the only file of
  `~/.local/share/DaVinciResolve` under version control: `.gitignore` allows that one path
  and nothing else, because the rest (projects, LUTs, Fusion data, logs) is tens of
  megabytes that Resolve rewrites on every run. On a machine where the folder does not
  exist yet, Stow would link the whole folder into the checkout (measured with the real
  Stow), and Resolve would then write its Project Library inside the repository, where a
  `git clean -fdx` deletes it. So `.local/scripts/stow-backup-conflicts`, which `make
  install`, `make restow` and the bootstrap preflight run before Stow, first creates
  `~/.local/share/DaVinciResolve/configs` as a real folder (`REAL_FOLDERS` in the script;
  an existing folder is left alone and `--dry-run` creates nothing), and Stow then links
  only the file. `make dry-run` simulates plain Stow, so on such a machine it still shows
  the folder link that `make install` avoids. The value `200` suits a 133% panel: a
  machine with a different screen or scale needs its own number, and it is one shared
  setting, so change it in the repository copy and commit it deliberately. The file
  changes rarely: it was untouched by at least five start-and-quit cycles after its first
  run, while `UI.preset` and `user.data.xml` change on every run. Edit it only while
  Resolve is closed, because a running Resolve holds its own copy and could write that
  back over the edit when Preferences are saved (not tested). Whether Resolve writes
  through the link or replaces it with a plain file when it saves Preferences is not
  tested either: `make config-status` and `readlink` show which one happened, and a plain
  file there is the usual Stow conflict, backed up by the next `make install`.
- The Project Manager, the first window, has no title-bar buttons and stays on top of
  other windows. Resolve asks for that itself: its X11 properties carry
  `_MOTIF_WM_HINTS` with decorations off, `_NET_WM_STATE_MODAL`, and
  `_NET_WM_WINDOW_TYPE_DIALOG` (read with `xprop -id WINDOW`, GNOME 50, Resolve 21.1.1),
  so it is not the window manager and not a broken installation.
- Blackmagic's installer is not under this repository's control. The tests run the
  script against a fake installer, so the first real run on a new Resolve release
  is the real test; the `ldd` report and the `not_used` folder show what happened.

The uninstall checklists of `ubuntu` and `work` offer `davinci_uninstall`, which
removes `/opt/resolve` (only when `bin/resolve` is there, so a mistyped prefix
deletes nothing), the launchers whose `Exec=` line starts a program inside it
(also `~/Desktop/com.blackmagicdesign.resolve.desktop`), Resolve's two menu files
(`com.blackmagicdesign.resolve.directory` and `.menu`), and the udev rules files its
installer writes (`75-davincipanel.rules`, `75-davincikb.rules`, `75-sdx.rules`) plus
the override from step 5. A rules file goes only when it names Blackmagic's USB vendor
ID (`1edb`, or `096e` for the dongle) or carries this script's mark in its first line,
so a file of someone else with the same name stays. The `99-BlackmagicDevices.rules`
and `99-ResolveKeyboardHID.rules` names, which are the ones inside Blackmagic's
payload, are matched the same way for an older or manual installation. Your projects
and settings (`~/.local/share/DaVinciResolve`) and the libraries apt installed stay.

The installer also writes to shared places that the uninstall leaves alone, and says
so: panel libraries in `/usr/lib64` (or `/usr/lib` when that folder does not exist),
the data folder `/var/BlackmagicDesign/DaVinci Resolve` (mode 0777), icons and MIME
types registered with `xdg-icon-resource` and `xdg-mime`, and, when the installer's
Open FX Renderer option is on, the OFX renderer in `/usr/OFX/Plugins`. Delete those by
hand if you want them gone.

Run the hermetic tests with `bash tests/test_davinci_resolve_install.sh`.

## Shutdown fix

Run `sudo .local/scripts/permanent_shutdown_fix.sh` on a Linux machine that needs
the configured ACPI, PCIe, and NVIDIA kernel parameters. It detects Pop!_OS's
`kernelstub`/systemd-boot setup and uses `kernelstub`; on GRUB installations it
updates `/etc/default/grub` and runs `update-grub`. The script removes `quiet`
and `splash` when present so shutdown messages remain visible, then requires a
reboot.

Use `SHUTDOWN_FIX_BOOTLOADER=kernelstub` or `SHUTDOWN_FIX_BOOTLOADER=grub` to
select a supported bootloader explicitly. Run the hermetic regression test
with `bash .local/scripts/test_permanent_shutdown_fix.sh`.

## Brightness fix

On the ASUS ROG Strix SCAR 16 (G635LX) in GPU MUX "Ultimate" mode, the Fn keys
and the GNOME slider can change a value without changing the built-in panel,
while external monitors work. The firmware advertises an embedded-controller
backlight (`nvidia_wmi_ec_backlight`) that the panel ignores. The kernel
parameter `acpi_backlight=native` makes the kernel skip it so the NVIDIA driver
can register its own `nvidia_0` backlight.

Run `.local/scripts/brightness_fix.sh --status` to see what a machine needs,
`--dry-run` to preview the change without root, and
`sudo .local/scripts/brightness_fix.sh` to apply it, then reboot. `--revert`
removes the parameter again. The script supports GRUB and Pop!_OS `kernelstub`,
backs up `/etc/default/grub` to `.bak`, restores it if `update-grub` fails, and
only acts on boards listed in `AFFECTED_BOARDS` (currently `G635LX`) unless
`--force` is given. The `work` bootstrap runs it right after the shutdown fix; a
failure only warns.

Run the hermetic test with `bash tests/test_brightness_fix.sh`. The evidence,
verification steps, and fallbacks are in
[docs/ROG-BRIGHTNESS-DIAGNOSIS.md](../docs/ROG-BRIGHTNESS-DIAGNOSIS.md).

## asusctl

ASUS ships no Linux software for its laptops; `asusctl` and its daemon `asusd`
from the ASUS Linux project control firmware platform profiles, fan curves, the
battery charge limit, and keyboard lighting.
`.local/scripts/asusctl_install.sh` builds the pinned release from the
project's own repository as your user, verifies the commit, copies the files
under `/usr` with `sudo`, and records each one so `--uninstall` removes exactly
what it added. It acts only on a supported ASUS laptop (ASUS vendor, a family
that `asusd` supports, kernel 6.19 or newer, `asus-nb-wmi` bound) and skips every
other machine, so the bootstrap profiles call it unconditionally.

The `work` and `ubuntu` bootstraps run it through `asusctl_install`; a failure
only warns. `make repair REPAIR=asusctl` repeats it, and `DRY_RUN=true`
previews every step without `sudo`. Use `--status` for a read-only report and
`--unattended` for noninteractive `sudo`. While `power-profiles-daemon` runs, the
installer switches off `asusd`'s own AC/battery profile switching so the two
never fight over the profile; GNOME's Power Mode keeps working. The design, the
comparison with the Homebrew route that the ASUS Linux guide documents, and how
to bump the pinned release are in [docs/ASUSCTL.md](../docs/ASUSCTL.md).

`.local/scripts/asusctl_lighting.sh` sets the keyboard backlight to the
`rainbow-wave` effect. The `work` and `ubuntu` bootstraps run it right after the
installer through `asusctl_lighting`; a failure only warns. It needs no `sudo`
and skips machines without `asusctl`, without a lighting device, or whose
keyboard lacks the effect. It reads the keyboard's state over D-Bus, changes
nothing when the effect is already set, and confirms every change before it
reports success. `make repair REPAIR=asusctl-lighting` repeats it and
`DRY_RUN=true` previews it (`REPAIR=keyboard` is the unrelated X11 key layout);
`--effect`, `--speed`, and `--direction` try other settings for one run.
Rerunning it replaces an effect chosen by hand.

Run the hermetic tests with `bash tests/test_asusctl_install.sh` and
`bash tests/test_asusctl_lighting.sh`.

## Monitor scales

On the ASUS ROG Strix SCAR 16 (G635LX) under GNOME on Wayland, a monitor at a
fractional scale next to one at 100% makes Chromium-based apps (Cursor, VS Code,
Chrome, Obsidian) shake in fullscreen on the 100% monitor: text and buttons
jitter and the pointer flips between the text cursor and the arrow. With every
monitor at one scale it stops.

`match-monitor-scales` gives all connected monitors the same scale, the primary
monitor's unless `--scale 125%` (or `--scale 1.25`) says otherwise, and works
out the new positions, because a scale change alters a monitor's logical size.
Run `match-monitor-scales --dry-run` to see the plan and have mutter check it
without applying anything, `match-monitor-scales` to apply it until the
monitors change or you log out, and `match-monitor-scales --persistent` to also
save it in `~/.config/monitors.xml`, GNOME's own per-machine file that Stow
does not manage. The old file is copied to `monitors.xml.bak-DATE-TIME` first.
Each run that changes something prints the `gdctl` command that goes back.

It needs `gdctl` (package `mutter-common-bin`) and `busctl`, and no bootstrap
runs it: use it after a dock reconnect or a change in GNOME Settings. GNOME
matches a saved layout by connector and monitor, so a monitor on another dock
port has none; run it with `--persistent` once for that combination.

Run the hermetic test with `python3 tests/test_match_monitor_scales.py`. The
evidence, the rollback, and what is still unknown are in
[docs/FULLSCREEN-WIGGLE-DIAGNOSIS.md](../docs/FULLSCREEN-WIGGLE-DIAGNOSIS.md).

## YouCompleteMe build

YouCompleteMe's server (ycmd) refuses to start until its C++ core, `ycm_core`,
has been compiled for the Python that runs it, and a plugin update can leave an
outdated core behind. Vim and Neovim then report "The ycmd server SHUT DOWN".

Two layers keep it working. The `do` hook on the plugin's `Plug` line in
`.vim/vimrc` builds it when vim-plug installs or updates the plugin.
`.local/scripts/ycm.sh` is the idempotent safety net: it does nothing when the
build already works, and otherwise repairs plugin submodules left behind by an
interrupted install and runs the plugin's `install.py`. Run it by hand when the
server will not start: `--check` only reports, `--dry-run` previews, and
`--force` rebuilds. By default it builds the core plus the bundled clangd, the
same options as the hook; set `YCM_INSTALL_ARGS` to change them, for example
`--all` for the extra language completers, which need their own toolchains.

The script never uses `sudo` and never installs packages. The bootstrap
profiles install what it needs: `cmake`, `git`, the Python headers (for example
`python3-dev`), and the Neovim Python provider `pynvim`. The compiler comes from
`build-essential`, `base-devel`, the Xcode Command Line Tools, or the Gentoo
base system. When a prerequisite is missing, `ycm.sh` names it and stops before
building. The `arch`, `ubuntu`, and `mac` profiles call the script at the end of
their Vim step; `work` and `gentoo` use the shared `vim_plugins_install` step,
which installs the plugins headlessly and then runs it. In every profile a
failure only warns, and `make repair REPAIR=vim PROFILE=work` repeats the step
on a work machine.

Run the hermetic test with `bash tests/test_ycm.sh`.
