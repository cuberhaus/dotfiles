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
│   │   ├── logout-all         # Sign out of browsers, editors and CLIs; --audit lists stored credentials
│   │   ├── vault-secret       # Access SOPS-encrypted vault credentials
│   │   ├── pfetch             # Minimal system info display
│   │   ├── program            # Launch-or-focus helper for scratchpads
│   │   ├── prompt             # Custom prompt helper
│   │   └── yolo               # Alias for quick git push
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
  outside its standard checkout locations.
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
  `make audit-installation`. Set `PROFILE=<name>` to override auto-detection.

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
