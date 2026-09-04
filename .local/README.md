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
│   │   ├── clone-all          # Clone all repos from a GitHub user
│   │   ├── git-recurse        # Run git commands across multiple repos
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
│   │   ├── base_functions     # Shared helpers (logging, $DOTFILES, prep)
│   │   ├── arch_functions     # Arch/Manjaro package lists & installers
│   │   ├── ubuntu_functions   # Ubuntu package lists & installers
│   │   ├── mac_functions      # macOS (Homebrew) package lists & installers
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
│   ├── lint.sh                 # Lint all tracked shell scripts with shellcheck
│   ├── permanent_shutdown_fix.sh # Applies shutdown kernel parameters via kernelstub or GRUB
│   ├── toggle_theme            # Switch between light/dark themes
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
3. Ask if this is a first-time install.
4. Run system update.
5. Call installer functions in dependency order.
6. Switch default shell to zsh.
7. Install package-maintenance and workspace-pull schedules through the root
  Makefile target.

The work bootstrap mirrors all terminal output to a persistent per-run log at
`${XDG_STATE_HOME:-$HOME/.local/state}/cuberhaus/bootstrap/work-<UTC timestamp>-<PID>.log`.
The log path is printed when the bootstrap starts. Its source-safe `work_main`
entrypoint is exercised with destructive stages replaced by test doubles in
`tests/test_bootstrap_work.sh`. For unattended runs, authorize `sudo` first
with `sudo -v`; the entrypoint then uses noninteractive sudo and apt behavior.

See the root [README](../README.md) for quick-start instructions.

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
