# Cuberhaus's dotfiles

[![Lint](https://github.com/cuberhaus/dotfiles/actions/workflows/lint.yml/badge.svg)](https://github.com/cuberhaus/dotfiles/actions/workflows/lint.yml)
[![Markdown](https://github.com/cuberhaus/dotfiles/actions/workflows/markdown.yml/badge.svg)](https://github.com/cuberhaus/dotfiles/actions/workflows/markdown.yml)
[![XMonad](https://github.com/cuberhaus/dotfiles/actions/workflows/xmonad.yml/badge.svg)](https://github.com/cuberhaus/dotfiles/actions/workflows/xmonad.yml)
[![Vim](https://github.com/cuberhaus/dotfiles/actions/workflows/vim.yml/badge.svg)](https://github.com/cuberhaus/dotfiles/actions/workflows/vim.yml)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
## Table of Contents

- [Cuberhaus's dotfiles](#cuberhauss-dotfiles)
    - [How it works](#how-it-works)
    - [Installation](#installation)
    - [Usage](#usage)
    - [Volatile config files](#volatile-config-files)
    - [Bootstrap](#bootstrap)
    - [What's inside](#whats-inside)
    - [Supported OS](#supported-os)
    - [Window Managers](#window-managers)
    - [WIP](#wip)

<!-- markdown-toc end -->

Opinionated defaults. This repo is not meant to be used by everyone, just a personal configuration to take ideas out of. However, if you are brave enough you can install it with the instructions below. You are advised to read the installation scripts beforehand.

## How it works

### Why `dotfiles/dotfiles`?

You might wonder why the repository isn't just cloned directly into `~/dotfiles`. The nested `~/dotfiles/dotfiles` structure is required by how **[GNU Stow](https://www.gnu.org/software/stow/)** manages packages. Stow expects a "stow directory" (the parent, `~/dotfiles`) containing one or more "packages" (the child, `dotfiles`, which is this repo).

By cloning into `~/dotfiles/dotfiles`, stow correctly treats the inner `dotfiles` folder as the package name, allowing it to safely symlink the contents (like `.config/`, `.vim/`) directly into your `$HOME` directory without confusing the repository root with the target deployment.

So, the structure looks like this:

```
 ~/dotfiles/
 └── dotfiles/   (this repo)
     ├── .config/       ──┐
     ├── .local/         │  GNU Stow symlinks
     ├── .vim/           ├──────────────────►  $HOME/
     ├── .xmonad/        │                     ├── .config/ → ~/dotfiles/dotfiles/.config/
     ├── .zshenv        ──┘                    ├── .local/  → ~/dotfiles/dotfiles/.local/
     ├── Makefile  (make targets)              └── ...
     └── .local/scripts/
         └── bootstrap/  (OS-specific setup)
```

[GNU Stow](https://www.gnu.org/software/stow/) creates symlinks from `$HOME`
into this repo so that every config file stays version-controlled in one place.
The `Makefile` wraps stow and exposes common tasks. Bootstrap scripts install
packages and perform one-time setup for each supported OS.

The `$DOTFILES` variable (exported by `.zshenv`) points to the repo root,
auto-detected by resolving the `.zshenv` symlink. Scripts and configs that
need to reference the repo should use `$DOTFILES`.

## Installation

Clone the repo with its submodules and use [GNU Stow](https://www.gnu.org/software/stow/) to symlink everything into `$HOME`:

```bash
cd ~
mkdir -p dotfiles
git clone --recurse-submodules https://github.com/cuberhaus/dotfiles dotfiles/dotfiles
cd dotfiles/dotfiles
make install
```

> If you don't have `stow` installed, grab it first:
>
> - **Arch/Manjaro:** `sudo pacman -S stow`
> - **Ubuntu/Debian:** `sudo apt install stow`
> - **macOS:** `brew install stow`

## Usage

Common tasks are available via `make`:

```
make help              # Show all targets
make install           # Symlink dotfiles into $HOME
make uninstall         # Remove symlinks from $HOME
make restow            # Re-stow (cleans stale links)
make lint              # Run shellcheck on all scripts
make test              # Run deterministic unit tests
make check             # Run tests and all linters (shellcheck + markdownlint + vint)
make audit-installation # Report installation drift without changing the machine
make submodules        # Init and update submodules
make update            # Pull latest for every submodule
make skip-worktree     # Ignore runtime changes to volatile config files (run once after cloning)
make bootstrap-<os>    # Run bootstrap (arch, manjaro, ubuntu, mac, work)
```

Run `make audit-installation` regularly on each Unix machine to compare the
current checkout, Stow-managed configs and aliases, packages declared by the
active bootstrap profile, and native automations. The target changes nothing
and exits `1` when it finds actionable drift. It auto-detects the profile; use
`make audit-installation PROFILE=arch|manjaro|ubuntu|ubuntu-windows|mac|work`
to override detection. Package expectations are parsed only from functions the
selected bootstrap actually calls, so commented optional bundles are excluded.

### Volatile config files

Some tracked files (e.g. `user_preferences.json` for Warp, `javasettings_Linux_X86_64.xml` for LibreOffice) are **rewritten by their apps on every launch**. They are kept in the repo so the settings you care about are versioned, but the constant runtime changes make `git status` noisy and block `git pull`.

After cloning, run once:

```bash
make skip-worktree
```

This applies `git update-index --skip-worktree` to those files — git keeps the committed version but stops noticing local changes.

When you **intentionally** want to update one of them in the repo:

```bash
git update-index --no-skip-worktree .config/warp-terminal/user_preferences.json
# edit / copy the new settings you want to keep
git add .config/warp-terminal/user_preferences.json
git commit -m "update warp settings"
git update-index --skip-worktree .config/warp-terminal/user_preferences.json  # re-apply
```

## Bootstrap

OS-specific bootstrap scripts live in `.local/scripts/bootstrap/`.
**Read the script for your OS before running it** — they install hundreds
of packages and change system settings.

```bash
make bootstrap-arch      # Arch
make bootstrap-manjaro   # Manjaro
make bootstrap-ubuntu    # Ubuntu
make bootstrap-mac       # macOS
make bootstrap-work      # Work machine (Ubuntu + NVIDIA, minimal)
```

### Dual-boot hardware clock

For a machine that boots both Windows and physical Linux, opt in to a UTC
hardware clock during bootstrap:

```bash
make bootstrap-ubuntu BOOTSTRAP_ARGS=--dual-boot-utc
# Also supported: bootstrap-arch, bootstrap-manjaro, and bootstrap-work
```

For an existing physical Linux installation, run:

```bash
make dual-boot-utc
```

The helper rejects WSL and non-systemd systems. It enables network time, waits
for `NTPSynchronized=yes`, then runs `timedatectl set-local-rtc 0` and verifies
`LocalRTC=no`. It refuses to change the RTC if synchronization cannot be
established. Configure Windows with WinDotfiles' `-DualBootUtcRtc` option as
well; changing only one operating system leaves the original disagreement in
place.

Verify with `timedatectl`: it should report `System clock synchronized: yes`,
`NTP service: active`, and `RTC in local TZ: no`. Do not use
`timedatectl set-local-rtc 1` as a permanent workaround because local RTC is
fragile across DST and timezone changes. The option is intentionally not
forwarded to the WSL or macOS bootstrap targets.

Each bootstrap entrypoint follows the same pattern:

1. Sources `base_functions` (shared logging helpers and `$DOTFILES` auto-detection).
2. Sources the OS-specific `*_functions` file (package lists and installer functions).
3. Asks whether this is a first-time install (enables services, sets up hardware, etc.).
4. Runs a system update.
5. Calls installer functions in dependency order (base packages, WM-specific, optional apps).
6. Switches the default shell to zsh.
7. Installs the native maintenance schedules shown below.

The bootstrap installs these automations without requiring a separate command:

| Automation | Linux | macOS |
| --- | --- | --- |
| System packages | Sunday 10:00 via system `systemd` (`apt-get` or `pacman`) | Managed by macOS Software Update |
| User packages | Sunday 10:30 via user `systemd` (Homebrew and/or `yay`) | Sunday 10:30 via `launchd` (Homebrew) |
| `~/cuberhaus` pull | Daily 11:00 via user `systemd` | Daily 11:00 via `launchd` |

The workspace pull matches the shell helper's depth-3 discovery but uses
`git pull --ff-only --no-edit` with credential prompts disabled. Repositories
without upstreams are skipped; diverged or unauthenticated repositories are
left unchanged and reported. None of the maintenance jobs requests a reboot.

On Linux, `Persistent=true` runs a missed timer when the machine becomes
available. On macOS, the agents run at login as well as on schedule, with state
guards preventing duplicate work: 20 hours for workspace pulls and seven days
for user package updates. WSL deliberately installs no duplicate timers because
Windows Task Scheduler owns the same automations.

For an existing checkout, restow the new files and install the schedules:

```bash
make restow
make install-automations
```

Preview or remove scheduler registrations without deleting logs or state:

```bash
make uninstall-automations-dry-run
make uninstall-automations
```

`make uninstall` removes scheduler registrations before unstowing the files, so
no active job is left pointing at a removed script.

Inspect the schedules and logs:

```bash
# Linux
systemctl list-timers 'cuberhaus-*'
systemctl --user list-timers 'cuberhaus-*'
journalctl -u cuberhaus-system-maintenance.service
journalctl --user -u cuberhaus-user-package-maintenance.service
journalctl --user -u cuberhaus-workspace-pull.service

# macOS
launchctl print "gui/$UID/com.cuberhaus.user-package-maintenance"
launchctl print "gui/$UID/com.cuberhaus.workspace-pull"
tail -f "$HOME/Library/Logs/Cuberhaus/user-package-maintenance.log"
tail -f "$HOME/Library/Logs/Cuberhaus/workspace-pull.log"
```

See [`.local/README.md`](.local/README.md) for a detailed breakdown of the
scripts directory.

## What's inside

| Category | Tool / Config | Notes |
| --- | --- | --- |
| **Shells** | zsh (antigen, p10k), bash | XDG-compliant `$ZDOTDIR` in `.config/zsh/` |
| **Editors** | Vim, Neovim, Doom Emacs, personal Emacs (chemacs) | Vim config at `.vim/vimrc`; Emacs literate config in `.config/emacs.org` |
| **Terminals** | kitty, Alacritty, termite | |
| **Window Managers** | XMonad (+xmobar), i3 (+i3blocks +polybar), qtile, sway | XMonad is the primary config; i3 is the secondary |
| **Desktop Environments** | Cinnamon, GNOME | |
| **Utilities** | tmux, ranger, dunst, picom, rofi, fzf, bat, eza | |
| **Themes** | Arc, Dracula, OneDark, base16 | Managed via `toggle_theme` script |

## Supported OS

- ![Arch\_icon][arch_icon] Arch
- ![Manjaro\_icon][manjaro_icon] Manjaro
- ![Ubuntu\_icon][ubuntu_icon] Ubuntu
- ![MacOS\_icon][macos_icon] macOS

## Window Managers

- Xmonad (Main config)
- i3 (Second best)
- Cinnamon desktop
- Gnome

## WIP

- ![Gentoo\_icon][gentoo_icon] Gentoo
- Openbox

[manjaro_icon]: https://i.imgur.com/rfuvfYo.png
[arch_icon]: https://upload.wikimedia.org/wikipedia/commons/a/a5/Archlinux-icon-crystal-64.svg
[ubuntu_icon]: https://i.imgur.com/EX9n2Ib.png?1
[macos_icon]: https://i.imgur.com/olG7ewE.png?1
[gentoo_icon]: https://i.imgur.com/cKReKS2.png
