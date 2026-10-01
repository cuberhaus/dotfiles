# Gentoo bootstrap (experimental)

Provisions a Gentoo Linux machine with the portable core of these dotfiles, the way the Arch, Manjaro, and Ubuntu profiles do. It exists because of [issue #20](https://github.com/cuberhaus/dotfiles/issues/20).

## Status

This profile has never run on a real Gentoo machine. It was written from the explicit assumptions below. What has been checked is limited to:

- `tests/test_bootstrap_gentoo.sh` (part of `make test` and CI) runs the real entrypoint against a simulated machine made of stubbed commands. It covers target detection, privileges, package-list parsing, package selection, service enabling, the login shell, a dry run, a rerun, and the Makefile wiring.
- Every package atom in `.local/scripts/bootstrap/gentoo.packages` was looked up in the main Gentoo ebuild repository on 2026-10-01. Each has an `amd64` or `~amd64` ebuild.

Nothing has exercised a real `emerge`, USE-flag resolution, blockers, `rc-update`, `chsh`, or a started desktop session. Treat the first run as a trial and begin with a dry run.

The profile is deliberately not listed under "Supported OS" in the README, in `make audit-installation`, or in automatic profile detection until it has been proven on a real machine. Those tools refuse a Gentoo machine and ask for an explicit profile.

## What it does

In this order:

1. Validates the machine: Gentoo, amd64, a known init system. Read-only.
2. Authenticates `sudo` once and refreshes it in the background, so a long build cannot stall on a password prompt.
3. Optionally runs `emerge --sync` (`--sync`) and updates `@world` (`--update-world`). Both are off by default.
4. Links the checkout into `$HOME` with GNU Stow through the shared flow (preview, conflict backups, confirmation). Skipped with `--unattended` or `--no-stow`.
5. Creates the per-user directories the shell and Vim configs expect, writes `~/.config/distro` (`DISTRO=gentoo`), and raises the inotify watch limit.
6. Installs the packages from `gentoo.packages` that are not yet in `@world`.
7. Installs the SOPS release binary, because `app-admin/sops` is not in the main tree.
8. Enables `elogind` (boot runlevel) and `dbus` (default runlevel) on OpenRC. systemd needs nothing.
9. Adds the user to the `video` group, and to `i2c` when that group exists, for brightness control.
10. Makes zsh the login shell, when zsh is listed in `/etc/shells`.
11. Installs the Vim and Neovim plugins headlessly, then makes sure YouCompleteMe can start by building its C++ core and bundled clangd when they are missing or outdated (`.local/scripts/ycm.sh`). This needs the network and can compile for several minutes; a failure only warns.
12. Clones the Obsidian vault and installs its plugins.

Rerunning is safe. Packages, services, the login shell, the distro marker, the inotify limit, SOPS, and the vault clone are each checked first and reported as already satisfied. Adding the user to groups and installing the vault plugins are idempotent and simply run again. The plugin install is idempotent too, and compiles nothing once YouCompleteMe works.

## Assumptions

| Topic | Assumption |
| --- | --- |
| Architecture | amd64 only. Anything else is refused. |
| Init system | OpenRC or systemd, taken from the running init. Where none is running (a chroot), set `GENTOO_INIT`. An ambiguous or contradictory target is refused; the two are never mixed. |
| Account | A normal user with `sudo`. Root is refused, because the dotfiles are linked into the invoking user's `$HOME`. |
| Keywords | Stable `amd64`. Packages that exist only as `~amd64` are in optional sections. `/etc/portage` is never edited. |
| Desktop | i3 on X11, with xmonad optional. No display manager is installed. |
| Portage tree | Whatever you have. It is not synced and `@world` is not updated unless you ask. |
| Package policy | Packages are merged with `--noreplace --select`, so they are recorded in `@world`: a rerun skips them and `emerge --depclean` keeps them. |
| Hardware | None assumed. Video drivers, firmware, the audio server, and networking are yours. |

## Before you run it

### Prepare the machine

- A finished Gentoo installation (see the Gentoo Handbook) with working networking, a synced Portage tree, and your own `make.conf` (`VIDEO_CARDS`, `INPUT_DEVICES`, USE flags, `MAKEOPTS`).
- `sudo`. As root: `emerge --ask app-admin/sudo`, add your user to the `wheel` group, and enable the `%wheel` rule with `visudo`.
- Some atoms may need USE changes. Portage will say so; put them under `/etc/portage/package.use/` yourself.

### Privileges

Run the profile as a normal user. It uses `sudo` for `emerge`, `rc-update add`, `usermod`, `chsh`, writing `/etc/sysctl.d/99-inotify.conf`, `sysctl -w`, and installing `/usr/local/bin/sops`. With `--dual-boot-utc` on systemd it also changes the hardware-clock setting.

`--unattended` never prompts. It needs cached credentials (run `sudo -v` first) or passwordless `sudo`, and it skips Stow linking, which needs a confirmation.

### Compile time and binary packages

Expect the first run to be dominated by compilation. How long it takes depends on your CPU, `MAKEOPTS`, and USE flags, and no timing has been measured for this profile. The manifest flags `x11-base/xorg-server` and `media-gfx/flameshot` as large builds, and the optional `xmonad` section needs a Haskell toolchain, which is a very large build. This is also why `sudo` credentials are refreshed in the background.

The profile never configures binary-package hosts, `MAKEOPTS`, or parallelism. They are operator policy, passed through `GENTOO_EMERGE_OPTS`:

```sh
GENTOO_EMERGE_OPTS="--getbinpkg --usepkg --jobs=4 --load-average=4" make bootstrap-gentoo
```

`--getbinpkg` only helps once a binary-package host is configured on your machine.

Several tools in the base section (`ripgrep`, `fd`, `bat`, `eza`) are written in Rust. If Portage proposes building the Rust compiler from source, installing `dev-lang/rust-bin` first avoids that.

## Running it

Always preview first. The dry run validates the machine and prints every change as `[dry-run] would run: ...`. The only commands it executes are read-only checks, `stow` simulations, and `emerge --pretend` as your own user; it needs no privileges and writes nothing, not even a log. It cannot tell you whether a build will succeed.

Run from a terminal, the dry run also previews the Stow step: the links it would create and the existing files in `$HOME` it would move into `~/.dotfiles-backup/` first, so conflicts are visible before anything is touched. That needs Stow to be installed already; otherwise the dry run says so, and a real run installs `app-admin/stow` first. With `--unattended` or `--no-stow` the real run skips linking, and the dry run says that instead of previewing it.

```sh
make bootstrap-gentoo-dry-run     # same as: bash .local/scripts/bootstrap/gentoo --dry-run
```

Then run it:

```sh
# Interactive: asks before Stow links the dotfiles.
make bootstrap-gentoo BOOTSTRAP_ARGS=

# Unattended (the default BOOTSTRAP_ARGS): needs cached sudo credentials; Stow is skipped.
sudo -v && make bootstrap-gentoo

# Machine setup only, without the workspace deployment that make adds afterwards.
bash .local/scripts/bootstrap/gentoo
```

After the profile script, `make bootstrap-gentoo` runs `make bootstrap-workspace`, which needs an authenticated `gh` (installed by the base section). It deliberately does not chain `install-automations` or `restore-apps`: native automation needs systemd timers plus `apt` or `pacman`, and the app-data restore targets applications the manifest omits.

Real runs mirror their output to `~/.local/state/cuberhaus/bootstrap/gentoo-<timestamp>-<pid>.log`. Services are enabled, not started: reboot afterwards so `elogind`, `dbus`, and the new group memberships take effect.

## Options and environment

| Option | Effect |
| --- | --- |
| `--dry-run` | Validate the machine and preview every change; change nothing. |
| `--sync` | Run `emerge --sync` first. |
| `--update-world` | Run `emerge --update --deep --newuse @world`. This can rebuild much of the system and may need manual follow-up. |
| `--unattended` | Never prompt; needs cached `sudo` credentials; skips Stow linking. |
| `--no-stow` | Skip linking the dotfiles. |
| `--dual-boot-utc` | Keep the hardware clock in UTC. systemd only; on OpenRC set `clock="UTC"` in `/etc/conf.d/hwclock`. |
| `--high-dpi=yes\|no` | Accepted for Makefile compatibility and ignored. |
| `-h`, `--help` | Show usage. |

| Variable | Effect |
| --- | --- |
| `GENTOO_INIT` | `openrc` or `systemd`. Required where no init is running; must agree with a running init. |
| `GENTOO_SECTIONS` | Install exactly these manifest sections (space or comma separated). |
| `GENTOO_EMERGE_OPTS` | Extra `emerge` options placed before the atoms. |
| `GENTOO_MANIFEST` | Use another package manifest file. |
| `GENTOO_DRY_RUN`, `GENTOO_SYNC`, `GENTOO_UPDATE_WORLD` | Environment equivalents of `--dry-run`, `--sync`, and `--update-world` (set to `true`). |
| `GENTOO_SUDO_KEEPALIVE_SECONDS` | Interval for refreshing `sudo` credentials (default 60). |
| `GENTOO_SYSROOT` | Prefix for the inspected paths. A test seam; leave it unset. |

## Packages

`gentoo.packages` lists plain `category/package` atoms in sections. Versions, slots, sets, and options are rejected, so a manifest can never change what `emerge` does beyond "install this package". A malformed manifest is refused with a `file:line` message before anything runs.

| Section | Installed by default | Contents |
| --- | --- | --- |
| `base` | yes | Git, GitHub CLI, Stow, search and file tools, tmux, ranger |
| `shell` | yes | zsh and its completions |
| `editors` | yes | Vim, Neovim, and the `pynvim` provider Neovim needs to load YouCompleteMe |
| `fonts` | yes | Hack, Noto, DejaVu, Liberation, Roboto, Font Awesome |
| `x11` | yes | Xorg server, `startx`, X utilities, kitty, dunst, udiskie, polkit, audio utilities |
| `i3` | yes | i3, i3blocks, picom, rofi, feh, flameshot |
| `session-openrc` | OpenRC only | elogind |
| `xmonad` | no | xmonad, xmonad-contrib, and xmobar (all `~amd64`), dmenu, trayer |
| `hardware` | no | ddcutil |
| `extras` | no | cava, kbdd (`~amd64`) |
| `dev` | no | shellcheck (`~amd64`), cmake, subversion |
| `tools` | no | rclone, sshfs, nmap, smartmontools |

`GENTOO_SECTIONS` selects exactly the sections you name, so include the defaults you still want:

```sh
GENTOO_SECTIONS="base shell editors fonts x11 i3 session-openrc hardware" make bootstrap-gentoo-dry-run
```

Before selecting a section that contains `~amd64` packages, accept their keywords yourself, for example `x11-wm/xmonad ~amd64` in a file under `/etc/portage/package.accept_keywords/`.

## Recovery

- **Rerun.** Packages already in `@world` are skipped, and services, group memberships, the login shell, and the distro marker are checked before they are changed. Portage records each package in `@world` as it merges, so a rerun after a failed build should resume with what is missing. "Already in `@world`" is Portage's record of what you asked for, not a check that the package is installed; if a package is listed there but missing (a hand-edited world file, an interrupted unmerge), rerun with `--update-world` to reinstall it.
- **`emerge` failed.** The profile stops there; `emerge`'s own output explains why. Fix the reported USE, keyword, or blocker problem under `/etc/portage/`, preview with `make bootstrap-gentoo-dry-run`, and rerun.
- **Stopped at the login shell.** zsh is installed but not listed in `/etc/shells`, which `chsh` requires. The profile never edits that file. As root, add the path of zsh, then rerun.
- **Vim plugin setup failed.** The profile warns and carries on, because the step needs the network and a compile. Run `nvim +PlugInstall +qall`, then `.local/scripts/ycm.sh`: it names any missing build tool, repairs plugin submodules left behind by an interrupted install, and shows the build output. `.local/scripts/ycm.sh --check` only reports whether YouCompleteMe is ready.
- **Unattended run refused.** Run `sudo -v` in the same terminal, or configure passwordless `sudo`, then retry.
- **`GENTOO_INIT` errors in a chroot.** No init is running there; set `GENTOO_INIT=openrc` or `GENTOO_INIT=systemd`.
- **Backing out.** There is no uninstaller. By hand: `make uninstall` removes the Stow links, `sudo rc-update del elogind boot` and `sudo rc-update del dbus default` disable the services, `sudo emerge --ask --deselect <atom>` followed by `sudo emerge --ask --depclean` removes packages, and deleting `~/.config/distro` clears the marker.

## Unsupported choices

These are left to you on purpose, or are not supported yet:

- Kernel, bootloader, firmware, and `VIDEO_CARDS`.
- The audio server. `pavucontrol` needs a PulseAudio-compatible one that you install.
- Network management.
- A display manager or login screen.
- Browsers and applications.
- `/etc/portage` configuration: USE flags, keywords, mirrors, binary-package hosts.
- Native automation (`make install-automations`) and app-data restores.
- `make audit-installation`, repair, and automatic profile detection.
- An uninstaller and testing on real Gentoo hardware in CI.

## Known limitations

- **The desktop session is not started for you.** The tracked `.xinitrc` cannot start i3 on any distribution: it sources `$HOME/config/distro` (the leading dot is missing) and only starts i3 when `DISTRO` is `arch`. Fixing it is a small change to a file shared by every profile, so it is not part of this profile. Install and enable a display manager of your choice, or run `startx /usr/bin/i3` from a console. With an explicit client `startx` skips `~/.xinitrc`, and `~/.xprofile` is not read either.
- **Power bindings use `systemctl`.** The i3 power menu (`.config/i3/config`) calls `systemctl poweroff`, `suspend`, `hibernate`, and `reboot`, which do not exist on OpenRC.
- **No screen locker.** The lock binding calls `betterlockscreen`, which is not in the main Gentoo tree. The config guards it with `command -v`, so the binding silently does nothing.
- **Polkit agent.** The i3 config autostarts `/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1`, the Arch location. Gentoo installs the agent elsewhere (check the package's installed file list), so it will not start until the path is adjusted.
- **No package-manager aliases.** `update`, `updateall`, and `cleanup` are defined in `.config/zsh/aliases` for Arch, Manjaro, and Ubuntu only.
