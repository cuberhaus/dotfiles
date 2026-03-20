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
make check             # Run all linters (shellcheck + markdownlint + vint)
make submodules        # Init and update submodules
make update            # Pull latest for every submodule
make bootstrap-<os>    # Run bootstrap (arch, manjaro, ubuntu, mac, work)
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

Each bootstrap entrypoint follows the same pattern:

1. Sources `base_functions` (shared logging helpers and `$DOTFILES` auto-detection).
2. Sources the OS-specific `*_functions` file (package lists and installer functions).
3. Asks whether this is a first-time install (enables services, sets up hardware, etc.).
4. Runs a system update.
5. Calls installer functions in dependency order (base packages, WM-specific, optional apps).
6. Switches the default shell to zsh.

See [`.local/README.md`](.local/README.md) for a detailed breakdown of the
scripts directory.

## What's inside

| Category | Tool / Config | Notes |
|---|---|---|
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
