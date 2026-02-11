# Cuberhaus's dotfiles
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Cuberhaus's dotfiles](#cuberhauss-dotfiles)
    - [Installation](#installation)
    - [Bootstrap](#bootstrap)
    - [Supported OS](#supported-os)
    - [Window Managers](#window-managers)
    - [WIP](#wip)

<!-- markdown-toc end -->

Opinionated defaults. This repo is not meant to be used by everyone, just a personal configuration to take ideas out of. However, if you are brave enough you can install it with the instructions below. You are advised to read the installation scripts beforehand.

## Installation

Clone the repo with its submodules and use [GNU Stow](https://www.gnu.org/software/stow/) to symlink everything into `$HOME`:

```bash
cd ~
mkdir -p dotfiles
cd dotfiles
git clone --recurse-submodules https://github.com/cuberhaus/dotfiles
stow -vt ~ dotfiles/
```

> If you don't have `stow` installed, grab it first:
> - **Arch/Manjaro:** `sudo pacman -S stow`
> - **Ubuntu/Debian:** `sudo apt install stow`
> - **macOS:** `brew install stow`

## Bootstrap

OS-specific bootstrap scripts are located in `.local/scripts/bootstrap/`. Read the script for your OS before running it:

```bash
# Arch
bash .local/scripts/bootstrap/arch

# Manjaro
bash .local/scripts/bootstrap/manjaro

# Ubuntu
bash .local/scripts/bootstrap/ubuntu

# macOS
bash .local/scripts/bootstrap/mac
```

## Supported OS

* ![Arch\_icon][arch_icon] Arch
* ![Manjaro\_icon][manjaro_icon] Manjaro
* ![Ubuntu\_icon][ubuntu_icon] Ubuntu
* ![MacOS\_icon][macos_icon] macOS

## Window Managers

* Xmonad (Main config)
* i3 (Second best)
* Cinnamon desktop
* Gnome

## WIP

* ![Gentoo\_icon][gentoo_icon] Gentoo
* Openbox

[manjaro_icon]: https://i.imgur.com/rfuvfYo.png
[arch_icon]: https://upload.wikimedia.org/wikipedia/commons/a/a5/Archlinux-icon-crystal-64.svg
[ubuntu_icon]: https://i.imgur.com/EX9n2Ib.png?1
[macos_icon]: https://i.imgur.com/olG7ewE.png?1
[gentoo_icon]: https://i.imgur.com/cKReKS2.png
