#!/bin/sh
# ~/.zshenv
# Zsh files:
export ZDOTDIR="${XDG_CONFIG_HOME:-$HOME/.config}/zsh"
# For more information RTFM
# https://wiki.archlinux.org/index.php/Zsh#Startup/Shutdown_files

# Use vim
export VISUAL=vim
export EDITOR="$VISUAL"

export QT_QPA_PLATFORMTHEME="qt5ct"
# Add to path
if [ -d "$HOME/.local/bin" ] ; then
        PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/.config/i3/i3-layout-manager" ] ; then
        PATH="$HOME/.config/i3/i3-layout-manager:$PATH"
fi
export PATH="${PATH}:${HOME}/.local/bin/"
PATH="$HOME/.gem/ruby/2.7.0/bin:$PATH"

# ~/ Clean-up:
export GEM_HOME="$XDG_DATA_HOME"/gem
export GEM_SPEC_CACHE="$XDG_CACHE_HOME"/gem
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"
#export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority" # This line will break some DMs.
export NOTMUCH_CONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/notmuch-config"
export GTK2_RC_FILES="${XDG_CONFIG_HOME:-$HOME/.config}/gtk-2.0/gtkrc-2.0"
export LESSHISTFILE="-"
export WGETRC="${XDG_CONFIG_HOME:-$HOME/.config}/wget/wgetrc"
export INPUTRC="${XDG_CONFIG_HOME:-$HOME/.config}/inputrc"
#export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export WINEPREFIX="${XDG_DATA_HOME:-$HOME/.local/share}/wineprefixes/default"
export KODI_DATA="${XDG_DATA_HOME:-$HOME/.local/share}/kodi"
export PASSWORD_STORE_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/password-store"
export TMUX_TMPDIR="$XDG_RUNTIME_DIR"
export ANDROID_SDK_HOME="${XDG_CONFIG_HOME:-$HOME/.config}/android"
export CARGO_HOME="${XDG_DATA_HOME:-$HOME/.local/share}/cargo"
export GOPATH="${XDG_DATA_HOME:-$HOME/.local/share}/go"
export ANSIBLE_CONFIG="${XDG_CONFIG_HOME:-$HOME/.config}/ansible/ansible.cfg"
export UNISON="${XDG_DATA_HOME:-$HOME/.local/share}/unison"
export HISTFILE="$XDG_DATA_HOME"/bash/.bash_history
#export VIMINIT='source "$XDG_CONFIG_HOME/vim/vimrc"'
export ATOM_HOME="$XDG_DATA_HOME"/atom
# Android sdk
export ANDROID_SDK_HOME="$XDG_CONFIG_HOME"/android
export ANDROID_AVD_HOME="$XDG_DATA_HOME"/android/
export ANDROID_EMULATOR_HOME="$XDG_DATA_HOME"/android/ 
export ADB_VENDOR_KEY="$XDG_CONFIG_HOME"/android
# GTK
export GTK_RC_FILES="$XDG_CONFIG_HOME"/gtk-1.0/gtkrc
export GTK2_RC_FILES="$XDG_CONFIG_HOME"/gtk-2.0/gtkrc

export PATH="/usr/local/sbin:$PATH"
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    # DISTRO variable
    source ~/.config/distro
    if laptop-detect ; then
        setxkbmap es 
    fi
    # Swap escape with caps lock
    xmodmap -e "clear lock"
    xmodmap -e "keycode 9 = Caps_Lock NoSymbol Caps_Lock"
    xmodmap -e "keycode 66 = Escape NoSymbol Escape"
fi 

# Homebrew env variables
if [[ "$DISTRO" == "ubuntu"* ]]; then
    eval $(/home/linuxbrew/.linuxbrew/bin/brew shellenv)
fi
