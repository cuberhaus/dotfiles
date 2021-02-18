#!/bin/sh

###############################################################
# => Path
###############################################################

if [ -d "$HOME/.local/bin" ] ; then
        PATH="$HOME/.local/bin:$PATH"
fi

if [[ "$DESKTOP_SESSION" == "i3" ]]; then
fi

if [[ "$DESKTOP_SESSION" == "cinnamon" ]]; then
    if [ -d "$HOME/.local/scripts/cinnamon_path" ] ; then
            PATH="$HOME/.local/scripts/cinnamon_path:$PATH"
    fi
fi

if [ -d "$HOME/.config/i3/i3-layout-manager" ] ; then
        PATH="$HOME/.config/i3/i3-layout-manager:$PATH" 
fi
if [ -d "$HOME/.gem/ruby/2.7.0/bin" ] ; then
        PATH="$HOME/.gem/ruby/2.7.0/bin:$PATH"
fi
if [ -d "/usr/local/sbin" ] ; then
    PATH="/usr/local/sbin:$PATH"
fi
#PATH=".:$PATH" # Better not add '.' to PATH 

###############################################################
# => Variables
###############################################################

# Zsh files:
export ZDOTDIR="${XDG_CONFIG_HOME:-$HOME/.config}/zsh" # For more information RTFM https://wiki.archlinux.org/index.php/Zsh#Startup/Shutdown_files
export VISUAL=vim
export EDITOR="$VISUAL"
export DOTFILES="$HOME/dotfiles/dotfiles"

#INCLUSIONS="/Users/$USER/assig/pro2/inclusions"
#OBJECTES=/Users/$USER/assig/pro2/objectes
export DICPATH="$XDG_CONFIG_HOME/Dictionary"
export PATH="$HOME/.emacs.d/bin:$PATH" # DOOM emacs "bring the pain"
export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""' 
export FZF_DEFAULT_OPTS='--preview "bat --style=numbers --color=always --line-range :500 {}" --height 60% --border -m'
export MANPAGER="/bin/sh -c \"col -b | vim --not-a-term -c 'set ft=man ts=8 nomod nolist noma' -\"" # SET VIM AS MANPAGER

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    export TERMINAL="termite"
    export QT_QPA_PLATFORMTHEME="qt5ct"
    source $HOME/.config/distro     # DISTRO variable
    if laptop-detect ; then
        setxkbmap es 
    fi
fi 

if [[ "$OSTYPE" == "darwin"* ]]; then
    export LDFLAGS="-L/usr/local/opt/llvm/lib -Wl,-rpath,/usr/local/opt/llvm/lib"
    export CPPFLAGS="-I/usr/local/opt/llvm/include"
    PATH="/usr/local/opt/llvm/bin:$PATH"
fi

# Homebrew env variables
if [[ "$DISTRO" == "ubuntu"* ]]; then
    eval $(/home/linuxbrew/.linuxbrew/bin/brew shellenv)
fi

###############################################################
# => Clean home directory
###############################################################

#export GEM_HOME="$XDG_DATA_HOME"/gem
#export GEM_SPEC_CACHE="$XDG_CACHE_HOME"/gem
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
export ATOM_HOME="$XDG_DATA_HOME"/atom
# Android sdk
export ANDROID_SDK_HOME="$XDG_CONFIG_HOME"/android
export ANDROID_AVD_HOME="$XDG_DATA_HOME"/android/
export ANDROID_EMULATOR_HOME="$XDG_DATA_HOME"/android/ 
export ADB_VENDOR_KEY="$XDG_CONFIG_HOME"/android
# GTK
export GTK_RC_FILES="$XDG_CONFIG_HOME"/gtk-1.0/gtkrc
export GTK2_RC_FILES="$XDG_CONFIG_HOME"/gtk-2.0/gtkrc
