# shellcheck shell=bash
# best python linter is pylint
# IN MAC YOU MAY HAVE TO PUT HOMEBREW IN zprofile
# path_helper is executed from /etc/zprofile and sources /etc/paths then /etc/path.d
# zshrc and zprofile go after that, check order to make sure

###############################################################
# => XDG Base Directories (must be set before anything else)
###############################################################

export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

###############################################################
# => Path
###############################################################

case ":${PATH:-}:" in
    *:/usr/bin:*) ;;
    *) PATH="/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:${PATH:-}" ;;
esac
if [ -d "$HOME/google-cloud-sdk/bin" ]; then
    PATH="$HOME/google-cloud-sdk/bin:$PATH"
fi
export PATH

if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/.npm-global/bin" ] ; then
    PATH="$HOME/.npm-global/bin:$PATH"
fi

if [ -d "$HOME/.local/share/cargo/bin" ] ; then
    PATH="$HOME/.local/share/cargo/bin:$PATH"
fi
# if [[ "$DESKTOP_SESSION" == "i3" ]]; then
# DO NOT EVER LEAVE AN IF EMPTY OR IT WILL CAUSE APPS TO NOT HAVE PATH STUFF BEWARE
# TO CHECK IF PATH FOR APPS WORK OPEN OKULAR OR QT5 SETTINGS
# fi

if [ -d "$HOME/.npm-global/bin" ] ; then
    PATH="$HOME/.npm-global/bin:$PATH"
fi

if [ -d "$HOME/.local/scripts/bin" ] ; then
    PATH="$HOME/.local/scripts/bin:$PATH"
fi

if [[ "$DESKTOP_SESSION" == "cinnamon" ]]; then
    if [ -d "$HOME/.local/scripts/cinnamon_path" ] ; then
        PATH="$HOME/.local/scripts/cinnamon_path:$PATH"
    fi
fi
## WIP (do not know if this will work, check if $DESKTOP_SESSION on gnome actually becomes "gnome" on a gnome DE)
if [[ "$DESKTOP_SESSION" == "gnome" ]]; then
    if [ -d "$HOME/.local/scripts/gnome_path" ] ; then
        PATH="$HOME/.local/scripts/gnome_path:$PATH"
    fi
fi

if [ -d "$HOME/.config/i3/i3-layout-manager" ] ; then
    PATH="$HOME/.config/i3/i3-layout-manager:$PATH"
fi
# Add the most recent Ruby gem bin to PATH
_rubydir=$(find "$HOME/.gem/ruby" -maxdepth 1 -mindepth 1 -type d 2>/dev/null | sort -V | tail -1)
if [ -n "$_rubydir" ] && [ -d "$_rubydir/bin" ]; then
    PATH="$_rubydir/bin:$PATH"
fi
unset _rubydir
if [ -d "/usr/local/sbin" ] ; then
    PATH="/usr/local/sbin:$PATH"
fi
#PATH=".:$PATH" # Better not add '.' to PATH

# Only start gnome-keyring-daemon when no instance is running.
# On GNOME/Cinnamon the session manager launches it; on i3/xmonad it does not.
if [ -n "$DESKTOP_SESSION" ] && ! pgrep -u "$USER" gnome-keyring-daemon >/dev/null 2>&1; then
    eval "$(gnome-keyring-daemon --start 2>/dev/null)"
    export SSH_AUTH_SOCK
fi

###############################################################
# => Variables
###############################################################

# Zsh files:
export ZDOTDIR="$XDG_CONFIG_HOME/zsh" # For more information RTFM https://wiki.archlinux.org/index.php/Zsh#Startup/Shutdown_files
# Auto-detect the dotfiles repo by resolving the real path of this .zshenv
if [ -L "$HOME/.zshenv" ]; then
    DOTFILES="$(cd "$(dirname "$(readlink -f "$HOME/.zshenv")")" && pwd)"
else
    DOTFILES="$HOME/dotfiles/dotfiles"  # fallback (repo cloned to ~/dotfiles/dotfiles so stow works)
fi
export DOTFILES
# Wallpapers (fall back to a bundled wallpaper if the preferred one is missing)
_wp_light="$HOME/.local/xdg/wallpapers/doggo.jpeg"
_wp_dark="$HOME/Downloads/wallpapers/pexels-eberhard-grossgasteiger-1612351.jpg"
_wp_fallback="$HOME/.local/xdg/wallpapers/doggo.jpeg"
export WALLPAPER_LIGHT="${_wp_light}"
if [ -f "$_wp_dark" ]; then
    export WALLPAPER_DARK="$_wp_dark"
else
    export WALLPAPER_DARK="$_wp_fallback"
fi
unset _wp_light _wp_dark _wp_fallback

#INCLUSIONS="/Users/$USER/assig/pro2/inclusions"
#OBJECTES=/Users/$USER/assig/pro2/objectes
export DICPATH="$XDG_CONFIG_HOME/Dictionary"
if command -v ag &>/dev/null; then
    export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
fi
if command -v bat &>/dev/null; then
    export FZF_DEFAULT_OPTS='--preview "bat --style=numbers --color=always --line-range :500 {}" --height 60% --border -m'
else
    export FZF_DEFAULT_OPTS='--height 60% --border -m'
fi
# export MANPAGER="/bin/sh -c \"col -b | vim --not-a-term -c 'set ft=man ts=8 nomod nolist noma' -\"" # SET VIM AS MANPAGER
# export MANPAGER="vim -M +MANPAGER -"
if command -v nvim &>/dev/null; then
    export MANPAGER='nvim +Man!' # use neovim as manpager
fi

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    #If you are using openjdk6 >= 1.6.1, the cleanest way to work around the hardcoded list is to warn the vm that xmonad is non-reparenting by exporting the appropriate environment variable:
    export _JAVA_AWT_WM_NONREPARENTING=1 # this fixes matlab
    export VISUAL=vim
    export EDITOR="$VISUAL"

    export TERMINAL="kitty"
    export QT_QPA_PLATFORMTHEME="qt5ct"
    source "$HOME/.config/distro"    # DISTRO variable
    if command -v laptop-detect &>/dev/null && laptop-detect ; then
        setxkbmap es
    fi
    if [ -d "$HOME/fib/LI/picosat-965/bin" ] ; then
        export PATH="$HOME/fib/LI/picosat-965/bin:$PATH"
    fi
fi


if [ -d "$HOME/.config/doom-emacs" ] ; then
    export PATH="$HOME/.config/doom-emacs/bin/:$PATH"
fi

if [[ "$OSTYPE" == "darwin"* ]]; then
    export PYTORCH_ENABLE_MPS_FALLBACK=1 # MACHINE LEARNING
    # export JAVA_HOME=`/usr/libexec/java_home`
    export VISUAL=nvim
    export EDITOR="$VISUAL"
    export PATH="$HOME/.emacs.d/bin/:$PATH"
    if [ -d "$HOME/bin" ] ; then
        export PATH="$HOME/bin:$PATH"
    fi
    #https://stackoverflow.com/questions/603785/environment-variables-in-mac-os-x
    export KITTY_CONFIG_DIRECTORY="$DOTFILES/.config/kitty/mac"
    #launchctl setenv KITTY_CONFIG_DIRECTORY $KITTY_CONFIG_DIRECTORY
    export SHELL_SESSION_HISTORY=0
    export SHELL_SESSIONS_DISABLE=1
    export LDFLAGS="-L/usr/local/opt/llvm/lib -Wl,-rpath,/usr/local/opt/llvm/lib"
    export CPPFLAGS="-I/usr/local/opt/llvm/include"
    PATH="/usr/local/opt/llvm/bin:$PATH"
    export PATH=/opt/homebrew/sbin:$PATH
    export PATH="/opt/homebrew/opt/qt@5/bin:$PATH"
    if [ -d "$HOME/fib/LI/picosat-965-mac/bin" ] ; then
        export PATH="$HOME/fib/LI/picosat-965-mac/bin:$PATH"
    fi
    export PYTHONPATH
    # Add the most recent Python user bin to PATH
    if command -v python3 &> /dev/null; then
        _pyver=$(python3 -c 'import sys; print(f"{sys.version_info.major}.{sys.version_info.minor}")')
        if [ -d "$HOME/Library/Python/$_pyver/bin" ] ; then
            export PATH="$HOME/Library/Python/$_pyver/bin:$PATH"
        fi
        unset _pyver
    fi
fi

###############################################################
# => Clean home directory
###############################################################

#export GEM_HOME="$XDG_DATA_HOME"/gem
#export GEM_SPEC_CACHE="$XDG_CACHE_HOME"/gem
#export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority" # This line will break some DMs.
export NOTMUCH_CONFIG="$XDG_CONFIG_HOME/notmuch-config"
export LESSHISTFILE="-"
export WGETRC="$XDG_CONFIG_HOME/wget/wgetrc"
export INPUTRC="$XDG_CONFIG_HOME/inputrc"
#export GNUPGHOME="$XDG_DATA_HOME/gnupg"
export WINEPREFIX="$XDG_DATA_HOME/wineprefixes/default"
export KODI_DATA="$XDG_DATA_HOME/kodi"
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/password-store"
export TMUX_TMPDIR="$XDG_RUNTIME_DIR"
export CARGO_HOME="$XDG_DATA_HOME/cargo"
export GOPATH="$XDG_DATA_HOME/go"
export ANSIBLE_CONFIG="$XDG_CONFIG_HOME/ansible/ansible.cfg"
export UNISON="$XDG_DATA_HOME/unison"
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

###############################################################
# => Cargo (sourced before local overrides so they can adjust it)
###############################################################

_cargo_env="${CARGO_HOME:-${XDG_DATA_HOME:-$HOME/.local/share}/cargo}/env"
# shellcheck disable=SC1090
[ -r "$_cargo_env" ] && . "$_cargo_env"
unset _cargo_env

###############################################################
# => Local overrides (machine-specific, not tracked by git)
###############################################################

# shellcheck disable=SC1091
[ -r "$HOME/.zshenv.local" ] && source "$HOME/.zshenv.local"
