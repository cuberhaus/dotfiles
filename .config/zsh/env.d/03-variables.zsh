# shellcheck shell=bash
# Shell variables, editor, terminal, and OS-specific settings

# Zsh config directory
export ZDOTDIR="$XDG_CONFIG_HOME/zsh"

# Auto-detect the dotfiles repo by resolving the real path of this .zshenv
if [ -L "$HOME/.zshenv" ]; then
    DOTFILES="$(cd "$(dirname "$(readlink -f "$HOME/.zshenv")")" && pwd)"
else
    DOTFILES="$HOME/dotfiles/dotfiles"  # fallback
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

# Tools
export DICPATH="$XDG_CONFIG_HOME/Dictionary"
export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
export FZF_DEFAULT_OPTS='--preview "bat --style=numbers --color=always --line-range :500 {}" --height 60% --border -m'
export MANPAGER='nvim +Man!'

###############################################################
# => Linux
###############################################################

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    export _JAVA_AWT_WM_NONREPARENTING=1 # fixes matlab / xmonad
    export VISUAL=vim
    export EDITOR="$VISUAL"
    export TERMINAL="kitty"
    export QT_QPA_PLATFORMTHEME="qt5ct"
    source "$HOME/.config/distro"    # DISTRO variable
    if laptop-detect ; then
        setxkbmap es
    fi
    if [ -d "$HOME/fib/LI/picosat-965/bin" ] ; then
        export PATH="$HOME/fib/LI/picosat-965/bin:$PATH"
    fi
fi

###############################################################
# => macOS
###############################################################

if [[ "$OSTYPE" == "darwin"* ]]; then
    export PYTORCH_ENABLE_MPS_FALLBACK=1
    export VISUAL=nvim
    export EDITOR="$VISUAL"
    export PATH="$HOME/.emacs.d/bin/:$PATH"
    if [ -d "$HOME/bin" ] ; then
        export PATH="$HOME/bin:$PATH"
    fi
    export KITTY_CONFIG_DIRECTORY="$DOTFILES/.config/kitty/mac"
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
