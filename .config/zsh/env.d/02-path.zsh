# shellcheck shell=bash
# PATH additions — directories are checked before being added

if [ -d "$HOME/.local/bin" ] ; then
    PATH="$HOME/.local/bin:$PATH"
fi

if [ -d "$HOME/.local/share/cargo/bin" ] ; then
    PATH="$HOME/.local/share/cargo/bin:$PATH"
fi

if [ -d "$HOME/.local/scripts/bin" ] ; then
    PATH="$HOME/.local/scripts/bin:$PATH"
fi

if [[ "$DESKTOP_SESSION" == "cinnamon" ]]; then
    if [ -d "$HOME/.local/scripts/cinnamon_path" ] ; then
        PATH="$HOME/.local/scripts/cinnamon_path:$PATH"
    fi
fi

## WIP (check if $DESKTOP_SESSION on gnome actually becomes "gnome")
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

if [ -d "$HOME/.config/doom-emacs" ] ; then
    export PATH="$HOME/.config/doom-emacs/bin/:$PATH"
fi

# Gnome keyring (SSH agent)
if [ -n "$DESKTOP_SESSION" ];then
    eval "$(gnome-keyring-daemon --start)"
    export SSH_AUTH_SOCK
fi
