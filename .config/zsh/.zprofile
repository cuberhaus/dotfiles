# ~/.config/zsh/.zprofile
# if [[ "$DISTRO" == "arch" ]]; then
    # [[ -f ~/.config/zsh/.zshrc ]] && . ~/.config/zsh/.zshrc
# fi

# # Activating numlock on startup
# setleds -D +num

if [[ "$DISTRO" == "ubuntu" || "$DISTRO" == "ubuntu_windows" ]]; then
        if [ -n "${DISPLAY:-}" ] && command -v setxkbmap >/dev/null 2>&1; then
            if [ -x "${DOTFILES:-$HOME}/.local/scripts/bin/keyboard-swap" ]; then
                "${DOTFILES:-$HOME}/.local/scripts/bin/keyboard-swap" --sync-x11 >/dev/null 2>&1
            else
                setxkbmap -layout es -option caps:swapescape >/dev/null 2>&1
            fi
        fi
fi

if [[ "$OSTYPE" == "darwin"* ]]; then
    export PATH="/opt/homebrew/bin:$PATH"
fi
GPG_TTY="$(tty)" || true
export GPG_TTY

PATH="$HOME/.local/bin:$PATH"
export npm_config_prefix="$HOME/.local"
