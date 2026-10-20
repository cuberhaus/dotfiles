# ~/.config/zsh/.zprofile
# if [[ "$DISTRO" == "arch" ]]; then
    # [[ -f ~/.config/zsh/.zshrc ]] && . ~/.config/zsh/.zshrc
# fi

# # Activating numlock on startup
# setleds -D +num

if [[ "$DISTRO" == "ubuntu" || "$DISTRO" == "ubuntu_windows" ]]; then
        if [ -n "${DISPLAY:-}" ] && command -v setxkbmap >/dev/null 2>&1; then
            setxkbmap -layout es -option caps:swapescape >/dev/null 2>&1
        fi
fi

if [[ "$OSTYPE" == "darwin"* ]]; then
    export PATH="/opt/homebrew/bin:$PATH"
fi
export GPG_TTY=$(tty)

PATH="$HOME/.local/bin:$PATH"
export npm_config_prefix="$HOME/.local"
