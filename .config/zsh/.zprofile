# ~/.config/zsh/.zprofile
# if [[ "$DISTRO" == "arch" ]]; then
    # [[ -f ~/.config/zsh/.zshrc ]] && . ~/.config/zsh/.zshrc
# fi

# # Activating numlock on startup
# setleds -D +num

if [[ "$DISTRO" == "ubuntu" ]]; then
    setxkbmap -option caps:swapescape
fi

if [[ "$OSTYPE" == "darwin"* ]]; then
    export PATH="/opt/homebrew/bin:$PATH"
fi
export GPG_TTY=$(tty)

PATH="$HOME/.local/bin:$PATH"
export npm_config_prefix="$HOME/.local"
