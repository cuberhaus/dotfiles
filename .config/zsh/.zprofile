# ~/.config/zsh/.zprofile

#emulate sh -c 'source /etc/profile'

if [[ "$DISTRO" == "arch" ]]; then
    # Mounts external disks automatically
    # 
    # [[ -f ~/.config/zsh/.zshrc ]] && . ~/.config/zsh/.zshrc
fi

if [[ "$OSTYPE" == "darwin"* ]]; then
    export PATH="/opt/homebrew/bin:$PATH"   
fi
export GPG_TTY=$(tty)

