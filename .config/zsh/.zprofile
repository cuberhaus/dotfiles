# ~/.config/zsh/.zprofile
emulate sh -c 'source /etc/profile'

if [[ "$DISTRO" == "arch" ]]; then
    # Mounts external disks automatically
    udiskie > /dev/null 2>&1 &
    [[ -f ~/.config/zsh/.zshrc ]] && . ~/.config/zsh/.zshrc
fi
