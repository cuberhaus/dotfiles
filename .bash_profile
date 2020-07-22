#
# ~/.bash_profile
#
# Mounts external disks automatically
udiskie > /dev/null 2>&1 &
# bash completion vim file to see what it is
source /usr/share/bash-completion/bash_completion
# Use vim
export VISUAL=vim
export EDITOR="$VISUAL"

# Add to path
if [ -d "$HOME/scripts" ] ; then
        PATH="$HOME/scripts:$PATH"
fi

if [ -d "$HOME/.config/pfetch" ] ; then
        PATH="$HOME/.config/pfetch:$PATH"
fi
export PATH="${PATH}:${HOME}/.local/bin/"
# Source .bashrc
[[ -f ~/.bashrc ]] && . ~/.bashrc
