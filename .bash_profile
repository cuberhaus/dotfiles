#
# ~/.bash_profile
#
# Mounts external disks automatically
udiskie &

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

# Source .bashrc
[[ -f ~/.bashrc ]] && . ~/.bashrc
