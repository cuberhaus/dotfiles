#
# ~/.bash_profile
#
udiskie &
export VISUAL=vim
export EDITOR="$VISUAL"

if [ -d "$HOME/scripts" ] ; then
        PATH="$HOME/scripts:$PATH"
fi

if [ -d "$HOME/.config/pfetch" ] ; then
        PATH="$HOME/.config/pfetch:$PATH"
fi

[[ -f ~/.bashrc ]] && . ~/.bashrc
