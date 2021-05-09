#!/bin/sh
# When using xinit command instead of lightDM this file will be sourced
source $HOME/config/distro

# Set environmental variables before starting i3
# .xprofile sources .zshenv and starts apps
[ -f $HOME/.xprofile ] && source $HOME/.xprofile

# Start i3 on arch
if [[ "$DISTRO" == "arch" ]]; then
    i3 # Starts i3
fi
xset r rate 660 25
