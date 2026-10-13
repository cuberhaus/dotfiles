#!/usr/bin/env bash
# When using xinit command instead of lightDM this file will be sourced
# shellcheck source=/dev/null
[ -f "$HOME/.config/distro" ] && source "$HOME/.config/distro"

# Set environmental variables before starting i3
# .xprofile sources .zshenv and starts apps
# shellcheck source=/dev/null
[ -f "$HOME/.xprofile" ] && source "$HOME/.xprofile"

# Repeat rate for keyboard
xset r rate 660 25

# Start i3 if available
if command -v i3 >/dev/null 2>&1; then
    exec i3 # Starts i3
fi
