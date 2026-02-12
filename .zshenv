# shellcheck shell=bash
# .zshenv — sourced by zsh on every invocation (login, interactive, script).
# Modular config lives in $HOME/.config/zsh/env.d/ and is loaded in order.
#
# IN MAC YOU MAY HAVE TO PUT HOMEBREW IN zprofile
# path_helper is executed from /etc/zprofile and sources /etc/paths then /etc/path.d
# zshrc and zprofile go after that, check order to make sure

_envdir="$HOME/.config/zsh/env.d"
if [ -d "$_envdir" ]; then
    for _f in "$_envdir"/*.zsh; do
        [ -r "$_f" ] && source "$_f"
    done
    unset _f
fi
unset _envdir
