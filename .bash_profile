# Source .bashrc
[[ -f ~/.bashrc ]] && . ~/.bashrc

_cargo_env="${CARGO_HOME:-${XDG_DATA_HOME:-$HOME/.local/share}/cargo}/env"
[ -r "$_cargo_env" ] && . "$_cargo_env"
unset _cargo_env
