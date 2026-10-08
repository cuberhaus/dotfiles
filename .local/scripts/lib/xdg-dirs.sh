# shellcheck shell=bash
# Resolve XDG user directories without xdg-user-dir, so a script behaves the same on a
# minimal machine, in a container, and in a test with a throwaway HOME.
#
#   xdg_user_dir NAME [FALLBACK]   NAME is DESKTOP, DOWNLOAD, PICTURES, ...
#
# Prints XDG_<NAME>_DIR of $XDG_CONFIG_HOME/user-dirs.dirs, or FALLBACK when the file or the
# key is missing. Like GLib, which the desktop reads the file with, it expands a leading
# $HOME/ and nothing else. The user-dirs specification switches a folder off by pointing it
# at the home folder, so that value, and any relative one, also gives FALLBACK.

xdg_user_dir() {
    local name=$1 fallback=${2:-}
    local file="${XDG_CONFIG_HOME:-$HOME/.config}/user-dirs.dirs" line value

    if [[ -r $file ]]; then
        line=$(grep -E "^[[:space:]]*XDG_${name}_DIR=" -- "$file" | tail -n 1 || true)
        value=${line#*=}
        value=${value#\"}
        value=${value%\"}
        # The file holds the text $HOME/ literally; it is not meant to expand here.
        # shellcheck disable=SC2016
        case $value in
        '$HOME/'*) value=$HOME/${value#'$HOME/'} ;;
        esac
        value=${value%/}
        if [[ $value == /* && $value != "${HOME%/}" ]]; then
            printf '%s\n' "$value"
            return 0
        fi
    fi
    printf '%s\n' "$fallback"
}
