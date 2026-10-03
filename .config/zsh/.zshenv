# shellcheck shell=bash
# zsh reads this file, instead of ~/.zshenv, when ZDOTDIR is already set in its environment, which is
# the case in the shells an editor starts (Cursor's terminals and agent shells). Keep it to what must
# run there: the rest of their environment is inherited, and ~/.zshenv sets it up for a login session.

# --- AppImage ARGV0 guard ---
# An editor installed as an AppImage (Cursor) hands ARGV0 to the shells it starts. zsh uses an exported
# ARGV0 as argv[0] of every command it runs, so Python started there reports the AppImage as
# sys.executable and anything that re-launches it (multiprocessing workers, subprocess) starts the
# editor. The same block is in ~/.zshenv and in $ZDOTDIR/.zshenv (.config/zsh/.zshenv) because zsh reads
# only one of them: the second when ZDOTDIR is already set, as in the editor's shells. Keep them
# identical. `ARGV0=name command` on a single line still works.
if [[ -n "${APPIMAGE:-}" ]]; then
    unset ARGV0
fi
# --- end AppImage ARGV0 guard ---
