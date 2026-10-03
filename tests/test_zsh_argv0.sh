#!/usr/bin/env bash
# The AppImage ARGV0 guard in ~/.zshenv and $ZDOTDIR/.zshenv.
#
# zsh uses an exported ARGV0 as argv[0] of every command it runs, and an editor installed as an
# AppImage (Cursor) hands ARGV0 to the shells it starts, so Python started there reported the AppImage
# as sys.executable and anything that re-launched it started the editor. The guard drops ARGV0 when
# APPIMAGE is set. zsh reads $ZDOTDIR/.zshenv instead of ~/.zshenv when ZDOTDIR is already set, as in
# the editor's shells, so both files carry the same block. The checks run the real blocks in zsh;
# only the block is taken from ~/.zshenv because the rest of that file sets up a whole login session.

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
case_dir="$(mktemp -d)"
trap 'rm -rf "$case_dir"' EXIT

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

if ! command -v zsh >/dev/null 2>&1; then
    printf 'SKIP: zsh is not installed.\n'
    exit 0
fi

begin='# --- AppImage ARGV0 guard ---'
end='# --- end AppImage ARGV0 guard ---'
home_zshenv="$repo_root/.zshenv"
zdotdir_zshenv="$repo_root/.config/zsh/.zshenv"

block_of() {
    awk -v b="$begin" -v e="$end" '$0 == b { on = 1 } on { print } $0 == e { on = 0 }' "$1"
}

first_code_line() {
    awk '!/^[[:space:]]*(#|$)/ { print; exit }' "$1"
}

# shellcheck disable=SC2016
guard_condition='if [[ -n "${APPIMAGE:-}" ]]; then'

for file in "$home_zshenv" "$zdotdir_zshenv"; do
    [[ "$(grep -cFx -- "$begin" "$file" || true)" == 1 ]] || fail "$file must hold exactly one ARGV0 guard"
    [[ "$(block_of "$file" | tail -n 1)" == "$end" ]] || fail "$file: the ARGV0 guard has no end marker"
    # Nothing may run before it: a command started earlier would still get the editor as argv[0].
    [[ "$(first_code_line "$file")" == "$guard_condition" ]] ||
        fail "$file: the ARGV0 guard must be the first thing it runs"
done
diff <(block_of "$home_zshenv") <(block_of "$zdotdir_zshenv") >/dev/null ||
    fail 'the guard in .zshenv and in .config/zsh/.zshenv must be identical'

fake='/fake/Editor.AppImage'
# bash -c with no further arguments reports argv[0] as it was started in $0; the child must expand it.
# shellcheck disable=SC2016
probe='bash -c "printf %s \"\$0\""'

# argv0_seen ZDOTDIR HOME APPIMAGE ARGV0 COMMAND: what a child of `zsh -c COMMAND` is started as.
argv0_seen() {
    local zdotdir="$1" home="$2" appimage="$3" argv0="$4" command="$5"
    local -a settings=("PATH=$PATH" "HOME=$home")
    [[ -z "$zdotdir" ]] || settings+=("ZDOTDIR=$zdotdir")
    [[ -z "$appimage" ]] || settings+=("APPIMAGE=$appimage")
    [[ -z "$argv0" ]] || settings+=("ARGV0=$argv0")
    env -i "${settings[@]}" zsh -c "$command"
}

empty_zdotdir="$case_dir/empty-zdotdir"
login_home="$case_dir/login-home"
mkdir -p "$empty_zdotdir" "$login_home" "$case_dir/home"
block_of "$home_zshenv" >"$login_home/.zshenv"

# The control: without a guard zsh starts commands under the editor's name. If it did not, the checks
# below would pass without proving anything.
got="$(argv0_seen "$empty_zdotdir" "$case_dir/home" "$fake" "$fake" "$probe")"
[[ "$got" == "$fake" ]] || fail "control: expected commands to start as $fake, got: $got"

got="$(argv0_seen "$repo_root/.config/zsh" "$case_dir/home" "$fake" "$fake" "$probe")"
[[ "$got" == bash ]] || fail "\$ZDOTDIR/.zshenv left argv[0] as: $got"

got="$(argv0_seen '' "$login_home" "$fake" "$fake" "$probe")"
[[ "$got" == bash ]] || fail "the home .zshenv left argv[0] as: $got"

# A shell that is not inside an AppImage keeps a deliberate ARGV0.
got="$(argv0_seen "$repo_root/.config/zsh" "$case_dir/home" '' custom "$probe")"
[[ "$got" == custom ]] || fail "without APPIMAGE the guard must leave ARGV0 alone, got: $got"

# `ARGV0=name command` on one line still renames that one command.
got="$(argv0_seen "$repo_root/.config/zsh" "$case_dir/home" "$fake" "$fake" "ARGV0=renamed $probe")"
[[ "$got" == renamed ]] || fail "a one-off ARGV0 must still work after the guard, got: $got"

# The symptom that started it: Python must report an interpreter, not the editor.
if command -v python3 >/dev/null 2>&1; then
    python_name='python3 -c "import os, sys; print(os.path.basename(sys.executable))"'
    got="$(argv0_seen "$empty_zdotdir" "$case_dir/home" "$fake" "$fake" "$python_name")"
    [[ "$got" == Editor.AppImage ]] || fail "control: expected sys.executable to name the editor, got: $got"
    got="$(argv0_seen "$repo_root/.config/zsh" "$case_dir/home" "$fake" "$fake" "$python_name")"
    [[ "$got" == python* ]] || fail "Python still reports a non-Python sys.executable: $got"
fi

printf 'zsh ARGV0 guard tests passed.\n'
