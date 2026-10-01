#!/usr/bin/env bash
# Unit tests for the colors and prompt section of .bashrc.
#
# Every case runs in a clean `env -i` interactive Bash with a throwaway HOME so
# NO_COLOR, SSH_CONNECTION, TERM=dumb and friends from the developer's (or an
# agent's) environment cannot leak in. Requires Bash >= 4.4 for ${PS1@P}.

# The scripts handed to the child shells below are single-quoted on purpose: their
# variables must be expanded by the child Bash, not by this one.
# shellcheck disable=SC2016

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
case_dir="$(mktemp -d)"
trap 'rm -rf "$case_dir"' EXIT

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

if ((BASH_VERSINFO[0] < 4 || (BASH_VERSINFO[0] == 4 && BASH_VERSINFO[1] < 4))); then
    printf 'SKIP: bash >= 4.4 is required for prompt expansion tests.\n'
    exit 0
fi

home="$case_dir/home"
mkdir -p "$home"
esc=$'\e'

# clean_bash [VAR=value ...] -- ARGS...: run an interactive Bash with a minimal environment.
# Later assignments override the defaults, so callers can change TERM, LANG, NO_COLOR, ...
clean_bash() {
    local assignments=()
    while (($#)) && [[ $1 != -- ]]; do
        assignments+=("$1")
        shift
    done
    shift
    env -i PATH="$PATH" HOME="$home" HISTFILE="$home/.bash_history" \
        TERM=xterm-256color LANG=en_US.UTF-8 \
        GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_SYSTEM=/dev/null \
        "${assignments[@]}" \
        bash --noprofile --norc -i "$@" 2>/dev/null
}

# render_raw STATUS DIR [VAR=value ...]
# Print PS1 as Bash would draw it in DIR after a command exited with STATUS. The
# PROMPT_COMMAND chain runs first, exactly as it does before a real prompt.
render_raw() {
    local status=$1 dir=$2
    shift 2
    clean_bash "$@" -- -c '
        cd "$1" || exit 1
        source "$2"
        (exit "$3")
        eval "$PROMPT_COMMAND"
        printf "%s" "${PS1@P}"
    ' bash "$dir" "$repo_root/.bashrc" "$status"
}

# render STATUS DIR [VAR=value ...]: render_raw without the Readline \001/\002 width markers
# and without the carriage return Bash adds to every \n when line editing is active.
render() { render_raw "$@" | tr -d '\001\002\r'; }

# probe DIR SCRIPT [VAR=value ...]: run SCRIPT after sourcing .bashrc in DIR ($2 is the rc path).
probe() {
    local dir=$1 script=$2
    shift 2
    clean_bash "$@" -- -c '
        cd "$1" || exit 1
        source "$2"
        eval "$3"
    ' bash "$dir" "$repo_root/.bashrc" "$script"
}

assert_contains() { # haystack needle message
    [[ $1 == *"$2"* ]] || fail "$3: expected $(printf '%q' "$2") in $(printf '%q' "$1")"
}

assert_lacks() { # haystack needle message
    [[ $1 != *"$2"* ]] || fail "$3: did not expect $(printf '%q' "$2") in $(printf '%q' "$1")"
}

git_env=(
    GIT_AUTHOR_NAME=test GIT_AUTHOR_EMAIL=test@example.invalid
    GIT_COMMITTER_NAME=test GIT_COMMITTER_EMAIL=test@example.invalid
    GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_SYSTEM=/dev/null
)
git_() { env "${git_env[@]}" git "$@"; }

plain="$case_dir/plain"
mkdir -p "$plain"

# --- not interactive: nothing is defined and the environment is left alone -------------------
noninteractive="$(
    env -i PATH="$PATH" HOME="$home" TERM=xterm-256color LANG=en_US.UTF-8 \
        bash --noprofile --norc -c '
            before="${PS1-unset}"
            source "$1"
            declare -F __prompt_update >/dev/null && echo "hook-defined"
            [[ "${PS1-unset}" == "$before" ]] || echo "ps1-changed"
            [[ ${PROMPT_COMMAND-} != *__prompt_update* ]] || echo "hook-installed"
            echo done
        ' bash "$repo_root/.bashrc" 2>/dev/null
)"
[[ $noninteractive == 'done' ]] || fail "non-interactive shells must not get the prompt: $noninteractive"

# --- dumb terminals (Emacs/TRAMP, agent and CI terminals) keep the stock prompt ----------------
dumb="$(probe "$plain" 'declare -F __prompt_update >/dev/null && echo hook || echo no-hook' TERM=dumb)"
[[ $dumb == no-hook ]] || fail "TERM=dumb must not install the prompt hook: $dumb"
dumb_ps1="$(probe "$plain" 'printf "%s" "$PS1"' TERM=dumb)"
assert_lacks "$dumb_ps1" '__prompt' 'TERM=dumb PS1'

# --- colors: blue path, green prompt char after success, red after failure ---------------------
# Exact match: blank separator line, path line, prompt line, and no git segment outside a repo.
expected_ok=$'\n'"${esc}[34m$plain${esc}[0m"$'\n'"${esc}[32m❯${esc}[0m "
ok="$(render 0 "$plain")"
[[ $ok == "$expected_ok" ]] || fail "unexpected prompt after success: $(printf '%q' "$ok")"

expected_bad=$'\n'"${esc}[34m$plain${esc}[0m"$'\n'"${esc}[31m❯${esc}[0m "
bad="$(render 3 "$plain")"
[[ $bad == "$expected_bad" ]] || fail "unexpected prompt after failure: $(printf '%q' "$bad")"

# --- exit status passes through the hook so later PROMPT_COMMAND entries still see it ----------
status="$(probe "$plain" '(exit 7); __prompt_update; echo "rc=$?"')"
[[ $status == rc=7 ]] || fail "__prompt_update must preserve \$?: $status"

# --- re-sourcing keeps a single hook -----------------------------------------------------------
hooks="$(probe "$plain" 'source "$2"; source "$2"; grep -o __prompt_update <<<"$PROMPT_COMMAND" | wc -l')"
[[ ${hooks//[[:space:]]/} == 1 ]] || fail "expected exactly one __prompt_update hook after re-sourcing, got: $hooks"

# --- NO_COLOR keeps the layout but removes every escape sequence --------------------------------
expected_nc=$'\n'"$plain"$'\n'"❯ "
nc="$(render 0 "$plain" NO_COLOR=1)"
[[ $nc == "$expected_nc" ]] || fail "unexpected NO_COLOR prompt: $(printf '%q' "$nc")"

# --- ASCII fallback for non-UTF-8 locales and the Linux console ---------------------------------
ascii="$(render 0 "$plain" LANG=C LC_ALL=C)"
assert_contains "$ascii" "${esc}[32m>${esc}[0m " 'non-UTF-8 locale uses an ASCII prompt char'
assert_lacks "$ascii" '❯' 'non-UTF-8 locale avoids fancy glyphs'
console="$(render 0 "$plain" TERM=linux)"
assert_lacks "$console" '❯' 'Linux console avoids fancy glyphs'

# --- SSH sessions show user@host in yellow ------------------------------------------------------
user="$(id -un)"
ssh="$(render 0 "$plain" 'SSH_CONNECTION=192.0.2.1 50000 192.0.2.2 22')"
assert_contains "$ssh" "${esc}[33m$user@" 'SSH sessions show yellow user@host'
assert_lacks "$ok" "$user@" 'local sessions do not show user@host'

# --- git segment ---------------------------------------------------------------------------------
origin="$case_dir/origin.git"
work="$case_dir/work"
other="$case_dir/other"
git_ init -q --bare -b main "$origin"
git_ clone -q "$origin" "$work" 2>/dev/null
git_ -C "$work" checkout -q -b feature/demo
printf 'one\n' >"$work/a.txt"
printf 'one\n' >"$work/b.txt"
git_ -C "$work" add -A
git_ -C "$work" commit -qm init
git_ -C "$work" push -q -u origin feature/demo 2>/dev/null
git_ clone -q -b feature/demo "$origin" "$other" 2>/dev/null
printf 'two\n' >"$other/c.txt"
git_ -C "$other" add -A
git_ -C "$other" commit -qm remote
git_ -C "$other" push -q 2>/dev/null
printf 'local\n' >"$work/d.txt"
git_ -C "$work" add -A
git_ -C "$work" commit -qm local
git_ -C "$work" fetch -q 2>/dev/null
printf 'staged\n' >>"$work/b.txt"
git_ -C "$work" add b.txt
printf 'unstaged\n' >>"$work/a.txt"
printf 'x\n' >"$work/new.txt"
printf 'y\n' >"$work/other.txt"

repo_prompt="$(render 0 "$work")"
assert_contains "$repo_prompt" "${esc}[32mfeature/demo ⇣1⇡1" 'branch with behind/ahead is green'
assert_contains "$repo_prompt" "${esc}[33m+1" 'staged count is yellow'
assert_contains "$repo_prompt" "${esc}[33m!1" 'unstaged count is yellow'
assert_contains "$repo_prompt" "${esc}[34m?2" 'untracked count is blue'

# PS1 itself must stay constant while the directory, git state and exit status change:
# VS Code/Cursor shell integration wraps PS1 with its own markers and re-wraps it whenever it
# changes, so only the variables PS1 references may be refreshed by PROMPT_COMMAND.
static="$(probe "$plain" '
    before=$PS1
    cd "$WORK"
    (exit 1)
    eval "$PROMPT_COMMAND"
    [[ $PS1 == "$before" ]] && echo static || echo rewritten
' WORK="$work")"
[[ $static == static ]] || fail "PROMPT_COMMAND must not rewrite PS1: $static"

# Every escape sequence must sit inside its own Readline \001...\002 pair, or long command
# lines wrap in the wrong place. Checked on a prompt that uses all four colors: once the
# well-formed pairs are cut out, no marker and no escape character may remain.
shopt -s extglob
raw="$(render_raw 1 "$work")"
[[ $raw == *$'\001'* ]] || fail 'prompt has no Readline width markers'
unwrapped="${raw//$'\001'*([!$'\001\002'])$'\002'/}"
[[ $unwrapped != *[$'\001\002\e']* ]] ||
    fail "escape sequence or marker outside a well-formed \\001...\\002 pair: $(printf '%q' "$unwrapped")"
shopt -u extglob

repo_nc="$(render 0 "$work" NO_COLOR=1)"
assert_contains "$repo_nc" "$work feature/demo ⇣1⇡1 +1 !1 ?2" 'git segment without color'

repo_ascii="$(render 0 "$work" LC_ALL=C LANG=C)"
assert_contains "$repo_ascii" 'feature/demo v1^1' 'ASCII behind/ahead markers'

assert_lacks "$(render 0 "$work" PROMPT_GIT=0)" 'feature/demo' 'PROMPT_GIT=0 hides the git segment'

# --- drawing the prompt never writes to the repository ---------------------------------------------
# A plain `git status` opportunistically rewrites a stale index and takes index.lock while doing
# so, which would race with the editor's own background git calls. GIT_OPTIONAL_LOCKS=0 avoids it.
quiet="$case_dir/quiet"
git_ init -q -b main "$quiet"
printf 'x\n' >"$quiet/f"
git_ -C "$quiet" add -A
git_ -C "$quiet" commit -qm init
touch -t 200201010000 "$quiet/f"         # same content, different mtime: the index entry is stale
touch -t 200101010000 "$quiet/.git/index"
touch -t 200101010001 "$case_dir/index.marker"
render 0 "$quiet" >/dev/null
[[ -z "$(find "$quiet/.git/index" -newer "$case_dir/index.marker")" ]] ||
    fail 'rendering the prompt rewrote the git index'

# --- git states ----------------------------------------------------------------------------------
states="$case_dir/states"
git_ init -q -b main "$states"
printf 'x\n' >"$states/f"
git_ -C "$states" add -A
git_ -C "$states" commit -qm init
sha="$(git_ -C "$states" rev-parse HEAD)"
git_ -C "$states" checkout -q --detach
assert_contains "$(render 0 "$states")" "@${sha:0:8}" 'detached HEAD shows the short commit'

git_ -C "$states" checkout -q main
git_ -C "$states" checkout -q -b side
printf 'side\n' >"$states/f"
git_ -C "$states" commit -qam side
git_ -C "$states" checkout -q main
printf 'main\n' >"$states/f"
git_ -C "$states" commit -qam main
git_ -C "$states" merge -q side >/dev/null 2>&1 || true
assert_contains "$(render 0 "$states")" "${esc}[31m~1" 'merge conflicts are counted in red'

long="$case_dir/long"
git_ init -q -b feature/this-is-a-really-long-branch-name-for-testing "$long"
printf 'x\n' >"$long/f"
git_ -C "$long" add -A
git_ -C "$long" commit -qm init
assert_contains "$(render 0 "$long")" 'feature/this…-for-testing' 'long branch names are shortened'

# --- hostile branch names must be drawn literally, never executed --------------------------------
hostile="$case_dir/hostile"
git_ init -q -b main "$hostile"
printf 'x\n' >"$hostile/f"
git_ -C "$hostile" add -A
git_ -C "$hostile" commit -qm init
for name in 'a$(touch${IFS}PWNED)b' 'a`touch${IFS}PWNED`b' 'a"${IFS}"$((1+1))b'; do
    git_ -C "$hostile" checkout -q -b "$name"
    drawn="$(render 0 "$hostile")"
    [[ ! -e $hostile/PWNED ]] || fail "branch name was executed: $name"
    assert_contains "$drawn" "$name" 'hostile branch name is drawn literally'
    git_ -C "$hostile" checkout -q main
    git_ -C "$hostile" branch -q -D "$name"
done

# --- colors for the rest of the toolchain only in color mode ---------------------------------------
expected_ls=''
command -v dircolors >/dev/null 2>&1 && expected_ls='set'
tools="$(probe "$plain" 'echo "${GROFF_NO_SGR-unset}|${LESS_TERMCAP_md+set}|${LS_COLORS:+set}"')"
[[ $tools == "1|set|$expected_ls" ]] || fail "unexpected man page and LS_COLORS variables in color mode: $tools"
tools_nc="$(probe "$plain" 'echo "${GROFF_NO_SGR-unset}|${LESS_TERMCAP_md+set}|${LS_COLORS:+set}"' NO_COLOR=1)"
[[ $tools_nc == 'unset||' ]] || fail "NO_COLOR must not export color variables, got: $tools_nc"

printf 'Bash prompt tests passed.\n'
