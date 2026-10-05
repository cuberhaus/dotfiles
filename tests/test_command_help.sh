#!/usr/bin/env bash
# Every command that the `commands` catalog lists answers -h and --help, and asking for help
# never does anything else.
#
# `NAME -h` is how you find out what a command does, so it must not act. An alias hands every
# word you type to its last command, and a script or function that does not look for -h takes
# it as its first argument. Before this test, these requests for help acted:
#   yolo -h              staged and committed everything (the chain ran before git push saw -h)
#   gc -h                committed the staged changes with the message "-h"
#   add_pat -h           rewrote every GitHub remote below the directory to use "-h" as its token
#   rclonepull_anki -h   restored Anki's data from Google Drive (the script ignored the third word)
#   tobash -h            asked for sudo and changed the login shell
#
# The commands come from `commands --list`, so a command added tomorrow is covered the day it
# exists, with no second list to keep in step. For each kind:
#   - scripts (.local/scripts/bin) and functions answer both -h and --help with exit status 0, a
#     text on stdout that names the command and has a "Usage" line, and nothing on stderr;
#   - help runs nothing else: no program is started that the command would need to act, and no
#     file appears in HOME, the working directory or the temporary directory;
#   - an alias is a shortcut, so -h reaches whatever it runs. That is only safe when the alias is
#     one simple command: a chain (`a; b`, `a && b`, `a | b`, `a &`) has already run its first
#     part when -h arrives at the last, and fixed arguments of one of our own scripts
#     (`restore-app-data anki --apply`) come before yours. Such an alias must be a function that
#     answers -h itself. An alias of one of our commands reaches that command's help, and an
#     alias of a git subcommand must leave a repository alone when it gets -h (`git commit -m`
#     takes "-h" as the message), and so is an alias of ssh or a browser with fixed arguments,
#     where what you add lands after the host or address as a remote command or another page
#     (`erag-tunnel -h` would open the tunnel). An alias of any other program leaves -h to that
#     program, which is why ls, df and grep keep their own -h;
#   - the functions that replaced aliases load in a shell that still holds the old aliases: a
#     function whose name is a live alias is a parse error, so each drops the alias first;
#   - yolo, clone-all and add_pat refuse a word that looks like an unknown option (exit status 2,
#     a message on stderr, nothing started): `yolo --dry-run` must not push.
#
# The commands run in the same variants test_commands.sh uses (each distro, macOS, Bash and
# Zsh), because functions and aliases exist only in some of them. Every probe runs in a clean
# `env -i` shell whose PATH holds a few harmless tools and tripwires, stand-ins for git, sudo,
# ssh, rclone, apt and the like that record what they were asked to run and refuse, so a command
# that lacks help is reported with what it tried to start instead of starting it. The HOME and
# the working directory are throwaway, and stdin is /dev/null.
# ALIASES=path and FUNCTIONS=path run the cases against other copies of the two files.

# The scripts handed to the child shells below are single-quoted on purpose: their variables
# must be expanded by the child shell, not by this one.
# shellcheck disable=SC2016

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
real_aliases="${ALIASES:-$repo_root/.config/zsh/aliases}"
real_functions="${FUNCTIONS:-$repo_root/.config/zsh/functions}"
real_scripts="$repo_root/.local/scripts"
sandbox="$(mktemp -d)"
trap 'rm -rf "$sandbox"' EXIT

bash_bin="$(command -v bash)"
zsh_bin="$(command -v zsh || true)"
us=$'\037'

shells=(bash)
if [[ -n $zsh_bin ]]; then
    shells+=(zsh)
else
    printf 'SKIP: zsh is not installed; testing Bash only.\n'
fi

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

# Problems are collected, not fatal, so one run lists every command that needs work. The same
# problem shows up in every shell and variant that defines the command, so it is stored once
# with the places it was seen.
problems=()
problem_where=()
problem() {
    local text="$1: $2" where=${3:-} i
    for ((i = 0; i < ${#problems[@]}; i++)); do
        if [[ ${problems[i]} == "$text" ]]; then
            problem_where[i]+="${where:+$where$'\n'}"
            return 0
        fi
    done
    problems+=("$text")
    problem_where+=("${where:+$where$'\n'}")
}

# join_with SEPARATOR ITEM...
join_with() {
    local separator=$1 item first=1
    shift
    for item in "$@"; do
        if ((first)); then
            printf '%s' "$item"
            first=0
        else
            printf '%s%s' "$separator" "$item"
        fi
    done
}

# brief TEXT: the first two lines of TEXT on one short line without color codes, for a message.
brief() {
    printf '%s' "$1" | LC_ALL=C sed $'s/\033\\[[0-9;]*m//g' | head -n 2 | tr '\n' ' ' | cut -c1-110
}

###############################################################
# => The sandbox
###############################################################

tools="$sandbox/tools"
home="$sandbox/home dir"
work_dir="$sandbox/work dir"
tmp_dir="$sandbox/tmp"
zdotdir="$home/.config/zsh"
tripwire_log="$sandbox/tripwire.log"
mkdir -p "$tools" "$home/.local" "$work_dir" "$tmp_dir" "$zdotdir"
: > "$tripwire_log"
ln -s "$real_scripts" "$home/.local/scripts"
ln -s "$real_aliases" "$zdotdir/aliases"
ln -s "$real_functions" "$zdotdir/functions"

# The only real programs a command can reach: they read, print or filter, and rm and mkdir
# stay inside the throwaway HOME. Everything else is missing (a command that needs it fails
# with "not found") or a tripwire.
for tool in awk basename bash cat cut date dirname env find grep head hostname id ls mkdir \
    mktemp python3 readlink rm sed sh sleep sort stty tail timeout tput tr uname uniq wc \
    whoami xargs; do
    tool_path="$(type -P "$tool" || true)"
    if [[ -n $tool_path ]]; then
        ln -s "$tool_path" "$tools/$tool"
    fi
done

# Stand-ins for everything that acts on the system, the network or a repository. Each one
# appends "name arguments" to the log and fails, so a command that starts one is caught and
# nothing happens. Several also decide which optional aliases and functions exist (nvim, eza,
# asusctl and busctl), so a tripwire is how those branches get defined.
for tool in apt apt-get asusctl brew busctl chsh cmatrix curl discord dpkg dunstify emacs \
    emacsclient eza gcloud gh ghc-pkg git google-chrome journalctl killall light brightnessctl \
    amixer notify-send npm nvim open pacman pacman-mirrors pactl pip pip3 pipes.sh pkill \
    reflector rclone rstudio-bin scp setxkbmap snap ssh sudo systemctl vim wget wmctrl xdg-open \
    xdotool xrdb yay docker; do
    cat > "$tools/$tool" <<EOF
#!/bin/sh
printf '%s\n' "\${0##*/} \$*" >> '$tripwire_log'
exit 97
EOF
    chmod +x "$tools/$tool"
done

if [[ -x $tools/timeout ]]; then
    limit=("$tools/timeout" 30)
else
    limit=()
fi

# What every child process sees. The scripts folder is on PATH as in Zsh (.zshenv does it).
run_env=(
    env -i
    HOME="$home" ZDOTDIR="$zdotdir" PATH="$tools:$home/.local/scripts/bin" TMPDIR="$tmp_dir"
    TERM=dumb LC_ALL=C COLUMNS=100 NO_COLOR=1 PYTHONDONTWRITEBYTECODE=1
    PIP_NO_INDEX=1 PIP_DISABLE_PIP_VERSION_CHECK=1
)

# Result of the last probe.
out='' err='' rc=0 tripped='' changed=''
probe_dir=$work_dir

snapshot() {
    find "$home" "$work_dir" "$tmp_dir" -print | LC_ALL=C sort
}

# probe COMMAND...: run COMMAND in probe_dir with stdin closed. Sets out, err, rc, tripped (what
# the tripwires were asked to run) and changed (files that appeared, vanished or moved).
probe() {
    local before after
    : > "$tripwire_log"
    before=$(snapshot)
    (cd "$probe_dir" && "$@") > "$sandbox/stdout" 2> "$sandbox/stderr" < /dev/null && rc=0 || rc=$?
    after=$(snapshot)
    out=$(< "$sandbox/stdout")
    err=$(< "$sandbox/stderr")
    tripped=$(< "$tripwire_log")
    changed=''
    if [[ $before != "$after" ]]; then
        changed=$(diff <(printf '%s\n' "$before") <(printf '%s\n' "$after") | grep '^[<>]' | head -n 3 | tr '\n' ' ' || true)
    fi
}

# The child shell sources both files like an interactive shell would, then either calls its
# arguments as a command (functions and `commands`) or evaluates a line of text, so that the
# alias on that line is expanded.
driver_call='shopt -s expand_aliases 2>/dev/null
if [ -n "${CASE_OSTYPE:-}" ]; then OSTYPE=$CASE_OSTYPE; fi
. "$ZDOTDIR/aliases"
. "$ZDOTDIR/functions"
"$@"'
driver_eval='shopt -s expand_aliases 2>/dev/null
if [ -n "${CASE_OSTYPE:-}" ]; then OSTYPE=$CASE_OSTYPE; fi
. "$ZDOTDIR/aliases"
. "$ZDOTDIR/functions"
eval "$1"'

# Extra NAME=value pairs for the next shell_probe, for a driver that needs more than the above.
extra_env=()

# shell_probe SHELL DISTRO OSTYPE DRIVER ARGUMENT...
shell_probe() {
    local shell=$1 distro=$2 ostype=$3 driver=$4
    shift 4
    local -a launcher
    case $shell in
        bash) launcher=("$bash_bin" --noprofile --norc -c) ;;
        zsh) launcher=("$zsh_bin" -f -c) ;;
        *) fail "unknown shell: $shell" ;;
    esac
    probe "${run_env[@]}" DISTRO="$distro" CASE_OSTYPE="$ostype" ${extra_env[@]+"${extra_env[@]}"} \
        ${limit[@]+"${limit[@]}"} "${launcher[@]}" "$driver" "$shell" "$@"
}

# The sandbox has to work before anything can be trusted to it: tripwires record calls, file
# changes are seen, and a program that is not allowed is missing.
probe "${run_env[@]}" "$bash_bin" --noprofile --norc -c 'git add -A; sudo true; : > "$HOME/canary"; cp a b 2>/dev/null; echo "cp status: $?"'
case $tripped in
    *"git add -A"*"sudo true"*) ;;
    *) fail "sandbox: the tripwires do not record the commands they stand in for (got: $tripped)" ;;
esac
[[ -n $changed ]] || fail "sandbox: a file created in HOME is not noticed"
[[ $out == "cp status: 127" ]] || fail "sandbox: a program outside the allowed list can be reached (got: $out)"
rm -f "$home/canary"

###############################################################
# => What counts as help
###############################################################

# check_help LABEL NAME [WHERE]: the last probe asked for help, and the command named NAME must
# have given it and done nothing else. WHERE says which shell and variant it ran in.
check_help() {
    local label=$1 name=$2 where=${3:-}
    local -a found=()
    if ((rc == 124)); then
        found+=("it did not finish in 30 seconds")
    elif ((rc != 0)); then
        found+=("exit status $rc instead of 0")
    fi
    if [[ -n $err ]]; then
        found+=("wrote to stderr: $(brief "$err")")
    fi
    if [[ -z $out ]]; then
        found+=("printed nothing on stdout")
    else
        [[ $out == *"$name"* ]] || found+=("the text does not name $name: $(brief "$out")")
        printf '%s\n' "$out" | grep -qi 'usage' || found+=("the text has no Usage line: $(brief "$out")")
    fi
    if [[ -n $tripped ]]; then
        found+=("it started: $(printf '%s' "$tripped" | head -n 3 | tr '\n' ',')")
    fi
    if [[ -n $changed ]]; then
        found+=("it changed files: $changed")
    fi
    if ((${#found[@]})); then
        problem "$label" "$(join_with '; ' "${found[@]}")" "$where"
    fi
}

###############################################################
# => Aliases are shortcuts
###############################################################

# alias_hazard EXPANSION: print why -h would not be safe at the end of this alias, or nothing.
alias_hazard() {
    local run=$1 bare
    local -a words
    local index=0 program rest
    # Text in quotes and redirections hold no operators, so drop them before looking for any.
    bare=$(printf '%s' "$run" | sed -E "s/'[^']*'//g; s/\"[^\"]*\"//g; s/[0-9]*[<>]+&[0-9-]*//g; s/&>+//g")
    if [[ $bare == *';'* || $bare == *'&'* || $bare == *'|'* || $bare == *'`'* || $bare == *'$('* ]]; then
        printf 'it chains commands (%s), so -h reaches only the last one after the others have run; make it a function that answers -h first' "$run"
        return 0
    fi
    # Fixed arguments for one of our own scripts sit in front of what you type.
    read -r -a words <<< "$run"
    while ((index < ${#words[@]})) && [[ ${words[index]} == sudo || ${words[index]} == bash || ${words[index]} == sh || ${words[index]} == env ]]; do
        index=$((index + 1))
    done
    program=${words[index]:-}
    rest=$((${#words[@]} - index - 1))
    if [[ $program == *'.local/scripts/'* || $program == '$scripts/'* ]] && ((rest > 0)); then
        printf 'it runs %s with arguments of its own (%s) before yours, so -h can be ignored or taken as data; make it a function that answers -h first' "${program##*/}" "$run"
    fi
    # ssh and the browsers read what follows their fixed host or address as a remote command or
    # another page, so -h reaches them as data: erag-tunnel -h would open the tunnel.
    case ${program##*/} in
        ssh | google-chrome | chromium | chromium-browser | firefox)
            if ((rest > 0)); then
                printf 'it runs %s with fixed arguments (%s), and -h lands after them as a remote command or another address, never as an option; make it a function that answers -h first' "${program##*/}" "$run"
            fi
            ;;
    esac
    return 0
}

git_state() {
    env -i HOME="$git_home" PATH="$git_bin:$tools" GIT_CONFIG_NOSYSTEM=1 \
        git -C "$scratch" rev-parse HEAD
    env -i HOME="$git_home" PATH="$git_bin:$tools" GIT_CONFIG_NOSYSTEM=1 \
        git -C "$scratch" status --porcelain=v1
    env -i HOME="$git_home" PATH="$git_bin:$tools" GIT_CONFIG_NOSYSTEM=1 \
        git -C "$scratch" for-each-ref
    env -i HOME="$git_home" PATH="$git_bin:$tools" GIT_CONFIG_NOSYSTEM=1 \
        git -C "$scratch" stash list
    env -i HOME="$git_home" PATH="$git_bin:$tools" GIT_CONFIG_NOSYSTEM=1 \
        git -C "$scratch" config --local --list
}

# The scratch repository for the git aliases: one commit, one staged file that a commit would
# take, one file nobody added.
git_real="$(type -P git || true)"
git_home="$sandbox/git home"
git_bin="$sandbox/git bin"
scratch="$sandbox/scratch repo"
if [[ -n $git_real ]]; then
    mkdir -p "$git_home" "$git_bin" "$scratch"
    ln -s "$git_real" "$git_bin/git"
    git_state_setup() {
        env -i HOME="$git_home" PATH="$git_bin:$tools" GIT_CONFIG_NOSYSTEM=1 git -C "$scratch" "$@"
    }
    git_state_setup init -q
    git_state_setup symbolic-ref HEAD refs/heads/main
    git_state_setup config user.name test
    git_state_setup config user.email test@example.invalid
    git_state_setup config commit.gpgsign false
    printf 'one\n' > "$scratch/one.txt"
    git_state_setup add one.txt
    git_state_setup commit -q -m initial
    printf 'two\n' > "$scratch/two.txt"
    git_state_setup add two.txt
    printf 'three\n' > "$scratch/three.txt"
fi

# check_git_alias NAME RUN: -h on this git alias must leave the repository as it was.
check_git_alias() {
    local name=$1 run=$2 before after
    before=$(git_state)
    probe_dir=$scratch
    probe env -i HOME="$git_home" ZDOTDIR="$zdotdir" PATH="$git_bin:$tools" TMPDIR="$tmp_dir" \
        TERM=dumb LC_ALL=C NO_COLOR=1 GIT_CONFIG_NOSYSTEM=1 GIT_TERMINAL_PROMPT=0 GIT_EDITOR=true \
        GIT_PAGER=cat PAGER=cat PYTHONDONTWRITEBYTECODE=1 \
        ${limit[@]+"${limit[@]}"} "$bash_bin" --noprofile --norc -c "$driver_eval" bash "$name -h"
    probe_dir=$work_dir
    after=$(git_state)
    if [[ $before != "$after" ]]; then
        problem "alias $name -h" "it changed the repository ($run); take it from a function that answers -h first"
    fi
}

###############################################################
# => Walk the catalog
###############################################################

# The scripts do not depend on the variant, so each is checked once; Python is needed for the
# two that start it.
scripts_seen=$'\n'
checked_scripts=0
checked_functions=0
checked_aliases=0
git_aliases=()
git_alias_runs=()

check_script() {
    local name=$1 path=$2 flag
    if grep -q 'python3' "$path" && [[ ! -e $tools/python3 ]]; then
        printf 'SKIP: python3 is not installed; not checking %s.\n' "$name"
        return 0
    fi
    for flag in -h --help; do
        probe "${run_env[@]}" DISTRO= ${limit[@]+"${limit[@]}"} "$path" "$flag"
        check_help "script $name $flag" "$name"
    done
    checked_scripts=$((checked_scripts + 1))
}

check_function() {
    local shell=$1 label=$2 distro=$3 ostype=$4 name=$5 flag
    for flag in -h --help; do
        shell_probe "$shell" "$distro" "$ostype" "$driver_call" "$name" "$flag"
        check_help "function $name $flag" "$name" "$shell, $label"
    done
    checked_functions=$((checked_functions + 1))
}

# check_alias SHELL LABEL DISTRO OSTYPE NAME RUN OWN: OWN lists the scripts and functions.
check_alias() {
    local shell=$1 label=$2 distro=$3 ostype=$4 name=$5 run=$6 own=$7 hazard first flag i known
    checked_aliases=$((checked_aliases + 1))
    hazard=$(alias_hazard "$run")
    if [[ -n $hazard ]]; then
        problem "alias $name" "$hazard" "$shell, $label"
        return 0
    fi
    first=${run%%[[:space:]]*}
    if printf '%s\n' "$own" | grep -qxF -- "$first"; then
        for flag in -h --help; do
            shell_probe "$shell" "$distro" "$ostype" "$driver_eval" "$name $flag"
            check_help "alias $name $flag (it runs $first)" "$first" "$shell, $label"
        done
    elif [[ $first == git && $shell == bash ]]; then
        known=0
        for ((i = 0; i < ${#git_aliases[@]}; i++)); do
            if [[ ${git_aliases[i]} == "$name" ]]; then
                known=1
            fi
        done
        if ((!known)); then
            git_aliases+=("$name")
            git_alias_runs+=("$run")
        fi
    fi
}

variants=(
    'plain::linux-gnu'
    'ubuntu:ubuntu:linux-gnu'
    'arch:arch:linux-gnu'
    'manjaro:manjaro:linux-gnu'
    'ubuntu_windows:ubuntu_windows:linux-gnu'
    'macOS::darwin21'
)

for variant in "${variants[@]}"; do
    IFS=: read -r label distro ostype <<< "$variant"
    for shell in "${shells[@]}"; do
        shell_probe "$shell" "$distro" "$ostype" "$driver_call" commands --list
        if ((rc != 0)) || [[ -z $out ]]; then
            problem "commands --list" "failed with exit $rc; $(brief "$err")" "$shell, $label"
            continue
        fi
        # Tabs are IFS whitespace, which would swallow the empty fields, so awk converts the
        # list to a separator that is not.
        catalog=$(printf '%s\n' "$out" | awk -F '\t' -v OFS="$us" '{ print $1, $2, $3, $4, $5 }')
        own=$(printf '%s\n' "$out" | awk -F '\t' '$1 != "alias" { print $2 }')
        while IFS=$us read -r kind name _group run source; do
            case $kind in
                script)
                    if [[ $scripts_seen != *$'\n'"$name"$'\n'* ]]; then
                        scripts_seen+="$name"$'\n'
                        check_script "$name" "$source"
                    fi
                    ;;
                function) check_function "$shell" "$label" "$distro" "$ostype" "$name" ;;
                alias) check_alias "$shell" "$label" "$distro" "$ostype" "$name" "$run" "$own" ;;
                *) problem "commands --list" "printed an unknown kind: $kind" "$shell, $label" ;;
            esac
        done <<< "$catalog"
    done
done

if [[ -n $git_real ]]; then
    for ((i = 0; i < ${#git_aliases[@]}; i++)); do
        check_git_alias "${git_aliases[i]}" "${git_alias_runs[i]}"
    done
else
    printf 'SKIP: git is not installed; not checking the git aliases.\n'
fi

###############################################################
# => A shell that still holds the old aliases
###############################################################

# These functions replaced aliases of an older copy of the aliases file. A shell that loaded
# that copy still has them as aliases when the file is sourced again, and a function whose name
# is a live alias is a parse error that stops the rest of the file. The file drops each alias
# first, and the Arch alias `mirror`, which shares its name with the Manjaro function, has to
# survive that. The driver defines the stale aliases, sources the file and reports what each
# name is afterwards.
stale_driver='shopt -s expand_aliases 2>/dev/null
if [ -n "${CASE_OSTYPE:-}" ]; then OSTYPE=$CASE_OSTYPE; fi
while IFS= read -r name; do alias "$name=true"; done <<< "$STALE_NAMES"
. "$ZDOTDIR/aliases"
while IFS= read -r name; do
    if [ -n "${ZSH_VERSION:-}" ]; then kind=$(whence -w "$name"); kind=${kind##* }; else kind=$(type -t "$name"); fi
    printf "%s=%s\n" "$name" "$kind"
done <<< "$STALE_NAMES"'

# check_stale SHELL LABEL DISTRO OSTYPE FUNCTIONS ALIASES: the names in FUNCTIONS (one per line)
# must be functions after the file is sourced over aliases of the same names, and the names in
# ALIASES must be aliases again.
check_stale() {
    local shell=$1 label=$2 distro=$3 ostype=$4 expect_functions=$5 expect_aliases=$6 name kind
    extra_env=(STALE_NAMES="$expect_functions${expect_aliases:+$'\n'$expect_aliases}")
    shell_probe "$shell" "$distro" "$ostype" "$stale_driver"
    extra_env=()
    if ((rc != 0)) || [[ -n $err ]]; then
        problem "sourcing the aliases file over the aliases of an older copy" \
            "exit $rc; $(brief "$err")" "$shell, $label"
        return 0
    fi
    while IFS= read -r name; do
        [[ -n $name ]] || continue
        kind=$(printf '%s\n' "$out" | sed -n "s/^$name=//p")
        [[ $kind == function ]] || problem "function $name over a live alias" \
            "it is still $kind; put an unalias for it before the definition" "$shell, $label"
    done <<< "$expect_functions"
    while IFS= read -r name; do
        [[ -n $name ]] || continue
        kind=$(printf '%s\n' "$out" | sed -n "s/^$name=//p")
        [[ $kind == alias ]] || problem "alias $name over a live alias" \
            "it became ${kind:-nothing}; an unalias meant for another command removed it" "$shell, $label"
    done <<< "$expect_aliases"
}

stale_functions='gc
gitsync
tobash
tozsh
discord
colemak
rclonepull_calibre
rclonepull_thunderbird
rclonepull_anki
rstudio
rs
erag-tunnel
erag-chrome'

for variant in "${variants[@]}"; do
    IFS=: read -r label distro ostype <<< "$variant"
    expect_functions=$stale_functions
    expect_aliases=''
    case $label in
        macOS) expect_functions+=$'\nclean' ;;
        manjaro) expect_functions+=$'\nmirror' ;;
        arch) expect_aliases=mirror ;;
    esac
    for shell in "${shells[@]}"; do
        check_stale "$shell" "$label" "$distro" "$ostype" "$expect_functions" "$expect_aliases"
    done
done

###############################################################
# => Unknown options are refused
###############################################################

# A command that acts and has no use for an extra argument must not act on one that looks like an
# option: `yolo --dry-run` would push, and a word that starts with a hyphen is never a GitHub
# token or owner. The last probe ran NAME with --bogus; it must have exited 2 with a message
# that names it on stderr, started nothing and changed nothing.
check_refusal() {
    local label=$1 name=$2 where=${3:-}
    local -a found=()
    ((rc == 2)) || found+=("exit status $rc instead of 2")
    [[ $err == *"$name"* ]] || found+=("stderr does not name $name: $(brief "$err")")
    [[ -z $out ]] || found+=("wrote to stdout: $(brief "$out")")
    [[ -z $tripped ]] || found+=("it started: $(printf '%s' "$tripped" | head -n 3 | tr '\n' ',')")
    [[ -z $changed ]] || found+=("it changed files: $changed")
    if ((${#found[@]})); then
        problem "$label" "$(join_with '; ' "${found[@]}")" "$where"
    fi
}

for refusing in yolo clone-all; do
    probe "${run_env[@]}" DISTRO= ${limit[@]+"${limit[@]}"} "$real_scripts/bin/$refusing" --bogus
    check_refusal "script $refusing --bogus" "$refusing"
done
for shell in "${shells[@]}"; do
    shell_probe "$shell" '' linux-gnu "$driver_call" add_pat --bogus
    check_refusal "function add_pat --bogus" add_pat "$shell"
done

###############################################################
# => Verdict
###############################################################

if ((${#problems[@]})); then
    # A problem seen in every shell and variant needs no place; the others say where.
    everywhere=$((${#shells[@]} * ${#variants[@]}))
    {
        printf 'FAIL: command help: %d problem(s)\n' "${#problems[@]}"
        for ((i = 0; i < ${#problems[@]}; i++)); do
            printf '  - %s\n' "${problems[i]}"
            places=$(printf '%s' "${problem_where[i]}" | sort -u | paste -sd ';' - | sed 's/;/; /g')
            count=$(printf '%s' "${problem_where[i]}" | sort -u | grep -c . || true)
            if ((count > 0 && count < everywhere)); then
                printf '      seen in: %s\n' "$places"
            fi
        done
        printf '\nA script or function handles -h and --help before it does anything else: print\n'
        printf '"Usage: NAME ..." and what the command does on stdout, and exit 0. An alias that\n'
        printf 'chains commands or puts arguments in front of yours becomes a function.\n'
        printf 'A command that acts on its arguments (yolo, clone-all, add_pat) refuses a word that\n'
        printf 'looks like an unknown option, with exit status 2 and a message on stderr, instead of\n'
        printf 'taking it as data. See "Every command answers -h" under "Shell command catalog" in\n'
        printf 'AGENTS.md.\n'
    } >&2
    exit 1
fi

printf 'PASS: command help (%d scripts, and %d function and %d alias checks over %d shells and variants)\n' \
    "$checked_scripts" "$checked_functions" "$checked_aliases" $((${#shells[@]} * ${#variants[@]}))
