#!/usr/bin/env bash
# Unit tests for the `commands` function in .config/zsh/functions, the catalog of the aliases,
# functions and scripts that these dotfiles define.
#
# They guard what the catalog must get right:
#   - Groups and descriptions come from annotations next to the code (`##@ Group` and `## text`
#     in the aliases and functions files, `# Description:` and `# Group:` in the header of each
#     script in .local/scripts/bin), never from a second list that can drift.
#   - Only what this shell has defined is listed: an alias in an `if` branch that did not run, a
#     documented function that was never defined, and a script without the executable bit are
#     not offered.
#   - When a name is defined twice, the definition that runs wins (alias, then function, then
#     script) and `-v` names what it hides. An alias that only renames one command is listed
#     next to that command, unless it has a description of its own.
#   - A filter word matches the name, group or description as typed, and the expansion only as a
#     whole word (`cp` must not find the cpu in /sys/devices/system/cpu).
#   - The output is the same from Bash and Zsh and from gawk, mawk and busybox awk, fits the
#     width of the terminal, and is colored only on a terminal (or with --color).
#   - The real files stay fully documented: every command has a description and a group, in
#     every branch (each distro, macOS, every optional tool), so a new alias without its `##`
#     line fails here instead of showing "(no description)".
#
# Every case runs in a clean `env -i` shell with a throwaway HOME (whose path has a space, to
# catch missing quotes), so nothing depends on or touches the real home folder. Terminal
# behavior is checked through a Python `pty`, because it only exists on a terminal.
# ALIASES=path and FUNCTIONS=path run the cases against other copies of the two files.

# The scripts handed to the child shells below are single-quoted on purpose: their variables
# must be expanded by the child shell, not by this one.
# shellcheck disable=SC2016

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
real_zdotdir="$repo_root/.config/zsh"
real_aliases="${ALIASES:-$real_zdotdir/aliases}"
real_functions="${FUNCTIONS:-$real_zdotdir/functions}"
real_bin="$repo_root/.local/scripts/bin"
case_dir="$(mktemp -d)"
trap 'rm -rf "$case_dir"' EXIT

bash_bin="$(command -v bash)"
zsh_bin="$(command -v zsh || true)"
base_path=/usr/bin:/bin
esc="$(printf '\033')"
nl=$'\n'

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

assert_equals() {
    local expected=$1 actual=$2 message=$3
    if [[ $expected != "$actual" ]]; then
        printf 'FAIL: %s\n--- expected\n%s\n--- actual\n%s\n' "$message" "$expected" "$actual" >&2
        exit 1
    fi
}

assert_contains() {
    [[ $1 == *"$2"* ]] || fail "$3 (missing: $2; got: $1)"
}

assert_not_contains() {
    [[ $1 != *"$2"* ]] || fail "$3 (unexpected: $2; got: $1)"
}

# has_line TEXT NEEDLE...: some single line of TEXT contains every NEEDLE.
has_line() {
    local text=$1 line needle ok
    shift
    while IFS= read -r line; do
        ok=1
        for needle in "$@"; do
            if [[ $line != *"$needle"* ]]; then
                ok=0
                break
            fi
        done
        if ((ok)); then
            return 0
        fi
    done <<< "$text"
    return 1
}

# has_exact_line TEXT LINE: some line of TEXT is exactly LINE.
has_exact_line() {
    local text=$1 wanted=$2 line
    while IFS= read -r line; do
        if [[ $line == "$wanted" ]]; then
            return 0
        fi
    done <<< "$text"
    return 1
}

assert_line() {
    local message=$1 text=$2
    shift 2
    has_line "$text" "$@" || fail "$message (no line has: $*; got: $text)"
}

assert_no_line() {
    local message=$1 text=$2
    shift 2
    if has_line "$text" "$@"; then
        fail "$message (a line has: $*; got: $text)"
    fi
}

# widest TEXT: the length of the longest line once color codes are removed.
widest() {
    local text=$1 line stripped longest=0
    while IFS= read -r line; do
        stripped=$(printf '%s' "$line" | sed "s/${esc}\\[[0-9;]*m//g")
        if ((${#stripped} > longest)); then
            longest=${#stripped}
        fi
    done <<< "$text"
    printf '%s\n' "$longest"
}

# assert_width TEXT LOW HIGH MESSAGE: the longest line is between LOW and HIGH characters.
assert_width() {
    local text=$1 low=$2 high=$3 message=$4 width
    width=$(widest "$text")
    if ((width < low || width > high)); then
        fail "$message (longest line is $width, expected $low to $high)"
    fi
}

###############################################################
# => Harness
###############################################################

# The function under test, cut out of the real file, so a case can load it next to a fixture
# of its own instead of next to the real aliases.
commands_fn="$case_dir/commands.sh"
awk '/^commands\(\) \{/ { p = 1 } p { print } p && /^\}/ { exit }' "$real_functions" > "$commands_fn"
[[ -s $commands_fn ]] || fail "commands() is not defined in $real_functions"

case_number=0
case_home=''
case_err="$case_dir/stderr"
case_columns=100
case_no_color=''
case_distro=''
case_ostype=''
case_path=$base_path
out='' rc=0 err=''

reset_settings() {
    case_columns=100
    case_no_color=''
    case_distro=''
    case_ostype=''
    case_path=$base_path
}

# new_case: a fresh HOME (its path holds a space) with an empty zsh folder and script folder.
new_case() {
    case_number=$((case_number + 1))
    case_home="$case_dir/case$case_number/home dir"
    mkdir -p "$case_home/.config/zsh" "$case_home/.local/scripts/bin"
    reset_settings
}

# put RELATIVE_PATH [MODE]: write stdin below the case HOME.
put() {
    local target="$case_home/$1"
    mkdir -p "$(dirname "$target")"
    cat > "$target"
    chmod "${2:-644}" "$target"
}

# catalog SHELL [ARGS...]: run `commands ARGS...` in a clean child shell that sees only the
# case. Aliases are expanded in function bodies, as in an interactive shell.
catalog() {
    local shell=$1
    shift
    local -a launcher
    case $shell in
        bash) launcher=("$bash_bin" --noprofile --norc -c) ;;
        zsh) launcher=("$zsh_bin" -f -c) ;;
        *) fail "unknown shell: $shell" ;;
    esac
    local script='shopt -s expand_aliases 2>/dev/null
if [ -n "${CASE_OSTYPE:-}" ]; then OSTYPE=$CASE_OSTYPE; fi
if [ -f "$ZDOTDIR/aliases" ]; then . "$ZDOTDIR/aliases"; fi
if [ -f "$ZDOTDIR/functions" ]; then . "$ZDOTDIR/functions"; fi
. "$COMMANDS_FN"
commands "$@"'
    env -i HOME="$case_home" ZDOTDIR="$case_home/.config/zsh" PATH="$case_path" TERM=dumb \
        COLUMNS="$case_columns" NO_COLOR="$case_no_color" DISTRO="$case_distro" \
        CASE_OSTYPE="$case_ostype" COMMANDS_FN="$commands_fn" \
        "${launcher[@]}" "$script" shell "$@" 2> "$case_err"
}

# run_case SHELL [ARGS...]: sets out, rc and err.
run_case() {
    out=$(catalog "$@") && rc=0 || rc=$?
    err=$(cat "$case_err")
}

# in_every_shell FUNCTION: call FUNCTION once per available shell, with the shell as argument.
in_every_shell() {
    local shell
    for shell in "${shells[@]}"; do
        "$1" "$shell"
    done
}

###############################################################
# => Fixture: every kind of definition the scanner must handle
###############################################################

new_case
fixture_home=$case_home
put .config/zsh/aliases <<'EOF'
# Plain comments and banners are not descriptions.
###############################################################

##@ First group

## Go up one directory.
alias ..="cd .."
## Copy, asking first.
alias cp="cp -i -v"
## Joined across
## two lines.
alias two="echo two"
alias plain="echo not described"
alias gr="fixture-recurse"
## Shorter name that has a description of its own.
alias gx="fixture-recurse"
## The alias that wins.
alias dup="echo alias wins"
v='echo from variable'
## Built from a variable.
alias fromvar="$v"
## Defined after a condition on the same line.
true && alias andalias="echo and"
## Multi-line value.
alias multi="echo one
echo	two"
if false
then
    ## Never defined, because this branch does not run.
    alias ghost="echo ghost"
fi
if true
then
    ## Described in the first branch only.
    alias both="echo one"
else
    alias both="echo two"
fi

##@ Second group

## A function in the aliases file.
fn_alias_file() { :; }
## A helper for other functions.
_hidden_helper() { :; }
## The end of a chain of aliases.
alias chain_b="echo end"
alias chain_a="chain_b"
EOF
put .config/zsh/functions <<'EOF'
##@ First group

## The function that the alias hides.
function dup() { echo function; }

##@ Third group

## Keyword form.
function fn_keyword() { :; }
## Space before the parentheses.
fn_spaced () { :; }
## Without parentheses.
function fn_bare { :; }
## Brace on the next line.
fn_brace()
{
    :
}
if false; then
    ## Documented but never defined.
    fn_ghost() { :; }
fi
EOF
put .local/scripts/bin/fixture-recurse 755 <<'EOF'
#!/usr/bin/env bash
# Description: Run things everywhere.
# Group: Third group
echo recurse
EOF
put .local/scripts/bin/dup 755 <<'EOF'
#!/bin/sh
# Description: The script that the alias and the function hide.
# Group: First group
echo script
EOF
put .local/scripts/bin/bare-script 755 <<'EOF'
#!/bin/sh
echo no header at all
EOF
put .local/scripts/bin/late-header 755 <<'EOF'
#!/bin/sh
echo code comes first
# Description: too late to count
# Group: Third group
EOF
put .local/scripts/bin/not-executable 644 <<'EOF'
#!/bin/sh
# Description: Missing the executable bit, so the shell cannot run it.
# Group: Third group
EOF
put .local/scripts/bin/subdir/nested 755 <<'EOF'
#!/bin/sh
# Description: Inside a folder, so not a command.
EOF

use_fixture() {
    case_home=$fixture_home
    reset_settings
}

# What the default view must print: groups in the order they first appear (aliases file, then
# functions file, then scripts), names sorted without regard to case inside a group, the
# alias gr folded into its script, and what has no group last.
expected_default='First group
  ..                    Go up one directory.
  andalias              Defined after a condition on the same line.
  both                  Described in the first branch only.
  cp                    Copy, asking first. (adds -i -v)
  dup                   The alias that wins.
  fromvar               Built from a variable.
  gx                    Shorter name that has a description of its own.
  multi                 Multi-line value.
  plain                 (no description) runs: echo not described
  two                   Joined across two lines.

Second group
  chain_a               (no description) runs: chain_b
  chain_b               The end of a chain of aliases.
  fn_alias_file         A function in the aliases file.

Third group
  fixture-recurse (gr)  Run things everywhere.
  fn_bare               Without parentheses.
  fn_brace              Brace on the next line.
  fn_keyword            Keyword form.
  fn_spaced             Space before the parentheses.

Other
  bare-script           (no description)
  late-header           (no description)'

check_default_view() {
    local shell=$1
    use_fixture
    run_case "$shell"
    assert_equals 0 "$rc" "$shell: commands exits 0"
    assert_equals '' "$err" "$shell: commands prints nothing on stderr"
    assert_equals "$expected_default" "$out" "$shell: default view of the fixture"
}
in_every_shell check_default_view

check_hidden_and_verbose() {
    local shell=$1
    use_fixture
    run_case "$shell"
    assert_not_contains "$out" _hidden_helper "$shell: a name that starts with an underscore is a helper and is hidden"
    assert_not_contains "$out" ghost "$shell: an alias or function that was never defined is not listed"
    assert_not_contains "$out" not-executable "$shell: a file without the executable bit is not listed"
    assert_not_contains "$out" nested "$shell: a file in a subfolder is not a command"

    run_case "$shell" -a
    assert_line "$shell: -a lists the helpers" "$out" _hidden_helper "A helper for other functions."

    run_case "$shell" -v
    assert_line "$shell: -v shows what an alias runs" "$out" "runs: cd .."
    assert_line "$shell: -v shows what the alias that wins hides (the function)" "$out" "hides: function in " "/.config/zsh/functions"
    assert_line "$shell: -v shows what the alias that wins hides (the script)" "$out" "hides: script in " "/.local/scripts/bin/dup"
    assert_line "$shell: -v shows where a function lives" "$out" "function: " "/.config/zsh/functions"
    assert_line "$shell: -v shows where a script lives" "$out" "script: " "/.local/scripts/bin/fixture-recurse"
    assert_line "$shell: a newline and a tab in an alias value become single spaces" "$out" "runs: echo one echo two"
    assert_no_line "$shell: -v replaces the (adds ...) hint with the full expansion" "$out" "(adds"
    assert_line "$shell: -v shows the full expansion of a self-named alias" "$out" "runs: cp -i -v"
}
in_every_shell check_hidden_and_verbose

check_filter() {
    local shell=$1
    use_fixture

    run_case "$shell" two
    assert_equals 0 "$rc" "$shell: a matching word exits 0"
    assert_line "$shell: a word matches a name and a description" "$out" two "Joined across two lines."
    assert_line "$shell: a word that is a whole word of an expansion matches" "$out" multi
    assert_no_line "$shell: other rows are filtered out" "$out" "Copy, asking first."

    run_case "$shell" THIRD
    assert_line "$shell: a word matches a group, whatever its case" "$out" fn_bare
    assert_line "$shell: ... and the scripts of that group" "$out" fixture-recurse
    assert_no_line "$shell: rows of other groups are filtered out" "$out" andalias

    run_case "$shell" condition
    assert_line "$shell: a word matches a description" "$out" andalias

    run_case "$shell" first group
    assert_line "$shell: every word has to match" "$out" ".." "Go up one directory."
    assert_no_line "$shell: a row that misses one of the words is out" "$out" fn_bare

    run_case "$shell" nothing-like-this
    assert_equals 1 "$rc" "$shell: no match exits 1"
    assert_equals '' "$out" "$shell: no match prints nothing on stdout"
    assert_equals 'commands: nothing matches: nothing-like-this' "$err" "$shell: no match says so on stderr"

    # "ech" is no whole word anywhere, while "echo" is a whole word in several expansions.
    run_case "$shell" ech
    assert_equals 1 "$rc" "$shell: a part of a word of an expansion is not a match"
    run_case "$shell" echo
    assert_equals 0 "$rc" "$shell: a whole word of an expansion is a match"
    assert_line "$shell: ... and finds the alias that runs it" "$out" plain

    run_case "$shell" -- -i
    assert_line "$shell: -- ends the options, so a word can start with a dash" "$out" cp "(adds -i -v)"

    run_case "$shell" fixture-recurse
    assert_line "$shell: the shortcut next to its command is found by the command name" "$out" "fixture-recurse (gr)"
    run_case "$shell" gr
    assert_line "$shell: ... and by the shortcut" "$out" "fixture-recurse (gr)"
}
in_every_shell check_filter

check_options() {
    local shell=$1
    use_fixture

    run_case "$shell" -h
    assert_equals 0 "$rc" "$shell: --help exits 0"
    assert_equals '' "$err" "$shell: --help prints nothing on stderr"
    assert_contains "$out" "Usage: commands" "$shell: --help prints the usage"
    assert_contains "$out" "--verbose" "$shell: --help lists the options"

    run_case "$shell" --bogus
    assert_equals 2 "$rc" "$shell: an unknown option exits 2"
    assert_equals '' "$out" "$shell: an unknown option prints nothing on stdout"
    assert_equals 'commands: unknown option: --bogus (try commands --help)' "$err" "$shell: an unknown option is named on stderr"

    run_case "$shell" -v -a --color two
    assert_equals 0 "$rc" "$shell: options combine with a filter word"
}
in_every_shell check_options

check_color_and_width() {
    local shell=$1
    use_fixture

    run_case "$shell"
    assert_not_contains "$out" "$esc" "$shell: piped output has no color"

    run_case "$shell" --color
    assert_contains "$out" "${esc}[1;34mFirst group${esc}[0m" "$shell: --color colors the group names"
    assert_contains "$out" "${esc}[36m..${esc}[0m" "$shell: --color colors the command names"

    # The dim words of a row share one span instead of getting one each.
    run_case "$shell" --color plain
    assert_contains "$out" "(no description) ${esc}[2mruns: echo not described${esc}[0m" "$shell: dim words share one color span"

    case_no_color=1
    run_case "$shell" --color
    assert_contains "$out" "$esc" "$shell: --color wins over NO_COLOR"
    use_fixture

    case_columns=60
    run_case "$shell"
    assert_width "$out" 0 60 "$shell: nothing is wider than COLUMNS=60"
    run_case "$shell" --color
    assert_width "$out" 0 60 "$shell: color codes do not count toward the width"

    case_columns=10
    run_case "$shell"
    assert_width "$out" 0 40 "$shell: a tiny COLUMNS is raised to 40"

    # Zsh keeps COLUMNS as an integer, so it turns a value that is not a number into 0 before
    # the function sees it, and the clamp above raises that to 40; only Bash passes it through.
    if [[ $shell == bash ]]; then
        case_columns=abc
        run_case "$shell"
        assert_equals 0 "$rc" "$shell: a COLUMNS that is not a number is ignored"
        assert_equals "$expected_default" "$out" "$shell: ... and the default width applies"
    fi
}
in_every_shell check_color_and_width

###############################################################
# => Long text and awkward input
###############################################################

# A description of 120 short words: it fills any width and wraps at spaces.
long_description=''
for i in $(seq 1 120); do
    long_description+="w$((i % 10))x "
done

check_wrapping() {
    local shell=$1
    new_case
    put .config/zsh/aliases <<EOF
##@ Wrapping
## $long_description
alias filler="echo filler"
## A description that ends in a word that is longer than any line could ever hold, a_word_that_is_longer_than_any_line_could_ever_hold_so_it_is_cut_short
alias cut_word="echo word"
## Name longer than the name column.
alias a_name_that_is_longer_than_the_name_column_allows="echo long"
EOF
    case_columns=60
    run_case "$shell"
    assert_width "$out" 50 60 "$shell: wrapped lines fill COLUMNS=60 without passing it"
    assert_contains "$out" "..." "$shell: a word that cannot fit is cut with an ellipsis"
    assert_line "$shell: a name longer than the column gets a line of its own" "$out" a_name_that_is_longer_than_the_name_column_allows
    assert_no_line "$shell: ... with no description on it" "$out" a_name_that_is_longer_than_the_name_column_allows "Name longer"
    assert_line "$shell: ... and the description on the next line" "$out" "Name longer"

    case_columns=120
    run_case "$shell"
    assert_width "$out" 110 120 "$shell: wrapped lines fill COLUMNS=120 without passing it"
    case_columns=500
    run_case "$shell"
    assert_width "$out" 110 120 "$shell: a huge COLUMNS is cut to 120"
}
in_every_shell check_wrapping

check_nothing_to_list() {
    local shell=$1
    new_case
    rm -r "$case_home/.config/zsh" "$case_home/.local"
    run_case "$shell"
    assert_equals 1 "$rc" "$shell: with no files and no scripts there is nothing to list"
    assert_equals '' "$out" "$shell: ... and nothing on stdout"
    assert_equals 'commands: no aliases, functions or scripts found' "$err" "$shell: ... only a message on stderr"
}
in_every_shell check_nothing_to_list

check_script_headers() {
    local shell=$1
    new_case
    printf '#!/usr/bin/env bash\n\n# Description:   Spaces   around and\ta tab are squeezed.\n#\n# Group:    Spaced group\n# Description: a second Description line is ignored\nset -e\n' \
        | put .local/scripts/bin/shebang-and-blank 755
    put .local/scripts/bin/no-shebang 755 <<'EOF'
# Description: A script whose header starts on line one.
# Group: Spaced group
echo hi
EOF
    run_case "$shell"
    assert_equals 0 "$rc" "$shell: scripts alone are enough to list"
    assert_equals 'Spaced group
  no-shebang         A script whose header starts on line one.
  shebang-and-blank  Spaces around and a tab are squeezed.' "$out" "$shell: the header is read from the first comment block"
}
in_every_shell check_script_headers

# Bash before 4.0 has no BASH_ALIASES; the expansion then comes from the text `alias NAME` prints.
check_bash_without_alias_table() {
    new_case
    put .config/zsh/aliases <<'EOF'
##@ Group
## Documented.
alias documented="echo documented"
EOF
    out=$(env -i HOME="$case_home" ZDOTDIR="$case_home/.config/zsh" PATH="$base_path" TERM=dumb COLUMNS=100 \
        COMMANDS_FN="$commands_fn" "$bash_bin" --noprofile --norc -c '
. "$ZDOTDIR/aliases"
unset BASH_ALIASES
. "$COMMANDS_FN"
commands -v documented' 2> "$case_err") || fail "bash: commands failed without BASH_ALIASES: $(cat "$case_err")"
    assert_line "bash: without the alias table the expansion is read from the alias output" "$out" "runs: echo documented"
}
check_bash_without_alias_table

###############################################################
# => The real files
###############################################################

# link_real_files: the case sees the repository's own aliases, functions and scripts, through
# symbolic links like the ones Stow makes. ALIASES and FUNCTIONS may point to other copies.
link_real_files() {
    new_case
    rm -r "$case_home/.config/zsh" "$case_home/.local/scripts/bin"
    mkdir -p "$case_home/.config/zsh"
    ln -s "$real_aliases" "$case_home/.config/zsh/aliases"
    ln -s "$real_functions" "$case_home/.config/zsh/functions"
    ln -s "$real_bin" "$case_home/.local/scripts/bin"
}

# Stub programs that make the optional branches of the aliases file run: eza for the file
# listing, a lock screen, nvim, and the ASUS tools for anime-toggle.
real_stubs="$case_dir/real_stubs"
mkdir -p "$real_stubs"
for stub in eza pipes.sh cmatrix nvim asusctl busctl; do
    printf '#!/bin/sh\nexit 0\n' > "$real_stubs/$stub"
    chmod +x "$real_stubs/$stub"
done

# check_real_catalog SHELL LABEL: the real files, with the case settings, are fully documented.
check_real_catalog() {
    local shell=$1 label=$2
    run_case "$shell" -a
    assert_equals 0 "$rc" "$shell, $label: the real catalog exits 0"
    assert_equals '' "$err" "$shell, $label: the real catalog prints nothing on stderr"
    assert_not_contains "$out" "(no description)" "$shell, $label: every command has a description (write a ## line above it)"
    if has_exact_line "$out" Other; then
        fail "$shell, $label: every command has a group (write a ##@ line above it): $out"
    fi
    assert_not_contains "$out" " $nl" "$shell, $label: no line ends in a space"
    if printf '%s' "$out" | LC_ALL=C grep -q '[^[:print:][:space:]]'; then
        fail "$shell, $label: the catalog is ASCII only, so its columns line up"
    fi
}

check_real_files() {
    local shell=$1 label
    link_real_files
    check_real_catalog "$shell" "this machine"

    # Every branch of the aliases file, whatever this machine is: each distro on Linux and
    # macOS, with and without the optional tools.
    for label in arch manjaro ubuntu ubuntu_windows; do
        link_real_files
        case_distro=$label
        case_ostype=linux-gnu
        case_path="$real_stubs:$base_path"
        check_real_catalog "$shell" "$label with the optional tools"
        link_real_files
        case_distro=$label
        case_ostype=linux-gnu
        check_real_catalog "$shell" "$label without them"
    done
    link_real_files
    case_ostype=darwin21
    check_real_catalog "$shell" "macOS"
}
in_every_shell check_real_files

# Scripts that the catalog skips are scripts the shell cannot run, so each one in the scripts
# folder must be executable, and describe itself near the top.
for script in "$real_bin"/*; do
    [[ -f $script ]] || continue
    [[ -x $script ]] || fail "$script is not executable; the shell cannot run it and the catalog skips it (chmod +x)"
    header="$nl$(head -n 12 "$script")$nl"
    [[ $header == *"${nl}# Description: "* ]] || fail "$script has no '# Description:' line in its first lines"
    [[ $header == *"${nl}# Group: "* ]] || fail "$script has no '# Group:' line in its first lines"
done

# Three scripts print their own header as --help, by line number, so the catalog lines above
# their text must stay out of it.
for script in git-ahead git-clean-branches clone-team; do
    help_text=$("$bash_bin" "$real_bin/$script" --help 2>&1) || fail "$script --help failed: $help_text"
    case $help_text in
        "$script:"*) ;;
        *) fail "$script --help does not start with its own name: ${help_text%%"$nl"*}" ;;
    esac
    assert_not_contains "$help_text" "Description:" "$script --help shows no catalog metadata"
    assert_not_contains "$help_text" "Group:" "$script --help shows no catalog metadata"
done

# Bash and Zsh agree on the real files, down to the byte.
if [[ -n $zsh_bin ]]; then
    for mode in default -v -a; do
        args=()
        if [[ $mode != default ]]; then
            args=("$mode")
        fi
        link_real_files
        case_distro=ubuntu
        case_ostype=linux-gnu
        case_path="$real_stubs:$base_path"
        run_case bash ${args[@]+"${args[@]}"}
        bash_out=$out
        run_case zsh ${args[@]+"${args[@]}"}
        assert_equals "$bash_out" "$out" "bash and zsh print the same real catalog ($mode)"
    done
fi

###############################################################
# => awk implementations
###############################################################

# The catalog is built by awk programs, and the awk in use is whichever comes first on PATH.
# Run the real files through every other implementation that is installed and expect the same
# bytes as with the default one.
make_awk_shim() {
    local dir="$case_dir/awk-$1"
    mkdir -p "$dir"
    printf '#!/bin/sh\nexec %s "$@"\n' "$2" > "$dir/awk"
    chmod +x "$dir/awk"
    printf '%s\n' "$dir"
}
link_real_files
case_distro=ubuntu
case_ostype=linux-gnu
case_path="$real_stubs:$base_path"
run_case bash -v
reference_out=$out
for variant in mawk busybox; do
    shim=''
    case $variant in
        mawk)
            if command -v mawk > /dev/null 2>&1; then
                shim=$(make_awk_shim mawk "$(command -v mawk)")
            fi
            ;;
        busybox)
            if command -v busybox > /dev/null 2>&1 && busybox awk 'BEGIN { exit 0 }' > /dev/null 2>&1; then
                shim=$(make_awk_shim busybox "$(command -v busybox) awk")
            fi
            ;;
    esac
    if [[ -z $shim ]]; then
        printf 'SKIP: %s is not installed.\n' "$variant"
        continue
    fi
    for shell in "${shells[@]}"; do
        case_path="$shim:$real_stubs:$base_path"
        run_case "$shell" -v
        assert_equals 0 "$rc" "$shell with $variant: exits 0 (stderr: $err)"
        assert_equals "$reference_out" "$out" "$shell with $variant: same catalog as with the default awk"
    done
done

###############################################################
# => Terminal behavior
###############################################################

if command -v python3 > /dev/null 2>&1; then
    cat > "$case_dir/pty_run.py" <<'EOF'
"""Run a command with a pseudo-terminal as its stdout and print what it wrote.

usage: pty_run.py COLUMNS COMMAND [ARG...]
"""
import fcntl
import os
import pty
import struct
import subprocess
import sys
import termios

columns = int(sys.argv[1])
master, slave = pty.openpty()
fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 24, columns, 0, 0))
child = subprocess.Popen(
    sys.argv[2:],
    stdin=subprocess.DEVNULL,
    stdout=slave,
    stderr=subprocess.DEVNULL,
    start_new_session=True,
)
os.close(slave)
chunks = []
while True:
    try:
        data = os.read(master, 65536)
    except OSError:
        break
    if not data:
        break
    chunks.append(data)
status = child.wait()
sys.stdout.buffer.write(b"".join(chunks).replace(b"\r", b""))
sys.exit(status)
EOF

    # on_terminal SHELL COLUMNS NO_COLOR [ARGS...]: out is what commands wrote to a terminal that is
    # COLUMNS wide. COLUMNS is not in the environment, so the width has to come from the terminal.
    on_terminal() {
        local shell=$1 columns=$2 no_color=$3
        shift 3
        local -a launcher
        case $shell in
            bash) launcher=("$bash_bin" --noprofile --norc -c) ;;
            zsh) launcher=("$zsh_bin" -f -c) ;;
        esac
        out=$(python3 "$case_dir/pty_run.py" "$columns" \
            env -i HOME="$case_home" ZDOTDIR="$case_home/.config/zsh" PATH="$base_path" TERM=xterm \
            NO_COLOR="$no_color" COMMANDS_FN="$commands_fn" \
            "${launcher[@]}" '
if [ -f "$ZDOTDIR/aliases" ]; then . "$ZDOTDIR/aliases"; fi
if [ -f "$ZDOTDIR/functions" ]; then . "$ZDOTDIR/functions"; fi
. "$COMMANDS_FN"
commands "$@"' shell "$@") || fail "$shell: commands failed on a terminal: $out"
    }

    check_terminal() {
        local shell=$1
        use_fixture

        on_terminal "$shell" 70 ''
        assert_contains "$out" "${esc}[1;34mFirst group${esc}[0m" "$shell: a terminal gets color without asking"
        assert_contains "$out" "commands WORD filters the list" "$shell: a terminal gets the hint after the full list"
        assert_width "$out" 0 70 "$shell: nothing is wider than the 70 columns of the terminal"

        on_terminal "$shell" 70 1
        assert_not_contains "$out" "$esc" "$shell: NO_COLOR turns the color off on a terminal"
        assert_contains "$out" "commands WORD filters the list" "$shell: ... and keeps the hint"

        on_terminal "$shell" 70 '' two
        assert_contains "$out" "${esc}[" "$shell: a filtered list is colored too"
        assert_not_contains "$out" "commands WORD filters" "$shell: ... but has no hint"

        on_terminal "$shell" 70 '' -v
        assert_not_contains "$out" "commands WORD filters" "$shell: -v has no hint"

        # The width comes from the terminal when COLUMNS is not set (Bash sets it only in an
        # interactive shell), so lines of a long description fill the terminal.
        new_case
        put .config/zsh/aliases <<EOF
##@ Wrapping
## $long_description
alias filler="echo filler"
EOF
        on_terminal "$shell" 70 1
        assert_width "$out" 60 70 "$shell: a terminal 70 columns wide gets lines of about 70"
        on_terminal "$shell" 110 1
        assert_width "$out" 100 110 "$shell: a terminal 110 columns wide gets lines of about 110"
    }
    in_every_shell check_terminal
else
    printf 'SKIP: python3 is not installed; the terminal checks are skipped.\n'
fi

printf 'PASS: commands\n'
