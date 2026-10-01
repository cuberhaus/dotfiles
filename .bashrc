###############################################################
# => Theme
###############################################################

### Base16 Shell THEME
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
    eval "$("$BASE16_SHELL/profile_helper.sh")"

###############################################################
# => Path
###############################################################

case ":${PATH:-}:" in
    *:"$HOME/.local/bin":*) ;;
    *) PATH="$HOME/.local/bin${PATH:+:$PATH}" ;;
esac
export PATH

# Prevent standalone installers (uv, cargo-dist, etc.) from modifying shell startup files
export UV_NO_MODIFY_PATH=1
export INSTALLER_NO_MODIFY_PATH=1

###############################################################
# => Configuration
###############################################################

# Enable Readline not waiting for additional input when a key is pressed.
#bind 'set keyseq-timeout 50'

export HISTCONTROL=ignoredups:erasedups   # no duplicate entries

# Enable vim mode
set -o vi

# If there are multiple matches for completion, Tab should cycle through them
bind 'TAB':menu-complete

# Display a list of the matching files
bind "set show-all-if-ambiguous on"

# Perform partial completion on the first Tab press,
# only start cycling full results on the second Tab press
bind "set menu-complete-display-prefix on"

# Complete backwards
bind '"\e[Z":menu-complete-backward'

# append to the history file, don't overwrite it
shopt -s histappend

# After each command, append to the history file and reread it
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=8192
HISTFILESIZE=16384

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

###############################################################
# => Colors and prompt
###############################################################

# Interactive shells only. TERM=dumb (Emacs/TRAMP, agent and CI terminals) keeps the
# stock prompt, and NO_COLOR (https://no-color.org) keeps the layout but drops every
# escape sequence. Only the 16 basic ANSI colors are used, so everything follows the
# Base16 palette selected above, including the light/dark toggle.
#
# The prompt mirrors the powerlevel10k "lean" look used in zsh:
#
#   ~/cuberhaus/dotfiles main ⇣1⇡2 ~1 +2 !3 ?4
#   ❯
#
# PS1 is assigned once and never rewritten: PROMPT_COMMAND only refreshes the
# variables it references. VS Code/Cursor shell integration wraps PS1 with its own
# markers and re-wraps it whenever it changes, so avoid prompt plugins that rebuild
# PS1 on every prompt. Set PROMPT_GIT=0 to hide the git segment, e.g. on huge
# repositories or slow network file systems.
if [[ $- == *i* && ${TERM:-dumb} != dumb ]]; then
    shopt -s promptvars

    __use_color=1
    [[ -n ${NO_COLOR-} ]] && __use_color=0

    # Colors are wrapped in \001..\002 (what \[ and \] expand to) so Readline does not
    # count them toward the prompt width. Unlike \[ \], this also works inside variables.
    __c_red='' __c_green='' __c_yellow='' __c_blue='' __c_reset=''
    if ((__use_color)); then
        __c_red=$'\001\e[31m\002' __c_green=$'\001\e[32m\002'
        __c_yellow=$'\001\e[33m\002' __c_blue=$'\001\e[34m\002'
        __c_reset=$'\001\e[0m\002'
    fi

    # Fancy glyphs need a UTF-8 locale and font: use ASCII on the Linux console and in
    # non-UTF-8 sessions.
    if [[ $TERM == linux ]] || ! [[ ${LC_ALL:-${LC_CTYPE:-${LANG-}}} =~ [Uu][Tt][Ff]-?8 ]]; then
        __g_char='>' __g_up='^' __g_down='v' __g_dots='...'
    else
        __g_char='❯' __g_up='⇡' __g_down='⇣' __g_dots='…'
    fi
    if ((EUID == 0)); then __g_char='#'; fi

    # Sets __prompt_git to " branch ⇣behind⇡ahead ~conflicts +staged !unstaged ?untracked"
    # for the current directory (empty outside a work tree). One git call per prompt.
    __prompt_git_info() {
        local out line branch='' oid='' ahead=0 behind=0 staged=0 unstaged=0 untracked=0 conflicts=0 xy info
        out=$(GIT_OPTIONAL_LOCKS=0 git status --porcelain=v2 --branch --ignore-submodules=dirty 2>/dev/null) || return 0
        while IFS= read -r line; do
            case $line in
                '# branch.oid '*) oid=${line#'# branch.oid '} ;;
                '# branch.head '*) branch=${line#'# branch.head '} ;;
                '# branch.ab '*)
                    line=${line#'# branch.ab +'}
                    ahead=${line%% *}
                    behind=${line##*-}
                    ;;
                '1 '* | '2 '*)
                    xy=${line:2:2}
                    [[ ${xy:0:1} == . ]] || staged=$((staged + 1))
                    [[ ${xy:1:1} == . ]] || unstaged=$((unstaged + 1))
                    ;;
                'u '*) conflicts=$((conflicts + 1)) ;;
                '? '*) untracked=$((untracked + 1)) ;;
            esac
        done <<<"$out"
        [[ -n $branch ]] || return 0
        if [[ $branch == '(detached)' ]]; then branch=@${oid:0:8}; fi
        if ((${#branch} > 32)); then branch=${branch:0:12}$__g_dots${branch: -12}; fi
        info=" $__c_green$branch"
        if ((behind)); then info+=" $__g_down$behind"; fi
        if ((ahead)); then
            ((behind)) || info+=' '
            info+=$__g_up$ahead
        fi
        if ((conflicts)); then info+=" $__c_red~$conflicts"; fi
        if ((staged)); then info+=" $__c_yellow+$staged"; fi
        if ((unstaged)); then info+=" $__c_yellow!$unstaged"; fi
        if ((untracked)); then info+=" $__c_blue?$untracked"; fi
        __prompt_git=$info$__c_reset
    }

    # Refreshes the dynamic prompt pieces. It runs first in PROMPT_COMMAND, so $? is still
    # the status of the command that just finished, and it is passed on unchanged.
    __prompt_update() {
        local rc=$?
        if ((rc == 0)); then
            __prompt_char=$__c_green$__g_char$__c_reset
        else
            __prompt_char=$__c_red$__g_char$__c_reset
        fi
        __prompt_git=''
        if [[ ${PROMPT_GIT-1} != 0 ]] && hash git 2>/dev/null; then __prompt_git_info; fi
        return "$rc"
    }

    # user@host is only shown when it matters: as root (red) or over SSH (yellow).
    __prompt_ctx=''
    if ((EUID == 0)); then
        __prompt_ctx=$__c_red'\u@\h'$__c_reset' '
    elif [[ -n ${SSH_CONNECTION-} ]]; then
        __prompt_ctx=$__c_yellow'\u@\h'$__c_reset' '
    fi
    __prompt_char=$__c_green$__g_char$__c_reset
    __prompt_git=''
    PS1='\n'$__prompt_ctx$__c_blue'\w'$__c_reset'${__prompt_git}\n${__prompt_char} '
    unset __prompt_ctx

    if [[ ${PROMPT_COMMAND-} != *__prompt_update* ]]; then
        PROMPT_COMMAND="__prompt_update${PROMPT_COMMAND:+$'\n'$PROMPT_COMMAND}"
    fi

    if ((__use_color)); then
        # LS_COLORS drives ls, eza, fd, tree and the Readline completion list below.
        if [[ -z ${LS_COLORS-} ]] && command -v dircolors >/dev/null 2>&1; then
            if [[ -r $HOME/.dircolors ]]; then
                eval "$(dircolors -b "$HOME/.dircolors")"
            else
                eval "$(dircolors -b)"
            fi
        fi

        # Color completion candidates by file type and highlight their common prefix.
        bind 'set colored-stats on'
        bind 'set colored-completion-prefix on'

        # Colored man pages. groff >= 1.23 emits its own SGR codes, which bypass
        # LESS_TERMCAP_* unless GROFF_NO_SGR is set.
        export GROFF_NO_SGR=1
        export LESS_TERMCAP_mb=$'\e[1;31m' # blink
        export LESS_TERMCAP_md=$'\e[1;34m' # bold: headings, options
        export LESS_TERMCAP_me=$'\e[0m'    # end bold/blink
        export LESS_TERMCAP_so=$'\e[1;33m' # standout: status line, search hits
        export LESS_TERMCAP_se=$'\e[0m'    # end standout
        export LESS_TERMCAP_us=$'\e[1;32m' # underline: arguments
        export LESS_TERMCAP_ue=$'\e[0m'    # end underline
    fi
    unset __use_color
fi

###############################################################
# => Aliases and functions
###############################################################

if [ -r "$HOME/.config/distro" ]; then
    source "$HOME/.config/distro"
fi

_zshdir="$HOME/.config/zsh"
if [ -f "$_zshdir/aliases" ]; then
    source "$_zshdir/aliases"
fi

if [ -f "$_zshdir/functions" ]; then
    source "$_zshdir/functions"
fi
unset _zshdir

_cargo_env="${CARGO_HOME:-${XDG_DATA_HOME:-$HOME/.local/share}/cargo}/env"
[ -r "$_cargo_env" ] && . "$_cargo_env"
unset _cargo_env
