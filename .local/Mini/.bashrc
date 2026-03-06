###############################################################
# => Configuration
###############################################################

# Enable Readline not waiting for additional input when a key is pressed.
set keyseq-timeout 50

export HISTCONTROL=ignoredups:erasedups   # no duplicate entries
export EDITOR=vim
export VISUAL=vim
export LESSHISTFILE="-"

# Enable vim mode
set -o vi

# Ctrl+L to clear screen in vi mode
bind -m vi-command 'Control-l: clear-screen'
bind -m vi-insert 'Control-l: clear-screen'

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

# cd into a directory by just typing its name
shopt -s autocd

# Auto-correct minor typos in cd directory names
shopt -s cdspell

###############################################################
# => Path
###############################################################

[ -d "$HOME/.local/bin" ] && PATH="$HOME/.local/bin:$PATH"
[ -d "$HOME/.local/scripts/bin" ] && PATH="$HOME/.local/scripts/bin:$PATH"

###############################################################
# => Prompt (with git branch)
###############################################################

__git_branch() {
    local branch
    branch=$(git symbolic-ref --short HEAD 2>/dev/null || git rev-parse --short HEAD 2>/dev/null)
    [[ -n "$branch" ]] && printf ' (%s)' "$branch"
}

if [ "$(id -u)" -eq 0 ]; then
    PS1='\[\e[1;31m\]\u\[\e[0m\]@\[\e[1;33m\]\h\[\e[0m\]:\[\e[1;34m\]\w\[\e[0;35m\]$(__git_branch)\[\e[0m\]\$ '
else
    PS1='\[\e[1;32m\]\u\[\e[0m\]@\[\e[1;33m\]\h\[\e[0m\]:\[\e[1;34m\]\w\[\e[0;35m\]$(__git_branch)\[\e[0m\]\$ '
fi

###############################################################
# => Functions
###############################################################

mkcd() { mkdir -pv "$1" && cd "$1" || return; }

extract() {
    if [ ! -f "$1" ]; then
        echo "'$1' is not a valid file"
        return 1
    fi
    case "$1" in
        *.tar.bz2) tar xjf "$1"   ;;
        *.tar.gz)  tar xzf "$1"   ;;
        *.tar.xz)  tar xf "$1"    ;;
        *.tar.zst) unzstd "$1"    ;;
        *.bz2)     bunzip2 "$1"   ;;
        *.rar)     unrar x "$1"   ;;
        *.gz)      gunzip "$1"    ;;
        *.tar)     tar xf "$1"    ;;
        *.tbz2)    tar xjf "$1"   ;;
        *.tgz)     tar xzf "$1"   ;;
        *.zip)     unzip "$1"     ;;
        *.Z)       uncompress "$1";;
        *.7z)      7z x "$1"      ;;
        *.deb)     ar x "$1"      ;;
        *)         echo "'$1' cannot be extracted via extract()" ;;
    esac
}

# Normalize `open` across Linux, macOS, and Windows
if [ ! "$(uname -s)" = 'Darwin' ]; then
    if grep -q Microsoft /proc/version 2>/dev/null; then
        alias open='explorer.exe'
    else
        alias open='xdg-open'
    fi
fi

# `o` with no arguments opens the current directory, otherwise opens the given location
o() {
    if [ $# -eq 0 ]; then
        open . </dev/null &>/dev/null &
    else
        open "$@" </dev/null &>/dev/null &
    fi
}

###############################################################
# => Colored man pages
###############################################################

export LESS_TERMCAP_mb=$'\e[1;31m'
export LESS_TERMCAP_md=$'\e[1;34m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[1;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;32m'

###############################################################
# => Aliases
###############################################################

# Prefer nvim over vim
if command -v nvim &>/dev/null; then
    alias vim="nvim"
    export EDITOR=nvim
    export VISUAL=nvim
fi

# Easier navigation: .., ..., ...., ....., ~ and -
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias cr='cd $HOME/repos'

## Colorize the grep command output for ease of use (good for log files)##
alias grep='grep -i --color=auto'
alias egrep='egrep -i --color=auto'
alias fgrep='fgrep -i --color=auto'

# adding flags
alias cp="cp -iv"          # confirm before overwriting something
alias mv="mv -iv"
alias rm="rm -vI"
alias mkd="mkdir -pv"
alias df="df -h"          # human-readable sizes
alias free="free -m"      # show sizes in MB

# Clear
alias c="clear"

# Git
alias gs="git status"
alias gf="git fetch"
alias gl="git pull"
alias ga="git add "
alias gm="git merge"
alias gc="git commit -m "
alias gp="git push"
alias gitsync="git submodule sync; git submodule update --init --recursive"
alias gsu="git submodule update --recursive --remote"
## Alternative (makes easier finding out which commits have no message)
alias yolo='git add -A; git commit -m "This is a placeholder"; git push'

# List files -- prefer eza > exa > ls
if command -v eza &>/dev/null; then
    alias ls="eza --group-directories-first"
    alias la="eza --group-directories-first -a"
    alias l="eza -a -F --long --header --links --group --group-directories-first --git"
elif command -v exa &>/dev/null; then
    alias ls="exa --group-directories-first"
    alias la="exa --group-directories-first -a"
    alias l="exa -a -F --long --header --links --group --group-directories-first --git"
else
    alias ls="ls --color=auto"
    alias la="ls -a --color=auto"
fi

# Print each PATH entry on a separate line
alias path='echo -e ${PATH//:/\\n}'

# Userlist
alias userlist="cut -d: -f1 /etc/passwd"

# Switch between bash and zsh
# shellcheck disable=SC2139
alias tobash="sudo chsh $USER -s /bin/bash && echo 'Now log out.'"
# shellcheck disable=SC2139
alias tozsh="sudo chsh $USER -s /bin/zsh && echo 'Now log out.'"

# Sleep management
alias disableSleep="sudo systemctl mask sleep.target suspend.target hibernate.target hybrid-sleep.target"
alias enableSleep="sudo systemctl unmask sleep.target suspend.target hibernate.target hybrid-sleep.target"

# Get error messages from journalctl
alias jctl="journalctl -p 3 -xb"

###############################################################
# => Tool integrations
###############################################################

# fzf defaults and keybindings (Ctrl+R for history, Ctrl+T for files)
if command -v fzf &>/dev/null; then
    export FZF_DEFAULT_OPTS='--preview "bat --style=numbers --color=always --line-range :500 {}" --height 60% --border -m'
    if command -v ag &>/dev/null; then
        export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
    fi
    eval "$(fzf --bash 2>/dev/null)" || {
        [ -f /usr/share/doc/fzf/examples/key-bindings.bash ] && source /usr/share/doc/fzf/examples/key-bindings.bash
        [ -f /usr/share/doc/fzf/examples/completion.bash ] && source /usr/share/doc/fzf/examples/completion.bash
    }
fi
