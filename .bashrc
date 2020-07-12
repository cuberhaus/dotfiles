# Sections:
#   -> Config
#   -> Variables
#   -> Alias
#   -> Runtime
###############################################################

# ~/.bashrc: executed by bash(1) for non-login shells.
# dash is the FASTEST SHELL

###############################################################
# => Config
###############################################################

# Base16 Shell
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"

export HISTCONTROL=ignoredups:erasedups   # no duplicate entries
export TERM=xterm-256color

### SET VIM AS MANPAGER ###
export MANPAGER="/bin/sh -c \"col -b | vim --not-a-term -c 'set ft=man ts=8 nomod nolist noma' -\""

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

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

###############################################################
# => Variables
###############################################################

 # echo "" >> $HOME/.bashrc && echo "export XAUTHORITY=$HOME/.Xauthority" >> $HOME/.bashrc && cd $HOME && . .bashrc
 INCLUSIONS=/Users/$USER/assig/pro2/inclusions
 OBJECTES=/Users/$USER/assig/pro2/objectes
 EDITOR=vim

###############################################################
# => Alias
###############################################################

# Info:
# alias="" Is created during shell startup and won't change
# alias='' Is created during runtime and will evaluate every time it's called
# $RANDOM will generate a random number from 0 through 32767
# array=('Element1' 'Element2')

# Easier navigation: .., ..., ...., ....., ~ and -
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."

# root privileges
alias doas="doas --"
alias sudo='sudo '

# adding flags
alias cp="cp -i"                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB

# Clear
alias c="clear"

# Get Ubuntu Software Updates, and update installed Ruby gems, Homebrew, npm, and their installed packages
alias update="sudo pacman -Syu"

# Git
alias gs="git status"
alias gf="git fetch"
alias ga="git add "
alias gm="git merge"
alias gc="git commit -m "
alias gp="git push"

# Nautilus
alias o='thunar . & 2> /dev/null'

# Lock screen
LockScreens=('pipes.sh' 'cmatrix')
alias lock='${LockScreens[$RANDOM % 2]}'
alias afk="gnome-screensaver-command -l & disown 2> /dev/null"

# List files
alias l="ls -G -F --color=auto"
alias ll="ls -a -G -F --color=auto"
alias ls='exa -G --long --header --git'
#alias l='ls -l -G -F --color=auto'
alias la='exa -G -a --long --header --git'
#alias ll='ls -a -l -G -F --color=auto'
alias lsd='exa -D -a'

# IP adresses
alias ip='dig +short myip.opendns.com @resolver1.opendns.com'
alias ips="ifconfig -a | grep -o 'inet6\? \(addr:\)\?\s\?\(\(\([0-9]\+\.\)\{3\}[0-9]\+\)\|[a-fA-F0-9:]\+\)' | awk '{ sub(/inet6? (addr:)? ?/, \"\"); print }'"

# Reload the shell (i.e. invoke as a login shell)
alias reload='exec ${SHELL} -l'

# Print each PATH entry on a separate line
alias path='echo -e ${PATH//:/\\n}'

# Cpp
alias p1++="g++ -ansi -O2 -DNDEBUG -D_GLIBCXX_DEBUG -Wall -Wextra -Werror -Wno-uninitialized -Wno-sign-compare -Wshadow"
alias p2++="g++ -D GLIBCXX_DEBUG -O2 -Wall -Wextra -Werror -Wno-sign-compare -std=c++11"



###############################################################
# => Runtime
###############################################################

# Neofetch
bash ~/.config/pfetch/pfetch # Pfetch (fast neofetch)

# Fast Prompt
GIT_PROMPT_ONLY_IN_REPO=0 # Set config variables first

# GIT_PROMPT_FETCH_REMOTE_STATUS=0   # uncomment to avoid fetching remote status
# GIT_PROMPT_IGNORE_SUBMODULES=1 # uncomment to avoid searching for changed files in submodules
# GIT_PROMPT_WITH_VIRTUAL_ENV=0 # uncomment to avoid setting virtual environment infos for node/python/conda environments

# GIT_PROMPT_SHOW_UPSTREAM=1 # uncomment to show upstream tracking branch
# GIT_PROMPT_SHOW_UNTRACKED_FILES=normal # can be no, normal or all; determines counting of untracked files

# GIT_PROMPT_SHOW_CHANGED_FILES_COUNT=0 # uncomment to avoid printing the number of changed files

# GIT_PROMPT_STATUS_COMMAND=gitstatus_pre-1.7.10.sh # uncomment to support Git older than 1.7.10

# GIT_PROMPT_START=...    # uncomment for custom prompt start sequence
# GIT_PROMPT_END=...      # uncomment for custom prompt end sequence

# as last entry source the gitprompt script
# GIT_PROMPT_THEME=Custom # use custom theme specified in file GIT_PROMPT_THEME_FILE (default ~/.git-prompt-colors.sh)
# GIT_PROMPT_THEME_FILE=~/.git-prompt-colors.sh
GIT_PROMPT_THEME=Solarized # use theme optimized for solarized color scheme
source ~/.config/.bash-git-prompt/gitprompt.sh
# Fast prompt ends

# Powerline Shell
#function _update_ps1() {
#    PS1=$(powerline-shell $?)
#}

#if [[ $TERM != linux && ! $PROMPT_COMMAND =~ _update_ps1 ]]; then
#    PROMPT_COMMAND="_update_ps1; $PROMPT_COMMAND"
#fi
#Powerline ends

# TMUX
# - The following tests to make sure that (1) tmux exists on the system, (2) we're in an interactive shell, and (3) tmux doesn't try to run within itself:
if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
    exec tmux attach
fi

