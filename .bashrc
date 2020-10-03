#   ____       _     ____ 
#  |  _ \ ___ | |   / ___|
#  | |_) / _ \| |  | |    
#  |  __/ (_) | |  | |___ 
#  |_|   \___/|_|   \____|
#
###############################################################
# Info:
# alias="" Is created during shell startup and won't change
# alias='' Is created during runtime and will evaluate every time it's called
# $RANDOM will generate a random number from 0 through 32767
# array=('Element1' 'Element2')

###############################################################
# Sections:
#   -> Theme
#   -> Config
#   -> Variables
#   -> Alias
#   -> Runtime
#
###############################################################

# ~/.bashrc: executed by bash(1) for non-login shells.
# dash is the FASTEST SHELL

###############################################################
# => Theme
###############################################################

### Base16 Shell THEME
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"

### Prompt
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
# It has to go here, cant go inside the repo
GIT_PROMPT_THEME_FILE=~/.config/.git-prompt-colors.sh
GIT_PROMPT_THEME=Custom # use theme optimized for solarized color scheme
source ~/.config/bash-git-prompt/gitprompt.sh

#if [[ $TERM != linux && ! $PROMPT_COMMAND =~ _update_ps1 ]]; then
#    PROMPT_COMMAND="_update_ps1; $PROMPT_COMMAND"
#fi
## Powerline ends

###############################################################
# => Config
###############################################################

# Enable Readline not waiting for additional input when a key is pressed.
set keyseq-timeout 50

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
#shopt -s globstar


###############################################################
# => Variables
###############################################################

 # echo "" >> $HOME/.bashrc && echo "export XAUTHORITY=$HOME/.Xauthority" >> $HOME/.bashrc && cd $HOME && . .bashrc

###############################################################
# => Alias
###############################################################

if [ -f $ZDOTDIR/.aliases ]; then
    source $ZDOTDIR/.aliases
fi

###############################################################
# => Runtime
###############################################################

# Pfetch (fast neofetch)
pfetch

# TMUX
# - The following tests to make sure that (1) tmux exists on the system, (2) we're in an interactive shell, and (3) tmux doesn't try to run within itself:
#if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
#    exec tmux attach
#fi
[[ -s /etc/profile.d/autojump.sh ]] && source /etc/profile.d/autojump.sh
