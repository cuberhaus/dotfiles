###############################################################
# Sections:
#   -> Theme
#   -> Configuration
#   -> Aliases
#   -> Runtime
#
###############################################################
# => Theme
###############################################################

### Base16 Shell THEME
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
    eval "$("$BASE16_SHELL/profile_helper.sh")"

### Prompt
# For more info: https://github.com/magicmonty/bash-git-prompt
GIT_PROMPT_ONLY_IN_REPO=0 # Set config variables first
# It has to go here, cant go inside the repo
GIT_PROMPT_THEME_FILE=~/.config/.git-prompt-colors.sh
GIT_PROMPT_THEME=Custom # use theme optimized for solarized color scheme
source ~/.config/bash-git-prompt/gitprompt.sh

###############################################################
# => Configuration
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
shopt -s globstar

###############################################################
# => Aliases
###############################################################

if [ -f $ZDOTDIR/aliases ]; then
    source $ZDOTDIR/aliases
fi

###############################################################
# => Runtime
###############################################################

# Pfetch (fast neofetch)
pfetch

# TMUX at startup
# - The following tests to make sure that (1) tmux exists on the system, (2) we're in an interactive shell, and (3) tmux doesn't try to run within itself:
#if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
#    exec tmux attach
#fi
[[ -s /etc/profile.d/autojump.sh ]] && source /etc/profile.d/autojump.sh
