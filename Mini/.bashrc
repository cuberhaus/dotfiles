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

### ALIASES

# Easier navigation: .., ..., ...., ....., ~ and -
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."

## Colorize the grep command output for ease of use (good for log files)##
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

# adding flags
alias cp="cp -iv"          # confirm before overwriting something
alias mv="mv -iv"
alias mkd="mkdir -pv"
alias df="df -h"          # human-readable sizes
alias free="free -m"      # show sizes in MB

# Clear
alias c="clear"

# Git
alias gs="git status"
alias gf="git fetch"
alias ga="git add "
alias gm="git merge"
alias gc="git commit -m "
alias gp="git push"
alias gitsync="git submodule sync; git submodule update --init --recursive"
alias gsu="git submodule update --recursive --remote"
## Fun commit
#alias yolo='git add -A; git commit -m "$(curl -s http://whatthecommit.com/index.txt)"; git push'
## Alternative (makes easier finding out which commits have no message)
alias yolo='git add -A; git commit -m "This is a placeholder"; git push'

# List files
# using -a -a will show '.' and ".."
# --git causes to crash when symbolic link is gone
alias ls="ls -l"
alias la="ls -a"

# Print each PATH entry on a separate line
alias path='echo -e ${PATH//:/\\n}'

# Userlist
alias userlist="cut -d: -f1 /etc/passwd"

# Switch between bash and zsh
alias tobash="sudo chsh $USER -s /bin/bash && echo 'Now log out.'"
alias tozsh="sudo chsh $USER -s /bin/zsh && echo 'Now log out.'"
