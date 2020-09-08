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
export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
export FZF_DEFAULT_OPTS='--preview "bat --style=numbers --color=always --line-range :500 {}" --height 60% --border -m'

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
if [ -d "$HOME/.config/i3/i3-layout-manager" ] ; then
        PATH="$HOME/.config/i3/i3-layout-manager:$PATH"
fi

###############################################################
# => Alias
###############################################################

# Config Files
alias zshenv="vim ~/.zshenv"
alias zshrc="vim ~/.config/zsh/.zshrc"
alias zprofile="vim ~/.config/zsh/.zprofile"
alias vimrc="vim ~/.vim/vimrc"
alias bashrc="vim ~/.bashrc"
alias vlightdm="sudo vim /etc/lightdm/lightdm.conf"
alias vpacman="sudo vim /etc/pacman.conf"
alias vgrub="sudo vim /etc/default/grub"
alias vmkinitcpio="sudo vim /etc/mkinitcpio.conf"
alias vslim="sudo vim /etc/slim.conf"
alias voblogout="sudo vim /etc/oblogout.conf"
alias vmirrorlist="sudo vim /etc/pacman.d/mirrorlist"
alias vconfgrub="sudo vim /boot/grub/grub.cfg"

# Clean-up
alias Xres="xrdb -load ~/.config/X11/xresources"
alias nvidia-settings="nvidia-settings --config="$XDG_CONFIG_HOME"/nvidia/settings"
alias wget='wget --hsts-file="$XDG_CACHE_HOME/wget-hsts"'

# Use NeoVim
alias vim="nvim"

# Easier navigation: .., ..., ...., ....., ~ and -
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."

## Colorize the grep command output for ease of use (good for log files)##
alias grep='grep --color=auto'
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'

#alias vfzf='vim $(fzf)'
#alias fzf='fzf'
alias fz='vim $(fzf)'
alias qt='fortune -c | cowthink -f $(find /usr/share/cows -type f | shuf -n 1)'

# root privileges
alias doas="doas --"
#alias sudo='sudo '

# adding flags
alias cp="cp -i"                          # confirm before overwriting something
alias df="df -h"                          # human-readable sizes
alias free="free -m"                      # show sizes in MB

# Clear
alias c="clear"

# Git
alias gs="git status"
alias gf="git fetch"
alias ga="git add "
alias gm="git merge"
alias gc="git commit -m "
alias gp="git push"
alias yolo='git commit -m "$(curl -s http://whatthecommit.com/index.txt)"'

# Lock screen
LockScreens=('pipes.sh' 'cmatrix')
alias lock='${LockScreens[$RANDOM % 2]}'
alias afk="betterlockscreen -l"

# List files
alias ls='exa -G -F --long --header --git  --group-directories-first'
alias la='exa -G -a -F --long --header --git  --group-directories-first'
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

#userlist
alias userlist="cut -d: -f1 /etc/passwd"

#switch between bash and zsh
alias tobash="sudo chsh $USER -s /bin/bash && echo 'Now log out.'"
alias tozsh="sudo chsh $USER -s /bin/zsh && echo 'Now log out.'"

#get the error messages from journalctl
alias jctl="journalctl -p 3 -xb"

#check vulnerabilities microcode
alias microcode='grep . /sys/devices/system/cpu/vulnerabilities/*'

#youtube-dl
alias yta-aac="youtube-dl --extract-audio --audio-format aac "
alias yta-best="youtube-dl --extract-audio --audio-format best "
alias yta-flac="youtube-dl --extract-audio --audio-format flac "
alias yta-m4a="youtube-dl --extract-audio --audio-format m4a "
alias yta-mp3="youtube-dl --extract-audio --audio-format mp3 "
alias yta-opus="youtube-dl --extract-audio --audio-format opus "
alias yta-vorbis="youtube-dl --extract-audio --audio-format vorbis "
alias yta-wav="youtube-dl --extract-audio --audio-format wav "

alias ytv-best="youtube-dl -f bestvideo+bestaudio "

# # ex = EXtractor for all kinds of archives
# # usage: ex <file>
ex ()
{
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)   tar xjf $1   ;;
      *.tar.gz)    tar xzf $1   ;;
      *.bz2)       bunzip2 $1   ;;
      *.rar)       unrar x $1   ;;
      *.gz)        gunzip $1    ;;
      *.tar)       tar xf $1    ;;
      *.tbz2)      tar xjf $1   ;;
      *.tgz)       tar xzf $1   ;;
      *.zip)       unzip $1     ;;
      *.Z)         uncompress $1;;
      *.7z)        7z x $1      ;;
      *.deb)       ar x $1      ;;
      *.tar.xz)    tar xf $1    ;;
      *.tar.zst)   unzstd $1    ;;      
      *)           echo "'$1' cannot be extracted via ex()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

### MacOS
if [[ "$OSTYPE" == "darwin"* ]]; then
    alias update="sudo softwareupdate -i -a; brew update; brew upgrade; brew cleanup"
    alias o='open . > /dev/null 2>&1 &'
    # Show active network interfaces
    alias ifactive="ifconfig | pcregrep -M -o '^[^\t:]+:([^\n]|\n\t)*status: active'"

    # Clean up LaunchServices to remove duplicates in the “Open With” menu
    alias lscleanup="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user && killall Finder"

    # Flush Directory Service cache
    alias flush="dscacheutil -flushcache && killall -HUP mDNSResponder"

    # Clean up LaunchServices to remove duplicates in the “Open With” menu
    alias lscleanup="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user && killall Finder"

    # Recursively delete `.DS_Store` files
    alias cleanup="find . -type f -name '*.DS_Store' -ls -delete"

    # Empty the Trash on all mounted volumes and the main HDD.
    # Also, clear Apple’s System Logs to improve shell startup speed.
    # Finally, clear download history from quarantine. https://mths.be/bum
    alias emptytrash="sudo rm -rfv /Volumes/*/.Trashes; sudo rm -rfv ~/.Trash; sudo rm -rfv /private/var/log/asl/*.asl; sqlite3 ~/Library/Preferences/com.apple.LaunchServices.QuarantineEventsV* 'delete from LSQuarantineEvent'"

    # Show/hide hidden files in Finder
    alias show="defaults write com.apple.finder AppleShowAllFiles -bool true && killall Finder"
    alias hide="defaults write com.apple.finder AppleShowAllFiles -bool false && killall Finder"
fi

### Arch
if [[ "$DISTRO" == "arch" ]]; then
    alias o='thunar . > /dev/null 2>&1 &'
    alias update='sudo pacman -Syyu'
    #get fastest mirrors in your neighborhood
    alias mirror="sudo reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist"
    alias mirrord="sudo reflector --latest 50 --number 20 --sort delay --save /etc/pacman.d/mirrorlist"
    alias mirrors="sudo reflector --latest 50 --number 20 --sort score --save /etc/pacman.d/mirrorlist"
    alias mirrora="sudo reflector --latest 50 --number 20 --sort age --save /etc/pacman.d/mirrorlist"

    alias pacman='sudo pacman --color auto'
    #pacman unlock
    alias unlock="sudo rm /var/lib/pacman/db.lck"
fi

### Manjaro
if [[ "$DISTRO" == "manjaro" ]]; then
    alias o='nautilus . > /dev/null 2>&1 &'
fi

### Ubuntu
if [[ "$DISTRO" == "ubuntu" ]]; then
    alias o='nautilus . > /dev/null 2>&1 &'
    alias update="sudo apt-get update; sudo apt-get full-upgrade; brew update; brew upgrade; sudo apt-get autoclean; sudo apt-get clean; sudo apt-get autoremove"
    alias tmux="tmux -f ~/.config/tmux/tmux.conf"
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
