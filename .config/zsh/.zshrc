#   ____       _     ____
#  |  _ \ ___ | |   / ___|
#  | |_) / _ \| |  | |
#  |  __/ (_) | |  | |___
#  |_|   \___/|_|   \____|
#
###############################################################
###############################################################
# Sections:
#   -> Theme
#   -> Variables
#   -> Oh My Zsh
#   -> Alias
#   -> Runtime
#   -> Options	

###############################################################

# Info:
# alias="" Is created during shell startup and won't change
# alias='' Is created during runtime and will evaluate every time it's called
# $RANDOM will generate a random number from 0 through 32767
# array=('Element1' 'Element2')

###############################################################
# => Theme
###############################################################

# Base16 Shell
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"

# Tmux
#if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
#  exec tmux attach
#fi

###############################################################
# => Variables
###############################################################

export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
export FZF_DEFAULT_OPTS='--preview "bat --style=numbers --color=always --line-range :500 {}" --height 60% --border -m'

INCLUSIONS=/Users/$USER/assig/pro2/inclusions
OBJECTES=/Users/$USER/assig/pro2/objectes
EDITOR=vim
if [ -d "$HOME/.config/i3/i3-layout-manager" ] ; then
        PATH="$HOME/.config/i3/i3-layout-manager:$PATH"
fi

###############################################################
# => Oh My Zsh
###############################################################

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
if [[ "$OSTYPE" == "darwin"* ]]; then
    export ZSH="/Users/PolC/.config/oh-my-zsh"
fi

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    export ZSH="/home/pol/.config/oh-my-zsh"
fi
# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="robbyrussell"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
 export UPDATE_ZSH_DAYS=7

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
#COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git alias-finder sudo zsh_reload)
ZSH_ALIAS_FINDER_AUTOMATIC=true
#src  will reload ohmyzsh
# Double ESC will type previous command with sudo (sudo !! does the job as well)
#chuck, chuck_cow
source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
source ~/.config/powerlevel10k/powerlevel10k.zsh-theme
# It has to go here, cant go inside the repo
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.config/.p10k.zsh ]] || source ~/.config/.p10k.zsh

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
alias gsu="git submodule update --recursive --remote"
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
    alias update="sudo softwareupdate -i -a; brew update; brew upgrade ; brew update --cask; brew cleanup"
    alias o='open . > /dev/null 2>&1 &'
    # Show active network interfaces
    alias ifactive="ifconfig | pcregrep -M -o '^[^\t:]+:([^\n]|\n\t)*status: active'"

    # Clean up LaunchServices to remove duplicates in the “Open With” menu
    alias lscleanup="/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user && killall Finder"

    # Flush Directory Service cache
    alias flush="dscacheutil -flushcache && killall -HUP mDNSResponder"

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
# => Options
###############################################################

# History in cache directory:
HISTSIZE=32768
SAVEHIST=32768
HISTFILE=~/.cache/zsh/history

## Basic auto/tab complete:
#autoload -U compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION
#zstyle ':completion:*' menu select
#zmodload zsh/complist
#compinit -d ~/.cache/zsh/zcompdump-$ZSH_VERSION
#_comp_options+=(globdots)		# Include hidden files.
#zstyle ':completion:*:(cd|mv|cp):*' ignore-parents parent pwd # Ignores current directory and previous

# vi mode
bindkey -v
export KEYTIMEOUT=1

# Change cursor shape for different vi modes.
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

bindkey -s '^f' 'cd "$(dirname "$(fzf)")"\n'

# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line

###############################################################
# => Runtime
###############################################################

#Don't show message
typeset -g POWERLEVEL9K_INSTANT_PROMPT=quiet

# Pfetch (fast neofetch)
pfetch

# Autojump
[[ -s /etc/profile.d/autojump.sh ]] && source /etc/profile.d/autojump.sh
# Accept suggestion ctrl+space
bindkey '^ ' autosuggest-accept
# This one is a submodule
source ~/.config/zsh/zsh-autosuggestions/zsh-autosuggestions.zsh 
# Syntax highlight
source ~/.config/zsh/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
