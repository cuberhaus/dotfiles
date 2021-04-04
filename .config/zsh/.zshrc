###############################################################
# => Theme
###############################################################

# Base16 Shell
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"

SPACESHIP_EXIT_CODE_SHOW=true
# SPACESHIP_USER_SHOW=always

###############################################################
# => Antigen
###############################################################

# CONFIGURATION
ADOTDIR=~/.config/antigen/
ANTIGEN_CHECK_FILES=(~/.config/zsh/.zshrc)

# START ANTIGEN
source ~/.config/antigen/antigen.zsh

# Load the oh-my-zsh's library.
antigen use oh-my-zsh

# Bundles from the default repo (robbyrussell's oh-my-zsh).
antigen bundle systemd
antigen bundle archlinux
antigen bundle command-not-found
antigen bundle alias-finder 

# Load the theme.
# https://github.com/romkatv/powerlevel10k
#antigen theme romkatv/powerlevel10k
#antigen bundle mafredri/zsh-async # Pure
#antigen bundle sindresorhus/pure # Pure
antigen theme denysdovhan/spaceship-prompt # Spaceship

# Load bundles from external repos.
#antigen bundle jeffreytse/zsh-vi-mode # Better keybindings and solves zsh+vim issues like starting vim with wrong cursor
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-history-substring-search # syntax first

# Tell Antigen that you're done.
antigen apply

###############################################################
# => Oh My Zsh
###############################################################

# For more info: https://github.com/ohmyzsh/ohmyzsh/blob/master/templates/zshrc.zsh-template

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=7

# copyfile <file> // copia un archivo al clipboard
# command-not-found // Si no existe un comando da sugerencias
ZSH_ALIAS_FINDER_AUTOMATIC=true

###############################################################
# => Powerlevel10k
###############################################################

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.config/.p10k.zsh ]] || source ~/.config/.p10k.zsh

###############################################################
# => Aliases and functions
###############################################################

if [ -f $ZDOTDIR/aliases ]; then
    source $ZDOTDIR/aliases
fi

if [ -f $ZDOTDIR/functions ]; then
    source $ZDOTDIR/functions
fi

###############################################################
# => Configurations
###############################################################

# Make sure we swap keys (doesn't really slow down termite that much)
setxkbmap -option caps:swapescape > /dev/null 2>&1

# History in cache directory:
HISTSIZE=32768
SAVEHIST=32768
HISTFILE=~/.cache/zsh/history

# History substring search
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

#Don't show message
typeset -g POWERLEVEL9K_INSTANT_PROMPT=quiet

eval spaceship_vi_mode_enable

###############################################################
# => Vim
###############################################################

# vi mode
bindkey -v
export KEYTIMEOUT=1

#HAS TO BE AT THE END
# Edit line in vim with ctrl-e (it's vv with zsh-vim extension):
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line
# ctrl-f Search a file and cd into its directory
bindkey -s '^f' 'cd "$(dirname "$(fzf)")"\n'
# Accept suggestion ctrl+space
bindkey '^ ' autosuggest-accept
