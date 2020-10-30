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
#   -> Oh My Zsh
#   -> Aliases
#   -> Options	
#   -> Runtime
#
###############################################################

###############################################################
# => Theme
###############################################################

# Base16 Shell
BASE16_SHELL="$HOME/.config/base16-shell/"
[ -n "$PS1" ] && \
    [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"

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
antigen bundle zsh_reload
antigen bundle archlinux
antigen bundle copyfile
antigen bundle command-not-found
antigen bundle alias-finder 
# Load the theme.
antigen theme denysdovhan/spaceship-prompt
# Load bundles from external repos.
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
# https://github.com/romkatv/powerlevel10k

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=7

# Uncomment the following line to enable command auto-correction.
#ENABLE_CORRECTION="true"

# dirhistory // https://github.com/ohmyzsh/ohmyzsh/tree/master/plugins/dirhistory
# copyfile <file> // copia un archivo al clipboard
# command-not-found // Si no existe un comando da sugerencias
ZSH_ALIAS_FINDER_AUTOMATIC=true

###############################################################
# => Powerlevel10k
###############################################################
# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
#if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
#  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
#fi

#source ~/.config/powerlevel10k/powerlevel10k.zsh-theme
# It has to go here, cant go inside the repo
# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.config/.p10k.zsh ]] || source ~/.config/.p10k.zsh

###############################################################
# => Aliases
###############################################################
if [ -f $ZDOTDIR/aliases ]; then
    source $ZDOTDIR/aliases
fi
###############################################################
# => Options
###############################################################

# History in cache directory:
HISTSIZE=32768
SAVEHIST=32768
HISTFILE=~/.cache/zsh/history

# Accept suggestion ctrl+space
bindkey '^ ' autosuggest-accept

# History substring search
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

#Don't show message
typeset -g POWERLEVEL9K_INSTANT_PROMPT=quiet

# ctrl-f Search a file and cd into its directory
bindkey -s '^f' 'cd "$(dirname "$(fzf)")"\n'
###############################################################
# => Vim
###############################################################
# vi mode
bindkey -v
export KEYTIMEOUT=1

# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line

# Change cursor with support for inside/outside tmux
function _set_cursor() {
    if [[ $TMUX = '' ]]; then
      echo -ne $1
    else
      echo -ne "\ePtmux;\e\e$1\e\\"
    fi
}

function _set_block_cursor() { _set_cursor '\e[2 q' }
function _set_beam_cursor() { _set_cursor '\e[6 q' }

function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] || [[ $1 = 'block' ]]; then
      _set_block_cursor
  else
      _set_beam_cursor
  fi
}
zle -N zle-keymap-select
# ensure beam cursor when starting new terminal
precmd_functions+=(_set_beam_cursor) #
# ensure insert mode and beam cursor when exiting vim
zle-line-init() { zle -K viins; _set_beam_cursor }

###############################################################
# => Runtime
###############################################################
# Pfetch (fast neofetch)
pfetch

# Autojump
[[ -s /etc/profile.d/autojump.sh ]] && source /etc/profile.d/autojump.sh

# Tmux
#if command -v tmux &> /dev/null && [ -n "$PS1" ] && [[ ! "$TERM" =~ screen ]] && [[ ! "$TERM" =~ tmux ]] && [ -z "$TMUX" ]; then
#  exec tmux attach
#fi
