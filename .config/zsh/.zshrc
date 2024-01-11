# Theme {{{
if [[ "$OSTYPE" == "darwin"* ]]; then
    # Base16 Shell
    BASE16_SHELL="$HOME/.config/base16-shell/"
    [ -n "$PS1" ] && \
        [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
        eval "$("$BASE16_SHELL/profile_helper.sh")"
fi

SPACESHIP_EXIT_CODE_SHOW=true
# }}}

# Antigen {{{

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
# antigen bundle globalias # pressing space after an alias, expands it

# Load the theme.
# https://github.com/romkatv/powerlevel10k
antigen theme romkatv/powerlevel10k
#antigen bundle mafredri/zsh-async # Pure
#antigen bundle sindresorhus/pure # Pure
#antigen theme denysdovhan/spaceship-prompt # Spaceship

# Load bundles from external repos.
#antigen bundle jeffreytse/zsh-vi-mode # Better keybindings and solves zsh+vim issues like starting vim with wrong cursor
antigen bundle zsh-users/zsh-completions
antigen bundle zsh-users/zsh-autosuggestions
antigen bundle zsh-users/zsh-syntax-highlighting
antigen bundle zsh-users/zsh-history-substring-search # syntax first

# Tell Antigen that you're done.
antigen apply
#}}}

# Oh My Zsh {{{

# For more info: https://github.com/ohmyzsh/ohmyzsh/blob/master/templates/zshrc.zsh-template

# Uncomment the following line to change how often to auto-update (in days).
export UPDATE_ZSH_DAYS=7

ZSH_ALIAS_FINDER_AUTOMATIC=true
# }}}

# Powerlevel10k {{{

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
    source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.config/.p10k.zsh ]] || source ~/.config/.p10k.zsh
#}}}

# Aliases and functions {{{

if [ -f $ZDOTDIR/aliases ]; then
    source $ZDOTDIR/aliases
fi

if [ -f $ZDOTDIR/functions ]; then
    source $ZDOTDIR/functions
fi
#}}}

# Configurations {{{

# This can be used to specify that SIGHUP should not be sent to the jobs in the jobs table when the shell exits.
#https://superuser.com/questions/178587/how-do-i-detach-a-process-from-terminal-entirely
setopt NOHUP

# History in cache directory:
HISTSIZE=32768
SAVEHIST=32768
HISTFILE=~/.cache/zsh/history

# History substring search
bindkey -M vicmd 'k' history-substring-search-up
bindkey -M vicmd 'j' history-substring-search-down

#Don't show message
typeset -g POWERLEVEL9K_INSTANT_PROMPT=quiet
#}}}

# Vim / Has to be at the end of file {{{
if [ -f "/home/pol/anaconda3/bin/conda" ]; then
    eval "$(/home/pol/anaconda3/bin/conda shell.zsh hook)"
fi


#eval spaceship_vi_mode_enable

# vi mode
bindkey -v
export KEYTIMEOUT=1
# TRIED ON ZSHENV AND ZPROFILE ONLY WORKS IN ZSHRC
if [[ "$OSTYPE" == "linux-gnu"* ]]; then
    export CLASSPATH=".:/usr/local/lib/antlr-4.10.1-complete.jar:$CLASSPATH"
fi

if [[ "$OSTYPE" == "darwin"* ]]; then
    export CLASSPATH=".:$HOME/.local/bin/antlr-4.10.1-complete.jar:$CLASSPATH"
    export PATH=/opt/homebrew/bin:$PATH
fi
#HAS TO BE AT THE END
# Edit line in vim with ctrl-e (it's vv with zsh-vim extension): IN INSERT MODE
autoload edit-command-line; zle -N edit-command-line
bindkey '^e' edit-command-line
# ctrl-f Search a file and cd into its directory
bindkey -s '^f' 'cd "$(dirname "$(fzf)")"\n'
# Accept suggestion ctrl+space
bindkey '^ ' autosuggest-accept
# Activating numlock on startup
# setleds -D +num
#}}}
