# Enable 256 colors in xfce4-terminal
if [ "$COLORTERM" = "xfce4-terminal" -a "$TERM" = "xterm" ]; then
    TERM=xterm-256color
fi

# Clone zplug if unavailable
if [ ! -f "$ZPLUG_HOME/init.zsh" ]; then
    git clone https://github.com/zplug/zplug.git "$ZPLUG_HOME"
fi

# Load zplug and plugins
local my_path="$PATH"
source "$ZPLUG_HOME/init.zsh"

zplug 'zplug/zplug', hook-build:"zplug update --self"

zplug "olivierverdier/zsh-git-prompt", use:zshrc.sh, hook-build:"zplug clear"
zplug "zsh-users/zsh-completions"

zplug "plugins/vi-mode", from:oh-my-zsh
zplug "plugins/pip", from:oh-my-zsh

# Abuse zplug to keep track of weechat plugins
zplug "rawdigits/wee-slack", use:
zplug "torhve/weechat-matrix-protocol-script", use:
zplug "keith/urlview-weechat", use:
zplug "paretje/weechat-vimode", at:paretje, use:

zplug "paretje/unisister", as:command, use:unisister
zplug "paretje/qutebrowser", as:command, use:".venv/bin/qutebrowser", at:paretje, hook-build:"tox -r -e mkvenv && scripts/asciidoc2html.py"

zplug "paretje/dwm", as:command, use:dwm, hook-build:"make clean && rm -f config.h && make && ln -srf dwm.1 '$ZPLUG_HOME/doc/man/man1'"

zplug load
PATH="$my_path"

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
setopt histignorealldups sharehistory histignorespace

HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Use modern completion system
zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# disable user completion for ssh
zstyle ':completion:*:(ssh|scp):*:users' users
zstyle ':completion:*:(ssh|scp):*:hosts' hosts

# autocompletion for tsocks
compdef tsocks=exec

# just use url completion for mpc
_mpc_helper_files() {
    _urls
}

# enable bash completions
autoload bashcompinit
bashcompinit

# autocompletion for pandoc
# TODO: use a file to cache output?
eval "$(pandoc --bash-completion)"

# Load aliases
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Load gpg-agent variables
if [ -f "$GPG_ENV_FILE" ]; then
    . "$GPG_ENV_FILE"
    export GPG_TTY=$(tty)
    export GPG_AGENT_INFO
    export SSH_AUTH_SOCK
    export SSH_AGENT_PID
fi

# Set additional keybindings for vi-mode
bindkey -M viins '^a' beginning-of-line
bindkey -M viins '^e' end-of-line
bindkey -M viins '^d' delete-char
bindkey -M viins '^r' history-incremental-search-backward
bindkey -M vicmd '/' history-incremental-search-backward
bindkey -M vicmd '?' history-incremental-search-forward

# Use similar word definition as vim
WORDCHARS="_-."

# Set up the prompt
autoload -Uz promptinit
promptinit

PROMPT='%K{blue}%n@%m%k %B%F{green}%147<...<%~ %b$(git_super_status)
%}%F{white} %# %b%f%k'
