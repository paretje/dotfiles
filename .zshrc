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

zplug 'zplug/zplug', hook-build:"zplug --self-manage"

zplug "zsh-users/zsh-completions"

zplug "paretje/unisister", as:command, use:unisister
zplug "paretje/qutebrowser", as:command, use:".venv/bin/qutebrowser", at:paretje, hook-build:"tox -r -e mkvenv && scripts/asciidoc2html.py"

zplug "paretje/urxvt-vim-scrollback", use:, hook-build:"mkdir -p ~/.urxvt/ext && ln -srf vim-scrollback ~/.urxvt/ext"

zplug "esr/sshexport", from:gitlab, as:command, use:sshexport, hook-build:"sed -i '1s/python/python3/' sshexport && make sshexport.1 && ln -srf sshexport.1 '$ZPLUG_HOME/doc/man/man1/'"

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
zstyle ':completion:*:(ssh|scp|rranger):*:hosts' hosts

# autocompletion for tsocks
compdef tsocks=exec

# autocompletion for rranger
_rranger() {
    local service
    service= _ssh
    _ssh_hosts
}
compdef _rranger rranger

# just use url completion for mpc
_mpc_helper_files() {
    _urls
}

# enable bash completions
autoload bashcompinit
bashcompinit

# autocompletion for pandoc
_pandoc() {
    eval "$(pandoc --bash-completion)"
}
compdef _pandoc pandoc

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

# Basic alternative to oh-my-zsh vi-mode
bindkey '^p' up-history
bindkey '^n' down-history

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
prompt adam1

autoload -Uz add-zsh-hook

function xterm_title_precmd () {
    print -n '\e]2;zsh\a'
}

function xterm_title_preexec () {
    print -n "\e]2;$1\a"
}

if [[ "$TERM" == (xterm*|rxvt*) ]]; then
    add-zsh-hook -Uz precmd xterm_title_precmd
    add-zsh-hook -Uz preexec xterm_title_preexec
fi
