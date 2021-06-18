# Enable 256 colors in xfce4-terminal
if [ "$COLORTERM" = "xfce4-terminal" -a "$TERM" = "xterm" ]; then
    TERM=xterm-256color
fi

# Add local completion path
fpath+=~/.zfunc

# Clone zplug if unavailable
if [ ! -f "$ZPLUG_HOME/init.zsh" ]; then
    git clone https://github.com/zplug/zplug.git "$ZPLUG_HOME"
fi

# Load zplug and plugins
local my_path="$PATH"
source "$ZPLUG_HOME/init.zsh"

zplug 'zplug/zplug', hook-build:"zplug --self-manage"

zplug "olivierverdier/zsh-git-prompt", use:zshrc.sh, hook-build:"zplug clear"
zplug "zsh-users/zsh-completions"
zplug "jonmosco/kube-ps1", use:kube-ps1.sh

zplug "plugins/vi-mode", from:oh-my-zsh
zplug "plugins/pip", from:oh-my-zsh

# Abuse zplug to keep track of weechat plugins
zplug "keith/urlview-weechat", use:
zplug "GermainZ/weechat-vimode", use:

zplug "paretje/urxvt-perls", use:, hook-build:"mkdir -p ~/.urxvt/ext && ln -srf keyboard-select deprecated/clipboard deprecated/url-select ~/.urxvt/ext"

zplug "soimort/translate-shell", at:develop, use:, hook-build:"make install PREFIX='$HOME/.local'"
zplug "trizen/pipe-viewer", at:main, hook-build:"perl Build.PL && ./Build install --install_base='$HOME/.local'"
zplug "spion/adbfs-rootless", hook-build:"make && make install INSTALL_DIR='$HOME/.local'"
zplug "carnager/rofi-pass", as:command, use:rofi-pass

zplug "jamedjo/gnome-dbus-emulation-wlr", from:gitlab, hook-build:"bundle install"
zplug "dunst-project/dunst", at:master, hook-build:"make && make install DESTDIR='$HOME/.local' PREFIX=''"

zplug load
PATH="$my_path"

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
setopt histignorealldups sharehistory histignorespace incappendhistory

HISTSIZE=10000
SAVEHIST=10000
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
zstyle ':completion:*:(ssh|scp|rtmux):*:users' users
zstyle ':completion:*:(ssh|scp|rranger|rtmux):*:hosts' hosts

# use other commands as source for autocompletion
compdef tsocks=exec
compdef autossh=ssh

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

# autocompletion for rtmux
compdef rtmux=ssh

# autocompletion for git flow on old Ubuntu and Debian versions
if (( ! $+functions[_git-flow])); then
    if [ -f /usr/share/git-flow/git-flow-completion.zsh ]; then
        . /usr/share/git-flow/git-flow-completion.zsh
    fi
fi

# Load aliases
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# Load gpg-agent variables
export GPG_TTY=$(tty)

# Don't quit zsh on C-d
setopt ignoreeof

# Set additional keybindings for vi-mode
bindkey -M viins '^a' beginning-of-line
bindkey -M viins '^e' end-of-line
bindkey -M viins '^d' delete-char
bindkey -M viins '^r' history-incremental-search-backward
bindkey -M vicmd '/' history-incremental-search-backward
bindkey -M vicmd '?' history-incremental-search-forward

# Use similar word definition as vim
WORDCHARS="_"

# Enable report time
REPORTTIME=5

# Configure kube-ps1
autoload -U regexp-replace

function kubeps1_get_cluster_short() {
    context="$1"
    regexp-replace context '.*_' ''
    echo "$context"
}

KUBE_PS1_SYMBOL_ENABLE=false
KUBE_PS1_CLUSTER_FUNCTION=kubeps1_get_cluster_short
KUBE_PS1_PREFIX="(%B"
KUBE_PS1_DIVIDER="%b:"

if ! hash kubectl > /dev/null 2>&1 ; then
    alias kube_ps1='true'
fi


# Set up the prompt
autoload -Uz promptinit
promptinit

PROMPT='%K{blue}%n@%m%k %B%F{green}%147<...<%~%b%F{white} $(git_super_status) $(kube_ps1)
%}%F{white} %# %b%f%k'

# Set up terminal title
autoload -Uz add-zsh-hook

function xterm_title_precmd() {
    print -n '\e]2;zsh\a'
}

function xterm_title_preexec() {
    print -n "\e]2;$1\a"
}

if [[ "$TERM" == (xterm*|rxvt*|st*|alacritty) ]]; then
    add-zsh-hook -Uz precmd xterm_title_precmd
    add-zsh-hook -Uz preexec xterm_title_preexec
fi
