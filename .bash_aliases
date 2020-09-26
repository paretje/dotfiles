# enable color support of ls and also add handy aliases
alias ls='ls --color=auto'
alias dir='dir --color=auto'
alias vdir='vdir --color=auto'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# some more ls aliases
alias ll='ls -lh'
alias la='ls -lAh'
alias l='ls -CF'

# some conveniences
alias su='su -l'
alias cal='ncal -b'
alias less='less -icRn'
alias info='info --vi-keys'

# use gpg2
alias gpg='gpg2'

# neovim coloring doesn't seem to work well on tty
if [ "$TERM" = "linux" ]; then
  alias vi='vim'
fi

# transform symlinks when using sshfs
alias sshfs='sshfs -o transform_symlinks'

# alias for weather forecasts
alias weather='curl --cacert ~/.local/share/ca-certificates/wttr.crt "https://wttr.in/Gent"'

# use pager for output
alias ag='ag --pager "less -iRFX"'
alias mysql='mysql --pager="less -iSFX"'

# give diff some color
alias diff='colordiff'

# batchvtest
alias batchvtest='find -iname "*.mpg" -exec vtest "{}" ";"'

# straw-listener
alias straw-listener='straw-viewer -n --resolution=audio'

# gcalcli
alias gcalm='gcalcli calm --monday'
alias gagenda='gcalcli --lineart=ascii agenda --details=all| less'
