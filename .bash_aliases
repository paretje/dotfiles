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

alias su='su -l'
alias cal='ncal -b'
alias mutt-pers='mutt -F "$HOME/.mutt/personal.rc"'
alias mutt-info='mutt -F "$HOME/.mutt/info.rc"'
alias mutt-ugent='mutt -F "$HOME/.mutt/ugent.rc"'
alias mutting='screen -c "$HOME/.mutt/screen.rc"'
