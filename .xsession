. "$HOME/.profile"
if [ -f "$GPG_ENV_FILE" ]; then
    . "$GPG_ENV_FILE"
    export GPG_TTY=$(tty)
    export GPG_AGENT_INFO
    export SSH_AUTH_SOCK
    export SSH_AGENT_PID
fi
eval $(/usr/bin/gnome-keyring-daemon --start --components=secrets)
setxkbmap -option caps:super
exec startxfce4
