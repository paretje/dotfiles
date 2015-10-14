. "$HOME/.profile"
if [ -f "$GPG_ENV_FILE" ]; then
    . "$GPG_ENV_FILE"
    export GPG_TTY=$(tty)
    export GPG_AGENT_INFO
    export SSH_AUTH_SOCK
    export SSH_AGENT_PID
fi
eval $(/usr/bin/gnome-keyring-daemon --start --components=secrets)
if [ "$HOST" = "kevin-laptop" ]; then
	xmodmap -e 'keysym Super_L = Escape'
else
	xmodmap -e 'keysym Caps_Lock = Escape'
fi
exec startxfce4
