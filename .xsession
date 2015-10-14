. "$HOME/.profile"
if [ -f "$GPG_ENV_FILE" ]; then
    . "$GPG_ENV_FILE"
    export GPG_TTY=$(tty)
    export GPG_AGENT_INFO
    export SSH_AUTH_SOCK
    export SSH_AGENT_PID
fi
eval $(/usr/bin/gnome-keyring-daemon --start --components=secrets)
xmodmap -e 'clear Control'
xmodmap -e 'clear Mod3'
xmodmap -e 'add Control = Control_L'
xmodmap -e 'add Mod3 = Control_R'
if [ "$HOST" = "kevin-laptop" ]; then
	xmodmap -e 'keysym Super_L = Escape'
else
	xmodmap -e 'keysym Caps_Lock = Escape'
fi
exec startxfce4
