. "$HOME/.profile"
if [ -f "$GPG_ENV_FILE" ]; then
	. "$GPG_ENV_FILE"
	export GPG_TTY=$(tty)
	export GPG_AGENT_INFO
	export SSH_AUTH_SOCK
	export SSH_AGENT_PID
fi

# Remap some keys
xmodmap -e 'clear Control'
xmodmap -e 'clear Mod3'
xmodmap -e 'add Control = Control_L'
xmodmap -e 'add Mod3 = Control_R'
if [ "$HOST" = "kevin-laptop" ]; then
	xmodmap -e 'keysym Super_L = Escape'
else
	xmodmap -e 'keysym Caps_Lock = Escape'
fi

# Add HD output mode
if [ "$HOST" = "kevin-laptop" ]; then
	xrandr --newmode "1920x1080"  141.50  1920 2032 2232 2544  1080 1083 1088 1114 -hsync +vsync
	xrandr --addmode HDMI1 1920x1080
fi

# Start some daemons
eval $(/usr/bin/gnome-keyring-daemon --start --components=secrets)
xscreensaver -no-splash &
/usr/lib/x86_64-linux-gnu/xfce4/notifyd/xfce4-notifyd &
exec xmonad
