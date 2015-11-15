. "$HOME/.profile"
if [ -f "$GPG_ENV_FILE" ]; then
	. "$GPG_ENV_FILE"
	export GPG_TTY=$(tty)
	export GPG_AGENT_INFO
	export SSH_AUTH_SOCK
	export SSH_AGENT_PID
fi

# Remap some keys
xmodmap -e 'clear lock'
xmodmap -e 'keysym Super_L = Escape'
xmodmap -e 'keysym Caps_Lock = Escape'

xmodmap -e 'remove control = Control_R'
xmodmap -e 'remove mod4 = Super_R'
xmodmap -e 'add mod3 = Control_R'
xmodmap -e 'add mod3 = Super_R'

# Add HD output mode
xrandr --newmode "1920x1080_sharp"  141.50  1920 2032 2232 2544  1080 1083 1088 1114 -hsync +vsync
if [ "$HOST" = "kevin-laptop" ]; then
	xrandr --addmode HDMI1 1920x1080_sharp
elif [ "$HOST" = "kevin-desktop" ]; then
	xrandr --addmode VGA-1 1920x1080_sharp
	xrandr --output VGA-1 --mode 1920x1080_sharp
fi

# Load Xresources
xrdb ~/.Xresources

# Start some daemons
eval $(/usr/bin/gnome-keyring-daemon --start --components=secrets)
xscreensaver -no-splash &
dunst &
clipit --daemon &
exec xmonad
