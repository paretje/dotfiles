#!/bin/sh
# Load profile if not done yet
if [ -z "$LEDGER_FILE" ]; then
	. "$HOME/.profile"
fi

if [ -n "$GPG_ENV_FILE" -a -f "$GPG_ENV_FILE" ]; then
	. "$GPG_ENV_FILE"
	export GPG_TTY=$(tty)
	export GPG_AGENT_INFO
	export SSH_AUTH_SOCK
	export SSH_AGENT_PID
fi

# Set the appropriate VDPAU driver to use
if [ "$(hostname)" = "kevin-laptop" ]; then
	export VDPAU_DRIVER="va_gl"
fi
if [ "$(hostname)" = "kevin-desktop" ]; then
	export VDPAU_DRIVER="nouveau"
fi

# Use GTK style in Qt5 applications
export QT_STYLE_OVERRIDE=gtk2
export QT_QPA_PLATFORMTHEME=gtk2

# Disable overlay scrollbar in GTK 3
export GTK_OVERLAY_SCROLLING=0

# Set keyboard layout
setxkbmap -layout us -variant altgr-intl

# Remap some keys
xmodmap -e 'clear lock'
xmodmap -e 'keysym Caps_Lock = Escape'

if [ "$HOST" = "kevin-laptop" ]; then
	xmodmap -e 'keysym Super_L = Escape'
fi

xmodmap -e 'keysym Menu = Hyper_L'
xmodmap -e 'remove control = Control_R'
xmodmap -e 'remove mod4 = Super_R'
xmodmap -e 'remove mod4 = Hyper_L'
xmodmap -e 'add mod3 = Control_R'
xmodmap -e 'add mod3 = Super_R'
xmodmap -e 'add mod3 = Hyper_L'

# Set brightness of laptop
if [ "$HOST" = "kevin-laptop" ]; then
	xbacklight -set 30
fi

# Some settings for laptop when working on desk
if [ "$HOST" = "kevin-laptop" ] && ifconfig eth0 | fgrep -q 'inet'; then
	if xrandr | fgrep -q 'HDMI1 connected'; then
		xrandr --output HDMI1 --auto && xrandr --output eDP1 --off
	fi

	pactl set-default-sink kevin-desktop
fi

# Set default mouse pointer
xsetroot -cursor_name left_ptr

# Set wallpaper
xloadimage -onroot /usr/share/wallpapers/joy_inksplat/contents/images/1920x1080.png

# Load Xresources
xrdb ~/.Xresources

# Start some daemons
eval $(/usr/bin/gnome-keyring-daemon --start --components=secrets)
xscreensaver -no-splash &
dunst &
xclipman &
test "$HOST" = "kevin-laptop" && syndaemon -d -t -R
getmails --daemon &
! pgrep unisister > /dev/null && unisister --no-gui &

# Start daemon with conky
status=$(mktemp -u)
mkfifo "$status"
sh -c "exec > '$status' ; while true; do conky; sleep 5s ; done" &

exec dwm < "$status"
