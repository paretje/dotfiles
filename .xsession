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
if [ -e "/usr/lib/x86_64-linux-gnu/qt5/plugins/styles/libqgtk2style.so" ]; then
	export QT_STYLE_OVERRIDE=gtk2
else
	export QT_STYLE_OVERRIDE=gtk
fi
export QT_QPA_PLATFORMTHEME=gtk2

# Disable overlay scrollbar in GTK 3
export GTK_OVERLAY_SCROLLING=0

# Set keyboard layout
setxkbmap -layout us -variant altgr-intl

# Remap some keys
xmodmap -e 'clear lock'
xmodmap -e 'keysym Caps_Lock = Escape'

if [ "$HOST" = "kevin-laptop" -o "$HOST" = "chervil" ]; then
	xmodmap -e 'keysym Print = Hyper_L'
	xmodmap -e 'keysym Prior = Left'
	xmodmap -e 'keysym Next = Right'
fi

if [ "$HOST" = "kevin-chromebook" ]; then
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
	brightnessctl set 30%
elif [ "$HOST" = "parsley" ]; then
	brightnessctl set 40%
fi

# Some settings for laptop when working at desk
if [ "$HOST" = "kevin-laptop" ] && ifconfig eth0 | fgrep -q 'inet'; then
	if xrandr | fgrep -q 'HDMI1 connected'; then
		xrandr --output HDMI1 --auto && xrandr --output eDP1 --off
	fi

	pactl set-default-sink kevin-desktop
fi

if [ "$HOST" = "chervil" ]; then
	# xrandr --output eDP1 --mode 2560x1440 --dpi 120
	xrandr --output eDP1 --mode 2048x1152
fi

# Set default mouse pointer
xsetroot -cursor_name left_ptr

# Set wallpaper
display -window root /usr/share/images/desktop-base/desktop-background &

# Load Xresources
xrdb ~/.Xresources

# Touchpad configuration
if [ "$HOST" = "kevin-laptop" ]; then
	xinput set-prop 10 281 1
	xinput set-prop 11 311 0 1
elif [ "$HOST" = "chervil" ]; then
	xinput set-prop 11 278 1
	# xinput set-prop 12 298 -0.4
	xinput set-prop 12 307 0 1
	xinput set-prop 12 298 1
else
	synclient TapButton1=1 Tapbutton2=3 TapButton3=2 SingleTapTimeout=100 MaxTapTime=100 ClickTime=5 LockedDrags=1 LockedDragTimeout=100
	if [ "$HOST" = "kevin-chromebook" ]; then
		# TODO: I don't think I should use/need AreaBottomEdge, and is BottomEdge really what I need?
		#       maybe ClickPad, RightButtonArea*, ...?
		synclient FingerLow=10 FingerHigh=10 BottomEdge=400 AreaBottomEdge=400 MaxTapMove=20
		# TODO: options for intel?
	else [ "$HOST" = "parsley" ]
		synclient FingerHigh=18 MaxTapMove=20 ClickPad=1
	fi
	syndaemon -d -t -R
fi

# Substitute variables in user-dirs.dirs file
user-dirs-make

# Start some daemons
xscreensaver -no-splash &
dunst &
xclipman &
maildaemon &
test -x "$GOPATH/bin/matterircd" && sh -c 'while true; do matterircd -config "$HOME/.config/matterircd/matterircd.toml"; sleep 5s; done' &

if acpi -b | fgrep -vq 'No support'; then
	sh -c 'while true; do batmonitor ; sleep 2m ; done' &
fi

# Start daemon with conky
status=$(mktemp -u)
mkfifo "$status"
sh -c "exec > '$status' ; while true; do conky; sleep 5s ; done" &

exec dwm < "$status"
