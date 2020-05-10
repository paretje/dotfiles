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

# Use GTK style in Qt5 applications
export QT_STYLE_OVERRIDE=gtk2
export QT_QPA_PLATFORMTHEME=gtk2

# Disable overlay scrollbar in GTK 3
export GTK_OVERLAY_SCROLLING=0

# Fix Java programs
export _JAVA_AWT_WM_NONREPARENTING=1

# Configure keyboard layout
keyboard-configuration

# Set brightness of laptop
if [ "$HOST" = "kevin-laptop" ]; then
	brightnessctl set 30%
elif [ "$HOST" = "kevin-vib-laptop" ]; then
	brightnessctl set 20%
fi

# Set default mouse pointer
xsetroot -cursor_name left_ptr

# Load Xresources
xrdb ~/.Xresources

# Touchpad configuration
if [ "$HOST" = "kevin-laptop" ]; then
	xinput set-prop "pointer:Synaptics TM3075-002" "libinput Tapping Enabled" 1
	xinput set-prop "pointer:TPPS/2 IBM TrackPoint" "libinput Accel Profile Enabled" 0 1
elif [ "$HOST" = "chervil" ]; then
	xinput set-prop 11 278 1
	# xinput set-prop 12 298 -0.4
	xinput set-prop 12 307 0 1
	xinput set-prop 12 298 1
elif [ "$HOST" = "kevin-chromebook" ]; then
	synclient TapButton1=1 Tapbutton2=3 TapButton3=2 SingleTapTimeout=100 MaxTapTime=100 ClickTime=5 LockedDrags=1 LockedDragTimeout=100
	# TODO: I don't think I should use/need AreaBottomEdge, and is BottomEdge really what I need?
	#       maybe ClickPad, RightButtonArea*, ...?
	synclient FingerLow=10 FingerHigh=10 BottomEdge=400 AreaBottomEdge=400 MaxTapMove=20
	# TODO: options for intel?
	syndaemon -d -t -R
fi

# Disable back and forward buttons on Logitech M500 mouse
xinput set-button-map 'Logitech USB Laser Mouse' 1 2 3 0 0 6 7 0 0 10 11 12

# Substitute variables in user-dirs.dirs file
user-dirs-make

# Start some daemons
xclipman &
if acpi -b | fgrep -vq 'No support'; then
	sh -c 'while true; do batmonitor ; sleep 2m ; done' &
fi

exec urxvt -e sh -c 'xwit -resize 800 480;exec tmux'
