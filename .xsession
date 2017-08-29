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

# Just start kodi on EeePC
if [ "$HOST" = "kevin-eeepc" ]; then
	xinput --disable "AT Translated Set 2 keyboard"
	exec kodi
fi

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
elif [ "$HOST" = "parsley" ]; then
	xbacklight -set 40
fi

# Set default mouse pointer
xsetroot -cursor_name left_ptr

# Load Xresources
xrdb ~/.Xresources

# Touchpad configuration
synclient TapButton1=1 Tapbutton2=3 TapButton3=2 SingleTapTimeout=100 MaxTapTime=100 ClickTime=5 LockedDrags=1 LockedDragTimeout=100
if [ "$HOST" = "kevin-laptop" ]; then
	# TODO: I don't think I should use/need AreaBottomEdge, and is BottomEdge really what I need?
	#       maybe ClickPad, RightButtonArea*, ...?
	synclient FingerLow=10 FingerHigh=10 BottomEdge=400 AreaBottomEdge=400 MaxTapMove=20
	# TODO: options for intel?
else [ "$HOST" = "parsley" ]
	synclient FingerHigh=18 MaxTapMove=20 ClickPad=1
fi

# Substitute variables in user-dirs.dirs file
user-dirs-make

# Start some daemons
xclipman &
syndaemon -d -t -R
if acpi -b | fgrep -vq 'No support'; then
	sh -c 'while true; do batmonitor ; sleep 2m ; done' &
fi

exec urxvt -e sh -c 'xwit -resize 800 480;exec tmux'
