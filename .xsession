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

xrandr --output eDP1 --scale 1.75x1.75 --dpi 192

# Set default mouse pointer
xsetroot -cursor_name left_ptr

# Set wallpaper
display -window root /usr/share/images/desktop-base/desktop-background &

# Load Xresources
xrdb ~/.Xresources

# Touchpad configuration
if xinput list --name-only | fgrep -q 'Elan Touchpad'; then
	xinput set-prop "pointer:Elan Touchpad" "libinput Tapping Enabled" 1
	xinput set-prop "pointer:Elan TrackPoint" "libinput Accel Profile Enabled" 0 1
elif xinput list --name-only | fgrep -q 'Synaptics TM3075-002'; then
	xinput set-prop "pointer:Synaptics TM3075-002" "libinput Tapping Enabled" 1
	xinput set-prop "pointer:TPPS/2 IBM TrackPoint" "libinput Accel Profile Enabled" 0 1
fi

# Disable back and forward buttons on Logitech M500 mouse
xinput set-button-map 'Logitech USB Laser Mouse' 1 2 3 0 0 6 7 0 0 10 11 12

# Substitute variables in user-dirs.dirs file
user-dirs-make

# Start some daemons
start-pulseaudio-x11
xscreensaver -no-splash &
dunst &
xclipman &
maildaemon &
test -x "$GOPATH/bin/matterircd" && sh -c 'while true; do matterircd -conf "$HOME/.config/matterircd/matterircd.toml"; sleep 5s; done' &

if acpi -b | fgrep -vq 'No support'; then
	sh -c 'while true; do batmonitor ; sleep 2m ; done' &
fi

# Start daemon with conky
status=$(mktemp -u)
mkfifo "$status"
sh -c "exec > '$status' ; while true; do echo starting conky 1>&2 ; conky; sleep 5s ; done" &

exec dwm < "$status"
