#!/bin/sh
# Fix Java programs
export _JAVA_AWT_WM_NONREPARENTING=1

# Configure keyboard layout
setxkbmap -layout us -variant altgr-intl -option caps:escape -option altwin:prtsc_rwin

# Set brightness of laptop
brightnessctl set 20%

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

# Start some daemons
# xscreensaver -no-splash &
xclipman &

# Start daemon with conky
status=$(mktemp -u)
mkfifo "$status"
sh -c "exec > '$status' ; while true; do echo starting conky 1>&2 ; conky; sleep 5s ; done" &

exec dwm < "$status"
