#!/bin/sh
upower -i /org/freedesktop/UPower/devices/battery_BAT0 | fgrep 'percentage' | sed 's/^.* \([^ ]*\)%$/\1%/'
