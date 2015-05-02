#!/bin/sh
if [ -f /sys/class/net/ppp0/statistics/rx_bytes ] ; then
	USAGE=$(vnstat --oneline | sed 's/1;.*;\([^;]*\);[^;]*;[^;]*;[^;]*;[^;]*$/\1/')
	echo $USAGE

	ORDER=$(echo $USAGE | cut -d ' ' -f2)
	SIZE=$(echo $USAGE | cut -d '.' -f1)
	if [ "$ORDER" = "MiB" -a $SIZE -gt 50 ]; then
		notify-send 'Bandwidth used!'
	fi
else
	echo ""
fi
