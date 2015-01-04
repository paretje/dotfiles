#!/bin/sh
if [ -f /sys/class/net/ppp0/statistics/rx_bytes ] ; then
	vnstat --oneline | sed 's/1;.*;\([^;]*\);[^;]*;[^;]*;[^;]*;[^;]*$/\1/'
else
	echo ""
fi
