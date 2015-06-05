#!/bin/sh
if [ -d /sys/class/net/tun0 ]; then
	echo "VPN"
else
	echo ""
fi
