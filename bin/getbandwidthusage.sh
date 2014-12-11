#!/bin/sh
if [ -f /sys/class/net/ppp0/statistics/rx_bytes ] ; then
	# echo aanvaardt standaard geen \n
	# Opmerkelijk: bij dash wel? Wordt al geïnterpreteerd voor echo?
	printf '+\np' | cat /sys/class/net/ppp0/statistics/rx_bytes /sys/class/net/ppp0/statistics/tx_bytes - | dc | /home/kevin/bin/bytes2human.sh
else
	echo ""
fi
