#!/bin/sh
ssh rpi "ls -1 /media/sda1-ata-WDC_WD10EADS-00M | sed '/^lost+found$/d'" | \
while read dir; do
	if [ -d "/storage/media/series/$dir" ]; then
		echo "$dir"
		rsync -av --delete $1 --rsync-path=/storage/.xbmc/addons/network.backup.rsync/bin/rsync /storage/media/series/"$dir" rpi:/media/sda1-ata-WDC_WD10EADS-00M
	fi
done
