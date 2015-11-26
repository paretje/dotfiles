#!/bin/sh
ssh rpi "ls -1 /media/sda1-ata-WDC_WD10EADS-00M | sed '/^lost+found$/d'" | \
while read dir; do
	if [ -d "/storage/media/series/$dir" ]; then
		echo "$dir"
		lftp -e "mirror -R --delete-first /storage/media/series/\"$dir\" \"$dir\" ; exit" 192.168.2.4
	fi
done
