#!/bin/sh
LIST="$1"

TITLE="Add download"
TEXT="Give the URL of the download you want to add to the queue:"
URL=$(zenity --title="$TITLE" --text="$TEXT" --entry)
if [ $? -eq 0 ]; then
	/home/kevin/bin/addtolist.sh "$URL" "$LIST"
fi
