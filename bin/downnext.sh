#!/bin/sh
LIST="$1"
FOLDER="/home/kevin/Downloads"

TITLE="Start download"
TEXT="Select the download you want to start:"
DOWNLOAD=$(zenity --list --title="$TITLE" --text="$TEXT" --width=750 --column="URL" $(cat $LIST))

/home/kevin/bin/plowdown -o "$FOLDER" "$DOWNLOAD"

notify-send -u 'low' -i '/usr/share/icons/Tango/scalable/actions/down.svg' 'Download Afgerond!' 'Kevin, uw download is afgerond.'

QUESTION="Do you want to delete \"$DOWNLOAD\" from the list of downloads?"
zenity --question --text="$QUESTION" 
if [ $? -eq 0 ]; then
	/home/kevin/bin/delfromlist.sh "$DOWNLOAD" "$LIST"
fi
