#!/bin/sh
# Made by Paretje, 03/07/2013

# find /media/folder -iregex '.*\.cbr$' -execdir "/location/of/this/file/cbr2cbz.sh" ["/pad/voor/backup"] "{}" ";"

# Cut the extension from the file
NAME=$( echo "$1" | sed 's/\.[a-zA-Z]\{3\}$//' )

if [ "$2" = "" ]; then
    convert "$1" -crop 1x2+20+20@  +repage  +adjoin +rotate 270 "$NAME"_%d.jpg ; rm "$1"
else
    convert "$1" -crop 1x2+$2+$2@  +repage  +adjoin +rotate 270 "$NAME"_%d.jpg ; rm "$1"
fi
