#!/bin/sh
# Made by Paretje, 27/05/2013

# find /media/folder -iregex '.*\.cbr$' -execdir "/location/of/this/file/cbr2cbz.sh" ["/pad/voor/backup"] "{}" ";"

# Cut the extension from the file
NAME=$( echo "$1" | sed 's/\.cbr$//' )

# Print the filename, so the user knows we're bussy
echo "$PWD/$1"

# Extract with unrar, and zip as cbz
mkdir "$NAME"
unrar e "$1" "$NAME" > /dev/null
zip -j -r "$NAME.cbz" "$NAME" > /dev/null
rm -r "$NAME"

# Move the cbr file to a backup-dir, or simply hold it where it is
if [ "$2" != "" ]; then
	mv "$1" "$2"
fi
