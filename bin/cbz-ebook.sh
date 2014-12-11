#!/bin/sh
# Made by Paretje, 03/07/2013

# Cut the extension from the file
NAME=$( echo "$1" | sed 's/\.cbz$//' )

# Print the filename, so the user knows we're bussy
#echo "$PWD/$1"

# Extract with unrar, and zip as cbz
#mkdir "$NAME"
unzip "$1" -d "$NAME" > /dev/null
find "$NAME" -type f -execdir "/home/kevin/Cloud/bin/split-image.sh" "{}" "$2" ";"
zip -j -r "$NAME-ebook.cbz" "$NAME" > /dev/null
rm -r "$NAME"
