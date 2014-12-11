#!/bin/sh
URL=$( echo "$1" |  sed 's/\//\\\//g' | sed 's/'\''/'\'\\\\\'\''/g' )
sed -i '/^'"$URL"'$/d' "$2"
