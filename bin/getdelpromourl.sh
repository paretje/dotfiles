#!/bin/sh
WEEK=$(date +%V)
DAY=$(date +%u)
if [ $DAY -gt 3 ]; then
	DELWEEK=$(printf "%.2d" $(echo "$WEEK+1" | bc))
else
	DELWEEK="$WEEK"
fi
echo "http://nl.delhaize.be/-/media/Images/Promotions/Folders/DigiMag/2014/$DELWEEK/Promo-$DELWEEK-NL/download/magazine.pdf"
