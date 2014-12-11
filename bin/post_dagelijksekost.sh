#!/bin/bash
FILE="$1"
DATE="$(date -d @$2 +%Y-%m-%d)"
SEASON=$4
DESC="$3"

OFSET_D=$5
OFSET_W=$6
let WEEK=$(date -d @$2 +%-V)-$OFSET_W
let DAYS=$(date -d @$2 +%-j)-$OFSET_D

if [ $(date +%u) -eq 7 ]; then
	let EPISODE=6*$WEEK
	TITLE="Weekoverzicht $WEEK"
else
	let EPISODE=$DAYS-$WEEK
	let NR=$DAYS-2*$WEEK
	TITLE="Aflevering $NR"
fi

cat > ${FILE%.ts}.nfo <<-EOF
<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>
<episodedetails>
	<title>$TITLE</title>
	<season>$SEASON</season>
	<episode>$EPISODE</episode>
	<plot>$DESC</plot>
	<aired>$DATE</aired>
</episodedetails>
EOF

dvbcut -generateidx "$FILE"
