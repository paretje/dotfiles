#!/bin/bash
FILE=$1
NAME=${FILE%.mpg}
EPISODE=${NAME:0:3}
let WEEK=EPISODE/6

cat > $NAME.nfo <<-EOF
<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>
<episodedetails>
	<title>Weekoverzicht $WEEK</title>
	<season>2</season>
	<episode>$EPISODE</episode>
</episodedetails>
EOF
