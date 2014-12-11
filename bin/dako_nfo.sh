#!/bin/bash
FILE=$1
NAME=${FILE%.ts}
EPISODE=${NAME:0:3}
let WEEK=EPISODE/6
let NR=EPISODE%6
let AFLEVERING=WEEK*5+NR

cat > $NAME.nfo <<-EOF
<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>
<episodedetails>
	<title>Aflevering $AFLEVERING</title>
	<season>4</season>
	<episode>$EPISODE</episode>
</episodedetails>
EOF
