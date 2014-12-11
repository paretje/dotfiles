#!/bin/bash
/usr/sbin/chat TIMEOUT 1 "" "AT+CMGF=1" "OK" > /dev/ttyUSB1
/usr/sbin/chat TIMEOUT 1 "" "AT+CMGS=\"0499210906\"" "OK" > /dev/ttyUSB1
/usr/sbin/chat TIMEOUT 1 "" "$1" "OK" > /dev/ttyUSB1
/usr/sbin/chat TIMEOUT 1 "" "^Z" > /dev/ttyUSB1
