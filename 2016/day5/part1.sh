#!/bin/bash

ID=cxdnnyjw

SALT=0
NUM_CHARS=0
PASS=
while [ "$NUM_CHARS" != 8 ]; do
	HASH=`echo -n "$ID$SALT" | md5sum`
	if echo $HASH | grep "^00000" >/dev/null; then
		CHAR=`echo $HASH | cut -c6`
		PASS="$PASS$CHAR"
		NUM_CHARS=$(($NUM_CHARS+1))
		echo $PASS using salt $SALT
	fi
	echo "testing $SALT"
	SALT=$(($SALT+1))
done
