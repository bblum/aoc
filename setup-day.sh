#1/bin/bash

TEMPLATE_DIR=template-c

function die() {
	echo "$1"
	exit 1
}
if [ -z "$1" ]; then
	die "need day number"
fi
mkdir day$1 || die "couldnt make dir"
cp $TEMPLATE_DIR/* day$1 || die "couldnt copy contents"
cd day$1 || die "wut"
