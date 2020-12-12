#1/bin/bash

function die() {
	echo "$1"
	exit 1
}
if [ -z "$1" ]; then
	die "need day number"
fi
mkdir day$1 || die "couldnt make dir"
#cp template-c/* day$1 || die "couldnt copy contents"
cp template-hs/Template.hs day$1/Day$1.hs || die "couldnt copy hs"
touch day$1/input.txt || die "couldnt make input txt"
cd day$1 || die "wut"
