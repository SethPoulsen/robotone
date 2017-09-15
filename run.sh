#!/usr/bin/bash
wd=$(dirname "$0")
output=$1 #path to desired output file

pushd "$wd/src"
if [[ -n "$output" ]]; then # If command-line argument present
	runghc Main.hs "$1"
else
	echo "Error: Missing path to output file."
	exit
fi
popd

