#!/usr/bin/env sh

for fname in scripts/*; do
	if [ -x "$fname" ] ; then
		echo "Loaded: $fname" | sed -e 's/\..*$//'
	else
		echo "$fname"
	fi
done
