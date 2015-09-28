#!/usr/bin/env sh

for fname in *; do
	if [ -x "$fname" ] ; then
		echo -n " [*]$fname" | sed -e 's/\..*$//'
	else
		echo -n " [ ]$fname" | sed -e 's/\..*$//'
	fi
done
