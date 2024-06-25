#!/bin/bash

echo "Hello world!"
echo $@
#/bin/bash
for i in $*; do
	echo $i
	mv "$i" /media/roman/f7be3162-a44a-4e2d-84b3-fc27631b3f83/roman/Downloads/
	somehow mv just copies
	# rm -rf "$i"
done
