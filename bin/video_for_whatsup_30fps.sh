#!/usr/bin/env bash

FILE="$1"

if [ ! -f "$FILE" ]; then
	echo "Provide file name"
	exit 1
fi

EXT="${FILE##*.}"
NAME="${FILE%.*}"
#OUT="${NAME}.whatsapp.${EXT}"
OUT="${NAME}.whatsapp.30fps.mp4"
echo "OUT=${OUT}"

ffmpeg -i "$FILE" -c:v libx265 -an -x265-params crf=25 "$OUT"

# https://stackoverflow.com/questions/39887869/ffmpeg-whatsapp-video-format-not-supported/45882902#45882902
