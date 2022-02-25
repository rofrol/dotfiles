#!/usr/bin/env bash

FILE="$1"

if [ ! -f "$FILE" ]; then
  echo "Provide file name"
  exit 1
fi

EXT="${FILE##*.}"
NAME="${FILE%.*}"
OUT="${NAME}.working.${EXT}"
echo "OUT=${OUT}"

ffmpeg -i "$FILE" -c:v libx264 -profile:v baseline -level 3.0 -pix_fmt yuv420p "$OUT"

# https://stackoverflow.com/questions/39887869/ffmpeg-whatsapp-video-format-not-supported/45882902#45882902
