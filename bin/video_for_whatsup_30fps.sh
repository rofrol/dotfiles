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

ffmpeg -i "$FILE" \
  -vf "fps=30,scale=1280:-2" \
  -c:v libx264 -profile:v high -preset veryfast -crf 23 \
  -c:a aac -b:a 128k \
  -movflags +faststart \
  "$OUT"
# -c:v libx265 -x265-params crf=25 -c:a aac -b:a 256k "$OUT"

# https://chatgpt.com/c/6803e780-0a34-800b-8d89-03ba249b174d
# https://stackoverflow.com/questions/39887869/ffmpeg-whatsapp-video-format-not-supported/45882902#45882902
# https://chatgpt.com/c/6803e555-8cb0-800b-83c8-4b74d624a9e0
