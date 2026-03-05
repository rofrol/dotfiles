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

# for ffmpeg to display in Automator output only errors from processing, add `-loglevel error -v quiet -stats`
# https://www.reddit.com/r/bash/comments/18i8zpn/comment/kdbs4a8/
# https://stackoverflow.com/questions/35169650/differentiate-between-error-and-standard-terminal-log-with-ffmpeg-nodejs/35215447#35215447
ffmpeg -loglevel error -v quiet -stats \
  -i "$FILE" \
  -vf "fps=30,scale=1280:-2" \
  -c:v libx264 -profile:v high -preset veryfast -crf 23 \
  -c:a aac -b:a 128k \
  -movflags +faststart \
  "$OUT"
# -c:v libx265 -x265-params crf=25 -c:a aac -b:a 256k "$OUT"

# https://chatgpt.com/c/6803e780-0a34-800b-8d89-03ba249b174d
# https://stackoverflow.com/questions/39887869/ffmpeg-whatsapp-video-format-not-supported/45882902#45882902
# https://chatgpt.com/c/6803e555-8cb0-800b-83c8-4b74d624a9e0

# - `Automator > Quick Action`
#   - `Workflow receives current: files or folders`
#   - `in: Finder.app`
#   - on the left search for `Run Shell Script` and drag-and-drop it to the right.
#   - `Pass input: as arguments`
#   - `Cmd+s` to save it. It will be saved in `~/Library/Services`.
#
# The conent will be:
#
# ```bash
#. $HOME/.profile
#. $HOME/.zshrc
# file_name.sh "$@"
# ```
#
# chmod +x file_name.sh

# Automator runs scripts in non-login and non-interactive mode. So env variable should be put in .zshenv
# for homebrew add this to .zshenv:
#if [[ -x /opt/homebrew/bin/brew ]]; then
#    eval "$(/opt/homebrew/bin/brew shellenv)"
#fi
