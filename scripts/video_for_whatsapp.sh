#!/usr/bin/env bash

FILE="$1"

if [ ! -f "$FILE" ]; then
  echo "Provide file name"
  exit 1
fi

EXT="${FILE##*.}"
NAME="${FILE%.*}"
#OUT="${NAME}.whatsapp.${EXT}"
OUT="${NAME}.whatsapp.mp4"
echo "OUT=${OUT}"

# ffmpeg -i "$FILE" -c:v libx264 -profile:v baseline -level 3.0 -pix_fmt yuv420p "$OUT"

# https://dev.to/alfg/ffmpeg-for-instagram-35bi
# https://www.martin-riedl.de/2022/01/09/two-pass-encoding-with-ffmpeg/
# 1st pass is only for analysis https://superuser.com/questions/1549360/second-pass-while-using-concat-in-ffmpeg#comment2356163_1549360
# https://img.ly/blog/ultimate-guide-to-ffmpeg/
# https://superuser.com/questions/502364/ffmpeg-settings-for-youtube-and-facebook-video-uploads
# https://trac.ffmpeg.org/wiki/Encode/H.264
# https://github.com/term7/FFmpeg-A-short-Guide
# https://shotstack.io/learn/how-to-use-ffmpeg/
# below errors
# ffmpeg -i "$FILE" -vf scale=-2:720 -c:v libx264 -profile:v main -level:v 3.0 -x264-params scenecut=0:open_gop=0:min-keyint=72:keyint=72:ref=4 -c:a aac -b:v 3500k -maxrate 3500k -bufsize 3500k -r 30 -ar 44100 -b:a 256k -pass 1 -sn -f mp4 NUL && \
# ffmpeg -i "$FILE" -vf scale=-2:720 -c:v libx264 -profile:v main -level:v 3.0 -x264-params scenecut=0:open_gop=0:min-keyint=72:keyint=72:ref=4 -c:a aac -b:v 3500k -maxrate 3500k -bufsize 3500k -r 30 -ar 44100 -b:a 256k -pass 2 "$OUT"

# for ffmpeg to display in Automator output only errors from processing, add `-loglevel error -v quiet -stats`
# https://www.reddit.com/r/bash/comments/18i8zpn/comment/kdbs4a8/
# https://stackoverflow.com/questions/35169650/differentiate-between-error-and-standard-terminal-log-with-ffmpeg-nodejs/35215447#35215447
ffmpeg -loglevel error -v quiet -stats -i "$FILE" -vf scale=-2:720 -c:v libx264 -profile:v main -level:v 3.0 -x264-params scenecut=0:open_gop=0:min-keyint=72:keyint=72:ref=4 -c:a aac -crf 23 -maxrate 3500k -bufsize 3500k -r 30 -ar 44100 -b:a 256k -sn -f mp4 "$OUT"

# https://stackoverflow.com/questions/60122204/can-i-recycle-ffmpeg2pass-0-log
rm \
  "${FILE%/*}/ffmpeg2pass-0.log" \
  "${FILE%/*}/ffmpeg2pass-0.log.temp" \
  "${FILE%/*}/ffmpeg2pass-0.log.mbtree.temp" &>/dev/null

# https://stackoverflow.com/questions/39887869/ffmpeg-whatsapp-video-format-not-supported/45882902#45882902

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
