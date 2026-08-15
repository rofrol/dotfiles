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

# H.264/AAC in an MP4 container is the compatibility baseline for WhatsApp.
# Do not force an H.264 level: libx264 must signal one that matches the
# requested 720p/30fps stream instead of declaring the invalid Level 3.0
# previously used here.
# ffmpeg -i "$FILE" -vf scale=-2:720 -c:v libx264 -profile:v main -pix_fmt yuv420p -c:a aac "$OUT"

# https://dev.to/alfg/ffmpeg-for-instagram-35bi
# https://www.martin-riedl.de/2022/01/09/two-pass-encoding-with-ffmpeg/
# 1st pass is only for analysis https://superuser.com/questions/1549360/second-pass-while-using-concat-in-ffmpeg#comment2356163_1549360
# https://img.ly/blog/ultimate-guide-to-ffmpeg/
# https://superuser.com/questions/502364/ffmpeg-settings-for-youtube-and-facebook-video-uploads
# https://trac.ffmpeg.org/wiki/Encode/H.264
# https://github.com/term7/FFmpeg-A-short-Guide
# https://shotstack.io/learn/how-to-use-ffmpeg/
# The old two-pass examples below are retained as references only.
# The same rule applies to the old two-pass examples: a fixed Level 3.0 is
# not valid for every source after scaling and frame-rate conversion.
# ffmpeg -i "$FILE" -vf scale=-2:720 -c:v libx264 -profile:v main -x264-params scenecut=0:open_gop=0:min-keyint=72:keyint=72:ref=4 -c:a aac -b:v 3500k -maxrate 3500k -bufsize 3500k -r 30 -ar 44100 -b:a 256k -pass 1 -sn -f mp4 NUL && \
# ffmpeg -i "$FILE" -vf scale=-2:720 -c:v libx264 -profile:v main -x264-params scenecut=0:open_gop=0:min-keyint=72:keyint=72:ref=4 -c:a aac -b:v 3500k -maxrate 3500k -bufsize 3500k -r 30 -ar 44100 -b:a 256k -pass 2 "$OUT"

# `+faststart` moves MP4's `moov` index before the media payload. This lets
# WhatsApp inspect the file as a video without downloading the whole file.
# Keep errors visible: `-v quiet` hid failed conversions and stale outputs.
# `-sn` deliberately drops source subtitles because WebVTT is not a
# WhatsApp-compatible MP4 subtitle track.
# Do not use `-level:v 3.0`: 720p30 needs a higher compliant H.264 level.
# https://www.reddit.com/r/bash/comments/18i8zpn/comment/kdbs4a8/
# https://stackoverflow.com/questions/35169650/differentiate-between-error-and-standard-terminal-log-with-ffmpeg-nodejs/35215447#35215447

#pueue add --label "video_for_whatsapp.sh" ffmpeg -loglevel error -stats -i "$FILE" -vf scale=-2:720 -c:v libx264 -profile:v main -pix_fmt yuv420p -x264-params scenecut=0:open_gop=0:min-keyint=72:keyint=72:ref=4 -c:a aac -crf 23 -maxrate 3500k -bufsize 3500k -r 30 -ar 44100 -b:a 256k -sn -movflags +faststart -f mp4 "$OUT"

ffmpeg -y -loglevel error -stats \
  -i "$FILE" \
  -vf "scale=-2:'min(1080,ih)'" \
  -c:v hevc_videotoolbox \
  -profile:v main \
  -tag:v hvc1 \
  -prio_speed 1 \
  -b:v 1000k \
  -maxrate 1200k \
  -bufsize 2400k \
  -c:a aac \
  -b:a 128k \
  -sn \
  -movflags +faststart \
  -f mp4 \
  "$OUT"

# One-pass encoding normally creates no pass logs; `-f` keeps cleanup from
# turning a successful queued conversion into a false script failure.
# https://stackoverflow.com/questions/60122204/can-i-recycle-ffmpeg2pass-0-log
rm -f \
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
#   - If you already have workflow file, just double-click it to open in Automator and cmd+s to save. Then it will show up in `~/Library/Services`.
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
