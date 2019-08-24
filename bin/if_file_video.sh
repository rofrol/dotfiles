#!/bin/bash

input="$1"
file_result="$(file -N -i "$input" | rg '.*: video/.*; charset=binary')"
if [ ! -z "$file_result" ]; then echo "$input"; fi

# https://askubuntu.com/questions/844711/how-can-i-find-all-video-files-on-my-system/844720#844720
