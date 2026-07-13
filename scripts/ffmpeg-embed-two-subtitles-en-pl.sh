#!/bin/bash
ffmpeg -i "$1" -i "$2" -i "$3" \
	-c copy \
	-map 0 -map 1 -map 2 \
	-metadata:s:s:0 language=pol -metadata:s:s:0 title="English" \
	-metadata:s:s:1 language=eng -metadata:s:s:1 title="Polish" \
	"out.mkv"
