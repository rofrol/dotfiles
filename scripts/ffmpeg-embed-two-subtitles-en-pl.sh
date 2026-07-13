#!/bin/bash
# alternative, not tested
#mkvmerge -o "out.mkv" "$1" \
#--language 0:eng --track-name 0:"English" "$2" \
#--language 0:pol --track-name 0:"Polish" "$3" \
ffmpeg -i "$1" -i "$2" -i "$3" \
	-c copy \
	-map 0 -map 1 -map 2 \
	-metadata:s:s:0 language=pol -metadata:s:s:0 title="English" \
	-metadata:s:s:1 language=eng -metadata:s:s:1 title="Polish" \
	"out.mkv"
