#!/usr/bin/env bash
# Play every video file under DIR (default: current directory) in mpv,
# largest first. A file counts as video when `file` reports a video/* type.

set -euo pipefail

dir="${1:-$PWD}"

find "$dir" -type f -print0 |
	while IFS= read -r -d '' f; do
		case "$(file -b --mime-type -- "$f")" in
		video/*) printf '%s\t%s\n' "$(stat -f%z -- "$f")" "$f" ;;
		esac
	done |
	sort -t $'\t' -k1,1nr |
	cut -f2- |
	mpv --playlist=-

# https://askubuntu.com/questions/844711/how-can-i-find-all-video-files-on-my-system/844720#844720
# https://www.commandlinefu.com/commands/view/22244/find-files-and-play-them-as-a-playing-list-in-mpv
