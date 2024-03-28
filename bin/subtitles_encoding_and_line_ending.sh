#!/usr/bin/env bash
#pass input as arguments

FILE="$1"

if [ ! -f "$FILE" ]; then
	echo "Provide file name"
	exit 1
fi

# EXT="${FILE##*.}"
EXT=$(echo "$FILE" | sed 's/^.*\.//')
NAME="${FILE%.*}"
OUT="${NAME}.converted.${EXT}"
# OUT="${NAME}.whatsapp.mp4"
echo "OUT=${OUT}"

iconv -f CP1250 -t UTF-8 "$FILE" >"$OUT"

# - `Automator > Quick Action`
#   - `Workflow receives current: files or folders`
#   - `in: Finder.app`
#   - on the left search for `Run Shell Script` and drag-and-drop it to the right.
#   - `Pass input: as arguments`
#
# The conent will be:
#
# ```bash
# . $HOME/.zprofile
# file_name.sh "$@"
# ```
#
# chmod +x file_name.sh

# https://stackoverflow.com/questions/64860/best-way-to-convert-text-files-between-character-sets
