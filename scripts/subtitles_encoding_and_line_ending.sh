#!/usr/bin/env bash
# Convert subtitle files from CP1250 to UTF-8 in place. Takes any number of
# files (Automator passes the Finder selection as arguments).
#
# Files that are already valid UTF-8 are left alone: iconv -f CP1250 would
# accept them too and turn every Polish letter into mojibake. A file is
# replaced only after a successful conversion.

set -uo pipefail

if [ $# -eq 0 ]; then
  echo "Usage: $(basename "$0") file..." >&2
  exit 1
fi

status=0
for FILE in "$@"; do
  if [ ! -f "$FILE" ]; then
    echo "Not a file: $FILE" >&2
    status=1
    continue
  fi

  if iconv -f UTF-8 -t UTF-8 "$FILE" >/dev/null 2>&1; then
    echo "Already UTF-8, skipped: $FILE"
    continue
  fi

  # temp file next to the original, so mv is a rename on the same filesystem
  TMP=$(mktemp "$(dirname "$FILE")/.$(basename "$FILE").XXXXXX") || { status=1; continue; }
  if iconv -f CP1250 -t UTF-8 "$FILE" >"$TMP"; then
    chmod "$(stat -f %Lp "$FILE")" "$TMP"
    mv "$TMP" "$FILE"
    echo "Converted: $FILE"
  else
    rm -f "$TMP"
    echo "iconv failed, original kept: $FILE" >&2
    status=1
  fi
done
exit "$status"

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
