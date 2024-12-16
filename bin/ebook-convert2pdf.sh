#!/usr/bin/env bash

FILE="$1"

if [ ! -f "$FILE" ]; then
  echo "Provide file name"
  exit 1
fi

EXT="${FILE##*.}"
NAME="${FILE%.*}"
OUT="${NAME}.pdf"
echo "OUT=${OUT}"

/opt/homebrew/bin/ebook-convert "$FILE" "$OUT" --no-process

# https://www.epubor.com/convert-cbr-to-pdf.html
# https://askubuntu.com/questions/344339/how-do-i-convert-epub-to-mobi-using-calibre
# https://manual.calibre-ebook.com/generated/en/ebook-convert.html#comic-input-options
