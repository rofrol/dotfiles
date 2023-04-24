#!/usr/bin/env bash

# Bash unofficial strict mode
set -euo pipefail
IFS=$'\n\t'
LANG=''

function define() { IFS='\n' read -r -d '' ${1} || true; }

define TMPL << 'EOF'
{
  "commit": "%H",
  "tree": "%T",
  "parent": "%P",
  "refs": "%D",
  "encoding": "%e",
  "message": MSG,
  "commit_notes": "%N",
  "author": {
    "name": AUTHOR,
    "email": "%aE",
    "date": "%aI"
  },
  "commiter": {
    "name": COMMITER,
    "email": "%cE",
    "date": "%cI"
  }
}
EOF

function sanitize () {
  # strip newlines, strip all whitespace, escape newline
  sed -E -e ':a' -e '/./,$!d;/^\n*$/{$d;N;};/\n$/ba' -e 's/^\s+|\s+$//g' -e ':a;N;$!ba;s/\n/\\n/g' \
    | sed -E -e 's/%/%%/g' -e 's/"/\\"/g' -e 's/\\\\/\\/g' -e 's/\t/\\t/g' -e 's/\\([^nt"])/\\\\\1/g' -e 's/[\x00-\x1f]//g' | tr -d '\r\0\t'
  # escape percent sign for template, escape quotes, escape backslash, escape tabs, fix double escapes, delete control characters
}

function field () {
  #git show -s --format="$1" "$2" | sanitize
  #git show -s --format="$1" "$2" | jq -Rsa .
  #git show -s --format="$1" "$2" | sed -Ez '$ s/\n+$//' | jq --slurp --raw-input
  git show -s --format="$1" "$2" | perl -ne 'if (/./) { print "\n" x $n, $_; $n = 0 } else { $n++ }' | jq -Rsa .
}

git log --pretty=format:'%H' | while IFS='' read -r hash; do
  TMP="$TMPL"
  msg=$(field "%B" $hash)
  author=$(field "%aN" $hash)
  commiter=$(field "%cN" $hash)
  TMP="${TMP/MSG/$msg}"
  TMP="${TMP/AUTHOR/$author}"
  TMP="${TMP/COMMITER/$commiter}"
  git show $hash -s --format="$TMP"
done
