#!/bin/bash
# TODO: Get lates from https://ziglang.org/download/index.json
url="$1"
file="${url##*/}"
echo "file: $file"
dir=$(basename "$file" .tar.xz)
echo "dir: $dir"
cd ~/installed
curl -OJN "$url" && \
tar xf "$file" && \
ln -sfn "$dir" zig