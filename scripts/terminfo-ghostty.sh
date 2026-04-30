#!/usr/bin/env bash
HOST="$1"
PORT=${2:-22}

if [[ -z "$HOST" ]]; then
    echo "Usage: $(basename "$0") <host> [port]" >&2
    exit 1
fi

mkdir -p /tmp/terminfo && \
infocmp -x xterm-ghostty > /tmp/xterm-ghostty.info
sed '2s/|ghostty|Ghostty//' /tmp/xterm-ghostty.info > /tmp/xterm-ghostty-clean.info
tic -x -o /tmp/terminfo /tmp/xterm-ghostty-clean.info
ssh -p "$PORT" ${HOST} 'mkdir -p ~/.terminfo/78'
scp -P ${PORT} /tmp/terminfo/78/xterm-ghostty ${HOST}:~/.terminfo/78/
rm -rf /tmp/terminfo