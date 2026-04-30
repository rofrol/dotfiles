#!/usr/bin/env bash
HOST="$1"
PORT=${2:-22}

if [[ -z "$HOST" ]]; then
    echo "Usage: $(basename "$0") <host> [port]" >&2
    exit 1
fi

mkdir -p /tmp/terminfo && \
# error: alias ghostty multiply defined
infocmp -x xterm-ghostty | sed '2s/|ghostty|Ghostty//' > /tmp/xterm-ghostty.info
tic -x -o /tmp/terminfo /tmp/xterm-ghostty.info
ssh -p "$PORT" ${HOST} 'mkdir -p ~/.terminfo/78'
scp -P ${PORT} /tmp/terminfo/78/xterm-ghostty ${HOST}:~/.terminfo/78/
rm -rf /tmp/terminfo

# https://github.com/ghostty-org/ghostty/discussions/8268#discussioncomment-16769781