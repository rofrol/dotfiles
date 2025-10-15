#!/bin/bash
# https://x.com/NurmiWilliam/status/1823228630664634695
# sudo lsof -iTCP -sTCP:LISTEN -n -P | awk 'NR>1 {print $9, $1, $2}' | sed 's/.*://' | sort -u | while read port process pid; do echo "Port $port: $(ps -p $pid -o command= | sed 's/^-//') (PID: $pid)"; done | sort -n
# https://apple.stackexchange.com/questions/117644/how-can-i-list-my-open-network-ports-with-netstat/408832#408832
netstat -Watnlv | grep LISTEN | column -t -s "|"
