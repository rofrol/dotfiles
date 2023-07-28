#!/usr/bin/env bash

unameOut="$(uname -s)"
case "${unameOut}" in
Linux*) machine=Linux ;;
Darwin*) machine=Darwin ;;
CYGWIN*) machine=Cygwin ;;
MINGW*) machine=MinGw ;;
MSYS_NT*) machine=Git ;;
*) machine="UNKNOWN:${unameOut}" ;;
esac

if [ $machine == "Darwin" ]; then
	#ifconfig | grep "inet " | grep -Fv 127.0.0.1 | awk '{print $2}'
	#ifconfig | grep -E "([0-9]{1,3}\.){3}[0-9]{1,3}" | grep -v 127.0.0.1 | head -1 | awk '{ print $2 }'
	osascript -e 'return IPv4 address of (get system info)'
elif [ $machine == "Linux" ]; then
	ip route get 1 | awk '{print $NF;exit}'
fi

# https://apple.stackexchange.com/questions/20547/how-do-i-find-my-ip-address-from-the-command-line/
# https://stackoverflow.com/questions/3466166/how-to-check-if-running-in-cygwin-mac-or-linux
# https://stackoverflow.com/questions/13322485/how-to-get-the-primary-ip-address-of-the-local-machine-on-linux-and-os-x/25851186#25851186
