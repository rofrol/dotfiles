#!/usr/bin/env bash
# Local IPv4 address of the interface that carries the default route.

case "$(uname -s)" in
Darwin)
	ipconfig getifaddr "$(route -n get default | awk '/interface: / {print $2}')"
	;;
Linux)
	ip route get 1 | awk '{for (i = 1; i < NF; i++) if ($i == "src") {print $(i + 1); exit}}'
	;;
*)
	echo "unsupported OS: $(uname -s)" >&2
	exit 1
	;;
esac

# https://apple.stackexchange.com/questions/20547/how-do-i-find-my-ip-address-from-the-command-line/
# https://stackoverflow.com/questions/13322485/how-to-get-the-primary-ip-address-of-the-local-machine-on-linux-and-os-x/25851186#25851186
