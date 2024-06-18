if [ -n "${2}" ]; then
	echo "datch: creating socket at /tmp/dtach_${1}"
	dtach -A /tmp/dtach_${1} -e '^A' ${2}
elif [ -n "${1}" ]; then
	echo "dtach: attaching to socket /tmp/dtach_${1}"
	dtach -a /tmp/dtach_${1} -e '^A'
else
	echo "dtach: list"
	ls /tmp/dtach_*
fi
