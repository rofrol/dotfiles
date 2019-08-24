#!/bin/bash

if [ $# -eq 1 ]; then
    dir="$1"
else
    dir="$PWD"
fi

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"

find "$dir" -type f -print0 | xargs -0 -L1 -i bash -c "$SCRIPT_DIR/if_file_video.sh \"{}\"" | tr '\n' '\0'| xargs -0 -L1 -i du "{}" | sort -r -h | awk '{for (i = 2 ; i <= NF ; i++) printf "%s ", $i; printf "\n"}' | mpv --playlist -

# https://stackoverflow.com/questions/18909084/if-else-in-xargs/18909242#18909242
# https://askubuntu.com/questions/844711/how-can-i-find-all-video-files-on-my-system/844720#844720
# https://stackoverflow.com/questions/12137431/test-if-a-command-outputs-an-empty-string#12137504
# https://stackoverflow.com/questions/199266/make-xargs-execute-the-command-once-for-each-line-of-input/28806991#28806991
# https://stackoverflow.com/questions/1447809/awk-print-9-the-last-ls-l-column-including-any-spaces-in-the-file-name/1447818#1447818
# https://stackoverflow.com/questions/6482377/check-existence-of-input-argument-in-a-bash-shell-script/6482403#6482403
# https://stackoverflow.com/questions/59895/get-the-source-directory-of-a-bash-script-from-within-the-script-itself/35374073#35374073
# https://www.commandlinefu.com/commands/view/22244/find-files-and-play-them-as-a-playing-list-in-mpv
