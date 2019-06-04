#!/bin/bash

#run it like this: tmux set-hook -g client-resized 'run-shell "bash ~/bin/tmux_toggle.sh"'

# https://unix.stackexchange.com/questions/186390/tmux-get-number-of-panes-in-the-current-window-in-bash-variable
# https://superuser.com/questions/586835/how-do-i-shove-a-pane-to-the-left-in-tmux/707083#707083
# https://stackoverflow.com/questions/35016458/how-to-write-if-statement-in-tmux-conf-to-set-different-options-for-different-t/36953319#36953319
# https://devel.tech/tips/n/tMuXrSz9/resize-tmux-main-panes-by-percentage/

half_the_width=170

highest=$(tmux display-message -p '#{window_panes}')
highest_left=$(tmux display -p -t :1.${highest} '#{pane_left}')
highest_width=$(tmux display -p -t :1.${highest} '#{pane_width}')
previous_width=$(tmux display -p -t :1.$((highest-1)) '#{pane_width}')
both_width=$((highest_width + previous_width))

if [[ $highest_left -gt 0 && $both_width -lt $half_the_width ]] ; then
    tmux move-pane -v -s ${highest} -t $((highest-1))
    tmux resize-pane -t $((highest-2)) -U 10
    tmux resize-pane -t ${highest} -y 3
elif [[ $highest_left -eq 0 && $previous_width -gt $half_the_width ]] ; then
    tmux move-pane -h -s ${highest} -t $((highest-1))
    tmux resize-pane -t ${highest} -y 10
fi
