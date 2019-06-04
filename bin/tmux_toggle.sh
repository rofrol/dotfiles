#!/bin/bash

# https://unix.stackexchange.com/questions/186390/tmux-get-number-of-panes-in-the-current-window-in-bash-variable
highest_pane_number=$(tmux display-message -p '#{window_panes}')
previous_pane_number=$((highest_pane_number-1))
if [[ $(tmux display -p -t :1.${highest_pane_number} '#{pane_left}') -eq 0 ]] ; then
    tmux move-pane -h -s ${highest_pane_number} -t ${previous_pane_number};
else
    tmux move-pane -v -s ${highest_pane_number} -t ${previous_pane_number};
fi
