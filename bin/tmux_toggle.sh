#!/bin/bash

# https://unix.stackexchange.com/questions/186390/tmux-get-number-of-panes-in-the-current-window-in-bash-variable
highest=$(tmux display-message -p '#{window_panes}')
if [[ $(tmux display -p -t :1.${highest} '#{pane_left}') -eq 0 ]] ; then
    tmux move-pane -h -s ${highest} -t $((highest-1))
    tmux resize-pane -t ${highest} -y 10
else
    tmux move-pane -v -s ${highest} -t $((highest-1))
    tmux resize-pane -t $((highest-2)) -U 10
    tmux resize-pane -t ${highest} -y 3
fi
