#!/bin/bash

if [[ $(tmux display -p -t :1.3 '#{pane_left}') -eq 0 ]] ; then
    tmux move-pane -h -s 3 -t 2;
else
    tmux move-pane -v -s 3 -t 2;
fi
