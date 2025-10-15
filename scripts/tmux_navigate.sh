#!/bin/bash

# Based on https://github.com/tmux/tmux/issues/1158#issuecomment-344380259
case "$1" in
    "-U")
	(tmux display-message -p '#{pane_current_command}' | grep -iqE '(^|\/)g?(view|emacs?)(diff)?$' && tmux send-keys S-up) || if [ $(tmux display-message -p '#{pane_at_top}') -ne 1 ]; then tmux select-pane -U; fi
	;;
    "-D")
	(tmux display-message -p '#{pane_current_command}' | grep -iqE '(^|\/)g?(view|emacs?)(diff)?$' && tmux send-keys S-down) || if [ $(tmux display-message -p '#{pane_at_bottom}') -ne 1 ] ; then tmux select-pane -D; fi
	;;
    "-R")
	(tmux display-message -p '#{pane_current_command}' | grep -iqE '(^|\/)g?(view|emacs?)(diff)?$' && tmux send-keys S-right) || if [ $(tmux display-message -p '#{pane_at_right}') -ne 1 ]; then tmux select-pane -R; fi;
	;;
    "-L")
	(tmux display-message -p '#{pane_current_command}' | grep -iqE '(^|\/)g?(view|emacs?)(diff)?$' && tmux send-keys S-left) || if [ $(tmux display-message -p '#{pane_at_left}') -ne 1 ]; then tmux select-pane -L; fi;
	;;
esac
