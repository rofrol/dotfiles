#!/bin/bash
# Recompute the tmux tab bar: equal-width tabs filling the bar, and the
# session-name segment on the right at a fixed width with an ellipsis and an
# OSC 8 tooltip carrying the whole name.
#
# tmux 3.8 formats cannot compute widths (#{pN:...} takes only a literal or a
# variable value, #{s/.../.../:} does not exist yet), so this script measures
# the session and writes the widths into the formats literally. It is run by
# hooks in ~/.tmux.conf whenever the client size, the window count or the
# session name changes.
#
# Layout: leftover columns go to status-left as spaces, so every tab is the
# same width and the bar is filled exactly: extra + n*tab_w + right_w = width.
set -euo pipefail

NAME_W=24   # fixed width of the session-name field (ellipsis beyond that)
RIGHT_W=27  # arrow + space + NAME_W + space

command -v tmux >/dev/null || exit 0

read -r W N NAME <<EOF
$(tmux display-message -p '#{client_width} #{session_windows} #{session_name}')
EOF
[ "${W:-0}" -gt 0 ] && [ "${N:-0}" -gt 0 ] || exit 0

spare=$((W - RIGHT_W))
tab_w=$((spare / N))
title_w=$((tab_w - 3)) # space + title field + space + arrow per tab
[ "$title_w" -lt 6 ] && title_w=6
tab_w=$((title_w + 3))
extra=$((spare - N * tab_w))
[ "$extra" -lt 0 ] && extra=0

title='#{?#{m:π*,#T},#T,#W}'
field="#{=/$((title_w - 1))/…;p${title_w}:${title}}"
tmux set -g status-left "#{R: ,${extra}}"
tmux set -g window-status-format \
  "#[link=tmux://#{window_id},fg=#bcbcbc,bg=#d0d0d0]#[fg=#080808,bg=#d0d0d0] ${field} #[fg=#d0d0d0,bg=#bcbcbc]#[nolink]"
tmux set -g window-status-current-format \
  "#[link=tmux://#{window_id},fg=#bcbcbc,bg=#ffffff]#[fg=#080808,bg=#ffffff]#[bold] ${field} #[fg=#ffffff,bg=#bcbcbc]#[nolink]"

# The session segment's link carries the full name so a hyperlink-aware
# terminal shows it on hover. tmux drops a URI containing a raw space and eats
# a bare '%' in a format, but a user option is substituted after expansion, so
# percent-encoding here reaches the terminal untouched.
uri_name=${NAME//%/%25}
uri_name=${uri_name// /%20}
tmux set -g @sess_uri "tmux://session?${uri_name}"

# The session segment: arrow, space, a NAME_W-column name field, space. The
# name is cut with an ellipsis past NAME_W-2 and padded to the right, so the
# field is NAME_W columns whatever the name is (pN pads on the right, p-N on
# the left, and #S does not expand inside =/N/... - use session_name).
field="#{p${NAME_W}:#{=/$((NAME_W - 2))/…:session_name}}"
tmux set -g status-right-length 60
tmux set -g status-right \
  "#[link=#{@sess_uri}]#[fg=#bcbcbc,bg=#d0d0d0]#[fg=#080808,bg=#d0d0d0] ${field} #[nolink]"
