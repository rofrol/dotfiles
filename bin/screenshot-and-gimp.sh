#!/bin/bash
# - https://askubuntu.com/questions/758738/can-i-take-a-screenshot-and-directly-open-it-in-gimp/758752#758752
# - Safe for urls https://www.reddit.com/r/ISO8601/comments/d6mdaz/comment/f0v4y9l/
# - `Gnome Settings > Keyboard Shortcuts > + and the bottom` https://askubuntu.com/questions/194427/what-is-the-terminal-command-to-take-a-screenshot/1295826#1295826
# - `Gimp > Edit > Keyboard Shortcuts > Ovewerite > Alt+Shift+e` https://askubuntu.com/questions/332994/gimp-2-8-doesnt-allow-to-save-usual-image-file-formats-jpg-png-via-save-dial
# - or `flameshot gui` https://askubuntu.com/questions/1036473/how-to-change-screenshot-application-to-flameshot-on-ubuntu-18-04/1039949#1039949 

path=~/Pictures/Screenshot_$(date +"%Y-%m-%dT%H-%M-%S%z").png
gnome-screenshot -a -f "$path"
gimp "$path"
