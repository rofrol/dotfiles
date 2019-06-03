#!/bin/bash

ln -sf $PWD/bin/ ~/
ln -sf $PWD/.bashrc ~/
ln -sf $PWD/sync-history.sh ~/
ln -sf $PWD/.tmux.conf ~/
mkdir -p ~/.config/mpv/
ln -sf $PWD/.config/mpv/* ~/.config/mpv/
