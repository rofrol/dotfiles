#!/bin/bash

DIR=~/personal_projects/emacs/vendor/rexim--dotfiles
DIRTRG=~/personal_projects/emacs/vendor/rexim-emacs
mv ~/.config/emacs ~/.config/emacs.good
ln -sfn $DIR/.emacs ~/
ln -sfn $DIR/.emacs.rc ~/
ln -sfn $DIR/.emacs.local ~/
ln -sfn $DIR/.emacs.snippets ~/
mv ~/.emacs.d.bak ~/.emacs.d
