#!/bin/bash

mv ~/.config/emacs.good ~/.config/emacs
rm ~/.emacs
rm ~/.emacs.rc
rm ~/.emacs.local
rm ~/.emacs.snippets
mv ~/.emacs.d ~/.emacs.d.bak
