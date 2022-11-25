#!/bin/bash

# https://glow.li/posts/run-an-ssh-server-on-your-android-with-termux/
#$ brew install android-platform-tools
#$ adb devices
#List of devices attached
#8cff8552	unauthorized
# Popup on Android: allow for debug...

adb devices
# List of devices attached
# 8cff8552	device

# on Android: sshd
# check with: ssh localhost -p 8022
adb forward tcp:8022 tcp:8022

# brew install rsync
# https://stackoverflow.com/questions/30842005/upgrading-rsync-on-os-x-using-homebrew#comment131643782_52694461
# add to ~/.zprofile: export PATH=$PATH:/opt/homebrew/bin/
time rsync -xWrctzv --info=progress2 -e 'ssh -p 8022' -azv localhost:~/storage/shared/Download . 2>rsync_errors_reverse.txt
