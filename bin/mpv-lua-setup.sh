#!/bin/bash

cd ~/personal_projects/
git clone https://github.com/57op/simple-mpv-webui-windows-libs
source build.sh
cp -r ~/personal_projects/simple-mpv-webui-windows-libs/mpv ~/scoop/apps/mpv/current/
