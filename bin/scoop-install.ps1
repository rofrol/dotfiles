# using ffmpeg-nightly-shared-vulkan
# https://github.com/ScoopInstaller/Main/issues/1699
#
# 'windows-terminal' suggests installing 'extras/vcredist2019'
#
# Put shortcut to xmousebuttoncontrol in
# %USERPROFILE%\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup
#
# wezterm required Ubuntu Mono font https://design.ubuntu.com/font/
#
# scoop bucket add extras
# scoop bucket add games
# To generate list of apps
# test:
# "someapp 2.32 [bucket1]`nsomegood._- 1.23 [bucket2]" -split "`n" | sls '^(.+) .+ \[(.+)\]$' |% { "$($_.matches.groups[2])/$($_.matches.groups[1])" }
# (scoop export) | sls '^([\w\._-]+) .* \[(.*)\]$' |% sls '^(.+) .+ \[(.+)\]$' |% { "$($_.matches.groups[2])/$($_.matches.groups[1])" } > scoop-apps.txt
# https://github.com/lukesampson/scoop/issues/1543#issuecomment-821101351

$apps = Get-Content scoop-apps.txt
scoop install @apps
