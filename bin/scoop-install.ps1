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
# (scoop export) | sls '^([\w-]+) .* \[(.*)\]$' |% { "$($_.matches.groups[2])/$($_.matches.groups[1])" } > scoop-apps.txt
# https://github.com/lukesampson/scoop/issues/1543#issuecomment-716514613

$apps = gc scoop-apps.txt
scoop install @apps
