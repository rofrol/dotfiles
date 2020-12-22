rd /S /Q %USERPROFILE%\scoop\apps\mpv\current\portable_config
mklink /D %USERPROFILE%\scoop\apps\mpv\current\portable_config %USERPROFILE%\.config\mpv
cd %USERPROFILE%\personal_projects\
git clone https://github.com/open-dynaMIX/simple-mpv-webui
mklink /D %USERPROFILE%\.config\mpv\scripts\webui-page %USERPROFILE%\personal_projects\vendor\simple-mpv-webui\webui-page
mklink %USERPROFILE%\.config\mpv\scripts\webui.lua %USERPROFILE%\personal_projects\vendor\simple-mpv-webui\webui.lua
