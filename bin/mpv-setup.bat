cd %USERPROFILE%\personal_projects\
git clone https://github.com/open-dynaMIX/simple-mpv-webui
mklink /D %USERPROFILE%\.config\mpv\scripts\webui-page %USERPROFILE%\personal_projects\vendor\simple-mpv-webui\webui-page
mklink %USERPROFILE%\.config\mpv\scripts\webui.lua %USERPROFILE%\personal_projects\vendor\simple-mpv-webui\webui.lua

REM https://superuser.com/questions/167076/how-can-i-delete-a-symbolic-link/1373416#1373416
