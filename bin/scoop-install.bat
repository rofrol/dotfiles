REM using ffmpeg-nightly-shared-vulkan
REM https://github.com/ScoopInstaller/Main/issues/1699
REM
REM 'windows-terminal' suggests installing 'extras/vcredist2019'
REM
REM Put shortcut to xmousebuttoncontrol in
REM %USERPROFILE%\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup

REM wezterm required Ubuntu Mono font https://design.ubuntu.com/font/

REM scoop bucket add extras
REM scoop bucket add games

scoop install fzf neovim wezterm mpv 7zip git jq nodejs windirstat xmousebuttoncontrol sumatrapdf subtitleedit sharpkeys autohotkey gimp inkscape libreoffice-stable vcredist2015 calibre-normal obs-studio mkvtoolnix games/nbtexplorer vlc windows-terminal qnapi ffmpeg-nightly-shared-vulkan extras/vcredist2019 extras/firefox extras/vscode
