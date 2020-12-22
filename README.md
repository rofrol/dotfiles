Run `don` and `dof` to enable/disable git repo here.

Architecture based on https://github.com/jan-warchol/dotfiles

After cloning repository:

```bash
# .bashrc sets DOTFILES_HOME and sources don and dof functions
cd dotfiles
cp -r .config/* ~/.config/
cp .bashrc ~/
cp -r .git ~/.dotfiles.git
cd
source .bashrc
don
# .gitignore is read by ripgrep and fd-find in Ubuntu WSL2 and git bash, so I need to use different file name
git config core.excludesFile .dotfiles.gitignore
git checkout -f
dof
rm -rf dotfiles
```


## mpv standalone

On Windows run cmd.exe as Administrator. Then

```
mklink /D %APPDATA%\mpv %USERPROFILE\.config\mpv
```

for mpv from scoop, go to scoop section

## Ubuntu packages


```bash
sudo apt-add-repository ppa:git-core/ppa
sudo apt install -y build-essential gitk curl tig fzf libssl-dev
cargo install ripgrep fd-find tokei cargo-watch cargo-edit
```

### git

```bash
sudo add-apt-repository ppa:git-core/ppa
apt install git
```

https://git-scm.com/download/linux

### fzf

Install using git https://github.com/junegunn/fzf#using-git. Version from deb was to old for nvim integration script (0.20 vs 0.24).

## scoop

Scoop is a Windows package manager.


```cmd.exe
scoop bucket add extras
scoop bucket add games
bin\scoop-install.bat
scoop update *
scoop list
```

```bash
# in bash need to escape *
scoop update \*
```

- https://github.com/lukesampson/scoop#installation
- https://github.com/lukesampson/scoop/issues/897#issuecomment-391909564
- https://github.com/ScoopInstaller/Main/tree/master/bucket
- https://github.com/lukesampson/scoop-extras/tree/master/bucket
- https://github.com/lukesampson/scoop/wiki/Open-With-Icons

Shortcuts in `%USERPROFILE%\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Scoop Apps`

### mpv

mpv from scoop does not read `%APPDATA%\mpv`. Portable one reads it.

```
rd /S /Q %USERPROFILE%\scoop\apps\mpv\current\portable_config
mklink /D %USERPROFILE%\scoop\apps\mpv\current\portable_config %USERPROFILE%\.config\mpv
mklink /D %USERPROFILE%\.config\mpv\scripts\webui-page %USERPROFILE%\personal_projects\vendor\simple-mpv-webui\webui-page
mklink %USERPROFILE%\.config\mpv\scripts\webui.lua %USERPROFILE%\personal_projects\vendor\simple-mpv-webui\webui.lua
```

- https://superuser.com/questions/167076/how-can-i-delete-a-symbolic-link/1373416#1373416

in bash:

```bash
curl -so ~/scoop/apps/mpv/current/lua/json.lua https://raw.githubusercontent.com/craigmj/json4lua/master/json/json.lua
```

- https://github.com/57op/simple-mpv-webui-windows-libs/issues/2

Maybe add thumbnails with icaros https://www.majorgeeks.com/files/details/icaros.html ?

### wezterm

For wezterm use this `%USERPROFILE%\scoop\apps\wezterm\current\wezterm-gui.exe` instead of wezterm.exe. Othwerwise there will be problems.

### calibre

Instal calibre-normal instead of calibre. For calibre (which is portable version) there is problem.

Long paths needs to be enabled or it asks where to install. Otherwise scoop cannot create shim.

- https://github.com/lukesampson/scoop-extras/issues/1765#issuecomment-471170974
- https://github.com/lukesampson/scoop-extras/issues/2535

### vcredist2015

```
breoffice-stable' (7.0.1) was installed successfully!
'libreoffice-stable' suggests installing 'extras/vcredist2015'.
$ scoop install vcredist2015
Installing 'vcredist2015' (14.0.24215) [64bit]
vc_redist.x64.exe (14,6 MB) [=================================================================================================================] 100%
Checking hash of vc_redist.x64.exe ... ok.
vc_redist.x86.exe (13,8 MB) [=================================================================================================================] 100%
Checking hash of vc_redist.x86.exe ... ok.
Linking ~\scoop\apps\vcredist2015\current => ~\scoop\apps\vcredist2015\14.0.24215
Running post-install script...
ERROR Exit code was 1638!
ERROR Exit code was 1638!
'vcredist2015' (14.0.24215) was installed successfully!
Notes
-----
You can now remove this installer with 'scoop uninstall vcredist2015'
```

### authotkey

There are two versions:

- https://github.com/lukesampson/scoop-extras/blob/master/bucket/autohotkey.json
- https://github.com/lukesampson/scoop-extras/blob/master/bucket/autohotkey-installer.json
- 

## Map capslock to escape

`sudo ~/bin/maps_capslock_to_escape.sh`

## Firefox

`ln -s ~/.mozilla/firefox/shared/chrome ~/.mozilla/firefox/your_profile/`

## Depenendcies

`git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm`

