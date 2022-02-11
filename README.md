Run `don` and `dof` to enable/disable git repo here.

Architecture based on https://github.com/jan-warchol/dotfiles

After cloning repository:

```bash
# git clone <your dotfiles repo>
cp -r dotfiles/.git ~/.dotfiles.git
source dotfiles/dotfiles.sh
export DOTFILES_HOME=$HOME/.dotfiles.git
don
# .gitignore is read by ripgrep and fd-find in Ubuntu WSL2 and git bash, so I need to use different file name
git config core.excludesFile .dotfiles.gitignore
git restore .
dof
rm -rf dotfiles
```

## Ubuntu packages

```bash
sudo apt-add-repository ppa:git-core/ppa
sudo apt install -y build-essential gitk curl tig fzf libssl-dev
```

## Rust

Install Rust from rustup.rs/

On Ubuntu you need `sudo apt install -y build-essential`. On Windows `npm i -g windows-build-tools`.

```
cargo install ripgrep fd-find tokei cargo-watch cargo-edit watchexec
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
scoop cleanup *
scoop cache rm *
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
- https://rasa.github.io/scoop-directory/by-stars

Shortcuts in `%USERPROFILE%\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Scoop Apps`

### scoop does not install newest version

```
$ cd ~/scoop/buckets/main/
$ git status
# it showed that `bucket/proxychains.json` has local modifications but I could not get rid of them with `git checkout -f`
$ git fetch
$ git reset --hard origin/master
```

- https://gist.github.com/573/e806447bf55a09376cf457a8a403ec44
  - https://github.com/lukesampson/scoop/issues/3045#issuecomment-493345130

### mpv

mpv from scoop does not read `%APPDATA%\mpv`. Portable one reads it.

Create env MPV_HOME

```
setx MPV_HOME %USERPROFILE\.confing\mpv
```

https://github.com/mpv-player/mpv/blob/master/DOCS/man/mpv.rst#files-on-windows

#### simple-mpv-webui

```bash
bash bin/mpv-lua-setup.sh
```

run `bin/mpv-setup.bat` as Administrator.

#### Playlist script

```bash
curl --create-dirs -so ~/scoop/apps/mpv/current/lua/json.lua https://raw.githubusercontent.com/craigmj/json4lua/master/json/json.lua
```

- https://github.com/57op/simple-mpv-webui-windows-libs/issues/2

#### Thumbnails

Maybe add thumbnails with icaros https://www.majorgeeks.com/files/details/icaros.html ?

### wezterm

For wezterm use this `%USERPROFILE%\scoop\apps\wezterm\current\wezterm-gui.exe` instead of wezterm.exe. Othwerwise there will be problems.

Install UbuntuMono-R.ttf from https://design.ubuntu.com/font/

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

### vscode and git bash

```
    "terminal.integrated.shell.windows": "${env:USERPROFILE}\\scoop\\apps\\git\\current\\bin\\bash.exe"
```

### Inverse mouse scroll with x mouse button control

Add shortcut to `%USERPROFILE%\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup`

## Map capslock to escape

`sudo sh ~/bin/maps_capslock_to_escape.sh`

## Firefox

`ln -s ~/.mozilla/firefox/shared/chrome ~/.mozilla/firefox/your_profile/`

on Windows run cmd.exe as Administrator and:

`mklink /d C:\Users\user\AppData\Roaming\Mozilla\Firefox\Profiles\your-profile\chrome C:\Users\user\.mozilla\firefox\shared\chrome`

In Firefox run `about:config` and set

`toolkit.legacyUserProfileCustomizations.stylesheets` to `true`

- https://www.userchrome.org/how-create-userchrome-css.html

## tmux

`git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm`

## rust

Install from https://rustup.rs/

### error: linking with `link.exe` failed: exit code: 3221225781

#42744

Start PowerShell as Administrator and run:

`npm install --global windows-build-tools`

- https://github.com/rust-lang/rust/issues/42744#issuecomment-309387002
- https://github.com/felixrieseberg/windows-build-tools

### Packages

`cargo install ripgrep fd-find tokei cargo-edit`

## Emacs

On Windows10 you need to set user env `HOME` pointing `%USERPROFILE%` for emacs to read `~/.config/emacs/init.el`. Otherwise it will read from `~/AppData/Roaming/.emacs.d`.
