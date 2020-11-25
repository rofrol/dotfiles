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

On Windows run cmd.exe as Administrator. Then

```
mklink /D %APPDATA%\mpv %USERPROFILE\.config\mpv
```

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

## Windows packages

```
scoop install fzf neovim wezterm mpv 7zip git jq nodejs
scoop update \*
scoop list
```

- https://github.com/lukesampson/scoop#installation
- https://github.com/lukesampson/scoop/issues/897#issuecomment-391909564

Shortcuts in `%USERPROFILE%\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Scoop Apps`

mpv from scoop does not read ~/.config/mpv. Portable one reads it.

```
mklink /D %USERPROFILE%\scoop\apps\mpv\current\portable_config %USERPROFILE%\.config\mpv
```

For wezterm use this `%USERPROFILE%\scoop\apps\wezterm\current\wezterm-gui.exe` instead of wezterm.exe. Othwerwise there will be problems.

## Map capslock to escape

`sudo ~/bin/maps_capslock_to_escape.sh`

## Firefox

`ln -s ~/.mozilla/firefox/shared/chrome ~/.mozilla/firefox/your_profile/`

## Depenendcies

`git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm`

