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

## Depenendcies

`git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm`

## Packages


```bash
sudo apt-add-repository ppa:git-core/ppa
sudo apt install -y build-essential git curl tig fzf
cargo install ripgrep fd-find tokei
```

## Map capslock to escape

`~/bin/maps_capslock_to_escape.md`

## Firefox

`ln -s ~/.mozilla/firefox/shared/chrome ~/.mozilla/firefox/your_profile/`
