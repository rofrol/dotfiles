Dotfiles kept in a bare git repo (`$DOTFILES_HOME=~/personal_projects/dotfiles`) with `$HOME` as the work tree.

- `git` is shimmed by `~/scripts/dotfiles-shim/git` (put first in `PATH` by `~/.zshrc_dotfiles_mode`): in `$HOME` and in dirs not ignored by `.dotfiles.gitignore`, outside other repos, plain `git`, lazygit etc. operate on the dotfiles repo. `GIT_DIR` is not exported. Disable with `DOTFILES_AUTO=0`.
- `don` / `dof` from `dotfiles.sh` force / undo dotfiles mode in the current shell (export `GIT_DIR` and `GIT_WORK_TREE`).

Architecture based on <https://github.com/jan-warchol/dotfiles>

Another interesting approach <https://mitxela.com/projects/dotfiles_management>

## Bootstrap

macOS: install Homebrew and git first:

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
eval "$(/opt/homebrew/bin/brew shellenv)"
brew install git
export PATH="$(brew --prefix git)/bin/:$PATH"
```

Generate an SSH key and add `~/.ssh/id_ed25519.pub` to github.com > Settings > SSH and GPG keys > New SSH key:

```bash
ssh-keygen -t ed25519 -C "your_email@example.com"
```

Clone the repo (bash/zsh on macOS and Linux, Git Bash or WSL on Windows):

```bash
# Bare repo outside $HOME's discovery path; the work tree ($HOME) is attached
# only by the git shim and don (GIT_WORK_TREE=$HOME).
# Do not set core.worktree: then a plain `git` inside the repo dir would operate on $HOME.
mkdir -p ~/personal_projects
git clone --bare git@github.com:rofrol/dotfiles.git ~/personal_projects/dotfiles
export DOTFILES_HOME=$HOME/personal_projects/dotfiles
git --git-dir=$DOTFILES_HOME config remote.origin.fetch '+refs/heads/*:refs/remotes/origin/*'
git --git-dir=$DOTFILES_HOME fetch origin
git --git-dir=$DOTFILES_HOME branch --set-upstream-to=origin/master master
# .gitignore is read by ripgrep and fd-find in Ubuntu WSL2 and git bash, so I need to use different file name
# absolute path, so it also works when git runs from a cwd other than $HOME
git --git-dir=$DOTFILES_HOME config core.excludesFile "$HOME/.dotfiles.gitignore"
# fails on existing files instead of overwriting them; review, then add -f if intended
git --git-dir=$DOTFILES_HOME --work-tree=$HOME checkout
# new login shell: ~/.zprofile sets DOTFILES_HOME and sources dotfiles.sh,
# ~/.zshrc enables the git shim
exec zsh -l
```

## macOS

### Homebrew packages

Casks:

```shell
brew install --cask karabiner-elements alt-tab rectangle stats iina firefox google-chrome monitorcontrol
```

Formulas:

```shell
# coreutils for ls alias
# qpdf to decrypt and unprotect pdf files
# mkvtoolnix for mkvinfo and mkvextract
brew install git git-gui gh neovim ripgrep atuin fzf zsh-autosuggestions oh-my-posh zsh-git-prompt curl coreutils gnu-sed eza yt-dlp mpv qpdf mkvtoolnix alass ffmpeg
```

`atuin login`

### Neovim

```shell
git clone git@github.com:rofrol/LazyVim--starter.git ~/.config/nvim
```

mason in neovim needs npm:

```shell
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.40.3/install.sh | bash
. ~/.nvm/nvm.sh # or restart the shell
nvm install node
nvm alias default node
```

### lazygit

```shell
mkdir -p ~/Library/Application\ Support/lazygit
ln -s ~/.config/lazygit/config.yml ~/Library/Application\ Support/lazygit/config.yml
brew install diff-so-fancy
```

### nushell

`ln -s ~/.config/nushell ~/Library/Application\ Support/`

- <https://github.com/nushell/nushell/issues/10746>
- <https://github.com/nushell/nushell/issues/893>
- my answer <https://superuser.com/questions/1804643/how-do-i-change-the-default-location-for-nushell-configration-files/1827175#1827175>

In iTerm2 set `Preferences > Profiles > General > Command > Command` to `/opt/homebrew/bin/nu`

`brew install nushell starship`

Run in nushell:

```nu
mkdir ~/.cache/starship
starship init nu | save -f ~/.cache/starship/init.nu
```

## Common

### .ignore

I have added some directories like `/projects/` to `.ignore`, so that ripgrep or telescope in neovim do not search them.

A glob pattern starting with `/` is anchored to the directory containing the `.ignore` file (here `$HOME`), so it matches only that path, not `projects/` in subdirectories.

<https://stackoverflow.com/questions/64373137/ripgrep-to-only-exclude-a-file-in-the-root-of-the-folder/64389725#64389725>

### Rust

Install from <https://rustup.rs/>

On Ubuntu you need `sudo apt install -y build-essential`. On Windows install Visual Studio Build Tools with the "Desktop development with C++" workload.

```shell
cargo install ripgrep fd-find tokei cargo-watch cargo-edit watchexec-cli
```

### tmux

`git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm`

### Firefox

Link the shared `chrome` directory into your profile. Profile locations:

- macOS: `~/Library/Application Support/Firefox/Profiles/your-profile`
- Linux: `~/.mozilla/firefox/your-profile`

```shell
ln -s ~/.mozilla/firefox/shared/chrome ~/.mozilla/firefox/your-profile/
```

On Windows run cmd.exe as Administrator and:

`mklink /d C:\Users\user\AppData\Roaming\Mozilla\Firefox\Profiles\your-profile\chrome C:\Users\user\.mozilla\firefox\shared\chrome`

In Firefox run `about:config` and set

`toolkit.legacyUserProfileCustomizations.stylesheets` to `true`

- <https://www.userchrome.org/how-create-userchrome-css.html>

## Legacy: Ubuntu

```bash
sudo add-apt-repository ppa:git-core/ppa
sudo apt update
sudo apt install -y build-essential git gitk curl tig fzf libssl-dev
```

<https://git-scm.com/download/linux>

### fzf

Install using git <https://github.com/junegunn/fzf#using-git>. Version from deb was too old for nvim integration script (0.20 vs 0.24).

### Map capslock to escape

`sudo sh ~/bin/maps_capslock_to_escape.sh`

## Legacy: Windows

### scoop

Scoop is a Windows package manager. Install: <https://scoop.sh/>

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

- <https://github.com/ScoopInstaller/Scoop/issues/897#issuecomment-391909564>
- <https://github.com/ScoopInstaller/Main/tree/master/bucket>
- <https://github.com/ScoopInstaller/Extras/tree/master/bucket>
- <https://github.com/ScoopInstaller/Scoop/wiki/Open-With-Icons>
- <https://rasa.github.io/scoop-directory/by-stars>

Shortcuts in `%USERPROFILE%\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Scoop Apps`

#### scoop does not install newest version

```
$ cd ~/scoop/buckets/main/
$ git status
# it showed that `bucket/proxychains.json` has local modifications but I could not get rid of them with `git checkout -f`
$ git fetch
$ git reset --hard origin/master
```

- <https://gist.github.com/573/e806447bf55a09376cf457a8a403ec44>
  - <https://github.com/ScoopInstaller/Scoop/issues/3045#issuecomment-493345130>

### mpv

mpv from scoop does not read `%APPDATA%\mpv`. Portable one reads it.

Create env MPV_HOME (applies to newly started processes):

```cmd.exe
setx MPV_HOME "%USERPROFILE%\.config\mpv"
```

<https://github.com/mpv-player/mpv/blob/master/DOCS/man/mpv.rst#files-on-windows>

#### simple-mpv-webui

```bash
bash bin/mpv-lua-setup.sh
```

run `bin/mpv-setup.bat` as Administrator.

#### Playlist script

```bash
curl --create-dirs -so ~/scoop/apps/mpv/current/lua/json.lua https://raw.githubusercontent.com/craigmj/json4lua/master/json/json.lua
```

- <https://github.com/57op/simple-mpv-webui-windows-libs/issues/2>

#### Thumbnails

Maybe add thumbnails with icaros <https://www.majorgeeks.com/files/details/icaros.html> ?

### wezterm

For wezterm use this `%USERPROFILE%\scoop\apps\wezterm\current\wezterm-gui.exe` instead of wezterm.exe. Otherwise there will be problems.

Install UbuntuMono-R.ttf from <https://design.ubuntu.com/font/>

### calibre

Install calibre-normal instead of calibre. For calibre (which is portable version) there is problem.

Long paths need to be enabled or it asks where to install. Otherwise scoop cannot create shim.

- <https://github.com/ScoopInstaller/Extras/issues/1765#issuecomment-471170974>
- <https://github.com/ScoopInstaller/Extras/issues/2535>

### AutoHotkey

There are two versions:

- <https://github.com/ScoopInstaller/Extras/blob/master/bucket/autohotkey.json>
- <https://github.com/ScoopInstaller/Extras/blob/master/bucket/autohotkey-installer.json>

### vscode and git bash

```json
"terminal.integrated.profiles.windows": {
  "Git Bash": {
    "path": "${env:USERPROFILE}\\scoop\\apps\\git\\current\\bin\\bash.exe"
  }
},
"terminal.integrated.defaultProfile.windows": "Git Bash"
```

### Inverse mouse scroll with X-Mouse Button Control

Add shortcut to `%USERPROFILE%\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Startup`

### Rust: linking with `link.exe` failed: exit code: 3221225781

Install Visual Studio Build Tools with the "Desktop development with C++" workload (the old `windows-build-tools` npm package is deprecated).

- <https://github.com/rust-lang/rust/issues/42744#issuecomment-309387002>

### Emacs

On Windows 10 you need to set user env `HOME` pointing to `%USERPROFILE%` for Emacs to read `~/.config/emacs/init.el`. Otherwise it will read from `~/AppData/Roaming/.emacs.d`.
