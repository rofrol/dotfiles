Run `don` and `dof` to enable/disable git repo here.

Architecture based on https://github.com/jan-warchol/dotfiles

After cloning repository:

```bash
# .bashrc sets DOTFILES_HOME and sources don and dof functions
$ mv -r dotfiles/.config dotfiles/.bashrc ~/
$ mv dotfiles/.git ~/.dotfiles.git
$ cd
$ source dotfiles/.bashrc
$ don
# .gitignore is read by ripgrep and fd-find in Ubuntu WSL2 and git bash, so I need to use different file name
$ git config core.excludesFile .dotfiles.gitignore
$ git checkout -f
$ dof
$ rm -rf dotfiles
```

On Windows run cmd.exe as Administrator. Then

```
mklink /D %APPDATA%\mpv %USERPROFILE\.config\mpv
```
