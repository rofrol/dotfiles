source $HOME/.xdg.sh

[ -f ~/.bashrc_default_from_distribution ] && source ~/.bashrc_default_from_distribution
export DOTFILES_HOME=$HOME/.dotfiles.git
source ~/dotfiles.sh

# https://unix.stackexchange.com/questions/285924/how-to-compare-a-programs-version-in-a-shell-script/567537#567537
version_greater_equal()
{
    printf '%s\n%s\n' "$2" "$1" | sort --check=quiet --version-sort
}

[ ! -f $HOME/.gitconfig_local ] && echo "# Local config" >> $HOME/.gitconfig_local

if [ $(version_greater_equal "$(git version)" "git version 2.33") ]; then
   git config --file=$HOME/.gitconfig_local pull.twohead ort
fi

# https://github.com/git/git/blob/master/contrib/completion/git-prompt.sh
# https://www.quora.com/What-is-the-best-Bash-prompt-for-Git
if [ -f /etc/bash_completion.d/git-prompt ]; then
  source /etc/bash_completion.d/git-prompt
  GIT_PS1_SHOWDIRTYSTATE=true
  GIT_PS1_SHOWCOLORHINTS=true
  GIT_PS1_SHOWUNTRACKEDFILES=true
  #PS1='[\u@\h \W$(__git_ps1 " (%s)")]\$ '
  PS1='\w$(__git_ps1 " (%s)")\$ '
fi

alias gitka='gitk --all &'

export EDITOR=nvim

# xargs for trimming https://stackoverflow.com/questions/369758/how-to-trim-whitespace-from-a-bash-variable/12973694#12973694
# https://unix.stackexchange.com/questions/29981/how-can-i-tell-whether-a-build-is-debian-based/29985#29985
if [ "$(lsb_release -i | xargs)" = "Distributor ID: Debian" ]; then
  [ ! -f ~/.git-completion.bash ] && curl -s https://raw.githubusercontent.com/git/git/master/contrib/completion/git-completion.bash -o ~/.git-completion.bash
  # https://superuser.com/questions/1310317/why-does-debian-not-autocomplete-all-git-commands/1310326#1310326
  [ -f ~/.git-completion.bash ] && . ~/.git-completion.bash
fi

# https://github.com/Schniz/fnm
#export PATH=/home/roman/.fnm:$PATH
#eval "`fnm env`"

# for pip
export PATH=$PATH:~/.local/bin
export PATH=$PATH:~/bin

# https://stackoverflow.com/questions/4075287/node-express-eaddrinuse-address-already-in-use-kill-server/46276685#46276685
# https://stackoverflow.com/questions/11583562/how-to-kill-a-process-running-on-particular-port-in-linux/19060124#19060124
function killTcpListen () {
  kill -9 $(lsof -sTCP:LISTEN -t -i:$1)
}

alias dfo='df -x squashfs -x tmpfs -x devtmpfs -BM -H -T'
alias ncdumc='NCDU_SHELL="mc" ncdu'
alias ncdumpv='NCDU_SHELL="mpv_playlist.sh" ncdu'
# https://unix.stackexchange.com/questions/25327/watch-command-alias-expansion
alias watch='watch '

# https://nixos.org/guides/declarative-and-reproducible-developer-environments.html#declarative-reproducible-envs
#eval "$(direnv hook bash)"

#source ~/.config/bash/update_history

# Should be last
[ -f ~/.bashrc_local ] && source ~/.bashrc_local

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
. "$HOME/.cargo/env"
