[ -f ~/.bashrc_local ] && source ~/.bashrc_local
export DOTFILES_HOME=$HOME/.dotfiles.git
source ~/.config/bash/dotfiles.sh

# https://unix.stackexchange.com/questions/285924/how-to-compare-a-programs-version-in-a-shell-script/567537#567537
version_greater_equal()
{
    printf '%s\n%s\n' "$2" "$1" | sort --check=quiet --version-sort
}

[ ! -f $HOME/.gitconfig_local ] && echo "# Local config" >> $HOME/.gitconfig_local

if [ $(version_greater_equal "$(git version)" "git version 2.33") ]; then
   git config --file=$HOME/.gitconfig_local pull.twohead ort
fi
