# there are two ways of interacting with dotfiles repo: a `dotfiles` wrapper
# that runs git with desired parameters (advantage: being explicit), and
# functions that change the environment in the shell (advantage: git
# integration with the prompt will work).

if [ -z "$DOTFILES_HOME" ]; then
    echo "ERROR: dotfiles repository isn't configured correctly -"
    echo "       DOTFILES_HOME is undefined. This variable should"
    echo "       contain the path to dotfiles git-dir."
fi

# Alternative #1: `dotfiles` command
dotfiles() {
    set -u
    git --work-tree="$HOME" --git-dir="$DOTFILES_HOME" "$@"
    set +u
}

# https://stackoverflow.com/questions/9910966/how-to-get-shell-to-self-detect-using-zsh-or-bash/64036008#64036008
source_config() {
    if [ ! -z ${ZSH_VERSION+x} ]; then
      . "$HOME/.zprofile"
    elif [ ! -z ${BASH_VERSION+x} ]; then
      . "$HOME/.bashrc"  # refresh aliases such as g=git to include the safeguard
    else
      echo "not recognized"
    fi
}

# Alternative #2: environment modification
don() {
    # commenting out `set -u` and `set +u` because of this error
    # nvm: parameter not set
    # after setting https://github.com/nvm-sh/nvm#zsh

    #set -u
    GIT_BIN=`which git`
    # add safeguard against git clean
    git() {
        $GIT_BIN "$@"
    }
    pushd ~ 1>/dev/null  # remember location
    export GIT_DIR="$DOTFILES_HOME"; export GIT_WORK_TREE="$HOME"
    export GIT_PS1_FMT=" (dotfiles: %s)"

    # ripgrep does not read core.excludesFile from git repo
    # https://github.com/BurntSushi/ripgrep/issues/1014#issuecomment-625776121
    alias rg='rg --ignore-file .dotfiles.gitignore'

    #set +u

    source_config
}

dof() {
    unset -f git
    source_config

    unset GIT_DIR; unset GIT_WORK_TREE
    unset GIT_PS1_FMT
    popd 1>/dev/null  # restore previous location
    unalias rg
}
