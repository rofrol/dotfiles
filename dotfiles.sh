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
    if [[ "$@" == *clean* ]]; then
        echo "NEVER USE 'git clean' on the dotfiles repository!"
        echo "It would delete data from your HOME directory."
    else
        git --work-tree="$HOME" --git-dir="$DOTFILES_HOME" "$@"
    fi
    set +u
}

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
        if [[ "$@" == *clean* ]]; then
            echo "NEVER USE 'git clean' on the dotfiles repository!"
            echo "It would delete data from your HOME directory."
        else
            $GIT_BIN "$@"
        fi
    }
    pushd ~ 1>/dev/null  # remember location
    export GIT_DIR="$DOTFILES_HOME"; export GIT_WORK_TREE="$HOME"
    export GIT_PS1_FMT=" (dotfiles: %s)"

    #set +u

    source_config
}

dof() {
    unset -f git
    source_config

    unset GIT_DIR; unset GIT_WORK_TREE
    unset GIT_PS1_FMT
    popd 1>/dev/null  # restore previous location
}
