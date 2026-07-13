# there are two ways of interacting with dotfiles repo: a `dotfiles` wrapper
# that runs git with desired parameters (advantage: being explicit), and
# functions that change the environment in the shell (advantage: git
# integration with the prompt will work).

if [ -z "$DOTFILES_HOME" ]; then
	echo "ERROR: dotfiles repository isn't configured correctly -"
	echo "       DOTFILES_HOME is undefined. This variable should"
	echo "       contain the path to dotfiles git-dir."
fi

# usage: d code .
d() {
	GIT_DIR="$DOTFILES_HOME" GIT_WORK_TREE="$HOME" "$@"
}

# https://stackoverflow.com/questions/9910966/how-to-get-shell-to-self-detect-using-zsh-or-bash/64036008#64036008
source_config() {
	if [ ! -z ${ZSH_VERSION+x} ]; then
		. "$HOME/.zprofile"
	elif [ ! -z ${BASH_VERSION+x} ]; then
		. "$HOME/.bashrc" # refresh aliases such as g=git to include the safeguard
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
	pushd ~ 1>/dev/null # remember location
	export GIT_DIR="$DOTFILES_HOME"
	export GIT_WORK_TREE="$HOME"
	export GIT_PS1_FMT=" (dotfiles: %s)"

	# ripgrep does not read core.excludesFile from git repo
	# https://github.com/BurntSushi/ripgrep/issues/1014#issuecomment-625776121
	alias rg='rg --ignore-file .dotfiles.gitignore'
	alias fd='fd --ignore-file .dotfiles.gitignore'

	#set +u
}

dof() {
	unset GIT_DIR
	unset GIT_WORK_TREE
	unset GIT_PS1_FMT
	popd 1>/dev/null # restore previous location
	unalias rg
	unalias fd
}

# disabling, because it messes with repos inside ~/personal_projects etc.
# changed to check for != "true"
# https://www.reddit.com/r/unixporn/comments/1bbi7qr/comment/mrfg1b9/
# git() {
# 	if [[ "$1" != clone && "$PWD" =~ "$HOME" && "$(command git rev-parse --is-inside-work-tree 2>/dev/null)" != "true" ]]; then
# 		command git --git-dir="$HOME/.dotfiles.git" --work-tree="$HOME" "$@"
# 	else
# 		command git "$@"
# 	fi
# }
