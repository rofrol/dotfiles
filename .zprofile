echo '~/.zprofile sourced.'

# What Goes to ~/.zprofile and What Goes to ~/.zshrc?
# Since ~/.zprofile is loaded only once at login time, it’s best to put things that are loaded only once and can be inherited by subshells. An excellent example of this is environment variables.

# On the other hand, ~/.zshrc is typically reserved for things that are not inheritable by subshells, such as aliases and functions, custom prompts, history customizations, and so on.

# In addition, put the commands that should run every time you launch a new shell in the .zshrc file, too.
# https://web.archive.org/web/20210809001318/https://www.zerotohero.dev/zshell-startup-files/

# WARNING: this should be set in ~/.zprofile, not in ~/.zshrc.
# When set in ~/.zshrc, it will be run twice or more.
# Then `brew shellenv` will return empty output,
# unless you do first `unset HOMEBREW_SHELLENV_PREFIX`
# https://github.com/Homebrew/brew/issues/11851
# https://github.com/orgs/Homebrew/discussions/2547
# but still some packages are keg-only like curl,
# and needs to be added to path manually like:
# export PATH="$(brew --prefix curl)/bin/":$PATH
# to find keg-only packages:
# for i in $(brew leaves --installed-on-request); do brew info $i 2>/dev/null | head -1 | grep "keg-only"; done
# unless it is installed as dependency, like `brew uses --installed curl` shows php
# in that case do `brew uninstall --ignore-dependencies curl && brew install curl`
eval "$(/opt/homebrew/bin/brew shellenv)"

# https://apple.stackexchange.com/questions/337320/how-to-get-rid-of-application-downloaded-from-the-internet-message-when-instal/376476#376476
export HOMEBREW_CASK_OPTS=--no-quarantine

export DOTFILES_HOME=$HOME/.dotfiles.git
source ~/dotfiles.sh
