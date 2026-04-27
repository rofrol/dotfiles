# Automator runs scripts in non-login and non-interactive mode. So env variable should be put in .zshenv
# for homebrew add this to .zshenv:
if [[ -x /opt/homebrew/bin/brew ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
fi

export PATH="$(brew --prefix postgresql@18)/bin:$PATH"
export PATH="$HOME/scripts:$PATH"
export PATH="$(brew --prefix rsync)/bin:$PATH"

echo '~/.zshenv sourced.'