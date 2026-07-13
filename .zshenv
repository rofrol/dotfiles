# Automator runs scripts in non-login and non-interactive mode. So env variable should be put in .zshenv
# for homebrew add this to .zshenv:
if [[ -x /opt/homebrew/bin/brew ]]; then
	eval "$(/opt/homebrew/bin/brew shellenv)"
fi

# Paths are evaluated in the order given. One should also be weary that listing too many uncommon special-case paths before commonly used ones like /bin and /usr/bin could have a performance impact (especially if network file systems are involved). I only prepend paths if I really need to override existing commands, which is quite uncommon https://stackoverflow.com/questions/25235357/path-at-end-or-beginning-of-path-export-in-bash-profile-for-git-on-mac#comment39313448_25235487

export PATH="$(brew --prefix postgresql@18)/bin:$PATH"
export PATH="$HOME/scripts:$PATH"
export PATH="$(brew --prefix rsync)/bin:$PATH"
export PATH=$HOME/bin/tsMuxeR:$PATH
# cjxl
export PATH=$(brew --prefix jpeg-xl)/bin:$PATH

export PATH=$HOME/bin:$PATH
export PATH=$HOME/.local/pipx/venvs/openai-whisper/bin:$PATH

# pipx
export PATH=$HOME/.local/bin:$PATH

export PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"

export PATH=$PATH:~/bin

# export PATH="$(brew --prefix python)/libexec/bin":$PATH

export PATH="$(brew --prefix keydb)/bin":$PATH

export PATH=$HOME/.bun/bin:$PATH

# Odin

export PATH=$HOME/personal_projects/odin/vendor/ols:$PATH
# export PATH=$HOME/bin/odin:$PATH

export PATH=$PATH:/opt/homebrew/bin

export PATH=$PATH:~/.local/share/npm/bin

export PATH="$HOME/personal_projects/docker/vendor/regclient/bin/:$PATH"

export PATH="$(brew --prefix git)/bin:$PATH"

# Added by LM Studio CLI (lms)
export PATH="$PATH:/Users/roman.frolow/.lmstudio/bin"

export PATH="/opt/homebrew/opt/postgresql@17/bin:$PATH"
export PATH="/opt/homebrew/opt/ruby/bin:$PATH"
export PATH=$HOME/.cargo/bin:$PATH
export PATH="$HOMEBREW_PREFIX/opt/gnu-sed/libexec/gnubin:$PATH"

export PATH=$HOME/.opencode/bin:$PATH

export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
export PATH=$HOME/personal_projects/fix_polish_characters:$PATH

export PATH="$PATH:$HOME/.local/bin"

export PATH="$HOME/.antigravity-ide/antigravity-ide/bin:$PATH"

# https://blog.remibergsma.com/2012/07/10/setting-locales-correctly-on-mac-osx-terminal-application/
export LC_MESSAGES=en_US.UTF-8

# https://unix.stackexchange.com/questions/275728/set-ls-l-time-format/693168#693168
export TIME_STYLE=long-iso

export EDITOR=ki
echo '~/.zshenv sourced.'
