eval "$(/opt/homebrew/bin/brew shellenv)"
export HOMEBREW_CASK_OPTS=--no-quarantine
# brew cleanup doesn't clean ~/Library/Caches/Homebrew https://github.com/Homebrew/brew/issues/3784
alias brew-update='brew update && brew upgrade && brew upgrade --cask --greedy && brew cleanup -s && rm -rf $(brew --cache)'
alias brew-packages='{brew leaves --installed-on-request & brew list --cask -1;} | sort | uniq'


export PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"

export NVM_DIR="$HOME/.nvm"
[ -s "$(brew --prefix nvm)/nvm.sh" ] && \. "$(brew --prefix nvm)//nvm.sh"  # This loads nvm
[ -s "$(brew --prefix nvm)/etc/bash_completion.d/nvm" ] && \. "$(brew --prefix nvm)//etc/bash_completion.d/nvm"  # This loads nvm bash_completion

export DOTFILES_HOME=$HOME/.dotfiles.git
source $HOME/dotfiles.sh

export PATH=$PATH:~/bin

#export PATH=$PATH:~/.zvm/bin
#export PATH=$PATH:$HOME/.local/zig/current
#export PATH=$PATH:$HOME/personal_projects/zig/vendor/zig/stage3/bin/
export PATH=$PATH:~/bin/zig

alias ziglings='watchexec -w exercises -i zig-cache -e zig zig build'

alias gitka='gitk --all &'

export PATH="$(brew --prefix python)/libexec/bin":$PATH

export PATH="$(brew --prefix keydb)/bin":$PATH

export EDITOR=nvim

# https://apple.stackexchange.com/questions/88515/how-do-i-edit-current-shell-command-in-vi/443515#443515
# https://nuclearsquid.com/writings/edit-long-commands/
# Enable Ctrl-x-e to edit command line
autoload -U edit-command-line
# Emacs style
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

export PATH=~/.bun/bin:$PATH

# Load Angular CLI autocompletion.
#source <(ng completion script)


#eval "$(direnv hook zsh)"

# somehow ng needs to be before load-nvmrc
# also errors with pushd

# https://github.com/nvm-sh/nvm#zsh
# https://stackoverflow.com/questions/23556330/run-nvm-use-automatically-every-time-theres-a-nvmrc-file-on-the-directory/39519460#39519460
# https://github.com/daliusd/cfg/blob/69828995023cd0d1ccac0e42b13419428dca7766/.bash_private#L53
# https://blog.ffff.lt/posts/fnm-on-cd/
# place this after nvm initialization!
autoload -U add-zsh-hook
load-nvmrc() {
  local node_version="$(nvm version)"
  local nvmrc_path="$(nvm_find_nvmrc)"

  if [ -n "$nvmrc_path" ]; then
    local nvmrc_node_version=$(nvm version "$(cat "${nvmrc_path}")")

    if [ "$nvmrc_node_version" = "N/A" ]; then
      nvm install
    elif [ "$nvmrc_node_version" != "$node_version" ]; then
      nvm use
    fi
  elif [ "$node_version" != "$(nvm version default)" ]; then
    echo "Reverting to nvm default version"
    nvm use default
  fi
}
add-zsh-hook chpwd load-nvmrc
load-nvmrc

export PATH=~/zls:$PATH

# https://stackoverflow.com/questions/57972341/how-to-install-and-use-gnu-ls-on-macos
alias ls="/opt/homebrew/opt/coreutils/libexec/gnubin/ls"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/homebrew/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/homebrew/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/opt/homebrew/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/opt/homebrew/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"

export PATH=$PATH:$HOME/.local/zig/current
export PATH=$PATH:$HOME/zls
export PATH=$PATH:~/.pyenv/shims/yt-dlp
export PATH=$PATH:/opt/homebrew/bin/
export PATH=$PATH:~/personal_projects/zig/vendor/sm2/zig-out/bin/

# https://blog.remibergsma.com/2012/07/10/setting-locales-correctly-on-mac-osx-terminal-application/
export LC_MESSAGES=en_US.UTF-8

export PATH=$PATH:~/bin/roc

source "$HOME/.cargo/env"

source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh
source /Users/rfrolow/.config/broot/launcher/bash/br

