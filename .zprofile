eval "$(/opt/homebrew/bin/brew shellenv)"

export PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"

export NVM_DIR="$HOME/.nvm"
[ -s "/opt/homebrew/opt/nvm/nvm.sh" ] && \. "/opt/homebrew/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/opt/homebrew/opt/nvm/etc/bash_completion.d/nvm"  # This loads nvm bash_completion

export DOTFILES_HOME=$HOME/.dotfiles.git
source $HOME/dotfiles.sh

export PATH=$PATH:~/bin

export PATH=$PATH:$HOME/.local/zig/current

alias ziglings='watchexec -w exercises -i zig-cache -e zig zig build'

alias gitka='gitk --all &'

export PATH="$(brew --prefix python)/libexec/bin":$PATH

export PATH="$(brew --prefix keydb)/bin":$PATH

alias update-brew='brew update && brew upgrade && brew upgrade --cask --greedy && brew cleanup'

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

#eval "$(direnv hook zsh)"

export PATH=~/zls:$PATH


# Load Angular CLI autocompletion.
source <(ng completion script)


# https://stackoverflow.com/questions/57972341/how-to-install-and-use-gnu-ls-on-macos
alias ls="/opt/homebrew/opt/coreutils/libexec/gnubin/ls"
