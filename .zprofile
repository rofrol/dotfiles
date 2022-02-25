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

alias ziglings='watchexec -w exercises -i zig-cache -e zig zig build'
