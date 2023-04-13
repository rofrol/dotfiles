# What Goes to ~/.zprofile and What Goes to ~/.zshrc?
# Since ~/.zprofile is loaded only once at login time, it’s best to put things that are loaded only once and can be inherited by subshells. An excellent example of this is environment variables.

# On the other hand, ~/.zshrc is typically reserved for things that are not inheritable by subshells, such as aliases and functions, custom prompts, history customizations, and so on.

# In addition, put the commands that should run every time you launch a new shell in the .zshrc file, too.
# https://www.zerotohero.dev/zshell-startup-files/

eval "$(/opt/homebrew/bin/brew shellenv)"

# https://apple.stackexchange.com/questions/337320/how-to-get-rid-of-application-downloaded-from-the-internet-message-when-instal/376476#376476
export HOMEBREW_CASK_OPTS=--no-quarantine

alias brew-packages='{brew leaves --installed-on-request & brew list --cask -1;} | sort | uniq'

# https://stackoverflow.com/questions/19915683/how-to-find-package-for-installed-file-in-brew/36622898#36622898
function brew_find_pkg {
    file_to_search="$@"

    for package in $(brew-packages); do
        brew ls $package | grep -E -q "/${file_to_search}$"
        if [ $? -eq 0 ]; then
            echo $package
            break
        fi
    done
}



export PATH="$PATH:/Applications/Visual Studio Code.app/Contents/Resources/app/bin"

export DOTFILES_HOME=$HOME/.dotfiles.git
source $HOME/dotfiles.sh

export PATH=$PATH:~/bin

#export PATH=$PATH:~/.zvm/bin
#export PATH=$PATH:$HOME/personal_projects/zig/vendor/zig/stage3/bin/
#export PATH=$PATH:~/bin/zig
alias zupd='zig version && $HOME/personal_projects/zig/zig-utils/scripts/zupd aarch64-macos'

export PATH=$PATH:$HOME/.local/zig/current

export PATH=$PATH:$HOME/zls

alias ziglings='watchexec -w exercises -i zig-cache -e zig zig build'

alias gitka='gitk --all &'

export PATH="$(brew --prefix python)/libexec/bin":$PATH

export PATH="$(brew --prefix keydb)/bin":$PATH

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

export PATH=$PATH:~/.pyenv/shims/yt-dlp
export PATH=$PATH:/opt/homebrew/bin/
export PATH=$PATH:~/personal_projects/zig/vendor/sm2/zig-out/bin/

# https://blog.remibergsma.com/2012/07/10/setting-locales-correctly-on-mac-osx-terminal-application/
export LC_MESSAGES=en_US.UTF-8

export PATH=$PATH:~/bin/roc

# git clone https://github.com/zsh-users/zsh-autosuggestions ~/.zsh/zsh-autosuggestions
source ~/.zsh/zsh-autosuggestions/zsh-autosuggestions.zsh

alias gh-repo-private='gh repo create --private'
# https://stackoverflow.com/questions/73778273/how-to-add-a-remote-repo-using-gh-cli/74764582#74764582
alias gh-remote='git remote add origin $(gh repo view $repo --json sshUrl --jq .sshUrl)'

alias ghc='gh repo create --add-readme -c -l Apache-2.0 --public'

# ported fzf's own ctrl-r zsh widget to read from atuin instead of the shell's history to solve this.
# fzf's fuzzy search experience and speed with atuin's shell history management and syncing functionality.
# ctrl-r will search your shell history with fzf+atuin and ctrl-e will bring up atuin's own fuzzy finder in case you still want it.
# It only searches the last 5000 entries of your atuin history for speed,
# but you can tweak ATUIN_LIMIT to your desired value if that's not optimal.
# https://news.ycombinator.com/item?id=35256206
CUR_SHELL=zsh
atuin-setup() {
    if ! which atuin &> /dev/null; then return 1; fi
    # commenting out as I cannot go to end of line with ctrl+e when this is binded
    #bindkey '^E' _atuin_search_widget

    export ATUIN_NOBIND="true"
    eval "$(atuin init "$CUR_SHELL")"
    fzf-atuin-history-widget() {
        local selected num
        setopt localoptions noglobsubst noposixbuiltins pipefail no_aliases 2>/dev/null

        # local atuin_opts="--cmd-only --limit ${ATUIN_LIMIT:-5000}"
        local atuin_opts="--cmd-only"
        local fzf_opts=(
            --height=${FZF_TMUX_HEIGHT:-80%}
            --tac
            "-n2..,.."
            --tiebreak=index
            "--query=${LBUFFER}"
            "+m"
            "--bind=ctrl-d:reload(atuin search $atuin_opts -c $PWD),ctrl-r:reload(atuin search $atuin_opts)"
        )

        selected=$(
            eval "atuin search ${atuin_opts}" |
                fzf "${fzf_opts[@]}"
        )
        local ret=$?
        if [ -n "$selected" ]; then
            # the += lets it insert at current pos instead of replacing
            LBUFFER+="${selected}"
        fi
        zle reset-prompt
        return $ret
    }
    zle -N fzf-atuin-history-widget
    bindkey '^R' fzf-atuin-history-widget
}
atuin-setup

# https://stackoverflow.com/questions/122327/how-do-i-find-the-location-of-my-python-site-packages-directory/52638888#52638888
export PATH=$(python -c "import sysconfig; print(sysconfig.get_path('purelib'))"):$PATH

alias dfr="df -H | rg --color never '(^Filesystem | /$)'"

# Should be last
[ -f ~/.zprofile_local ] && source ~/.zprofile_local
