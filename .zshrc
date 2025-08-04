# Previously .zprofile content

eval "$(/opt/homebrew/bin/brew shellenv)"

# https://apple.stackexchange.com/questions/337320/how-to-get-rid-of-application-downloaded-from-the-internet-message-when-instal/376476#376476
export HOMEBREW_CASK_OPTS=--no-quarantine

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

export PATH=$PATH:~/bin

#export PATH=$PATH:~/.zvm/bin
#export PATH=$PATH:$HOME/personal_projects/zig/vendor/zig/stage3/bin/
#export PATH=$PATH:~/bin/zig
alias zig-update='zig version && $HOME/personal_projects/zig/zig-utils/scripts/zupd aarch64-macos'
export PATH=$PATH:$HOME/.local/zig/current
export PATH=$PATH:$HOME/zls

alias ziglings='watchexec -w exercises -i zig-cache -e zig zig build'

alias gitka='gitk --all &'

# https://unix.stackexchange.com/questions/275728/set-ls-l-time-format/693168#693168
export TIME_STYLE=long-iso
alias l='ls -lA'
alias ll='ls -lA --color=auto --group-directories-first'

alias et='eza --icons -lt modified --time-style "+%Y-%m-%d %H:%M"'
alias etr='eza --icons -lt modified --sort modified --time-style "+%Y-%m-%d %H:%M"'
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

export PATH=$PATH:/opt/homebrew/bin/
export PATH=$PATH:~/personal_projects/zig/vendor/sm2/zig-out/bin/

# https://blog.remibergsma.com/2012/07/10/setting-locales-correctly-on-mac-osx-terminal-application/
export LC_MESSAGES=en_US.UTF-8

export PATH=$PATH:~/bin/roc

alias gh-repo-private='gh repo create --private'
# https://stackoverflow.com/questions/73778273/how-to-add-a-remote-repo-using-gh-cli/74764582#74764582
alias gh-remote='git remote add origin $(gh repo view $repo --json sshUrl --jq .sshUrl)'

alias ghc='gh repo create --add-readme -c -l Apache-2.0 --public'

# ported fzf's own ctrl-r zsh widget to read from atuin instead of the shell's history to solve this.
# fzf's fuzzy search experience and speed with atuin's shell history management and syncing functionality.
# ctrl-r will search your shell history with fzf+atuin and ctrl-t will bring up atuin's own fuzzy finder in case you still want it.
# It only searches the last 5000 entries of your atuin history for speed,
# but you can tweak ATUIN_LIMIT to your desired value if that's not optimal.
# https://news.ycombinator.com/item?id=35256206
CUR_SHELL=zsh
atuin-setup() {
    if ! which atuin &> /dev/null; then return 1; fi
    bindkey '^Y' _atuin_search_widget

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
# why is it commented out?
# because of that?: commenting out as I cannot go to end of line with ctrl+e when this is binded
# atuin-setup

source <(fzf --zsh)

#https://www.reddit.com/r/linux4noobs/comments/egb644/fzf_newcomer_fd_or_ripgrep/
export FZF_DEFAULT_COMMAND='fd --hidden --no-ignore --exclude node_modules'

# https://www.reddit.com/r/fzf/comments/zazcrt/comment/jfo5z8k/
cdfzf() { file="$(fd --type file --hidden --no-ignore --exclude node_modules | fzf)"; [ -n "$file" ] && cd "$(dirname "$file")"; }

source ~/.zprofile_atuin

# brew install zsh-autosuggestions
source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh

# i.e. copied _cursor to completions from cursor installation files
fpath=(~/.zsh/completions $fpath)

# https://unix.stackexchange.com/questions/534942/auto-trigger-history-search-in-terminal-using-fzf-fuzzy-finder/676369#676369
# source ~/.zsh/zsh-autocomplete/zsh-autocomplete.plugin.zsh
#zstyle ':autocomplete:*' default-context fzf-atuin-history-widget
#bindkey -M menuselect '\r' .accept-line

# up-line-or-search-prefix () {
#   local CURSOR_before_search=$CURSOR
#   zle up-line-or-search "$LBUFFER"
#   CURSOR=$CURSOR_before_search
# }
# zle -N up-line-or-search-prefix
# bindkey '^T' up-line-or-search-prefix

alias exercismwatch="watchexec -i zig-cache -e zig -r -c reset 'zig test test* 2>&1| less'"

export PATH=$PATH:~/.local/share/npm/bin

export PATH=$PATH:~/go/bin/

# Added by OrbStack: command-line tools and integration
source ~/.orbstack/shell/init.zsh 2>/dev/null || :



# Previously .zshrc content

# https://github.com/zsh-git-prompt/zsh-git-prompt
# https://stackoverflow.com/questions/1128496/to-get-a-prompt-which-indicates-git-branch-in-zsh/2902338#2902338
source "$HOMEBREW_PREFIX/opt/zsh-git-prompt/zshrc.sh"
# an example prompt
# default macos:
#PROMPT=%n@%m %1~ %#
# default zsh-git-prompt
#PROMPT='%B%m%~%b$(git_super_status) %# '
# https://superuser.com/questions/1108413/zsh-prompt-with-current-working-directory/1108504#1108504
ZSH_THEME_GIT_PROMPT_PREFIX=" ["
#PROMPT='%2d$(git_super_status) %# '

# checks, whether the path is at least 4 elements long, and in that case prints the first element (%-1~),
# some dots (/…/) and the last 2 elements.
# otherwise the last 3 elements.
# https://unix.stackexchange.com/questions/273529/shorten-path-in-zsh-prompt/273567#273567
SHOW_DIRS=%(4~|%-1~/…/%2~|%3~)
PROMPT='$SHOW_DIRS$(git_super_status) %# '

# eval "$(starship init zsh)"
# eval "$(oh-my-posh init zsh)"
# eval "$(oh-my-posh init zsh --config $(brew --prefix oh-my-posh)/themes/M365Princess.omp.json)"
eval "$(oh-my-posh init zsh --config ~/.config/oh-my-posh/themes/rofrol.omp.json)"

# Since there is .zshrc, setting this in .zprofile was causing that,
# alt+. was not working correctly for bringing back last used last parameter
export EDITOR=nvim

# Load Angular CLI autocompletion.
#source <(ng completion script)

# https://gist.github.com/elijahmanor/b279553c0132bfad7eae23e34ceb593b
# [Neovim Config Switcher - YouTube](https://www.youtube.com/watch?v=LkHjJlSgKZY)
alias nvim-lazy="NVIM_APPNAME=LazyVim nvim"
alias nvim-kick="NVIM_APPNAME=kickstart nvim"
alias nvim-chad="NVIM_APPNAME=NvChad nvim"
alias nvim-astro="NVIM_APPNAME=AstroNvim nvim"
alias nvim-LandonSchropp="NVIM_APPNAME=LandonSchropp nvim"
alias nvim-LazyVim="NVIM_APPNAME=LazyVim nvim"
alias nvim-dpetka2001="NVIM_APPNAME=LazyVim nvim"
alias nvim-pasteTest="NVIM_APPNAME=LazyVim pasteTest"
alias nvim-olical="NVIM_APPNAME=nvim-olical nvim"
alias nvim-cajuse="NVIM_APPNAME=nvim-cajuse nvim"
alias nvim-vimstudio="NVIM_APPNAME=nvim-vimstudio nvim"
alias nvim-lazy4="NVIM_APPNAME=LazyVim--starter4 nvim"


function nvims() {
  items=("default" "kickstart" "NvChad" "AstroNvim" "LandonSchropp" "LazyVim" "dpetka2001", "pasteTest", "nvim-olical", "nvim-cajuse", "nvim-vimstudio", "LazyVim--starter4")
  config=$(printf "%s\n" "${items[@]}" | fzf --prompt=" Neovim Config  " --height=~50% --layout=reverse --border --exit-0)
  if [[ -z $config ]]; then
    echo "Nothing selected"
    return 0
  elif [[ $config == "default" ]]; then
    config=""
  fi
  NVIM_APPNAME=$config nvim $@
}

# alias nf='nvim $(fd | zf)'

export ATUIN_SESSION=$(atuin uuid)
ATUIN_HISTORY_ID=""

_atuin_preexec() {
    local id
    id=$(atuin history start -- "$1")
    export ATUIN_HISTORY_ID="$id"
    __atuin_preexec_time=${EPOCHREALTIME-}
}

_atuin_precmd() {
    local EXIT="$?" __atuin_precmd_time=${EPOCHREALTIME-}

    [[ -z "${ATUIN_HISTORY_ID:-}" ]] && return

    local duration=""
    if [[ -n $__atuin_preexec_time && -n $__atuin_precmd_time ]]; then
        printf -v duration %.0f $(((__atuin_precmd_time - __atuin_preexec_time) * 1000000000))
    fi

    (ATUIN_LOG=error atuin history end --exit $EXIT ${=duration:+--duration $duration} -- $ATUIN_HISTORY_ID &) >/dev/null 2>&1
    export ATUIN_HISTORY_ID=""
}

# https://unix.stackexchange.com/questions/522035/expand-alias-in-zsh-history/522040#522040
# https://github.com/ellie/atuin/issues/969
alias nf='f=$(fd | zf); print -rs nvim $f; _atuin_preexec "nvim $f"; nvim $f; _atuin_precmd $ATUIN_HISTORY_ID'

# alias n='nvim --headless "+Lazy! sync" "+TSUpdateSync" +qa && nvim'
alias n=nvim

# https://stackoverflow.com/questions/3855127/find-and-kill-process-locking-port-3000-on-mac/49587641#49587641
# https://stackoverflow.com/questions/3855127/find-and-kill-process-locking-port-3000-on-mac/37998980#37998980
function killTcpListen () {
  sudo lsof -sTCP:LISTEN -t -i tcp:$1
  kill -QUIT $(sudo lsof -sTCP:LISTEN -t -i tcp:$1)
}

# termporary until https://github.com/jqlang/jq/pull/2904 is released
export JQ_COLORS="0;90:0;39:0;39:0;39:0;32:1;39:1;39:1;34"

export PATH="$HOME/personal_projects/docker/vendor/regclient/bin/:$PATH"

export DOTFILES_HOME=$HOME/.dotfiles.git
source $HOME/dotfiles.sh

export PATH="$(brew --prefix git)/bin/:$PATH"
export PATH="$HOME/.zvm/self:$PATH"
export PATH="$HOME/.zvm/bin:$PATH"
export PATH="$HOME/.local/share/solana/install/active_release/bin:$PATH"

# https://github.com/kovidgoyal/kitty/issues/268#issuecomment-419342337
# https://www.reddit.com/r/KittyTerminal/comments/197iook/comment/ki0y0bs/
function clearScrollback() {
  printf '\033[2J\033[3J\033[1;1H'
}

# https://twitter.com/nektro/status/1396376520512339973
# usage: cat /etc/group | yargs cut -d':' -f1
function yargs() {
  cat /dev/stdin |
  while IFS= read -r line
  do
    echo $line | $@
  done
}

alias chrome='open -na Google\ Chrome'

# Added by LM Studio CLI (lms)
export PATH="$PATH:/Users/roman.frolow/.lmstudio/bin"

# bun completions
[ -s "/Users/roman.frolow/.bun/_bun" ] && source "/Users/roman.frolow/.bun/_bun"

# ZVM
export ZVM_INSTALL="$HOME/.zvm/self"
export PATH="$PATH:$HOME/.zvm/bin"
export PATH="$PATH:$ZVM_INSTALL/"

# no need to add quotes around pasted url or escape \?
# https://www.reddit.com/r/zsh/comments/15bxxyv/comment/jtur10r/
alias y='noglob yt-dlp'

# ? is a wildcard in Bash, too. The difference you are seeing occurs because in Zsh,
# a failed match leads to an error, whereas in Bash, it is silently ignored.
# I would actually recommend not turning it off. Silently ignoring errors is not a good thing and can even be dangerous when performing destructive operations.
# https://superuser.com/questions/1605802/completely-disable-and-globbing-in-zsh/1606090#1606090
# https://unix.stackexchange.com/questions/310540/how-to-get-rid-of-no-match-found-when-running-rm/313187#313187
#unsetopt nomatch

source ~/.zshrc_nvm




export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Should be last
[ -f ~/.zprofile_local ] && source ~/.zprofile_local
