# https://github.com/zsh-git-prompt/zsh-git-prompt
# https://stackoverflow.com/questions/1128496/to-get-a-prompt-which-indicates-git-branch-in-zsh/2902338#2902338
source $HOME/personal_projects/vendor/zsh-git-prompt/zshrc.sh
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

source ~/.zshrc_nvm


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


function nvims() {
  items=("default" "kickstart" "NvChad" "AstroNvim" "LandonSchropp" "LazyVim" "dpetka2001", "pasteTest", "nvim-olical", "nvim-cajuse", "nvim-vimstudio")
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

export SDKMAN_DIR=$(brew --prefix sdkman-cli)/libexec
[[ -s "${SDKMAN_DIR}/bin/sdkman-init.sh" ]] && source "${SDKMAN_DIR}/bin/sdkman-init.sh"

export PATH="$HOME/personal_projects/docker/vendor/regclient/bin/:$PATH"
export PATH="$(brew --prefix sdkman-cli)/bin/:$PATH"

export DOTFILES_HOME=$HOME/.dotfiles.git
source $HOME/dotfiles.sh

alias etr='eza --icons -lt modified --sort modified --time-style full-iso'
