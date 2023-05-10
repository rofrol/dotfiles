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

function nvims() {
  items=("default" "kickstart" "LazyVim" "NvChad" "AstroNvim")
  config=$(printf "%s\n" "${items[@]}" | fzf --prompt=" Neovim Config  " --height=~50% --layout=reverse --border --exit-0)
  if [[ -z $config ]]; then
    echo "Nothing selected"
    return 0
  elif [[ $config == "default" ]]; then
    config=""
  fi
  NVIM_APPNAME=$config nvim $@
}

alias nf='nvim $(fd | zf)'
