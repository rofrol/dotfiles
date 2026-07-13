# https://ellie.wtf/notes/profiling-zsh
# zmodload zsh/zprof

#  ~/.zshrc is typically reserved for things that are not inheritable by subshells, such as aliases and functions, custom prompts, history customizations, and so on.

## Aliases and functions

alias zig-update='zig version && $HOME/personal_projects/zig/zig-utils/scripts/zupd aarch64-macos'

alias ziglings='watchexec -w exercises -i zig-cache -e zig zig build'

alias gitkaa='gitk --all &'
# https://stackoverflow.com/questions/42211882/tell-gitk-to-ignore-all-branches-that-match-pattern
# https://stackoverflow.com/questions/20977520/is-there-any-way-to-exclude-branches-from-showing-in-gitk
# https://stackoverflow.com/questions/20979339/use-git-rev-list-to-exclude-a-branch-but-keep-common-ancestors-with-the-include
alias gitka='(git rev-parse --verify origin/gh-pages >/dev/null 2>&1 && gitk ^origin/gh-pages --all || gitk --all)&'

# https://www.reddit.com/r/commandline/comments/1r8tljk/comment/o67xaxn/
# https://stackoverflow.com/questions/57972341/how-to-install-and-use-gnu-ls-on-macos
eval "$(/opt/homebrew/bin/gdircolors -b)"
# https://zoxide.org/blog/advanced-zoxide-techniques/
# honours export TIME_STYLE=long-iso
alias ls='command gls --color=auto -h -H --group-directories-first -A -F -l'
alias ltr='ls -tr'

# honours export TIME_STYLE=long-iso
alias eza='eza --group --icons -lt modified --almost-all --group-directories-first'
alias etr='eza --sort modified'

alias gh-repo-private='gh repo create --private'
# https://stackoverflow.com/questions/73778273/how-to-add-a-remote-repo-using-gh-cli/74764582#74764582
alias gh-remote='git remote add origin $(gh repo view $repo --json sshUrl --jq .sshUrl)'

alias ghc='gh repo create --add-readme -c -l Apache-2.0 --public'

alias exercismwatch="watchexec -i zig-cache -e zig -r -c reset 'zig test test* 2>&1| less'"

# alias n='nvim --headless "+Lazy! sync" "+TSUpdateSync" +qa && nvim'
alias n=nvim

alias chrome='open -na Google\ Chrome'

# does not work in every situation
# no need to add quotes around pasted url or escape \?
# https://www.reddit.com/r/zsh/comments/15bxxyv/comment/jtur10r/
#alias y='noglob yt-dlp'

# When pasting, escapes ? as \?
# yt-dlp https://www.youtube.com/watch\?v\=-8U5V1rNSII
# does not work when in history is unescaped
# https://www.reddit.com/r/zsh/comments/15bxxyv/comment/jttk43f/
set zle_bracketed_paste
autoload -Uz bracketed-paste-magic url-quote-magic
zle -N bracketed-paste bracketed-paste-magic
zle -N self-insert url-quote-magic

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

# https://stackoverflow.com/questions/3855127/find-and-kill-process-locking-port-3000-on-mac/49587641#49587641
# https://stackoverflow.com/questions/3855127/find-and-kill-process-locking-port-3000-on-mac/37998980#37998980
function killTcpListen () {
  sudo lsof -sTCP:LISTEN -t -i tcp:$1
  kill -QUIT $(sudo lsof -sTCP:LISTEN -t -i tcp:$1)
}

# https://github.com/kovidgoyal/kitty/issues/268#issuecomment-419342337
# https://www.reddit.com/r/KittyTerminal/comments/197iook/comment/ki0y0bs/
function clearScrollback() {
  printf '\033[2J\033[3J\033[1;1H'
}

# https://twitter.com/nektro/status/1396376520512339973
# usage: cat /etc/group | yargs cut -d':' -f1
# leveraging Bash's read with an empty IFS to prevent line splitting
# (per Unix Stack Exchange insights on IFS=), a technique that contrasts
# with xargs' default behavior of splitting on whitespace, offering
# a more controlled input processing method.
function yargs() {
  cat /dev/stdin |
  while IFS= read -r line
  do
    echo $line | $@
  done
}

# https://www.reddit.com/r/fzf/comments/zazcrt/comment/jfo5z8k/
function cdfzf() { file="$(fd --type file --hidden --no-ignore --exclude node_modules | fzf)"; [ -n "$file" ] && cd "$(dirname "$file")"; }

eval "$(oh-my-posh init zsh --config ~/.config/oh-my-posh/themes/rofrol.omp.json)"

# https://apple.stackexchange.com/questions/88515/how-do-i-edit-current-shell-command-in-vi/443515#443515
# https://nuclearsquid.com/writings/edit-long-commands/
# Enable Ctrl-x-e to edit command line
autoload -U edit-command-line
# Emacs style
zle -N edit-command-line
bindkey '^xe' edit-command-line
bindkey '^x^e' edit-command-line

# Load Angular CLI autocompletion.
#source <(ng completion script)


#eval "$(direnv hook zsh)"


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

eval "$(atuin init zsh --disable-up-arrow)"
export ATUIN_SESSION=$(atuin uuid)
ATUIN_HISTORY_ID=""

function _atuin_preexec() {
    local id
    id=$(atuin history start -- "$1")
    export ATUIN_HISTORY_ID="$id"
    __atuin_preexec_time=${EPOCHREALTIME-}
}

function _atuin_precmd() {
    local EXIT="$?" __atuin_precmd_time=${EPOCHREALTIME-}

    [[ -z "${ATUIN_HISTORY_ID:-}" ]] && return

    local duration=""
    if [[ -n $__atuin_preexec_time && -n $__atuin_precmd_time ]]; then
        printf -v duration %.0f $(((__atuin_precmd_time - __atuin_preexec_time) * 1000000000))
    fi

    
    # ki editor reports error below: invalid parameter name.
    # probably because it is using tree-sitter-bash and not tree-sitter-zsh
    (ATUIN_LOG=error atuin history end --exit $EXIT ${=duration:+--duration $duration} -- $ATUIN_HISTORY_ID &) >/dev/null 2>&1
    export ATUIN_HISTORY_ID=""
}

# https://unix.stackexchange.com/questions/522035/expand-alias-in-zsh-history/522040#522040
# https://github.com/ellie/atuin/issues/969
# alias nf='nvim $(fd | zf)'
alias nf='f=$(fd | zf); print -rs nvim $f; _atuin_preexec "nvim $f"; nvim $f; _atuin_precmd $ATUIN_HISTORY_ID'
alias of='f=$(fd --type f | fzf); print -rs open "$f"; _atuin_preexec "open \"$f\""; open "$f"; _atuin_precmd $ATUIN_HISTORY_ID'
alias fdotfiles="fd -HI -E 'Library' -E '.cargo' -E 'personal_projects' -E '.vscode' -E '.npm' -E 'Downloads' -E '.cache' -E 'projects' -E '.rustup' -E '.nvm' -E '.antigravity-ide'"


# source <(fzf --zsh)

#https://www.reddit.com/r/linux4noobs/comments/egb644/fzf_newcomer_fd_or_ripgrep/
export FZF_DEFAULT_COMMAND='fd --hidden --no-ignore --exclude node_modules'


# brew install zsh-autosuggestions
source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh

# i.e. copied _cursor to completions from cursor installation files
fpath=(~/.zsh/completions $fpath)

# Added by OrbStack: command-line tools and integration
source ~/.orbstack/shell/init.zsh 2>/dev/null || :

# Since there is .zshrc, setting this in .zprofile was causing that,
# alt+. was not working correctly for bringing back last used last parameter

# Load Angular CLI autocompletion.
#source <(ng completion script)


# bun completions
[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"

# ? is a wildcard in Bash, too. The difference you are seeing occurs because in Zsh,
# a failed match leads to an error, whereas in Bash, it is silently ignored.
# I would actually recommend not turning it off. Silently ignoring errors is not a good thing and can even be dangerous when performing destructive operations.
# https://superuser.com/questions/1605802/completely-disable-and-globbing-in-zsh/1606090#1606090
# https://unix.stackexchange.com/questions/310540/how-to-get-rid-of-no-match-found-when-running-rm/313187#313187
#unsetopt nomatch

# eval "$(uv generate-shell-completion zsh)"


# . $HOME/.local/bin/env
# . $HOME/.config/openai

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads 

alias nodem='node --input-type=module -e'

eval "$(zoxide init zsh --cmd cd)"
alias cdi='__zoxide_zi'

# Should be last
[ -f ~/.zprofile_local ] && source ~/.zprofile_local

echo '~/.zshrc sourced.'