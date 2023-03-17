# https://github.com/zsh-git-prompt/zsh-git-prompt
# https://stackoverflow.com/questions/1128496/to-get-a-prompt-which-indicates-git-branch-in-zsh/2902338#2902338
source $HOME/personal_projects/vendor/zsh-git-prompt/zshrc.sh
# an example prompt
# default macos:
#PROMPT=%n@%m %1~ %#
# default zsh-git-prompt
#PROMPT='%B%m%~%b$(git_super_status) %# '
# https://superuser.com/questions/1108413/zsh-prompt-with-current-working-directory/1108504#1108504
ZSH_GIT_PROMPT_SHOW_AHEAD=' '
PROMPT='%2d$(git_super_status) %# '
