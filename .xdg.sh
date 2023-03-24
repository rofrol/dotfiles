# xdg-ninja recommended
# https://news.ycombinator.com/item?id=35285982
# For example, NVIDIA uses XDG_CACHE_HOME if set, otherwise improperly falls back to ~/.nv instead of ~/.cache. And you can easily wait years for something like that to be fixed.
# https://superuser.com/questions/365847/where-should-the-xdg-config-home-variable-be-defined/1609989#1609989
# on linux there is https://github.com/queer/boxxy

export XDG_DATA_HOME=$HOME/.local/share
export XDG_CONFIG_HOME=$HOME/.config
export XDG_STATE_HOME=$HOME/.local/state
export XDG_CACHE_HOME=$HOME/.cache

# vscode does not start with below setting on macos
#export XDG_RUNTIME_DIR=/run/user/$UID
# https://www.reddit.com/r/linuxquestions/comments/fxdq13/comment/ijnheev/
if [ ! -d /etc/pam.d ] && [ ! -x /bin/elogind-inhibit ]; then
  # Not using PAM or elogind.
  if [ ! -d /tmp/runtime-$USER ]; then
    # https://serverfault.com/questions/388840/good-default-for-xdg-runtime-dir/393351#393351
    mktemp -d /tmp/runtime-$USER
  fi
  if [ -d /tmp/runtime-$USER ]; then
    export XDG_RUNTIME_DIR=/tmp/runtime-$USER
  fi
fi

export ELINKS_CONFDIR="$XDG_CONFIG_HOME"/elinks
export LESSHISTFILE="$XDG_CACHE_HOME"/less/history

export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME"/npm/npmrc
mkdir -p ${XDG_CACHE_HOME}/npm
mkdir -p ${XDG_DATA_HOME}/npm/lib

export NVM_DIR="$XDG_DATA_HOME"/nvm
export INPUTRC="$XDG_CONFIG_HOME"/readline/inputrc

# does not work
# compinit -d "$XDG_CACHE_HOME"/zsh/zcompdump-"$ZSH_VERSION"
# export ZDOTDIR="$HOME"/.config/zsh