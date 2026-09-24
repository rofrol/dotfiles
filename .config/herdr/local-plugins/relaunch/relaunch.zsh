# Sourced from ~/.zshrc inside herdr panes. Records the command that is
# running in the foreground of this pane; the record is removed when the
# prompt returns. The local.relaunch startup hook replays surviving records
# after a herdr server restart (processes killed by the restart never reach
# precmd, so their record survives).
[[ -n $HERDR_PANE_ID && -n $HERDR_SOCKET_PATH ]] || return 0

_herdr_relaunch_file="${XDG_STATE_HOME:-$HOME/.local/state}/herdr/plugins/local.relaunch/${HERDR_SOCKET_PATH//\//_}/${HERDR_PANE_ID//:/_}"

_herdr_relaunch_preexec() {
  [[ -n $1 ]] || return
  local cmd=$1
  # Never record herdr itself (e.g. `herdr server stop` would replay on every start).
  [[ $cmd == herdr* || $cmd == *"&& herdr"* ]] && { rm -f -- "$_herdr_relaunch_file"; return; }
  (umask 077; mkdir -p "${_herdr_relaunch_file:h}" &&
    print -r -- "$HERDR_TAB_ID"$'\n'"$PWD"$'\n'"$cmd" >| "$_herdr_relaunch_file")
}
_herdr_relaunch_precmd() { rm -f -- "$_herdr_relaunch_file"; }

autoload -Uz add-zsh-hook
add-zsh-hook preexec _herdr_relaunch_preexec
add-zsh-hook precmd _herdr_relaunch_precmd
