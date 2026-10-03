If `git rev-parse --show-toplevel` prints your home directory (the dotfiles repository), read `$HOME/AGENTS.policy.md` and follow it before changing anything. Otherwise this file does not apply.

Before testing or reloading Hammerspoon, read `~/.hammerspoon/AGENTS.md`: reloading through `hs` IPC crashes it, and opening its windows steals the user's keyboard focus.
