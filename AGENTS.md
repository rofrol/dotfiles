If `git rev-parse --show-toplevel` prints your home directory (the dotfiles repository), read `$HOME/AGENTS.policy.md` and follow it before changing anything. Otherwise this file does not apply.

Before testing or reloading Hammerspoon, read `~/.hammerspoon/AGENTS.md`: reloading through `hs` IPC crashes it, and opening its windows steals the user's keyboard focus.

Commit messages in the dotfiles repository start with the area they touch, then a colon: the tool, app or script the
change is about, e.g. `rormpc: max_fps 60`, `hammerspoon: show app icons in the launcher`, `musicdb delete: …`,
`scripts: …`. Use the same prefix for that area's config and code (`rormpc:`, not `rormpc theme:`); a change spanning
areas gets the main one, or one commit per area.
