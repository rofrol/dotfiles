# local.relaunch

Herdr restores panes after a server restart as plain shells. This plugin makes
the programs that were running in them come back, with nothing to configure
and nothing running in the background.

- `relaunch.zsh` (sourced from `~/.zshrc` in herdr panes) writes the command
  you start to `~/.local/state/herdr/plugins/local.relaunch/<socket>/<pane>`
  (preexec) and deletes it when the prompt returns (precmd). A restart kills
  the program before precmd, so its record survives. Files are 0600 and may
  contain command-line secrets; treat them like shell history.
- The `[[startup]]` hook (`relaunch.js`) reruns each record in the same pane if
  it has the same id and tab, is not an agent pane and is an idle shell.

Limits: programs start fresh (no in-app state); only commands typed in zsh are
seen; a one-shot command killed mid-way (e.g. a migration) is run again.

Preview: `node relaunch.js --dry-run`. Disable: `herdr plugin unlink
local.relaunch` and remove the `source` line from `~/.zshrc`.
