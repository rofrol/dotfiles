# herdr config

## macOS notifications

`config.toml` sets `[ui.toast] delivery = "system"`, so herdr shows a macOS
notification (plus sound) when an agent finishes or needs input in a
background workspace.

Without `terminal-notifier` installed, herdr delivers notifications via
`osascript`, which macOS attributes to **Script Editor**. Setup:

1. Open Script Editor once so it appears in notification settings.
2. System Settings → Notifications → Script Editor → Allow notifications
   (Desktop, alert style Temporary).
3. Focus modes silently hide banners. To still get agent notifications:
   System Settings → Focus → (each mode) → Allowed Notifications → add
   **Script Editor**.

Test:

```sh
osascript -e 'display notification "test" with title "Herdr test"'
herdr notification show "Herdr test" --body "works" --sound done
```

Apply config changes without restarting: `herdr server reload-config`
(validate first with `herdr config check`).

## Relaunching programs after a restart

After a server restart (or reboot) herdr restores layout, cwd and supported
agents (pi, ...), but other panes come back as empty shells. The local plugin
`local-plugins/relaunch/` brings back whatever was running in them (lazygit,
ki, ...), with no list of programs to maintain and nothing running in the
background.

How it works:

- `relaunch.zsh` hooks zsh: `preexec` writes the command you start to
  `~/.local/state/herdr/plugins/local.relaunch/<socket>/<pane>`, `precmd`
  deletes it when the prompt returns. A restart kills the program before the
  prompt returns, so the record survives.
- The plugin's `[[startup]]` hook (`relaunch.js`) reruns each surviving command
  in the same pane, only if the pane has the same id and tab, is not an agent
  pane and is an idle shell. It never creates tabs or workspaces.
- If dotfiles mode is on (`don` from `~/dotfiles.sh`, i.e. `GIT_DIR` is
  `$DOTFILES_HOME`), the record becomes `don && <command>`, so e.g. lazygit in
  `$HOME` comes back on the dotfiles repo.

Setup:

1. Link the plugin once:

   ```sh
   herdr plugin link ~/.config/herdr/local-plugins/relaunch
   ```

2. Enable the zsh hook: add to the end of `~/.zshrc` (already done here):

   ```sh
   # herdr: remember foreground commands per pane (local.relaunch plugin)
   [[ -n $HERDR_PANE_ID ]] && source ~/.config/herdr/local-plugins/relaunch/relaunch.zsh
   ```

   It only takes effect in shells started afterwards (new panes, or
   `exec zsh` in an existing one). Check: `whence -v _herdr_relaunch_preexec`.

Use:

- Preview what would be relaunched:
  `node ~/.config/herdr/local-plugins/relaunch/relaunch.js --dry-run`
- Logs after a restart: `herdr plugin log list --plugin local.relaunch`
- Disable: `herdr plugin unlink local.relaunch` and remove the `~/.zshrc` line.

Limits: programs start fresh (no in-app state); only commands typed in zsh are
recorded; a one-shot command killed mid-way (e.g. a migration) runs again.
Records are 0600 and may contain command-line secrets, like shell history.
