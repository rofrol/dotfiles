# herdr config

## macOS notifications

`config.toml` sets `[ui.toast] delivery = "system"`, so herdr shows a macOS
notification (plus sound) when an agent finishes or needs input in a
background workspace.

herdr uses `terminal-notifier` (`brew install terminal-notifier`, installed
here). With the fork build (see "herdr from my fork" below) an agent
notification shows the agent's task (terminal title) as the message and
workspace · tab as the subtitle, replaces the previous one from the same pane,
and clicking it brings Ghostty to the front and focuses that agent's pane
(`herdr agent focus`, falling back to `herdr tab focus`). Upstream 0.9.1 only
activates the terminal (feature request herdrdev/herdr#4480). Setup:

1. System Settings → Notifications → **terminal-notifier** → Allow
   notifications (Desktop, alert style Temporary). The entry appears after
   the first notification.
2. Focus modes silently hide banners. To still get agent notifications:
   System Settings → Focus → (each mode) → Allowed Notifications → add
   **terminal-notifier**.

Without `terminal-notifier` herdr falls back to `osascript`: notifications
are attributed to **Script Editor** (allow it in the same places) and
clicking one opens Script Editor instead of the terminal.

Test:

```sh
herdr notification show "Herdr test" --body "works" --sound done
```

Apply config changes without restarting: `herdr server reload-config`
(validate first with `herdr config check`).

## herdr from my fork

herdr is installed from the fork `rofrol/herdr`, not Homebrew:

- checkout: `~/personal_projects/herdr` (`origin` = rofrol/herdr,
  `upstream` = herdrdev/herdr); fork commits live directly on `master`,
  rebased on `upstream/master` (no PRs upstream)
- binary: `~/.cargo/bin/herdr` (`cargo install --path . --locked`)
- fork changes: middle click on a tab / sidebar workspace closes it (same
  confirmation as the menu's Close); clickable, richer notifications (above);
  notification titles like `(repo)` no longer crash terminal-notifier

The dotfiles env (`GIT_DIR`/`GIT_WORK_TREE`) breaks git in the checkout and the
herdr git tests, so unset it: `env -u GIT_DIR -u GIT_WORK_TREE git ...`.
Building needs Zig 0.16.0 (`brew install zig`).

Do not run `herdr update`: it installs the upstream release over the fork.

Update from upstream and reinstall:

```sh
cd ~/personal_projects/herdr
env -u GIT_DIR -u GIT_WORK_TREE git fetch upstream
env -u GIT_DIR -u GIT_WORK_TREE git rebase upstream/master
env -u GIT_DIR -u GIT_WORK_TREE cargo test --locked --bin herdr -- client:: platform::
env -u GIT_DIR -u GIT_WORK_TREE cargo install --path . --locked
env -u GIT_DIR -u GIT_WORK_TREE git push --force-with-lease
herdr status   # compare client/server version and protocol
```

Then pick up the new binary without losing panes:

- Client-side changes (TUI: mouse, notifications — everything in this fork so
  far): detach with `prefix+q` and run `herdr` again. Panes and agents live in
  the server and keep running.
- Server-side changes: swap the running server in place with the live
  handoff (what `herdr update --handoff` uses; pane processes survive):

  ```sh
  herdr server live-handoff --import-exe ~/.cargo/bin/herdr
  herdr status
  ```

  Upstream marks live handoff experimental (Unix only); a running agent may
  briefly show as idle after it. Not tried here yet. If it fails, the old server normally keeps running;
  otherwise `herdr server stop` + `herdr` restarts it (panes come back as
  shells, see relaunch below).

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
- Dotfiles mode (`don`) is not replayed and not needed: the git shim
  (`~/scripts/dotfiles-shim/git`) picks the dotfiles repo per directory.

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
