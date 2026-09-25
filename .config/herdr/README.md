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

To pick up the new binary without losing panes, see the fork's README
(`~/personal_projects/herdr/README.md`, "Fork changes").

## Plugins from the fork

The plugins live in the herdr fork (`~/personal_projects/herdr/plugins/`) and
are linked from there; see their READMEs for details.

- **relaunch** reruns the programs panes were running (lazygit, ki, ...) after
  a server restart or reboot. Linked with
  `herdr plugin link ~/personal_projects/herdr/plugins/relaunch`; `~/.zshrc`
  sources its `relaunch.zsh`.
- **job**: `herdr-job run --name "Build" -- make` runs long work in its own tab
  and shows `⏳`/`✓`/`✗` in the sidebar (`$jobs` row in `config.toml`).
  `herdr-job` and `herdr-bg-badge` are symlinked into `~/.local/bin`; the
  "Long-running work" section in `~/.claude/CLAUDE.md` and
  `~/.pi/agent/AGENTS.md` tells agents to use it.
- Dotfiles mode (`don`) is not replayed by relaunch and not needed: the git
  shim (`~/scripts/dotfiles-shim/git`) picks the dotfiles repo per directory.
