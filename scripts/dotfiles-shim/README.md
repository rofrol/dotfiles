# dotfiles-shim

Scripts that are first in `PATH` (`.zshrc_dotfiles_mode`) and make the dotfiles
repository behave like an ordinary repository even though its worktree is HOME.

## `git`

Picks the dotfiles repository per git call when the cwd is HOME or a path that
`.dotfiles.gitignore` does not ignore (and is not inside another repository).
`GIT_DIR` is not exported, so tools started from HOME (lazygit, nvim, agents)
see the right repository wherever they run git.

## `pi` (removed)

There used to be a `pi` shim here that appended the policy. It was removed
because it did not work where it mattered: Herdr launches Pi with
`~/.pi/agent/bin` ahead of this directory, so agents bypassed the shim, and the
policy was missing exactly in the agent sessions that need it.

The policy is loaded by the Pi extension `~/.pi/agent/extensions/dotfiles-policy.ts`
instead. A user-directory extension loads in every Pi session and checks the git
root itself, so it needs no `PATH` and no shell. Add a `pi` shim or a `pi()`
shell function back only together with removing that extension, or the policy is
appended twice.

### What it guarantees, and what bypasses it

Guaranteed for Pi in every launch mode: interactive, `command pi`, absolute
paths, `mise`/`asdf`/`npx`, non-interactive runs, and agents Herdr starts, as
long as the extension loads. The extension appends nothing when the git root is
not HOME, when `git` fails, or when `~/AGENTS.policy.md` is missing.

Known limits:

- `pi --no-extensions` skips the extension, so the policy is missing silently;
- an inherited `GIT_DIR`/`GIT_WORK_TREE` can make the git root resolve elsewhere
  (then the policy is omitted, rather than added in the wrong repository);
- the check runs once per agent run (one `git rev-parse`), not per token;
- Claude Code does not load Pi extensions: `~/.claude/CLAUDE.md` only asks it to
  read the policy, which is a soft instruction, not a guarantee.

### Verification

```sh
# the git shim resolves the dotfiles repo from HOME and from an ignored subdir
cd ~                         && git rev-parse --show-toplevel   # -> $HOME
cd ~/.config/herdr           && git rev-parse --show-toplevel   # -> $HOME
cd ~/personal_projects/herdr && git rev-parse --show-toplevel   # -> that repo

# end to end: in HOME the policy reaches the system prompt exactly once, in
# another repository it must not appear at all
cd ~                         && pi -p "reply with exactly: ok"
cd ~/personal_projects/herdr && pi -p "reply with exactly: ok"
grep -c 'Architecture and Maintenance Policy' ~/.pi/agent/sessions/--Users-romanfrolow--/*.jsonl | tail -1        # -> 1
grep -c 'Architecture and Maintenance Policy' ~/.pi/agent/sessions/--Users-romanfrolow-personal_projects-herdr--/*.jsonl | tail -1  # -> 0
```

The extension is the single loader. If the policy shows up twice, something
still appends it: look for a `pi` shim in `PATH` and for a `pi()` function.
