# dotfiles-shim

Scripts that are first in `PATH` (`.zshrc_dotfiles_mode`) and make the dotfiles
repository behave like an ordinary repository even though its worktree is HOME.

## `git`

Picks the dotfiles repository per git call when the cwd is HOME or a path that
`.dotfiles.gitignore` does not ignore (and is not inside another repository).
`GIT_DIR` is not exported, so tools started from HOME (lazygit, nvim, agents)
see the right repository wherever they run git.

## Loading the policy

The dotfiles development policy is `~/AGENTS.policy.md`. Agents are sent to it by
`~/AGENTS.md`, a three-line file in this repository that says to read the policy
when the git root is HOME. Every agent reads `AGENTS.md` in the repository it
works in, so this needs no per-agent setup.

The policy is not named `AGENTS.md` itself because that file is loaded from the
working directory and all parent directories: the whole 452-line policy would
apply to every unrelated repository below HOME.

Earlier attempts, for the record:

- a `pi` shim in this directory: Herdr launches Pi with `~/.pi/agent/bin` ahead
  of this directory, so agent sessions bypassed it;
- a Pi user extension (`~/.pi/agent/extensions/dotfiles-policy.ts`): PATH-proof
  and verified, but Pi-only and one more moving part to keep.

Both were withdrawn in favour of the pointer, which every agent understands but
none is forced to follow. Do not reintroduce a second loader next to the pointer,
or the policy can arrive twice.

Because `~/AGENTS.md` is loaded from parent directories, it is also present in
sessions below HOME (for example in `~/personal_projects/herdr`). It costs a
line per session there and must stay inert: its condition is false whenever the
git root is not HOME, so that wording is load-bearing.

### Verification

```sh
# the git shim resolves the dotfiles repo from HOME and from a non-ignored subdir
cd ~                         && git rev-parse --show-toplevel   # -> $HOME
cd ~/.config/herdr           && git rev-parse --show-toplevel   # -> $HOME
cd ~/personal_projects/herdr && git rev-parse --show-toplevel   # -> that repo

# the pointer is loaded, and the policy is only referenced, never inlined
cd ~                         && pi -p "which AGENTS files are loaded here?"
cd ~/personal_projects/herdr && pi -p "which AGENTS files are loaded here?"
```
