# Long-running work

Run work that takes more than a minute (builds, exports, VM or remote jobs,
long test suites) with
`herdr-job run --name "<short description>" --why "<what it is for>" -- <command>`,
then wait for it in the background with `herdr-job wait <id>`. The command must
block until the work is really done: if it only starts work elsewhere (a VM,
a remote host, a detached process), make it wait for that work, e.g. by polling
its status file. Do not detach it with `nohup` or `&`.

# Language

- The user speaks Polish. In Polish, address the user with masculine forms
  ("zrobiłeś", "sprawdziłeś") and refer to yourself with masculine forms
  ("sprawdziłem", "zrobiłem").
- Everything written to files is in English: code, comments, commit messages
  and Markdown (READMEs, TODO.md, notes), even when the conversation is in
  Polish. Translate what the user dictates in Polish.

# Where lessons go

Project workflow rules and lessons learned go into the repository's agent
instructions (AGENTS.md/CLAUDE.md), so every agent and session follows them.
In the herdr fork, AGENTS.md is the only agent instruction file (CLAUDE.md was removed).

# Uncommitted work

Never end a session leaving your own edits uncommitted in a shared checkout:
commit them, or state in your final message exactly which files you left dirty.
Where a repository allows agent commits, a session commits its own notes as
soon as it writes them, with an explicit path (`git commit -- <paths>`), after
checking that `git diff -- <paths>` shows only its own hunks; where agent
commits are forbidden, say so instead of committing.

# Git ignores

`~/.dotfiles.gitignore` belongs to the dotfiles repository (wired through that
repository's local `core.excludesFile`) and applies only where HOME is the
worktree root. `~/.global_gitignore` is the machine-global `core.excludesFile`
and applies to every repository. Put tool scratch directories and caches (for
example `.playwright-mcp/`) in the global file, not in a repository's
`.git/info/exclude`, which covers one checkout only.

The dotfiles repository keeps its development policy in `~/AGENTS.policy.md` and
points to it from `~/AGENTS.md`, which every agent reads in that repository. The
policy is not named `AGENTS.md` itself: Pi and Claude Code load `AGENTS.md` from
the working directory and all parent directories, so the whole policy would
apply to every unrelated repository below HOME. `~/scripts/dotfiles-shim/README.md`
explains the setup.

# Claude settings.json

`~/.claude/settings.json` has two owners: herdr's integration installer writes
its awaiting-reply permission and its `UserPromptSubmit` reminder, and the
hand-written entries (for example the uncommitted-edits hooks) sit beside them.
Both are tracked here, because every machine that runs the herdr fork installs
the same integration with `herdr integration install claude`, which also writes
the hook script those entries call. The installer merges into this file and
removes only its own entries, so after installing or uninstalling the
integration check `git diff` here and commit the change (`herdr integration
status` lists what is installed).

# Added delay is a bug signal

Before you raise a timeout or add a sleep, retry or wait, treat the urge as a
symptom of an asynchronous bug (a race, a lost event, a dropped input, a
non-atomic write) and find the cause: reproduce it with a stress loop, wait for
an observable condition instead of a duration, retry until the effect is
observed, publish files atomically. Lengthen time only for a real external
deadline, rate limit, external polling, designed backoff or measured slow
hardware, and say which next to the value. Canonical text: Rule 10 in
~/personal_projects/agents.md/AGENTS.md.

# Asking other models

"Pytaj modeli" ("ask the models") means: consult the default set of other
models now, in this turn. Load the `consult` skill; it lists who joins a round
and how to run, rate and score it.
