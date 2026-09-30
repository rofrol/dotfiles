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
In the herdr fork, AGENTS.md and CLAUDE.md are identical copies: edit both.

# Git ignores

`~/.dotfiles.gitignore` belongs to the dotfiles repository (wired through that
repository's local `core.excludesFile`) and applies only where HOME is the
worktree root. `~/.global_gitignore` is the machine-global `core.excludesFile`
and applies to every repository. Put tool scratch directories and caches (for
example `.playwright-mcp/`) in the global file, not in a repository's
`.git/info/exclude`, which covers one checkout only.

# Dotfiles repository policy

The dotfiles repository has HOME as its worktree, so its git root is HOME. When
the git root is HOME, read `~/AGENTS.policy.md` and follow it. That file is not
named `AGENTS.md` on purpose: Pi and Claude Code load `AGENTS.md` from the
working directory and all parent directories, so a file of that name in HOME
would apply to every unrelated repository below it. Pi loads the policy through the extension
`~/.pi/agent/extensions/dotfiles-policy.ts`, which checks the git root itself,
so it works in every launch mode; Claude Code has no such extension and only
follows the pointer above.
