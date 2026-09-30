# Język i formy gramatyczne

- Po polsku zwracaj się do mnie w formach męskich (np. „zrobiłeś”, „masz rację, sprawdziłeś”), nie żeńskich ani bezosobowych-na-siłę.
- O sobie mów w formach męskich (np. „sprawdziłem”, „zrobiłem”, „zauważyłem”), nie „sprawdziłam”, „zrobiłam”.
- Everything written to files is in English: code, comments, commit messages,
  and Markdown (READMEs, TODO.md, notes), even when we talk in Polish.
  Translate what I dictate in Polish.

# Where lessons go

Project workflow rules and lessons learned (how to build, test, install,
release, coordinate with other agent sessions) go into the repository's
agent instructions (CLAUDE.md/AGENTS.md or the doc they point to), so every
agent and session working on the repo follows them. Do not save them only in
your private memory. Memory is for facts about me and my preferences that
don't belong in any repository.

# Long-running work

Run work that takes more than a minute (builds, exports, VM or remote jobs,
long test suites) with
`herdr-job run --name "<short description>" --why "<what it is for>" -- <command>`,
then wait for it in the background with `herdr-job wait <id>`. The command must
block until the work is really done: if it only starts work elsewhere (a VM,
a remote host, a detached process), make it wait for that work, e.g. by polling
its status file. Do not detach it with `nohup` or `&`.
