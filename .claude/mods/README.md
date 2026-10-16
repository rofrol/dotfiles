# Claude Code mods

Personal Claude Code mods (plugins with a hooks module). This folder is a local
marketplace, `local-mods`; Claude Code reads each mod from here, so an edit
reaches a session with `/reload-plugins`, without a reinstall.

Install on a new machine:

```sh
claude plugin marketplace add ~/.claude/mods
claude plugin install shell-path-completion@local-mods
```

| Mod | What it does |
|---|---|
| `shell-path-completion` | Shell-style paths in the prompt: Tab inserts the sole match or the matches' common prefix; when no longer prefix is shared, the typeahead lists every match below a first row that keeps the path as typed, so Tab picks nothing and closes the list; arrows choose a match, then Tab takes it. `@` mentions, `/` commands and `!` shell mode keep Claude Code's own completion, since the engine does not ask mods there (https://github.com/anthropics/claude-code/issues/102706). The typeahead still opens by itself while a path is typed. Same behavior in Pi: `~/.pi/agent/extensions/shell-path-completion.ts`. |

Test a mod with `claude plugin test <folder>`, and try it in one session with
`claude --plugin-dir <folder>`.
