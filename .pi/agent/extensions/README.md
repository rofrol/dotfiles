# Pi extensions

Pi loads every `.ts` and `.js` file in this directory as an extension. Other
formats, including this file, are ignored. Extensions run inside the Pi process
with the same permissions as Pi, so only keep code here that you trust.

| File | What it does |
|---|---|
| `bg-bash.ts` | Replaces the bash tool so a message typed while commands run is answered at once: running commands move to the background (log in `$TMPDIR/pi-bg/`), and the agent is woken with a `bg-job-done` message when they exit. `/bg` lists them. End-to-end check: `../tests/bg-bash.e2e.py` (spends model calls). |
| `handoff-notice.ts` | Prints a read-only notice at session start when `roagent handoff show --repo .` lists pending unclaimed notes for the cwd repository, naming `/skill:handoff --take`; never claims a note. |
| `herdr-agent-state.ts` | Installed and overwritten by Herdr. Reports pane and agent state to Herdr and adds the Herdr runtime context. Do not edit it; add custom code in a file beside it. |
| `remember-model.ts` | Personal model and reasoning defaults. `/m` picks a model plus its reasoning level and remembers both; interactive `/model` choices are saved as the default model. |
| `shell-path-completion.ts` | Shell-style paths: Tab inserts the common prefix or sole match; when no further prefix is shared, Tab opens a menu and the next Tab accepts the selection. Ordinary paths no longer open automatic menus; `/` commands and `@` search are unchanged. |

Usage details for the personal commands live in the kisswiki page
`src/artificial_intelligence/pi.md`. This file stays an index with one line per
extension, so the two documents cannot drift apart.

Run `/reload` after changing an extension, or test one file directly with
`pi --extension <path>`.
