# Pi extensions

Pi loads every `.ts` and `.js` file in this directory as an extension. Other
formats, including this file, are ignored. Extensions run inside the Pi process
with the same permissions as Pi, so only keep code here that you trust.

| File | What it does |
|---|---|
| `herdr-agent-state.ts` | Installed and overwritten by Herdr. Reports pane and agent state to Herdr and adds the Herdr runtime context. Do not edit it; add custom code in a file beside it. |
| `remember-model.ts` | Personal model and reasoning defaults. `/m` picks a model plus its reasoning level and remembers both; interactive `/model` choices are saved as the default model. |

Usage details for the personal commands live in the kisswiki page
`src/artificial_intelligence/pi.md`. This file stays an index with one line per
extension, so the two documents cannot drift apart.

Run `/reload` after changing an extension, or test one file directly with
`pi --extension <path>`.
