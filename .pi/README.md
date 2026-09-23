# Pi configuration

Global Pi config lives in `~/.pi/agent/` (tracked in the dotfiles repo:
`settings.json`, `models.json`, selected `extensions/` and `skills/`).

## Oracle: independent second opinion

Skill: `~/.pi/agent/skills/oracle/SKILL.md`. Requires the `pi-fabric` package
(listed in `settings.json`) and a Pi session started **inside Herdr**.

The oracle is a separate GPT Astra agent (`openai-codex/gpt-6-astra`, ChatGPT
Plus subscription) that reviews Main's solution read-only (`read/grep/find/ls`)
in its own Herdr tab and returns an attributed review.

### Usage

```
/skill:oracle
/skill:oracle check whether this change to ~/scripts/foo matches yt-dlp behaviour
```

Or ask in plain words ("ask the oracle about this plan"); the command is more
reliable. After editing the skill, start a new session or run `/reload`.

What happens:

1. Main writes a review package: problem + evidence (file paths, diffs), then
   its candidate solution and claims. The oracle does not see the conversation.
2. A Herdr tab `oracle-astra` opens; watch it live.
3. Main shows the review, lists disagreements, and verifies disputed claims.

Use it for uncertain API/library claims, hard-to-reverse decisions, or after a
failed attempt. Skip it for trivial changes.

### Failures

- `Oracle requires Herdr` — Pi was started outside Herdr.
- `timeout` (20 min) / `failed` / `stopped` — not a review; run the shown
  `herdr terminal attach …` command to inspect the child.
- `model mismatch` — the registry key no longer resolves exactly; check
  `agents.models({ runner: "pi" })`.
- Finished oracle tabs stay open; close them manually.

To change the model, edit the one-line `REGISTRY` in the skill.

### Why the skill looks like this

Verified against pi-fabric 0.93.1 (docs and runtime tests inside Herdr):

- **Fuzzy model resolution.** A near-miss key (e.g. `gpt-6-astr`) silently
  resolves to the closest model on the same provider. Hence exact registry keys
  and the `handle.model === expected` assertion.
- **`tools` alone is not read-only.** Extension-enabled children keep
  `fabric_exec` as an outer tool. `extensions: false` + `read/grep/find/ls`
  left only those four tools; a write attempt failed.
- **`timeoutMs` cannot shorten a run.** Values below `agents.timeoutMs`
  (default 24 h) are ignored. Hence the caller-side 20-minute race plus
  `agents.stop`.
- **Caller abort detaches, not stops,** a child that already made progress
  (any turn or tool call). Hence explicit `agents.stop` on timeout.

Fabric itself verifies the child's model before sending the task and fails
(without sending) on mismatch or unknown models; a failed run returns an empty
`text`, which the skill reports as an error, not as a review.
