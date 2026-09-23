# Pi configuration

Global Pi config lives in `~/.pi/agent/` (tracked in the dotfiles repo:
`settings.json`, `models.json`, `fabric.json`, selected `extensions/` and
`skills/`).

`fabric.json` sets `executor.shellHangMs: 0`: a nested `pi.bash` waits for the
command to finish instead of returning `ok: true` + "still running" after 2 min,
which made joins over long tests look successful. Use `background: true` only
for intentionally unbounded processes. Takes effect after restarting Pi.

## Fabric: why and how

Why: not for speed (see below: no measured gain in normal work). It is kept
because it is installed and verified, and it gives the oracle a checked child
run: visible Herdr tab, model verified before the task is sent, exact tool
whitelist without extensions, `wait`/`stop`/status and `usage.cost`. A plain
`pi -p --model … --no-extensions --tools read,grep,find,ls` from bash could do
most of this (not compared yet). Drop Fabric only if it causes problems.

How: in full code mode the model calls tools only through `fabric_exec`
(TypeScript: `pi.read/grep/bash/edit`, `Promise.all` for parallel work,
`agents.spawn/wait/stop` for children). You do not write this code; you use
skills such as `/skill:oracle`.

`/fabric chat [id-or-name]` (or ctrl+shift+a): full-screen live conversation
with a child agent; send steering/follow-ups to a running child, switch
agents, return to Main without stopping it; `/copy` copies text. Completed
one-shot children are read-only. For the oracle, steering with Main's
candidate breaks its blindness: use chat to watch, not to hint.

## What not to build (measured 2026-09)

A pilot benchmark (`pi --no-extensions` vs Fabric full code mode, same model,
5 task types incl. three independent 30 s checks) showed no gain in turns,
wall-clock or cost from Fabric; both configs parallelized on their own (bash
`&` vs `Promise.all`). Therefore do not add an async "operations" layer or
scheduler on top of Fabric/bash. Revisit only with new measurements.

Not measured, deferred for lack of usage data: automatic oracle routing (Jev),
multi-oracle review, councils, another subagent framework. Consider them only
after real oracle use shows a need (e.g. how often a review changed the answer
or found a defect).

## Oracle: independent second opinion

Skill: `~/.pi/agent/skills/oracle/SKILL.md`. Requires the `pi-fabric` package
(listed in `settings.json`) and a Pi session started **inside Herdr**.

The oracle is a separate read-only agent (`read/grep/find/ls`, no extensions)
in its own Herdr tab. Reviewers (closed registry, chosen explicitly, never
auto-fallback): `astra` = `openai-codex/gpt-6-astra` (default),
`astra-api` = `openai/gpt-6-astra`,
`astra-openrouter` = `openrouter/openai/gpt-6-astra` (~$2.40 per plan review vs
~$0.2 for deepseek; only on request), `luna` = `openai-codex/gpt-6-luna`, `sol` = `openai-codex/gpt-6-sol`,
`luna-openrouter` / `sol-openrouter` = `openrouter/openai/gpt-6-{luna,sol}`
(pay-per-token; only on request), `deepseek` = `deepseek/deepseek-v4-pro`.
luna/sol are unevaluated against astra; `gpt-6-terra` is not in the catalog yet.

### Usage

```
/skill:oracle
/skill:oracle check whether this change to ~/scripts/foo matches yt-dlp behaviour
```

Or ask in plain words ("ask the oracle about this plan"); the command is more
reliable. After editing the skill, start a new session or run `/reload`.

What happens:

1. Main sends only the problem + raw evidence (paths, diffs, output). Its own
   candidate stays with Main (candidate-blind). No web access: pass local docs.
2. A Herdr tab `oracle-<reviewer>` opens; watch it live (inspection only, no
   takeover; `/fabric chat` shows the transcript).
3. Main compares the review with its candidate and verifies disagreements.

Use it for uncertain API/library claims, hard-to-reverse decisions, or after a
failed attempt. Skip it for trivial changes.

### Failures

- `spawn failed` — usually Pi started outside Herdr (no transport fallback).
- `timeout` (20 min, best-effort, covers spawn too) / `cancelled` (reviewer
  stopped) / `error` / `invalid` — not a review; use
  the shown `herdr terminal attach …` to inspect. Provider limits (Codex usage
  limit, no API credits) surface here as `error`.
- `exact model … not in pi catalog` — key missing; nothing was spawned.
- If the `fabric_exec` call is cancelled or Pi crashes, the reviewer keeps
  running detached: find it with `agents.list()` and `agents.wait`/`agents.stop`
  instead of respawning.
- Finished oracle tabs stay open; close them manually.

To add a model, edit `REGISTRY` in the skill and the reviewer list above.

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
- **No follow-up after completion.** A finished one-shot child cannot receive a
  second message, so "independent answer, then show the candidate" in one child
  is not possible; Main does the comparison.
- **Exact-key preflight.** A missing `provider/id` key silently fuzzy-matches
  another model of that provider, and an alias named like a canonical key wins.
  The skill checks the catalog before spawning; residual races are accepted.
- **`stop` returning is not proof the process died**; reported as
  `stopAcknowledged`.
- **Candidate-blind one-shot is the default, not a weak fallback.** The
  reviewer's conclusion cannot anchor on a candidate it never saw; the only
  residual bias is Main doing the comparison. A two-turn actor review is
  deferred until usage shows Main wrongly dismissing correct reviews.
- **`workspaceLabel`, not a fingerprint.** `HEAD` + hash of `git status` misses
  content changes in already-modified files and evidence outside the repo.
- **Provider choice is a disclosure decision.** Read-only prevents mutation,
  not sending evidence to that provider.

Fabric itself verifies the child's model before sending the task and fails
(without sending) on mismatch or unknown models; a failed run returns an empty
`text`, which the skill reports as an error, not as a review.
