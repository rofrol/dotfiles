# Pi configuration

## TL;DR

- Config: `~/.pi/agent/` (dotfiles repo). After changing a skill: `/reload`.
- Second opinion: `/skill:oracle <question>` (Pi must run inside Herdr).
  Default reviewer `astra`; name another one: `/skill:oracle sol …`.
- Watch the reviewer: `ctrl+shift+a` (= `/fabric chat`) in the same Pi; type
  nothing; exit with `/back` or Esc Esc.
- Fabric is kept for the oracle, not for speed. Do not build schedulers,
  councils, Jev routing or multi-review without usage data.

## Oracle: independent second opinion

Skill: `~/.pi/agent/skills/oracle/SKILL.md`. Needs the `pi-fabric` package
(in `settings.json`) and a Pi session started **inside Herdr**.

A separate read-only agent (`read/grep/find/ls`, no extensions) in its own
Herdr tab. Main sends only the problem and raw evidence (paths, diffs of
existing code, output); its own candidate stays with Main (candidate-blind).
No web access: pass local docs. Main then compares the review with its
candidate and verifies disagreements.

Use for uncertain API/library claims, hard-to-reverse decisions, or after a
failed attempt; not for trivial changes. `/skill:oracle` is more reliable than
plain words ("ask the oracle about …").

### Reviewers

Closed registry, chosen explicitly, never automatic fallback.

| Name | Model | Billing |
|---|---|---|
| `astra` (default) | `openai-codex/gpt-6-astra` | Codex subscription |
| `luna`, `sol` | `openai-codex/gpt-6-{luna,sol}` | Codex subscription |
| `astra-api` | `openai/gpt-6-astra` | OpenAI API |
| `astra-openrouter` | `openrouter/openai/gpt-6-astra` | ~$2.40 per plan review |
| `luna-openrouter`, `sol-openrouter` | `openrouter/openai/gpt-6-{luna,sol}` | pay-per-token |
| `deepseek` | `deepseek/deepseek-v4-pro` | ~$0.2 per plan review |

All but `astra` only on request. luna/sol are unevaluated against astra;
`gpt-6-terra` is not in the catalog yet. Current OpenRouter prices:
`https://openrouter.ai/api/v1/models` (no key needed). To add a model, edit
`REGISTRY` in the skill and this table.

### Failures

Only `completed` is a review.

- `spawn failed`: usually Pi started outside Herdr (no transport fallback).
- `exact model … not in pi catalog`: key missing; nothing spawned.
- `timeout` (20 min, best-effort, includes spawn) / `cancelled` / `error` /
  `invalid`: inspect with the shown `herdr terminal attach …`. Provider limits
  (Codex usage limit, no credits) show up as `error`.
- Cancelled `fabric_exec` or crashed Pi: the reviewer keeps running detached;
  find it with `agents.list()` and `agents.wait`/`agents.stop`, do not respawn.
- Finished oracle tabs stay open; close them manually.

## `/fabric chat`

Open it in the same Pi as Main, not in a new Herdr tab or new Pi (one-shot
children belong to this Main process). `ctrl+shift+a` = `/fabric chat`; works
while Main is waiting. `/fabric chat oracle-astra` opens a specific child (Tab
completes IDs); without a name it picks the active child. Full-screen view over
the current Pi: no new session, Main and children keep running.

- Enter = steer: delivered after the child's current tool calls, before its
  next model call (does not cut an answer, but changes what it does next).
- Alt+Enter = follow-up: runs after the current run ends; must be queued
  before it ends.
- Completed one-shot child: `read-only`, messages rejected.
- Ctrl+N or `/agents` switches agent; `/copy` copies the last answer (also for
  completed runs); `/stop` stops the child (after confirmation).
- Exit: `/back` or Esc twice (first Esc clears a selection); nothing stops.

The Herdr tab shows only the raw worker terminal; chat shows the full
transcript (thinking, tools, cost). For the oracle, type nothing: any message
breaks its blindness.

## Fabric: why and how

Not for speed: a pilot benchmark (2026-09, `pi --no-extensions` vs Fabric full
code mode, same model, 5 task types incl. three independent 30 s checks) showed
no gain in turns, wall-clock or cost; both parallelized on their own (bash `&`
vs `Promise.all`). Kept because it is installed and verified and gives the
oracle a checked child run: Herdr tab, model verified before the task is sent,
exact tool whitelist, `wait`/`stop`/status and `usage.cost`. Plain `pi -p
--model … --no-extensions --tools read,grep,find,ls` could do most of it (not
compared). Drop Fabric only if it causes problems.

In full code mode the model calls tools only through `fabric_exec`
(TypeScript `pi.*`, `agents.*`); you do not write this code, you use skills.

`fabric.json` sets `executor.shellHangMs: 0`: a nested `pi.bash` waits for the
command instead of returning `ok: true` + "still running" after 2 min, which
made joins over long tests look successful. Use `background: true` only for
intentionally unbounded processes. Needs a Pi restart.

Deferred until measured need: an async "operations" layer or scheduler,
automatic oracle routing (Jev), multi-oracle review, councils, another
subagent framework.

## Why the oracle skill looks like this

Verified against pi-fabric 0.93.1 (docs and tests inside Herdr):

- **Fuzzy model resolution.** A near-miss or missing key silently resolves to
  the closest model of that provider; an alias named like a canonical key wins.
  Hence exact registry keys, catalog preflight and `model === expected` checks
  on handle and result. Fabric also verifies the model before sending the task;
  residual races are accepted.
- **`tools` alone is not read-only.** Extension-enabled children keep
  `fabric_exec`. `extensions: false` + `read/grep/find/ls` left exactly those
  four; a write attempt failed.
- **`timeoutMs` cannot shorten a run** (values below `agents.timeoutMs`,
  default 24 h, are ignored). Hence a caller-side 20-minute race plus
  `agents.stop`. Caller abort detaches rather than stops a started child.
  `stop` returning is not proof the process died (`stopAcknowledged`).
- **Candidate-blind one-shot is the default, not a weak fallback.** The
  reviewer cannot anchor on a candidate it never saw; the residual bias is Main
  doing the comparison. A finished one-shot takes no second message, so a
  two-turn review would need a per-review actor; deferred until usage shows
  Main wrongly dismissing correct reviews.
- **`workspaceLabel`, not a fingerprint.** `HEAD` + hash of `git status` misses
  content changes in modified files and evidence outside the repo.
- **Provider choice is a disclosure decision.** Read-only prevents mutation,
  not sending evidence to that provider.
