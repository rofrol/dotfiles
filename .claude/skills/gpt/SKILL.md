---
name: gpt
description: Consult OpenAI GPT (GPT-6 Astra/Sol/Luna, GPT-5.6 Terra) via Codex CLI + ChatGPT subscription for a second opinion — use when the user asks to "ask/consult GPT", "zapytaj GPT/Astrę/Sol/Terrę/Lunę", or wants an independent review of a plan, bug hypothesis, design or code snippet from OpenAI.
---

# Consulting GPT

Goes through Codex CLI (`codex exec`), billed to the user's ChatGPT Plus subscription — not the API.
Credentials come from pi: `openai-codex` in `~/.pi/agent/auth.json` (token via `pi auth print-bearer-token`, which refreshes it).
~/.codex (config.toml, auth.json) is not used — the script runs with `--ignore-user-config` and a temporary `CODEX_HOME`.

Models available on ChatGPT (as of 2026-09-25): `astra`/`sol`/`luna` = **gpt-6-***; `terra` = **gpt-5.6-terra**
(no GPT-6 Terra yet). Check with `jq -r '.models[].slug' ~/.codex/models_cache.json`; when gpt-6-terra appears there,
update the `case` in ask_gpt.sh.

```bash
~/.claude/skills/gpt/ask_gpt.sh "question"                  # astra = gpt-6-astra (default)
~/.claude/skills/gpt/ask_gpt.sh -e high "hard question"     # more reasoning, uses more of the Plus limit
~/.claude/skills/gpt/ask_gpt.sh -m sol "q"                  # gpt-6-sol; also terra (gpt-5.6-terra), luna (gpt-6-luna, fast)
~/.claude/skills/gpt/ask_gpt.sh -f src/foo.py "Find bugs in this file"
git diff | ~/.claude/skills/gpt/ask_gpt.sh -f - "Review this diff"   # stdin only via -f -
~/.claude/skills/gpt/ask_gpt.sh -r "Review ... (see Code review below)"  # run in the current git repo, read-only
```

Options: `-m astra|sol|terra|luna|<full id>`, `-e low|medium|high|xhigh` (reasoning effort), `-f FILE` (repeatable; `-f -` = stdin, never read implicitly), `-r` (repo mode), env `GPT_MODEL`.
Codex runs ephemeral, in a read-only sandbox. By default it runs in an empty temp dir and sees only what you put in the prompt.
With `-r` it runs at the top of the current git repo (refuses `$HOME`), so it can read files, callers, tests and git history itself;
everything in that checkout it reads (including untracked files like `.env`) goes to OpenAI.
Answers can take a few minutes — use a Bash timeout of 600000.

Inside herdr the script runs in its own herdr-job tab (no notification) so the user can watch it; output and exit code are unchanged.

Guidelines:
- GPT has no context of this conversation: include the goal, relevant code and constraints in the prompt.
- Sending code sends it to OpenAI's servers. Don't send secrets, credentials, or code the user marked as confidential; ask first if unsure.
- Treat the answer as a second opinion, not ground truth — verify claims, and tell the user where you agree/disagree.
- Data collection for oracle-stats: whenever you consult GPT, ask **astra, sol and terra in parallel** with the same
  prompt and effort (unless the user named one model), compare them, and rate each call separately — `--unique` counts
  what the other two (and Claude) missed. Luna only on request.
- If the user asks for "GPT and DeepSeek", run both in parallel and compare.
- On a usage-limit error, tell the user (Plus limits), don't retry in a loop.
- After triaging the answer, rate it (id is printed on stderr as `[oracle id: ...]`):
  `~/.claude/skills/oracle-stats/oracle.py rate <id> useful|partial|useless --findings N --accepted N --unique N --note "..."`
  — see the oracle-stats skill for what the fields mean. Then score yourself for the round with `oracle.py self`
  (write your own findings down before reading the answers).

## Code review

Use `-r` for reviewing changes in a repo, so the reviewer gathers evidence itself instead of seeing only what you picked.
Review consequential changes (auth, migrations, concurrency, data integrity, public APIs/protocols, unfamiliar code,
uncertain diagnoses), not routine edits. For expensive or hard-to-reverse designs, review the plan before implementing.

Give intent, not a summary or selection of the code. Prompt template:

```text
Independently review this change for actionable correctness, security and regression bugs.
Intent / acceptance criteria: ...
Constraints: ...
Scope: base <SHA>, head <SHA> (plus staged/unstaged changes, if intended)
Tests actually run: ...
Inspect the diff and the relevant repository context (callers, tests, history).
Report only problems introduced by this change. For each: file:line, trigger, impact, evidence.
Separate demonstrated bugs from unverified concerns. "No actionable findings" is a valid answer.
Do not edit files.
```

Use explicit SHAs: `master...HEAD` excludes uncommitted changes.
Each review is a fresh session; don't carry a reviewer across tasks.

After the review:
- Triage every finding as accepted / rejected / needs user decision. Reject only with concrete evidence
  (counterexample, invariant, code path), not "I disagree". Before fixing an alleged bug, trace it and preferably
  reproduce it or add a regression test.
- Show the user the triage. Questions of intent, scope and tradeoffs are the user's call; present both positions
  and a recommendation instead of arguing with the reviewer.
- At most one fix round plus one focused re-check of the disputed findings and the fixes. If serious issues remain,
  stop and ask the user; the plan is probably wrong.
