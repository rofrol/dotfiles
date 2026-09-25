---
name: gemini
description: Consult Google Gemini (Gemini 3.1 Pro, Gemini 3.8 Flash) via Antigravity CLI (agy) + Google AI subscription for a second opinion — use when the user asks to "ask/consult Gemini", "zapytaj Gemini/Gemini'ego", or wants an independent review of a plan, bug hypothesis, design or code snippet from Google.
---

# Consulting Gemini

Goes through Antigravity CLI (`agy -p`, headless), billed to the user's Google AI subscription — not the API.
Uses agy's own login (`~/.gemini/antigravity-cli/`). Gemini CLI (`gemini`) no longer serves AI Pro/Ultra accounts; don't use it.

Models on the plan (as of 2026-09-25, check with `agy models`): **gemini-3.1-pro-{high,low}** (Pro flagship; 3.5 Pro not out),
**gemini-3.8-flash-{high,medium,low}** (newest Flash). When a newer Pro/Flash appears there, update the `case` in ask_gemini.sh.

```bash
~/.claude/skills/gemini/ask_gemini.sh "question"                   # gemini-3.8-flash-high (default)
~/.claude/skills/gemini/ask_gemini.sh -m pro "q"                   # gemini-3.1-pro-high: only for hard questions
~/.claude/skills/gemini/ask_gemini.sh -m flash -e low "q"          # flash effort: low|medium|high; pro: low|high
~/.claude/skills/gemini/ask_gemini.sh -f src/foo.py "Find bugs in this file"
git diff | ~/.claude/skills/gemini/ask_gemini.sh -f - "Review this diff"   # stdin only via -f -
~/.claude/skills/gemini/ask_gemini.sh -r "Review ... (see Code review below)"  # run in the current git repo
```

Options: `-m pro|flash|<full id>`, `-e low|medium|high`, `-f FILE` (repeatable; `-f -` = stdin, never read implicitly), `-r` (repo mode), env `GEMINI_MODEL`.
The prompt goes to agy via stdin (stream-json), so large diffs are fine.

By default agy runs in an empty temp dir and sees only what you put in the prompt; it's told not to use tools.
With `-r` it runs at the top of the current git repo (refuses `$HOME`) and can view, list and search files itself;
everything it reads (including untracked files like `.env`) goes to Google, and agy loads the repo's AGENTS.md as rules.
The default model is **flash**: Flash and Pro share one weekly quota consumed by token cost, so Pro drains it
much faster (check with `agy -p /quota`). Use `-m pro` only when Flash's answer is not good enough. In `-r` stay
on Flash anyway: in agy, Pro has only `view_file` (no listing/grep) and guesses instead of searching.

Headless limits (agy 1.2.x):
- Writes are auto-denied, and so are shell commands except those in `permissions.allow` of
  `~/.gemini/antigravity-cli/settings.json` (e.g. `git status`, `find`, `odin`) — those *can* run in `-r` mode.
- A denied tool call ends the run with an empty answer; the script then exits 1 with "Pusta odpowiedź".
  No shell means no `git log/diff` in `-r`: pass the diff with `-f -` alongside `-r`.
- Conversations are saved in agy's history (no ephemeral mode).
Answers usually take seconds to a couple of minutes — use a Bash timeout of 600000.

Guidelines:
- Gemini has no context of this conversation: include the goal, relevant code and constraints in the prompt.
- Sending code sends it to Google's servers. Don't send secrets, credentials, or code the user marked as confidential; ask first if unsure.
- Treat the answer as a second opinion, not ground truth — verify claims, and tell the user where you agree/disagree.
- If the user asks for several models ("Gemini i GPT", "wszystkie"), run them in parallel and compare.
- On a quota/usage-limit error, tell the user (subscription limits), don't retry in a loop.
- After triaging the answer, rate it (id is printed on stderr as `[oracle id: ...]`):
  `~/.claude/skills/oracle-stats/oracle.py rate <id> useful|partial|useless --findings N --accepted N --unique N --note "..."`
  — see the oracle-stats skill for what the fields mean.

## Code review

For reviewing changes in a repo use `-r` plus the diff on stdin, so the reviewer can check callers and tests itself:

```bash
git diff <base>...HEAD | ~/.claude/skills/gemini/ask_gemini.sh -r -f - "$(cat <<'EOF'
Independently review this change (diff below) for actionable correctness, security and regression bugs.
Intent / acceptance criteria: ...
Constraints: ...
Tests actually run: ...
Inspect the relevant repository context (callers, tests) with your file tools.
Report only problems introduced by this change. For each: file:line, trigger, impact, evidence.
Separate demonstrated bugs from unverified concerns. "No actionable findings" is a valid answer.
EOF
)"
```

Give intent, not a summary of the code. Review consequential changes (auth, migrations, concurrency, data integrity,
public APIs, uncertain diagnoses), not routine edits. Each review is a fresh session.

After the review:
- Triage every finding as accepted / rejected / needs user decision. Reject only with concrete evidence
  (counterexample, invariant, code path), not "I disagree". Before fixing an alleged bug, trace it and preferably
  reproduce it or add a regression test.
- Show the user the triage. Questions of intent, scope and tradeoffs are the user's call.
- At most one fix round plus one focused re-check. If serious issues remain, stop and ask the user.
