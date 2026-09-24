---
name: gpt
description: Consult OpenAI GPT (GPT-6 Astra; GPT-5.6 Sol/Terra/Luna) via Codex CLI + ChatGPT subscription for a second opinion — use when the user asks to "ask/consult GPT", "zapytaj GPT/Astrę/Sol/Lunę", or wants an independent review of a plan, bug hypothesis, design or code snippet from OpenAI.
---

# Consulting GPT

Goes through Codex CLI (`codex exec`), billed to the user's ChatGPT Plus subscription — not the API.
Credentials come from pi: `openai-codex` in `~/.pi/agent/auth.json` (token via `pi auth print-bearer-token`, which refreshes it).
~/.codex (config.toml, auth.json) is not used — the script runs with `--ignore-user-config` and a temporary `CODEX_HOME`.

Models available on ChatGPT (as of 2026-09-24): only **gpt-6-astra** is GPT-6. GPT-6 Sol/Luna are API-only so far —
`sol`/`luna`/`terra` map to older **gpt-5.6-*** models. Check with `jq -r '.models[].slug' ~/.codex/models_cache.json`;
when gpt-6-sol/luna appear there, update the `case` in ask_gpt.sh.

```bash
~/.claude/skills/gpt/ask_gpt.sh "question"                  # astra = gpt-6-astra (default)
~/.claude/skills/gpt/ask_gpt.sh -e high "hard question"     # more reasoning, uses more of the Plus limit
~/.claude/skills/gpt/ask_gpt.sh -m luna "q"                 # gpt-5.6-luna: older, fast; also sol, terra
~/.claude/skills/gpt/ask_gpt.sh -f src/foo.py "Find bugs in this file"
git diff | ~/.claude/skills/gpt/ask_gpt.sh -f - "Review this diff"   # stdin only via -f -
```

Options: `-m astra|sol|terra|luna|<full id>`, `-e low|medium|high|xhigh` (reasoning effort), `-f FILE` (repeatable; `-f -` = stdin, never read implicitly), env `GPT_MODEL`.
Codex runs ephemeral, read-only sandbox, in an empty temp dir — it sees only what you put in the prompt.
Answers can take a few minutes — use a Bash timeout of 600000.

Guidelines:
- GPT has no context of this conversation: include the goal, relevant code and constraints in the prompt.
- Sending code sends it to OpenAI's servers. Don't send secrets, credentials, or code the user marked as confidential; ask first if unsure.
- Treat the answer as a second opinion, not ground truth — verify claims, and tell the user where you agree/disagree.
- If the user asks for "GPT and DeepSeek", run both in parallel and compare.
- On a usage-limit error, tell the user (Plus limits), don't retry in a loop.
