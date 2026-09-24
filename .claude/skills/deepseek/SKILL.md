---
name: deepseek
description: Consult DeepSeek (external LLM) for a second opinion — use when the user asks to "ask/consult DeepSeek", "zapytaj DeepSeek", or wants an independent review of a plan, bug hypothesis, design or code snippet.
---

# Consulting DeepSeek

Run the helper script (stdlib Python, no deps):

```bash
~/.claude/skills/deepseek/ask_deepseek.py "question"                  # deepseek-flash = DeepSeek V4.1 (default)
~/.claude/skills/deepseek/ask_deepseek.py -m deepseek-v4-pro "q"     # older V4-Pro-0813, only if asked
~/.claude/skills/deepseek/ask_deepseek.py -f src/foo.py "Find bugs in this file"
git diff | ~/.claude/skills/deepseek/ask_deepseek.py -f - "Review this diff"   # stdin only via -f -
```

Options: `-f FILE` (repeatable; `-f -` = stdin, never read implicitly), `-s SYSTEM_PROMPT`, `--show-reasoning`, env `DEEPSEEK_MODEL`, `DEEPSEEK_BASE_URL`.
Answers can take a few minutes — use a Bash timeout of 600000.

Guidelines:
- DeepSeek has no context of this conversation: include the goal, relevant code and constraints in the prompt.
- Sending code sends it to DeepSeek's servers (China). Don't send secrets, credentials, or code the user marked as confidential; ask first if unsure.
- Treat the answer as a second opinion, not ground truth — verify claims, and tell the user where you agree/disagree.

API key: `DEEPSEEK_API_KEY` env var or `~/.config/deepseek/api_key` (gitignored in the home dotfiles repo — never put the key in a tracked file).
Model list: `curl -s https://api.deepseek.com/models -H "Authorization: Bearer $(cat ~/.config/deepseek/api_key)"` (names change over time).
