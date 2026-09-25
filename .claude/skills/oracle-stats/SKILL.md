---
name: oracle-stats
description: Statistics of consulted oracle models (gpt, gemini, deepseek skills) — which were most useful. Use when the user asks for oracle/model statistics ("statystyki oracle", "który model najlepszy"), or to rate a past consultation.
---

# Oracle statistics

The gpt, gemini and deepseek scripts log every call to `~/.local/state/oracle/log.jsonl` (skill, model, mode, status,
seconds, prompt/answer size, cwd) and print `[oracle id: XXXXXXXX]` on stderr. Usefulness comes from ratings:

```bash
O=~/.claude/skills/oracle-stats/oracle.py
$O rate <id> useful|partial|useless [--findings N] [--accepted N] [--unique N] [--note "..."]
$O stats [--days 30]     # per skill/model: calls, errors, avg seconds, rated, score, accepted/findings, unique
$O recent [-n 20]        # latest calls with their ids and ratings (find unrated ones)
```

Rate after triaging the answer, not on first read:
- **useful**: changed what we did (a real bug, a better design, a disproved hypothesis); **partial**: something valid but
  minor or already known; **useless**: nothing actionable, wrong, or no answer.
- `--findings`: distinct claims/issues raised; `--accepted`: how many survived verification;
  `--unique`: accepted ones that neither Claude nor another oracle in the same round had. `unique` is the key signal.
- Be honest and consistent across models; don't upgrade a verdict because the model agreed with you.
- `--note`: a few words on why (e.g. "caught race in cache invalidation", "hallucinated API").

When showing stats, point out small samples (<5 rated calls per model) instead of drawing conclusions from them.
