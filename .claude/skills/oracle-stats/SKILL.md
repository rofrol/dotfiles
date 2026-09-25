---
name: oracle-stats
description: Statistics of consulted oracle models (gpt: astra/sol/terra, gemini flash, deepseek skills) — which were most useful. Use when the user asks for oracle/model statistics ("statystyki oracle", "który model najlepszy"), or to rate a past consultation.
---

# Oracle statistics

The gpt, gemini and deepseek scripts log every call to `~/.local/state/oracle/log.jsonl` (skill, model, effort, mode, status,
seconds, prompt/answer size, cwd, round id from `$ORACLE_ROUND`, token usage) and print `[oracle id: XXXXXXXX]` on stderr. Usefulness comes from ratings:

```bash
O=~/.claude/skills/oracle-stats/oracle.py
$O rate <id> useful|partial|useless [--findings N] [--accepted N] [--unique N] [--note "..."]
$O new-round            # round id: export ORACLE_ROUND=$($O new-round) before launching a round's oracles
$O self --round <round> --model <your model id> --findings N --accepted N --refuted N --unique N --missed N [--note "..."]
$O stats [--days 30]     # per skill/model + per coordinator (Claude): rounds, accepted/findings, refuted, unique, missed, recall
$O stats --pairs         # + token efficiency (acc/1M output tokens) and paired within-round token ratios (e.g. sol vs astra)
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

## Claude as coordinator

Claude is scored too, once per round (all oracle calls on the same question), with `self`:
- **Before reading any oracle answer**, write down your own findings/hypotheses (in the conversation or a scratchpad
  file). Counting them afterwards is biased — oracles' answers leak into what you "already knew".
- After triage: `--findings` your own claims; `--accepted` how many survived verification; `--refuted` your claims
  disproved by an oracle or by verification (your errors); `--unique` accepted ones no oracle had; `--missed` accepted
  oracle findings you did not have. `--model` = your exact model id (e.g. claude-opus-5-5).
- `--note`: what you got wrong or missed (e.g. "assumed MBID stable across releases; missed video recordings").
- Log it even for a single-oracle round. Re-running `self` with the same round (or calls) replaces the entry.
  `recent` lists rated calls that have no coordinator entry yet.

## Tokens

Usage is normalized across vendors: `input` includes `cached`, `output` includes `reasoning` (GPT's
`reasoning_output_tokens`, Gemini's `thinking_tokens`, DeepSeek's `reasoning_tokens`); the provider's own object is kept
as `usage_raw`. Codex and agy add ~10k input tokens of their own system prompt, so compare output tokens.
When reading `stats --pairs`: compare tokens only within one vendor (tokenizers differ), trust paired rounds over
per-model sums, and remember `unique` depends on who else was asked (a model asked alone gets everything as unique).
Calls before 2026-09-25 have no usage.
