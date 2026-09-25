#!/usr/bin/env python3
"""Log oracle consultations (gpt, gemini, deepseek skills), rate them after triage, show stats per model.

  oracle.py log --skill S --model M --status ok|error [--effort E] [--mode M] [--seconds N] [--prompt-chars N]
                [--answer-chars N] [--usage JSON] [--usage-raw JSON]      (round id from $ORACLE_ROUND)
  oracle.py new-round                                                     # prints a round id for ORACLE_ROUND
  oracle.py rate ID useful|partial|useless [--findings N] [--accepted N] [--unique N] [--note TEXT]
  oracle.py self (--round R | --calls ID,ID) [--model M] [--findings N] [--accepted N] [--refuted N] [--unique N] [--missed N] [--note TEXT]
  oracle.py stats [--days N] [--pairs]
  oracle.py recent [-n N]

Data: $ORACLE_LOG or ~/.local/state/oracle/log.jsonl (one JSON object per line; ratings are separate lines).
Usage is normalized by the ask_* scripts: input includes cached, output includes reasoning (both are subsets).
"""
import argparse, json, math, os, sys, time, uuid
from collections import Counter, defaultdict
from pathlib import Path

LOG = Path(os.environ.get("ORACLE_LOG", Path.home() / ".local/state/oracle/log.jsonl"))
VERDICTS = {"useful": 1.0, "partial": 0.5, "useless": 0.0}
USAGE_KEYS = ("input", "cached", "output", "reasoning")


def append(rec):
    LOG.parent.mkdir(parents=True, exist_ok=True)
    with LOG.open("a") as f:
        f.write(json.dumps(rec, ensure_ascii=False) + "\n")


def load():
    calls, ratings, rounds = {}, {}, {}
    if LOG.exists():
        for line in LOG.read_text().splitlines():
            try:
                r = json.loads(line)
            except ValueError:
                continue
            if r.get("type") == "call":
                calls[r["id"]] = r
            elif r.get("type") == "rating":
                ratings[r["id"]] = r  # the last rating of an id wins
            elif r.get("type") == "self":
                rounds[r.get("round") or r["calls"]] = r  # re-logging the same round replaces it
    return calls, ratings, rounds


def label(c):
    """skill/model, plus @effort when set explicitly (gemini has it in the model id, deepseek has none), -r for repo mode.
    Old calls without effort and new ones without -e (@default) share a row."""
    effort = c.get("effort")
    return (f'{c["skill"]}/{c["model"]}' + (f"@{effort}" if effort and effort != "default" else "")
            + (" -r" if c.get("mode") == "repo" else ""))


def out_tokens(c):
    return (c.get("usage") or {}).get("output")


def ktok(n):
    return "-" if n is None else f"{n / 1000:.1f}k" if n >= 1000 else str(round(n))


def cmd_log(a):
    cid = uuid.uuid4().hex[:8]
    rec = {"type": "call", "id": cid, "ts": int(time.time()), "skill": a.skill, "model": a.model,
           "effort": a.effort, "mode": a.mode, "status": a.status, "seconds": a.seconds,
           "prompt_chars": a.prompt_chars, "answer_chars": a.answer_chars, "cwd": os.getcwd()}
    if os.environ.get("ORACLE_ROUND"):
        rec["round"] = os.environ["ORACLE_ROUND"]
    for key, raw in (("usage", a.usage), ("usage_raw", a.usage_raw)):
        try:
            val = json.loads(raw) if raw else None
        except ValueError:
            val = None  # a broken usage blob must not lose the call
        if key == "usage" and isinstance(val, dict):
            val = {k: int(val[k]) for k in USAGE_KEYS if isinstance(val.get(k), (int, float))} or None
        if val:
            rec[key] = val
    append(rec)
    print(f"[oracle id: {cid}]", file=sys.stderr)


def cmd_new_round(a):
    print(time.strftime("%Y%m%d-%H%M%S-") + uuid.uuid4().hex[:4])


def check_counts(a, keys):
    vals = {k: getattr(a, k) for k in keys}
    if any(v is not None and v < 0 for v in vals.values()):
        sys.exit("Liczby nie mogą być ujemne")
    f, acc, u = vals.get("findings"), vals.get("accepted"), vals.get("unique")
    if f is not None and acc is not None and acc > f:
        sys.exit(f"accepted ({acc}) > findings ({f})")
    if acc is not None and u is not None and u > acc:
        sys.exit(f"unique ({u}) > accepted ({acc})")


def cmd_rate(a):
    calls, _, _ = load()
    if a.id not in calls:
        sys.exit(f"Nieznane id: {a.id} (zobacz: oracle.py recent)")
    check_counts(a, ("findings", "accepted", "unique"))
    append({"type": "rating", "id": a.id, "ts": int(time.time()), "verdict": a.verdict,
            "findings": a.findings, "accepted": a.accepted, "unique": a.unique, "note": a.note})


def cmd_self(a):
    calls, _, _ = load()
    if a.round:
        ids = sorted(i for i, c in calls.items() if c.get("round") == a.round)
        if not ids:
            sys.exit(f"Brak wywołań w rundzie {a.round}")
    else:
        ids = sorted({i.strip() for i in a.calls.split(",") if i.strip()})
        unknown = [i for i in ids if i not in calls]
        if not ids or unknown:
            sys.exit(f"Nieznane id: {unknown or '(brak)'} (zobacz: oracle.py recent)")
    check_counts(a, ("findings", "accepted", "unique"))
    rec = {"type": "self", "calls": ",".join(ids), "ts": int(time.time()), "model": a.model,
           "findings": a.findings, "accepted": a.accepted, "refuted": a.refuted,
           "unique": a.unique, "missed": a.missed, "note": a.note}
    if a.round:
        rec["round"] = a.round
    append(rec)


def cmd_stats(a):
    calls, ratings, rounds = load()
    since = time.time() - a.days * 86400 if a.days else 0
    calls = {i: c for i, c in calls.items() if c["ts"] >= since}
    rows = defaultdict(lambda: defaultdict(float))
    for c in calls.values():
        s = rows[label(c)]
        s["calls"] += 1
        s["errors"] += c["status"] != "ok"
        if c["status"] == "ok" and c.get("seconds") is not None:
            s["ok"] += 1
            s["secs"] += c["seconds"]
        if c["status"] == "ok" and out_tokens(c) is not None:
            s["used"] += 1
            s["out"] += out_tokens(c)
        r = ratings.get(c["id"])
        if r:
            s["rated"] += 1
            s["score"] += VERDICTS[r["verdict"]]
            # Missing counts stay missing: acc/find only over ratings that have both.
            if r.get("findings") is not None and r.get("accepted") is not None:
                s["findings"] += r["findings"]
                s["accepted"] += r["accepted"]
            s["unique"] += r.get("unique") or 0
    if not rows:
        print("Brak danych.")
        return
    hdr = (f'{"skill/model":34} {"calls":>5} {"err":>4} {"avg s":>6} {"rated":>5} {"score":>6} {"acc/find":>9} '
           f'{"unique":>6} {"out/call":>8}')
    print(hdr + "\n" + "-" * len(hdr))
    for k, s in sorted(rows.items(), key=lambda kv: -(kv[1]["score"] / kv[1]["rated"] if kv[1]["rated"] else -1)):
        avg = f'{s["secs"] / s["ok"]:.0f}' if s["ok"] else "-"
        score = f'{s["score"] / s["rated"]:.2f}' if s["rated"] else "-"
        acc = f'{int(s["accepted"])}/{int(s["findings"])}' if s["findings"] else "-"
        out = ktok(s["out"] / s["used"]) if s["used"] else "-"
        print(f'{k:34} {int(s["calls"]):5} {int(s["errors"]):4} {avg:>6} {int(s["rated"]):5} {score:>6} {acc:>9} '
              f'{int(s["unique"]):6} {out:>8}')
    print("\nscore: useful=1, partial=0.5, useless=0 (average of rated calls); "
          "unique: accepted findings nobody else (Claude, other oracles) had — depends on who else was asked;\n"
          "out/call: mean output tokens incl. reasoning, over ok calls with usage (older calls have none).")
    if a.pairs:
        print_pairs(calls, ratings)
    selves = defaultdict(lambda: defaultdict(float))
    for rd in rounds.values():
        if rd["ts"] < since:
            continue
        s = selves[rd.get("model") or "claude"]
        s["rounds"] += 1
        for k in ("findings", "accepted", "refuted", "unique", "missed"):
            s[k] += rd.get(k) or 0
    if selves:
        hdr = f'{"coordinator":34} {"rounds":>6} {"acc/find":>9} {"refuted":>7} {"unique":>6} {"missed":>6} {"recall":>6}'
        print("\n" + hdr + "\n" + "-" * len(hdr))
        for k, s in sorted(selves.items()):
            acc = f'{int(s["accepted"])}/{int(s["findings"])}' if s["findings"] else "-"
            known = s["accepted"] + s["missed"]
            recall = f'{s["accepted"] / known:.2f}' if known else "-"
            print(f'{k:34} {int(s["rounds"]):6} {acc:>9} {int(s["refuted"]):7} {int(s["unique"]):6} {int(s["missed"]):6} {recall:>6}')
        print("\nrefuted: Claude's own claims disproved (by an oracle or verification); missed: accepted oracle findings "
              "Claude did not have; recall: accepted / (accepted + missed), i.e. against findings anyone discovered.")


def print_pairs(calls, ratings):
    """Token efficiency per model, and paired token ratios within rounds (same prompt, mode and effort)."""
    usable = [c for c in calls.values() if c["status"] == "ok" and out_tokens(c)]
    eff = defaultdict(lambda: defaultdict(float))
    for c in usable:
        r = ratings.get(c["id"])
        if r and r.get("accepted") is not None:
            s = eff[label(c)]
            s["n"] += 1
            s["out"] += out_tokens(c)
            s["accepted"] += r["accepted"]
            s["score"] += VERDICTS[r["verdict"]]
    print(f'\n{"efficiency (rated, with usage)":34} {"n":>3} {"out tok":>8} {"acc/1M out":>10} {"score/100k":>10}')
    print("-" * 69)
    if not eff:
        print("(no rated calls with token usage yet)")
    for k, s in sorted(eff.items()):
        print(f'{k:34} {int(s["n"]):3} {ktok(s["out"]):>8} {1e6 * s["accepted"] / s["out"]:10.1f} '
              f'{1e5 * s["score"] / s["out"]:10.2f}')
    print("sums over calls, not means of per-call ratios; n<5 is anecdotal.")

    # Pair each model with its vendor's reference: the model in most shared rounds of that skill/mode/effort
    # (ties: alphabetical, so astra for gpt).
    groups = defaultdict(lambda: defaultdict(dict))  # (skill, mode, effort) -> round -> model -> call
    for c in usable:
        if c.get("round"):
            groups[(c["skill"], c.get("mode") or "", c.get("effort") or "")][c["round"]][c["model"]] = c
    lines = []
    for (skill, mode, effort), by_round in sorted(groups.items()):
        seen = Counter(m for models in by_round.values() if len(models) > 1 for m in models)
        if not seen:
            continue
        ref = max(sorted(seen), key=seen.get)
        for model in sorted(seen):
            if model == ref:
                continue
            logs, wtl = [], [0, 0, 0]
            for models in by_round.values():
                if model in models and ref in models:
                    m, r = models[model], models[ref]
                    logs.append(math.log(out_tokens(m) / out_tokens(r)))
                    rm, rr = ratings.get(m["id"]), ratings.get(r["id"])
                    if rm and rr:
                        d = VERDICTS[rm["verdict"]] - VERDICTS[rr["verdict"]]
                        wtl[0 if d > 0 else 1 if d == 0 else 2] += 1
            if logs:
                geo = math.exp(sum(logs) / len(logs))
                spread = f"{math.exp(min(logs)):.2f}–{math.exp(max(logs)):.2f}" if len(logs) > 1 else ""
                cfg = "/".join(x for x in (effort, "-r" if mode == "repo" else "") if x)
                lines.append(f'{skill} {model} vs {ref}{" [" + cfg + "]" if cfg else ""}: {len(logs)} rounds · '
                             f'out tokens {geo:.2f}× {spread} · score W/T/L {wtl[0]}/{wtl[1]}/{wtl[2]}')
    print("\npaired within rounds (geometric mean of per-round output-token ratios)")
    print("\n".join(lines) if lines else "(no rounds with token usage yet; set ORACLE_ROUND for parallel calls)")


def cmd_recent(a):
    calls, ratings, rounds = load()
    for c in sorted(calls.values(), key=lambda c: c["ts"])[-a.n:]:
        r = ratings.get(c["id"])
        rated = f'{r["verdict"]} {r.get("accepted") or 0}/{r.get("findings") or 0} u{r.get("unique") or 0}' if r else "unrated"
        when = time.strftime("%Y-%m-%d %H:%M", time.localtime(c["ts"]))
        print(f'{c["id"]}  {when}  {label(c):34} {c["status"]:5}  {ktok(out_tokens(c)):>6}  {rated}  '
              f'{Path(c.get("cwd") or "").name}')
    covered = {i for rd in rounds.values() for i in rd["calls"].split(",")}
    todo = sorted(i for i in calls if i not in covered and i in ratings)
    if todo:
        print(f"\nrated calls without a coordinator (self) entry: {', '.join(todo)}")


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = p.add_subparsers(dest="cmd", required=True)
    l = sub.add_parser("log")
    l.add_argument("--skill", required=True); l.add_argument("--model", required=True)
    l.add_argument("--status", required=True, choices=["ok", "error"]); l.add_argument("--mode", default="")
    l.add_argument("--effort", default="")
    for k in ("--seconds", "--prompt-chars", "--answer-chars"):
        l.add_argument(k, type=int)
    l.add_argument("--usage", default="", help='normalized JSON: {"input","cached","output","reasoning"}')
    l.add_argument("--usage-raw", default="", help="the provider's usage object, kept for later")
    sub.add_parser("new-round")
    r = sub.add_parser("rate")
    r.add_argument("id"); r.add_argument("verdict", choices=list(VERDICTS))
    for k in ("--findings", "--accepted", "--unique"):
        r.add_argument(k, type=int)
    r.add_argument("--note", default="")
    c = sub.add_parser("self")
    g = c.add_mutually_exclusive_group(required=True)
    g.add_argument("--calls"); g.add_argument("--round")
    c.add_argument("--model", default="claude")
    for k in ("--findings", "--accepted", "--refuted", "--unique", "--missed"):
        c.add_argument(k, type=int)
    c.add_argument("--note", default="")
    s = sub.add_parser("stats"); s.add_argument("--days", type=int)
    s.add_argument("--pairs", action="store_true", help="token efficiency and paired within-round comparisons")
    n = sub.add_parser("recent"); n.add_argument("-n", type=int, default=20)
    a = p.parse_args()
    {"log": cmd_log, "new-round": cmd_new_round, "rate": cmd_rate, "self": cmd_self, "stats": cmd_stats,
     "recent": cmd_recent}[a.cmd](a)


if __name__ == "__main__":
    main()
