#!/usr/bin/env python3
"""Log oracle consultations (gpt, gemini, deepseek skills), rate them after triage, show stats per model.

  oracle.py log --skill S --model M --status ok|error [--mode M] [--seconds N] [--prompt-chars N] [--answer-chars N]
  oracle.py rate ID useful|partial|useless [--findings N] [--accepted N] [--unique N] [--note TEXT]
  oracle.py stats [--days N]
  oracle.py recent [-n N]

Data: $ORACLE_LOG or ~/.local/state/oracle/log.jsonl (one JSON object per line; ratings are separate lines).
"""
import argparse, json, os, sys, time, uuid
from collections import defaultdict
from pathlib import Path

LOG = Path(os.environ.get("ORACLE_LOG", Path.home() / ".local/state/oracle/log.jsonl"))
VERDICTS = {"useful": 1.0, "partial": 0.5, "useless": 0.0}


def append(rec):
    LOG.parent.mkdir(parents=True, exist_ok=True)
    with LOG.open("a") as f:
        f.write(json.dumps(rec, ensure_ascii=False) + "\n")


def load():
    calls, ratings = {}, {}
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
    return calls, ratings


def cmd_log(a):
    cid = uuid.uuid4().hex[:8]
    append({"type": "call", "id": cid, "ts": int(time.time()), "skill": a.skill, "model": a.model,
            "mode": a.mode, "status": a.status, "seconds": a.seconds,
            "prompt_chars": a.prompt_chars, "answer_chars": a.answer_chars, "cwd": os.getcwd()})
    print(f"[oracle id: {cid}]", file=sys.stderr)


def cmd_rate(a):
    calls, _ = load()
    if a.id not in calls:
        sys.exit(f"Nieznane id: {a.id} (zobacz: oracle.py recent)")
    append({"type": "rating", "id": a.id, "ts": int(time.time()), "verdict": a.verdict,
            "findings": a.findings, "accepted": a.accepted, "unique": a.unique, "note": a.note})


def cmd_stats(a):
    calls, ratings = load()
    since = time.time() - a.days * 86400 if a.days else 0
    rows = defaultdict(lambda: defaultdict(float))
    for c in calls.values():
        if c["ts"] < since:
            continue
        s = rows[f'{c["skill"]}/{c["model"]}']
        s["calls"] += 1
        s["errors"] += c["status"] != "ok"
        if c["status"] == "ok" and c.get("seconds") is not None:
            s["ok"] += 1
            s["secs"] += c["seconds"]
        r = ratings.get(c["id"])
        if r:
            s["rated"] += 1
            s["score"] += VERDICTS[r["verdict"]]
            for k in ("findings", "accepted", "unique"):
                s[k] += r.get(k) or 0
    if not rows:
        print("Brak danych.")
        return
    hdr = f'{"skill/model":34} {"calls":>5} {"err":>4} {"avg s":>6} {"rated":>5} {"score":>6} {"acc/find":>9} {"unique":>6}'
    print(hdr + "\n" + "-" * len(hdr))
    for k, s in sorted(rows.items(), key=lambda kv: -(kv[1]["score"] / kv[1]["rated"] if kv[1]["rated"] else -1)):
        avg = f'{s["secs"] / s["ok"]:.0f}' if s["ok"] else "-"
        score = f'{s["score"] / s["rated"]:.2f}' if s["rated"] else "-"
        acc = f'{int(s["accepted"])}/{int(s["findings"])}' if s["findings"] else "-"
        print(f'{k:34} {int(s["calls"]):5} {int(s["errors"]):4} {avg:>6} {int(s["rated"]):5} {score:>6} {acc:>9} {int(s["unique"]):6}')
    print("\nscore: useful=1, partial=0.5, useless=0 (average of rated calls); "
          "unique: accepted findings nobody else (Claude, other oracles) had.")


def cmd_recent(a):
    calls, ratings = load()
    for c in sorted(calls.values(), key=lambda c: c["ts"])[-a.n:]:
        r = ratings.get(c["id"])
        rated = f'{r["verdict"]} {r.get("accepted") or 0}/{r.get("findings") or 0} u{r.get("unique") or 0}' if r else "unrated"
        when = time.strftime("%Y-%m-%d %H:%M", time.localtime(c["ts"]))
        print(f'{c["id"]}  {when}  {c["skill"] + "/" + c["model"]:34} {c["status"]:5}  {rated}  {Path(c.get("cwd") or "").name}')


def main():
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = p.add_subparsers(dest="cmd", required=True)
    l = sub.add_parser("log")
    l.add_argument("--skill", required=True); l.add_argument("--model", required=True)
    l.add_argument("--status", required=True, choices=["ok", "error"]); l.add_argument("--mode", default="")
    for k in ("--seconds", "--prompt-chars", "--answer-chars"):
        l.add_argument(k, type=int)
    r = sub.add_parser("rate")
    r.add_argument("id"); r.add_argument("verdict", choices=list(VERDICTS))
    for k in ("--findings", "--accepted", "--unique"):
        r.add_argument(k, type=int)
    r.add_argument("--note", default="")
    s = sub.add_parser("stats"); s.add_argument("--days", type=int)
    n = sub.add_parser("recent"); n.add_argument("-n", type=int, default=20)
    a = p.parse_args()
    {"log": cmd_log, "rate": cmd_rate, "stats": cmd_stats, "recent": cmd_recent}[a.cmd](a)


if __name__ == "__main__":
    main()
