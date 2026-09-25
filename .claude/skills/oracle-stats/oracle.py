#!/usr/bin/env python3
"""Log oracle consultations (gpt, gemini, deepseek skills), rate them after triage, show stats per model.

  oracle.py log --skill S --model M --status ok|error [--mode M] [--seconds N] [--prompt-chars N] [--answer-chars N]
  oracle.py rate ID useful|partial|useless [--findings N] [--accepted N] [--unique N] [--note TEXT]
  oracle.py self --calls ID,ID [--model M] [--findings N] [--accepted N] [--refuted N] [--unique N] [--missed N] [--note TEXT]
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
                rounds[r["calls"]] = r  # re-logging the same round replaces it
    return calls, ratings, rounds


def cmd_log(a):
    cid = uuid.uuid4().hex[:8]
    append({"type": "call", "id": cid, "ts": int(time.time()), "skill": a.skill, "model": a.model,
            "mode": a.mode, "status": a.status, "seconds": a.seconds,
            "prompt_chars": a.prompt_chars, "answer_chars": a.answer_chars, "cwd": os.getcwd()})
    print(f"[oracle id: {cid}]", file=sys.stderr)


def cmd_rate(a):
    calls, _, _ = load()
    if a.id not in calls:
        sys.exit(f"Nieznane id: {a.id} (zobacz: oracle.py recent)")
    append({"type": "rating", "id": a.id, "ts": int(time.time()), "verdict": a.verdict,
            "findings": a.findings, "accepted": a.accepted, "unique": a.unique, "note": a.note})


def cmd_self(a):
    calls, _, _ = load()
    ids = sorted({i for i in a.calls.split(",") if i})
    unknown = [i for i in ids if i not in calls]
    if not ids or unknown:
        sys.exit(f"Nieznane id: {unknown or '(brak)'} (zobacz: oracle.py recent)")
    append({"type": "self", "calls": ",".join(ids), "ts": int(time.time()), "model": a.model,
            "findings": a.findings, "accepted": a.accepted, "refuted": a.refuted,
            "unique": a.unique, "missed": a.missed, "note": a.note})


def cmd_stats(a):
    calls, ratings, rounds = load()
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
              "Claude did not have; recall: accepted / (accepted + missed).")


def cmd_recent(a):
    calls, ratings, rounds = load()
    for c in sorted(calls.values(), key=lambda c: c["ts"])[-a.n:]:
        r = ratings.get(c["id"])
        rated = f'{r["verdict"]} {r.get("accepted") or 0}/{r.get("findings") or 0} u{r.get("unique") or 0}' if r else "unrated"
        when = time.strftime("%Y-%m-%d %H:%M", time.localtime(c["ts"]))
        print(f'{c["id"]}  {when}  {c["skill"] + "/" + c["model"]:34} {c["status"]:5}  {rated}  {Path(c.get("cwd") or "").name}')
    covered = {i for k in rounds for i in k.split(",")}
    todo = sorted(i for i in calls if i not in covered and i in ratings)
    if todo:
        print(f"\nrated calls without a coordinator (self) entry: {', '.join(todo)}")


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
    c = sub.add_parser("self")
    c.add_argument("--calls", required=True); c.add_argument("--model", default="claude")
    for k in ("--findings", "--accepted", "--refuted", "--unique", "--missed"):
        c.add_argument(k, type=int)
    c.add_argument("--note", default="")
    s = sub.add_parser("stats"); s.add_argument("--days", type=int)
    n = sub.add_parser("recent"); n.add_argument("-n", type=int, default=20)
    a = p.parse_args()
    {"log": cmd_log, "rate": cmd_rate, "self": cmd_self, "stats": cmd_stats, "recent": cmd_recent}[a.cmd](a)


if __name__ == "__main__":
    main()
