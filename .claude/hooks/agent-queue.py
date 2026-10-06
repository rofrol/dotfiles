#!/usr/bin/env python3
"""Claude Code hooks for the agent queue (see ~/scripts/aq).

  Stop        -> agent-queue.py stop
  PreToolUse  -> agent-queue.py guard   (Bash|Edit|Write|MultiEdit|NotebookEdit)

stop acts only in a worker session, one started by `aq worker`, which sets
AQ_WORKER to the project; every other session stops as usual. A worker that
still has an item in progress must report it before it may stop; a worker
without one gets the next ready item with the instructions below. After three
blocked stops on the same unreported item the stop is let through, so a stuck
model cannot loop forever; the item stays in progress for the user to see.

guard stops every session, worker or not, from writing the queue files or
working around aq's refusal to add items inside an agent session: an item is
the user's approval, so only the user writes it.
"""

import json
import os
import re
import subprocess
import sys
from pathlib import Path

STATE = Path(os.environ.get("XDG_STATE_HOME", Path.home() / ".local" / "state")) / "agent-queue"
AQ = str(Path.home() / "scripts" / "aq")
MAX_REMINDERS = 3

INSTRUCTIONS = """Next item from the agent queue of {project}: {id}

{text}

The user added this item, which approves this work and its commits; the
project's other rules (AGENTS.md) still apply.

1. Check first. Compare the item with the repository's current state, its
   TODO and decision records, and the other open items (`aq status -p
   {project}`). If it is already done, outdated, contradicts another item or
   a decision, or needs a decision only the user can make, do not do it:
   report it blocked with the reason and what you need.
2. Do the work. Every commit for it carries the trailer `Queue-Item: {id}`.
3. If you cannot finish (permissions, access, a login, a missing tool),
   report it blocked with what you need from the user.
4. Report before you stop:
   aq report {id} done --notes "..."      or      aq report {id} blocked --notes "..."
   The notes are what you would otherwise tell the user at the end: remarks,
   open questions, suggested follow-ups, files left uncommitted. The user
   reads them later in `aq status`; do not wait for an answer.
Then end your turn; the next item follows."""


def aq(*args: str) -> str:
    return subprocess.run([sys.executable, AQ, *args], capture_output=True, text=True).stdout.strip()


def block(reason: str) -> None:
    print(json.dumps({"decision": "block", "reason": reason}))


def stop(payload: dict) -> None:
    project = os.environ.get("AQ_WORKER")
    session = payload.get("session_id", "")
    if not project or not session:
        return
    reminders = STATE / f"reminders-{session}"
    current = aq("current", "--session", session)
    if current:
        item = json.loads(current)
        count = int(reminders.read_text() or 0) + 1 if reminders.exists() else 1
        if count > MAX_REMINDERS:
            return
        reminders.write_text(str(count))
        block(f"{item['id']} is still in progress. Report it before you stop: "
              f"aq report {item['id']} done|blocked --notes \"...\" (remarks, questions, follow-ups, leftovers).")
        return
    reminders.unlink(missing_ok=True)
    taken = aq("take", "-p", project, "--session", session)
    if taken:
        item = json.loads(taken)
        block(INSTRUCTIONS.format(project=project, id=item["id"], text=item["text"]))


QUEUE_FILE = re.compile(r"agent-queue/\S*\.(jsonl|lock)")
WRITES = re.compile(r">|\btee\b|sed\s+-i|\b(rm|mv|cp|truncate|python3?|perl|ruby|node)\b")
# aq add itself refuses inside an agent session (CLAUDECODE is set there);
# this catches the deliberate way around that check.
UNSETS_AGENT_MARK = re.compile(r"(unset|env\s+(-\S+\s+)*-u)\s+CLAUDECODE")


def guard(payload: dict) -> None:
    tool = payload.get("tool_name", "")
    data = payload.get("tool_input") or {}
    if tool == "Bash":
        command = data.get("command", "")
        denied = ((QUEUE_FILE.search(command) and WRITES.search(command))
                  or (UNSETS_AGENT_MARK.search(command) and re.search(r"\baq\b", command)))
    else:
        path = data.get("file_path") or data.get("notebook_path") or ""
        denied = str(Path(path).expanduser()).startswith(str(STATE))
    if denied:
        print(json.dumps({"hookSpecificOutput": {
            "hookEventName": "PreToolUse", "permissionDecision": "deny",
            "permissionDecisionReason": "Only the user adds agent-queue items or edits the queue; "
                                        "report your item with `aq report` instead."}}))


if __name__ == "__main__":
    {"stop": stop, "guard": guard}[sys.argv[1]](json.load(sys.stdin))
