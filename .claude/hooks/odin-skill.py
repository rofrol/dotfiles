#!/usr/bin/env python3
"""PreToolUse hook: give a session the odin skill's pitfalls once.

An Odin session did not load the odin skill by itself (2026-10-31, agentdeck
item m75wrz: a stress job lost to `odin test` deleting its binary, which the
skill documents), so this hook injects the skill's body as additionalContext
at the session's first touch of Odin: a Read, Edit or Write of a `.odin`
file, or a Bash call running `odin` or `./odin`. The user decided this form
on 2026-11-02 (agentdeck item cmfihg, consult round 20261031-115553-5901).

Once per session: a marker ~/.local/state/claude-odin-skill/<session id>,
created exclusively, says the skill was given. A Skill call loading `odin`
creates the marker too, so a session that loaded it gets no second copy.

It never blocks a call: any failure exits 0 with no output.
"""

import json
import os
import re
import sys

SKILL = os.path.expanduser("~/.claude/skills/odin/SKILL.md")
STATE = os.path.expanduser("~/.local/state/claude-odin-skill")
FILE_TOOLS = {"Read", "Edit", "Write", "MultiEdit", "NotebookEdit"}
# `odin` or a path ending in `/odin` as a command word: at the start, or
# after a separator, a `(` or `$(`, or a command wrapper such as `exec`;
# and a path ending in `/odin` after `--`, the command `ad job run` starts.
ODIN_CMD = re.compile(r"(?:(?:^|[;&|(\n`]|\bexec|\btime|\benv)\s*(?:\S*/)?|\s--\s+\S*/)odin(?=\s|$|[;&|)])")


def touches_odin(tool, inp):
    if tool in FILE_TOOLS:
        path = inp.get("file_path") or inp.get("notebook_path") or ""
        return path.endswith(".odin")
    if tool == "Bash":
        return bool(ODIN_CMD.search(inp.get("command", "")))
    return False


def claim(session):
    """True the first time for this session, False after."""
    os.makedirs(STATE, exist_ok=True)
    try:
        fd = os.open(os.path.join(STATE, session), os.O_CREAT | os.O_EXCL | os.O_WRONLY, 0o600)
    except FileExistsError:
        return False
    os.close(fd)
    return True


def main():
    event = json.load(sys.stdin)
    session = os.path.basename(str(event.get("session_id") or ""))
    if not session:
        return
    tool = event.get("tool_name", "")
    inp = event.get("tool_input") or {}
    if tool == "Skill":
        if inp.get("skill") == "odin":
            claim(session)
        return
    if not touches_odin(tool, inp) or not claim(session):
        return
    with open(SKILL, encoding="utf-8") as f:
        body = f.read()
    text = ("This session touches Odin code: the odin skill's pitfalls follow, "
            "given once per session by ~/.claude/hooks/odin-skill.py.\n\n" + body)
    json.dump({"hookSpecificOutput": {"hookEventName": "PreToolUse",
                                      "additionalContext": text}}, sys.stdout)


if __name__ == "__main__":
    try:
        main()
    except Exception:
        pass
    sys.exit(0)
