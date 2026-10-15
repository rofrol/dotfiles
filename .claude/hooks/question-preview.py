#!/usr/bin/env python3
"""PreToolUse hook for AskUserQuestion: keep the whole question readable.

Claude Code clips a long option preview ("N lines hidden") and offers no way
to scroll it, so a user could approve text they never saw. This hook writes
every question, with each option's description and full preview, to
~/.local/state/claude-questions/<session id>.md, and for every question with
a preview opens that file in `less` in a herdr pane beside the agent's pane
(without taking focus; `q` closes it). Any preview counts: the box is narrow,
so wrapped lines clip a preview of few source lines too (2026-10-15: a
9-line preview of 72-column lines was cut off), and the hook cannot see the
box's width.

It never blocks the question: any failure is logged next to the file and the
hook exits 0 with no output.
"""
import json
import os
import shutil
import subprocess
import sys
import tempfile

STATE = os.path.expanduser("~/.local/state/claude-questions")


def render(questions):
    out = []
    for n, q in enumerate(questions, 1):
        out.append(f"# {n}. {q.get('header', '')}: {q.get('question', '')}\n")
        for opt in q.get("options", []):
            out.append(f"## {opt.get('label', '')}\n")
            if opt.get("description"):
                out.append(opt["description"] + "\n")
            if opt.get("preview"):
                out.append("\n" + opt["preview"].rstrip("\n") + "\n")
            out.append("")
    return "\n".join(out)


def has_preview(questions):
    return any(
        opt.get("preview", "").strip()
        for q in questions
        for opt in q.get("options", [])
    )


def publish(path, text):
    """Write the file atomically: a viewer never reads half of it."""
    fd, tmp = tempfile.mkstemp(dir=os.path.dirname(path), prefix=".tmp-")
    with os.fdopen(fd, "w") as f:
        f.write(text)
    os.replace(tmp, path)


def herdr(*args):
    return subprocess.run(["herdr", *args], capture_output=True, text=True, check=True).stdout


def open_viewer(path, session):
    pane = os.environ.get("HERDR_PANE_ID")
    if not pane or not shutil.which("herdr"):
        return
    # One viewer per session: close the previous one if it is still open.
    marker = os.path.join(STATE, f"{session}.pane")
    if os.path.exists(marker):
        old = open(marker).read().strip()
        subprocess.run(["herdr", "pane", "close", old], capture_output=True)
    info = json.loads(herdr("pane", "split", "--pane", pane, "--direction", "right"))
    viewer = info["result"]["pane"]["pane_id"]
    publish(marker, viewer + "\n")
    herdr("pane", "run", viewer, f"less {sh_quote(path)}; exit")


def sh_quote(s):
    return "'" + s.replace("'", "'\\''") + "'"


def main():
    event = json.load(sys.stdin)
    if event.get("tool_name") != "AskUserQuestion":
        return
    questions = (event.get("tool_input") or {}).get("questions") or []
    session = event.get("session_id") or "unknown"
    os.makedirs(STATE, exist_ok=True)
    path = os.path.join(STATE, f"{session}.md")
    publish(path, render(questions))
    if has_preview(questions):
        open_viewer(path, session)


if __name__ == "__main__":
    try:
        main()
    except Exception as e:  # never block the question
        try:
            os.makedirs(STATE, exist_ok=True)
            with open(os.path.join(STATE, "errors.log"), "a") as f:
                f.write(f"{type(e).__name__}: {e}\n")
        except OSError:
            pass
    sys.exit(0)
