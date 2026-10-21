#!/usr/bin/env python3
"""End-to-end check for extensions/confirm-interrupt.ts: does Escape while the
agent works open the "Stop the agent run?" dialog instead of aborting, does
cancelling let the work finish, and does confirming stop it?

Runs pi in a PTY (no herdr env, ephemeral session), types prompts that make the
agent run marked bash commands, and drives the dialog. This spends model calls
(a few cents).

Usage:
  confirm-interrupt.e2e.py [--model M] [--pi CMD]

Markers are split in the typed command (E2E-STAR"TED") so the echoed prompt can
never satisfy them: only the running command's output can. Every wait polls for
such a named positive condition, never for a duration alone.

Logs go to $TMPDIR/pi-confirm-interrupt-e2e/.
"""
import argparse
import fcntl
import os
import pty
import re
import select
import shlex
import struct
import subprocess
import sys
import tempfile
import termios
import time

ap = argparse.ArgumentParser()
ap.add_argument("--model")
ap.add_argument("--pi", default="pi")
a = ap.parse_args()

OUTDIR = os.path.join(tempfile.gettempdir(), "pi-confirm-interrupt-e2e")
os.makedirs(OUTDIR, exist_ok=True)
LOG = os.path.join(OUTDIR, "tui.log")
DIALOG = "Stop the agent run?"
KEEP = "Keep working"
STOP = "Stop the run"
# In the typed command each marker is split (E2E-STAR"TED"); the running command
# prints it whole, so only its output matches.
WORK_KEEP = 'echo E2E-STAR"TED"; sleep 20; echo E2E-WORK-COMPLE"TE"'
WORK_STOP = 'echo E2E-STAR"T2"; sleep 123.456'

cmd = shlex.split(a.pi) + ["--no-session", "--name", "confirm-interrupt-e2e"]
if a.model:
    cmd += ["--model", a.model]

master, slave = pty.openpty()
fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", 40, 120, 0, 0))
env = {k: v for k, v in os.environ.items() if not k.startswith("HERDR_")}
proc = subprocess.Popen(
    cmd, stdin=slave, stdout=slave, stderr=slave, env=env,
    cwd=tempfile.gettempdir(), close_fds=True,
)
os.close(slave)

buf = bytearray()
log = open(LOG, "wb")


def drain(seconds=0.2):
    end = time.monotonic() + seconds
    while time.monotonic() < end:
        ready, _, _ = select.select([master], [], [], max(0.0, end - time.monotonic()))
        if not ready:
            break
        chunk = os.read(master, 65536)
        if not chunk:
            break
        buf.extend(chunk)
        log.write(chunk)
        log.flush()


def wait_for(what, pattern, timeout, after=0):
    """Wait until `pattern` renders past offset `after`. `what` names the
    positive event that produces it."""
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if re.search(pattern, bytes(buf[after:])):
            return
        drain(0.2)
        if proc.poll() is not None:
            sys.exit(f"FAIL: pi exited ({proc.returncode}) while waiting for: {what}")
    sys.exit(f"FAIL: timed out waiting for: {what}")


def wait_process_gone(what, needle, timeout):
    """Wait until no process command line contains `needle`: confirming Stop
    must actually kill the running command."""
    deadline = time.monotonic() + timeout
    while time.monotonic() < deadline:
        if subprocess.run(["pgrep", "-f", needle], capture_output=True).returncode != 0:
            return
        drain(0.2)
    sys.exit(f"FAIL: timed out waiting for: {what}")


def absent(where, pattern, after=0):
    if re.search(pattern, bytes(buf[after:])):
        sys.exit(f"FAIL: {where}: unexpected marker {pattern!r}")


def send(data):
    os.write(master, data)
    drain(0.3)


def ask(command):
    send(b"Use the bash tool to run exactly this command and nothing else: " + command.encode() + b"\r")


try:
    wait_for("the TUI to render its header", rb"confirm-interrupt-e2e", 30)

    # Idle Escape keeps its old behavior: no dialog.
    idle = len(buf)
    send(b"\x1b")
    drain(1.0)
    absent("idle Escape", re.escape(DIALOG).encode(), after=idle)

    # Work that finishes on its own 20 seconds after its start marker.
    run1 = len(buf)
    ask(WORK_KEEP)
    wait_for("the first marked command to start", rb"E2E-STARTED", 120, after=run1)
    absent("before Escape", re.escape(DIALOG).encode(), after=run1)

    # Escape while working must ask first.
    send(b"\x1b")
    wait_for("the confirmation dialog to render", re.escape(DIALOG).encode(), 5, after=run1)
    wait_for("the dialog's safe option to render", re.escape(KEEP).encode(), 5, after=run1)
    wait_for("the dialog's stop option to render", re.escape(STOP).encode(), 5, after=run1)
    print("PASS: Escape while working opens the confirmation dialog")

    # Cancelling the dialog (Escape) must let the work finish on its own.
    send(b"\x1b")
    wait_for("the cancelled work to finish on its own", rb"E2E-WORK-COMPLETE", 60, after=run1)
    print("PASS: cancelling the dialog lets the work finish")

    # Work that would run for 123 seconds; confirming Stop must kill it.
    run2 = len(buf)
    ask(WORK_STOP)
    wait_for("the second marked command to start", rb"E2E-START2", 120, after=run2)

    send(b"\x1b")
    wait_for("the confirmation dialog to render again", re.escape(DIALOG).encode(), 5, after=run2)
    # The dialog preselects "Keep working"; move to "Stop the run" and confirm.
    send(b"\x1b[B")
    send(b"\r")
    wait_process_gone("confirming Stop to kill the running command", "sleep 123.456", 15)
    print("PASS: confirming Stop the run kills the running work")
    print("PASS: all checks")
finally:
    proc.terminate()
    try:
        proc.wait(timeout=5)
    except subprocess.TimeoutExpired:
        proc.kill()
    os.close(master)
    log.close()
    print(f"log: {LOG}")
