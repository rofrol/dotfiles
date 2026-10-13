#!/usr/bin/env python3
"""End-to-end check for extensions/bg-bash.ts: is a message sent while a long
bash command runs answered at once, and is the agent woken when it exits?

Runs pi headless in RPC mode (no herdr env, no session, no auto-discovered
extensions), asks it to run `sleep N; echo BUILD-OK`, sends a steering question
once the bash tool has started, and prints the answer latency. This spends
model calls (a few cents).

Usage:
  bg-bash.e2e.py bg|plain [--model M] [--bridge] [--pi CMD]

  bg       load bg-bash.ts (the fix); plain = stock pi, the control
  --model  pi model id, e.g. anthropic/claude-opus-5-5 (default: pi default)
  --bridge also load pi-claude-bridge, for --model claude-bridge/... (runs
           through Claude Code on the subscription)
  --pi     pi command, e.g. "bash ~/scripts/pi-claude-extra-credits"
  SLEEP=40 length of the fake build in seconds

Logs (RPC events, pi stderr) go to $TMPDIR/pi-bg-e2e/.
"""
import argparse, json, os, queue, shlex, subprocess, tempfile, threading, time

HERE = os.path.dirname(os.path.abspath(__file__))
EXT = os.path.join(HERE, "..", "extensions", "bg-bash.ts")
BRIDGE = os.path.join(HERE, "..", "npm", "node_modules", "pi-claude-bridge", "src", "index.ts")

ap = argparse.ArgumentParser()
ap.add_argument("mode", choices=["bg", "plain"])
ap.add_argument("--model")
ap.add_argument("--bridge", action="store_true")
ap.add_argument("--pi", default="pi")
a = ap.parse_args()
SLEEP = int(os.environ.get("SLEEP", "40"))

cmd = shlex.split(a.pi) + ["--mode", "rpc", "--no-session", "--no-extensions"]
if a.mode == "bg":
    cmd += ["-e", EXT]
if a.bridge:
    cmd += ["-e", BRIDGE]
if a.model:
    cmd += ["--model", a.model]

outdir = os.path.join(tempfile.gettempdir(), "pi-bg-e2e")
os.makedirs(outdir, exist_ok=True)
tag = f"{a.mode}_{(a.model or 'default').replace('/', '_')}_{os.getpid()}"
env = {k: v for k, v in os.environ.items() if not k.startswith("HERDR_")}
p = subprocess.Popen(cmd, stdin=subprocess.PIPE, stdout=subprocess.PIPE,
                     stderr=open(os.path.join(outdir, f"{tag}.stderr"), "w"), env=env, cwd=tempfile.gettempdir())
events: "queue.Queue[dict]" = queue.Queue()
log = open(os.path.join(outdir, f"{tag}.jsonl"), "w")


def reader():
    # Strict JSONL framing: split on LF only (not readline, which also splits on U+2028).
    buf = b""
    while True:
        chunk = p.stdout.read1(65536)
        if not chunk:
            events.put({"type": "__eof"})
            return
        buf += chunk
        while b"\n" in buf:
            line, buf = buf.split(b"\n", 1)
            line = line.rstrip(b"\r")
            if line:
                log.write(line.decode() + "\n")
                log.flush()
                events.put(json.loads(line))


threading.Thread(target=reader, daemon=True).start()
T0 = time.time()


def t():
    return f"{time.time() - T0:6.1f}s"


def send(o):
    p.stdin.write((json.dumps(o) + "\n").encode())
    p.stdin.flush()


def text_of(msg):
    return "".join(c.get("text", "") for c in msg.get("content", []) if c.get("type") == "text")


send({"id": "1", "type": "prompt",
      "message": f"Build step: run `sleep {SLEEP}; echo BUILD-OK` and tell me its output once it has finished."})
asked = answered = woken = None
# External deadline: the fake build plus generous model time; ends a hung run.
deadline = T0 + SLEEP + 240
while time.time() < deadline:
    try:
        e = events.get(timeout=deadline - time.time())
    except queue.Empty:
        break
    ty = e.get("type")
    if ty == "__eof":
        print(t(), "pi exited")
        break
    if ty == "tool_execution_start":
        print(t(), "tool start", e.get("toolName"), json.dumps(e.get("args"))[:120])
        if asked is None:
            asked = time.time()
            print(t(), "user asks 17*3 (steer)")
            send({"id": "2", "type": "prompt", "streamingBehavior": "steer",
                  "message": "Quick question meanwhile: what is 17*3? Answer with just the number."})
    elif ty == "tool_execution_end":
        print(t(), "tool end", e.get("toolName"))
    elif ty == "response":
        print(t(), "response", e.get("command"), e.get("success"), e.get("data"), e.get("error", ""))
    elif ty == "message_end":
        m = e.get("message", {})
        if m.get("role") == "assistant":
            tx = text_of(m).strip()
            print(t(), "assistant:", tx[:160].replace("\n", " ") or f"({m.get('stopReason')} {m.get('errorMessage', '')[:200]})")
            if asked and answered is None and "51" in tx:
                answered = time.time()
                print(t(), f"ANSWER LATENCY {answered - asked:.1f}s")
            if "BUILD-OK" in tx and (woken or a.mode == "plain"):
                print(t(), "reported build result")
                break
        elif m.get("customType"):
            print(t(), "custom message", m.get("customType"))
            woken = time.time()
p.stdin.close()
p.terminate()
p.wait()
latency = None if answered is None else round(answered - asked, 1)
print("RESULT", a.mode, a.model or "default", "answer_latency=", latency, "woken=", woken is not None)
print("logs:", outdir, tag)
