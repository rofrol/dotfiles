#!/usr/bin/env python3
"""Ask DeepSeek for a second opinion. Prompt from args; files via -f (-f - reads stdin).

Streams the answer (SSE) and enforces a hard deadline on the whole request:
DeepSeek keeps a queued connection alive with keep-alive lines, so a per-read
socket timeout alone can wait forever.
"""
import argparse, json, os, signal, subprocess, sys, time, urllib.request, urllib.error
from pathlib import Path

AUTH_FILE = Path.home() / ".pi/agent/auth.json"
ORACLE = Path.home() / ".claude/skills/oracle-stats/oracle.py"

class Deadline(Exception):
    pass

def get_key():
    try:
        return json.loads(AUTH_FILE.read_text())["deepseek"]["key"]
    except (OSError, ValueError, KeyError) as e:
        sys.exit(f"Brak klucza deepseek w {AUTH_FILE}: {e!r}")

def log_call(model, status, seconds, prompt_chars, answer_chars):
    """Record the call for oracle-stats; never let logging fail the consultation."""
    try:
        subprocess.run([str(ORACLE), "log", "--skill", "deepseek", "--model", model, "--status", status,
                        "--seconds", str(int(seconds)), "--prompt-chars", str(prompt_chars),
                        "--answer-chars", str(answer_chars)], timeout=10)
    except (OSError, subprocess.SubprocessError):
        pass

def run_in_herdr_job(model):
    """Re-run in its own herdr-job tab, so the user can watch it (see in_herdr_job.sh)."""
    import shutil
    if os.environ.get("ORACLE_IN_JOB") or not os.environ.get("HERDR_SOCKET_PATH") or not shutil.which("herdr-job"):
        return
    wrap = Path.home() / ".claude/skills/oracle-stats/in_herdr_job.sh"
    label = "deepseek " + model.removeprefix("deepseek-")  # the model shows in the tab name
    os.execv(str(wrap), [str(wrap), label, str(Path(__file__).resolve()), *sys.argv[1:]])

def live_output():
    """The herdr-job tab, for reasoning the caller should not get; None outside a job."""
    if not os.environ.get("HERDR_JOB_TTY"):
        return None
    try:
        return open(os.environ["HERDR_JOB_TTY"], "w")
    except OSError:
        return None

def main():
    p = argparse.ArgumentParser()
    p.add_argument("prompt", nargs="*")
    p.add_argument("-m", "--model", default=os.environ.get("DEEPSEEK_MODEL", "deepseek-flash"),
                   help="deepseek-flash = DeepSeek V4.1 (default) or deepseek-v4-pro = V4-Pro-0813")
    p.add_argument("-f", "--file", action="append", default=[], help="attach file contents (- = stdin)")
    p.add_argument("-s", "--system", default="You are a senior engineer giving a candid second opinion. Be concise and concrete; point out mistakes and risks.")
    p.add_argument("-t", "--timeout", type=int, default=int(os.environ.get("DEEPSEEK_TIMEOUT", 420)),
                   help="hard limit in seconds for the whole request (default 420)")
    p.add_argument("--show-reasoning", action="store_true")
    a = p.parse_args()
    run_in_herdr_job(a.model)

    prompt = " ".join(a.prompt)
    # stdin only via -f -: a background job can inherit an open stdin that never sends EOF.
    for f in a.file:
        if f == "-" and os.environ.get("ORACLE_STDIN"):  # saved by in_herdr_job.sh
            text = Path(os.environ["ORACLE_STDIN"]).read_text(errors="replace")
        else:
            text = sys.stdin.read() if f == "-" else Path(f).read_text(errors="replace")
        prompt += f"\n\n--- {'stdin' if f == '-' else f} ---\n{text}"
    if not prompt.strip():
        sys.exit("Pusty prompt")

    body = {"model": a.model, "stream": True, "messages": [
        {"role": "system", "content": a.system},
        {"role": "user", "content": prompt}]}
    req = urllib.request.Request(
        os.environ.get("DEEPSEEK_BASE_URL", "https://api.deepseek.com") + "/chat/completions",
        data=json.dumps(body).encode(),
        headers={"Authorization": f"Bearer {get_key()}", "Content-Type": "application/json",
                 "Accept": "text/event-stream"})

    def on_alarm(*_):
        raise Deadline
    signal.signal(signal.SIGALRM, on_alarm)
    signal.alarm(a.timeout)  # hard cap, fires even while blocked in a read

    start = time.monotonic()
    reasoning, content, finish = [], [], None
    live = live_output()
    try:
        with urllib.request.urlopen(req, timeout=min(120, a.timeout)) as r:
            for raw in r:
                line = raw.decode(errors="replace").strip()
                if not line.startswith("data:"):
                    continue  # empty lines and ": keep-alive" comments
                data = line[5:].strip()
                if data == "[DONE]":
                    break
                choice = json.loads(data)["choices"][0]
                delta = choice.get("delta") or {}
                if delta.get("reasoning_content"):
                    reasoning.append(delta["reasoning_content"])
                    if live:
                        live.write(f"\x1b[2m{delta['reasoning_content']}\x1b[0m")
                        live.flush()
                if delta.get("content"):
                    content.append(delta["content"])
                finish = choice.get("finish_reason") or finish
    except Deadline:
        finish = "deadline"
    except urllib.error.HTTPError as e:
        log_call(a.model, "error", time.monotonic() - start, len(prompt), 0)
        sys.exit(f"HTTP {e.code}: {e.read().decode(errors='replace')}")
    except (urllib.error.URLError, TimeoutError, OSError) as e:
        finish = f"error: {e}"
    finally:
        signal.alarm(0)

    answer = "".join(content)
    if live:
        live.write("\n\n--- answer ---\n")
        live.close()
    log_call(a.model, "ok" if finish in ("stop", None) else "error", time.monotonic() - start, len(prompt), len(answer))
    if a.show_reasoning and reasoning:
        print("=== reasoning ===\n" + "".join(reasoning) + "\n=== answer ===")
    print(answer)
    if finish not in ("stop", None):
        elapsed = int(time.monotonic() - start)
        state = "no answer yet" if not content else "answer is partial"
        print(f"\n[ask_deepseek: stopped after {elapsed}s ({finish}); {state}]", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
