#!/usr/bin/env python3
"""Ask DeepSeek for a second opinion. Prompt from args; files via -f (-f - reads stdin).

Streams the answer (SSE) and enforces a hard deadline on the whole request:
DeepSeek keeps a queued connection alive with keep-alive lines, so a per-read
socket timeout alone can wait forever.
"""
import argparse, json, os, signal, sys, time, urllib.request, urllib.error
from pathlib import Path

KEY_FILE = Path.home() / ".config/deepseek/api_key"

class Deadline(Exception):
    pass

def get_key():
    key = os.environ.get("DEEPSEEK_API_KEY")
    if not key and KEY_FILE.exists():
        key = KEY_FILE.read_text().strip()
    if not key:
        sys.exit(f"Brak klucza: ustaw DEEPSEEK_API_KEY albo zapisz go w {KEY_FILE}")
    return key

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

    prompt = " ".join(a.prompt)
    # stdin only via -f -: a background job can inherit an open stdin that never sends EOF.
    for f in a.file:
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
                if delta.get("content"):
                    content.append(delta["content"])
                finish = choice.get("finish_reason") or finish
    except Deadline:
        finish = "deadline"
    except urllib.error.HTTPError as e:
        sys.exit(f"HTTP {e.code}: {e.read().decode(errors='replace')}")
    except (urllib.error.URLError, TimeoutError, OSError) as e:
        finish = f"error: {e}"
    finally:
        signal.alarm(0)

    if a.show_reasoning and reasoning:
        print("=== reasoning ===\n" + "".join(reasoning) + "\n=== answer ===")
    print("".join(content))
    if finish not in ("stop", None):
        elapsed = int(time.monotonic() - start)
        state = "no answer yet" if not content else "answer is partial"
        print(f"\n[ask_deepseek: stopped after {elapsed}s ({finish}); {state}]", file=sys.stderr)
        sys.exit(1)

if __name__ == "__main__":
    main()
