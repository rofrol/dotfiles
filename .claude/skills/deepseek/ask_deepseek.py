#!/usr/bin/env python3
"""Ask DeepSeek for a second opinion. Prompt from args or stdin; files via -f."""
import argparse, json, os, sys, urllib.request, urllib.error
from pathlib import Path

KEY_FILE = Path.home() / ".config/deepseek/api_key"

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
    p.add_argument("-f", "--file", action="append", default=[], help="attach file contents")
    p.add_argument("-s", "--system", default="You are a senior engineer giving a candid second opinion. Be concise and concrete; point out mistakes and risks.")
    p.add_argument("--show-reasoning", action="store_true")
    a = p.parse_args()

    prompt = " ".join(a.prompt)
    if not sys.stdin.isatty():
        piped = sys.stdin.read()
        prompt = f"{prompt}\n\n{piped}" if prompt else piped
    for f in a.file:
        prompt += f"\n\n--- {f} ---\n{Path(f).read_text(errors='replace')}"
    if not prompt.strip():
        sys.exit("Pusty prompt")

    body = {"model": a.model, "messages": [
        {"role": "system", "content": a.system},
        {"role": "user", "content": prompt}]}
    req = urllib.request.Request(
        os.environ.get("DEEPSEEK_BASE_URL", "https://api.deepseek.com") + "/chat/completions",
        data=json.dumps(body).encode(),
        headers={"Authorization": f"Bearer {get_key()}", "Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=600) as r:
            data = json.load(r)
    except urllib.error.HTTPError as e:
        sys.exit(f"HTTP {e.code}: {e.read().decode(errors='replace')}")
    msg = data["choices"][0]["message"]
    if a.show_reasoning and msg.get("reasoning_content"):
        print("=== reasoning ===\n" + msg["reasoning_content"] + "\n=== answer ===")
    print(msg["content"])

if __name__ == "__main__":
    main()
