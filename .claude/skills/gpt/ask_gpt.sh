#!/usr/bin/env bash
# Ask GPT-6 (Sol/Luna/Astra) via Codex CLI, billed to the ChatGPT subscription (codex login).
# Usage: ask_gpt.sh [-m sol|luna|astra|<id>] [-e low|medium|high|xhigh] [-f FILE]... "prompt"   (stdin appended)
set -euo pipefail
model="${GPT_MODEL:-astra}"; effort=""; files=()
while getopts "m:e:f:" o; do
  case $o in m) model=$OPTARG;; e) effort=$OPTARG;; f) files+=("$OPTARG");; *) exit 2;; esac
done
shift $((OPTIND-1))
case $model in astra) model=gpt-6-astra;; sol|luna|terra) model="gpt-5.6-$model";; esac

prompt="$*"
[ -t 0 ] || prompt="$prompt"$'\n\n'"$(cat)"
for f in ${files[@]+"${files[@]}"}; do prompt="$prompt"$'\n\n'"--- $f ---"$'\n'"$(cat "$f")"; done
[ -n "${prompt//[[:space:]]/}" ] || { echo "Pusty prompt" >&2; exit 1; }

codex login status 2>&1 | grep -q ChatGPT || { echo "Codex nie jest zalogowany przez ChatGPT — uruchom: codex login" >&2; exit 1; }

out=$(mktemp); trap 'rm -f "$out"' EXIT
args=(exec --ignore-user-config --ephemeral --skip-git-repo-check -s read-only -C "$(mktemp -d)" -m "$model" -o "$out")
[ -n "$effort" ] && args+=(-c "model_reasoning_effort=\"$effort\"")
printf '%s' "$prompt" | codex "${args[@]}" - >/dev/null 2>"$out.err" || { cat "$out.err" >&2; rm -f "$out.err"; exit 1; }
rm -f "$out.err"
cat "$out"
