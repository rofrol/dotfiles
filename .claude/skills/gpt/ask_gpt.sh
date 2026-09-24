#!/usr/bin/env bash
# Ask GPT-6 (Sol/Luna/Astra) via Codex CLI, billed to the ChatGPT subscription (codex login).
# Usage: ask_gpt.sh [-m sol|luna|astra|<id>] [-e low|medium|high|xhigh] [-f FILE]... "prompt"   (-f - reads stdin)
set -euo pipefail
model="${GPT_MODEL:-astra}"; effort=""; files=()
while getopts "m:e:f:" o; do
  case $o in m) model=$OPTARG;; e) effort=$OPTARG;; f) files+=("$OPTARG");; *) exit 2;; esac
done
shift $((OPTIND-1))
case $model in astra) model=gpt-6-astra;; sol|luna|terra) model="gpt-5.6-$model";; esac

prompt="$*"
# stdin only via -f -: a background job can inherit an open stdin that never sends EOF.
for f in ${files[@]+"${files[@]}"}; do
  [ "$f" = - ] && label=stdin || label=$f
  prompt="$prompt"$'\n\n'"--- $label ---"$'\n'"$(cat "$f")"
done
# Regex match instead of ${prompt//[[:space:]]/}: the substitution is quadratic in bash and hangs on long prompts.
[[ $prompt =~ [^[:space:]] ]] || { echo "Pusty prompt" >&2; exit 1; }

codex login status 2>&1 | grep -q ChatGPT || { echo "Codex nie jest zalogowany przez ChatGPT — uruchom: codex login" >&2; exit 1; }

tmp=$(mktemp -d); trap 'rm -rf "$tmp"' EXIT
out="$tmp/answer"; mkdir "$tmp/cwd"
args=(exec --ignore-user-config --ephemeral --skip-git-repo-check -s read-only -C "$tmp/cwd" -m "$model" -o "$out")
[ -n "$effort" ] && args+=(-c "model_reasoning_effort=\"$effort\"")
printf '%s' "$prompt" | codex "${args[@]}" - >/dev/null 2>"$tmp/err" || { cat "$tmp/err" >&2; exit 1; }
cat "$out"
