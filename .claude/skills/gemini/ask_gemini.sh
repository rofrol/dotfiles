#!/usr/bin/env bash
# Ask Gemini via Antigravity CLI (agy), billed to the Google AI subscription; uses agy's own login.
# Usage: ask_gemini.sh [-m flash|<id>] [-e low|medium|high] [-r] [-f FILE]... "prompt"   (-f - reads stdin)
# -r: run agy in the current git repo, so it can read files itself (writes are denied in headless mode).
set -euo pipefail
orig=("$@")  # for the herdr-job re-run, once the model is known
model="${GEMINI_MODEL:-}"; effort=high; files=(); repo=""
while getopts "m:e:f:r" o; do
  case $o in m) model=$OPTARG;; e) effort=$OPTARG;; f) files+=("$OPTARG");; r) repo=1;; *) exit 2;; esac
done
shift $((OPTIND-1))
# Flash only: Pro drained the shared weekly quota, had only view_file in agy, and scored lowest in oracle-stats.
[ -n "$model" ] || model=flash
case $model in
  flash) model="gemini-3.8-flash-$effort";;
  pro|gemini-*-pro-*) echo "Gemini Pro jest wyłączony — użyj Flash" >&2; exit 2;;
esac
if [ -z "${ORACLE_IN_JOB:-}" ] && [ -n "${HERDR_SOCKET_PATH:-}" ] && command -v herdr-job >/dev/null; then
  exec ~/.claude/skills/oracle-stats/in_herdr_job.sh "gemini ${model#gemini-}" "$0" ${orig[@]+"${orig[@]}"}  # watch it in its own herdr tab
fi

prompt="$*"
# stdin only via -f -: a background job can inherit an open stdin that never sends EOF.
for f in ${files[@]+"${files[@]}"}; do
  [ "$f" = - ] && label=stdin || label=$f
  if [ "$f" = - ] && [ -n "${ORACLE_STDIN:-}" ]; then f=$ORACLE_STDIN; fi  # saved by in_herdr_job.sh
  prompt="$prompt"$'\n\n'"--- $label ---"$'\n'"$(cat "$f")"
done
# Regex match instead of ${prompt//[[:space:]]/}: the substitution is quadratic in bash and hangs on long prompts.
[[ $prompt =~ [^[:space:]] ]] || { echo "Pusty prompt" >&2; exit 1; }

start=$SECONDS; answer_chars=""
# Log every call for oracle-stats; logging must not change the exit code or fail the call.
oracle_log() {
  local rc=$? usage=""
  # Token usage from agy's result event; read before $tmp goes away. output_tokens includes thinking_tokens.
  usage=$(jq -c 'select(.event=="result") | .result.usage // empty' "$tmp/out" 2>/dev/null | tail -1) || true
  rm -rf "$tmp" 2>/dev/null || true
  ~/.claude/skills/oracle-stats/oracle.py log --skill gemini --model "$model" --mode "${repo:+repo}" \
    --status "$([ $rc = 0 ] && echo ok || echo error)" --seconds $((SECONDS-start)) \
    --prompt-chars ${#prompt} ${answer_chars:+--answer-chars $answer_chars} \
    ${usage:+--usage-raw "$usage"} ${usage:+--usage "$(jq -c '{input: .input_tokens, cached: .cache_read_tokens,
      output: .output_tokens, reasoning: .thinking_tokens}' <<<"$usage" 2>/dev/null)"} || true
  exit $rc
}
tmp=$(mktemp -d); trap oracle_log EXIT
mkdir "$tmp/cwd"; cwd="$tmp/cwd"
if [ -n "$repo" ]; then
  # The dotfiles env (GIT_DIR/GIT_WORK_TREE) would point git, and agy's git commands, at the home repo.
  unset GIT_DIR GIT_WORK_TREE
  cwd=$(git rev-parse --show-toplevel 2>/dev/null) || { echo "-r: $PWD nie jest w repozytorium git" >&2; exit 1; }
  [ "$cwd" != "$HOME" ] || { echo "-r: odmawiam uruchomienia w \$HOME (agy widziałby cały katalog domowy)" >&2; exit 1; }
  note="You are working in a git repository (the current directory). Use only read-only file tools (viewing, listing and searching files). Never use run_command or any shell command (not even ls or git): it is denied and aborts your answer. Do not use subagents or web access. Do not modify any files."
else
  # Tool calls outside a trusted workspace are auto-denied and end the run with an empty answer.
  note="Answer directly from the content of this message. Do not use any tools (no file reads, commands or web access)."
fi
prompt="$note"$'\n\n'"$prompt"

# Prompt goes via stdin as stream-json: -p "text" would hit ARG_MAX on large diffs.
jq -nc --arg p "$prompt" '{event:"user",message:{role:"user",content:$p}}' |
  (cd "$cwd" && agy --input-format stream-json --output-format stream-json --model "$model" -p=) >"$tmp/out" 2>"$tmp/err" ||
  { cat "$tmp/err" "$tmp/out" >&2; exit 1; }
result=$(jq -c 'select(.event=="result") | .result' "$tmp/out" | tail -1)
[ -n "$result" ] || { cat "$tmp/err" "$tmp/out" >&2; exit 1; }
if [ "$(jq -r .status <<<"$result")" != SUCCESS ]; then
  jq -r '.error // "agy: błąd bez opisu"' <<<"$result" >&2; cat "$tmp/err" >&2; exit 1
fi
denied=$(jq -r '[.denied_actions[]?.action] | unique | join(", ")' <<<"$result")
[ -z "$denied" ] || echo "Uwaga: agy odmówił narzędzi: $denied" >&2
answer=$(jq -r .response <<<"$result")
[[ $answer =~ [^[:space:]] ]] || { echo "Pusta odpowiedź (prawdopodobnie model próbował użyć zablokowanego narzędzia)" >&2; cat "$tmp/err" >&2; exit 1; }
answer_chars=${#answer}
printf '%s\n' "$answer"
