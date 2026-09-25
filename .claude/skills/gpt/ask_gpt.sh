#!/usr/bin/env bash
# Ask GPT via Codex CLI, billed to the ChatGPT subscription; credentials come from pi (~/.pi/agent/auth.json).
# Usage: ask_gpt.sh [-m astra|sol|terra|luna|<id>] [-e low|medium|high|xhigh] [-r] [-f FILE]... "prompt"   (-f - reads stdin)
# -r: run Codex in the current git repo (read-only), so it can read files and git history itself.
set -euo pipefail
orig=("$@")  # for the herdr-job re-run, once the model is known
model="${GPT_MODEL:-astra}"; effort=""; files=(); repo=""
while getopts "m:e:f:r" o; do
  case $o in m) model=$OPTARG;; e) effort=$OPTARG;; f) files+=("$OPTARG");; r) repo=1;; *) exit 2;; esac
done
shift $((OPTIND-1))
if [ -z "${ORACLE_IN_JOB:-}" ] && [ -n "${HERDR_SOCKET_PATH:-}" ] && command -v herdr-job >/dev/null; then
  exec ~/.claude/skills/oracle-stats/in_herdr_job.sh "gpt $model/${effort:-default}" "$0" ${orig[@]+"${orig[@]}"}  # watch it in its own herdr tab
fi
case $model in astra|sol|luna) model="gpt-6-$model";; terra) model=gpt-5.6-terra;; esac  # no GPT-6 Terra yet

prompt="$*"
# stdin only via -f -: a background job can inherit an open stdin that never sends EOF.
for f in ${files[@]+"${files[@]}"}; do
  [ "$f" = - ] && label=stdin || label=$f
  if [ "$f" = - ] && [ -n "${ORACLE_STDIN:-}" ]; then f=$ORACLE_STDIN; fi  # saved by in_herdr_job.sh
  prompt="$prompt"$'\n\n'"--- $label ---"$'\n'"$(cat "$f")"
done
# Regex match instead of ${prompt//[[:space:]]/}: the substitution is quadratic in bash and hangs on long prompts.
[[ $prompt =~ [^[:space:]] ]] || { echo "Pusty prompt" >&2; exit 1; }

# pi refreshes the OAuth token if needed and writes it back to its auth.json; Codex only gets a bearer token,
# so it never refreshes (rotating) tokens itself. Separate CODEX_HOME: ~/.codex (and its auth.json) is not used.
export PI_CODEX_TOKEN PI_CODEX_ACCOUNT
PI_CODEX_TOKEN=$(pi auth print-bearer-token --provider openai-codex --min-expiry 15m) || { echo "Brak tokenu openai-codex w pi — zaloguj się w pi (/login)" >&2; exit 1; }
PI_CODEX_ACCOUNT=$(jq -er '."openai-codex".accountId' ~/.pi/agent/auth.json) || { echo "Brak accountId openai-codex w ~/.pi/agent/auth.json" >&2; exit 1; }

start=$SECONDS; answer_chars=""
# Log every call for oracle-stats; logging must not change the exit code or fail the call.
oracle_log() {
  local rc=$?; rm -rf "$tmp" 2>/dev/null || true  # a straggling Codex child can still be writing there
  ~/.claude/skills/oracle-stats/oracle.py log --skill gpt --model "$model" --effort "${effort:-default}" --mode "${repo:+repo}" \
    --status "$([ $rc = 0 ] && echo ok || echo error)" --seconds $((SECONDS-start)) \
    --prompt-chars ${#prompt} ${answer_chars:+--answer-chars $answer_chars} || true
  exit $rc
}
tmp=$(mktemp -d); trap oracle_log EXIT
out="$tmp/answer"; mkdir "$tmp/cwd" "$tmp/home"
cwd="$tmp/cwd"
if [ -n "$repo" ]; then
  # The dotfiles env (GIT_DIR/GIT_WORK_TREE) would point git, and Codex's git commands, at the home repo.
  unset GIT_DIR GIT_WORK_TREE
  cwd=$(git rev-parse --show-toplevel 2>/dev/null) || { echo "-r: $PWD nie jest w repozytorium git" >&2; exit 1; }
  [ "$cwd" != "$HOME" ] || { echo "-r: odmawiam uruchomienia w \$HOME (Codex widziałby cały katalog domowy)" >&2; exit 1; }
fi
provider='model_providers.pi={name="pi",base_url="https://chatgpt.com/backend-api/codex",wire_api="responses",env_key="PI_CODEX_TOKEN",env_http_headers={"chatgpt-account-id"="PI_CODEX_ACCOUNT"}}'
export CODEX_HOME="$tmp/home"
args=(exec --ignore-user-config --ephemeral --skip-git-repo-check -s read-only -C "$cwd" -m "$model" -o "$out"
      -c model_provider=pi -c "$provider")
[ -n "$effort" ] && args+=(-c "model_reasoning_effort=\"$effort\"")
# Codex's progress (stderr) goes to the herdr-job tab when there is one, the answer to $out.
progress=${HERDR_JOB_TTY:-/dev/null}
printf '%s' "$prompt" | codex "${args[@]}" - 2>&1 >/dev/null | tee "$tmp/err" >"$progress" || { cat "$tmp/err" >&2; exit 1; }
answer_chars=$(wc -m <"$out" | tr -d " ")
cat "$out"
