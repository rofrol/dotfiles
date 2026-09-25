#!/usr/bin/env bash
# Runs an ask_* oracle script in its own herdr-job tab, so the user can watch it, and waits for it.
# The caller still gets the answer on stdout, the script's stderr on stderr, and its exit code.
# No desktop notification: the agent that asked reports the answer itself.
# Usage, in an ask_* script once the model is known (ORACLE_IN_JOB stops the recursion):
#   if [ -z "${ORACLE_IN_JOB:-}" ] && [ -n "${HERDR_SOCKET_PATH:-}" ] && command -v herdr-job >/dev/null; then
#     exec ~/.claude/skills/oracle-stats/in_herdr_job.sh "<skill> <model>" "$0" "$@"
#   fi
# Inside the job: ORACLE_STDIN is the file holding the caller's stdin (for -f -),
# and HERDR_JOB_TTY (set by herdr-job) is the job tab, for progress the caller should not get.
set -euo pipefail
skill=$1; shift
dir=$(mktemp -d); trap 'rm -rf "$dir"' EXIT

# The job runs in another pane, so it cannot read our stdin: save it for -f -.
prev=""
for a in "$@"; do
  if [[ $a == - && ($prev == -f || $prev == --file) ]]; then cat >"$dir/stdin"; break; fi
  prev=$a
done

# The job tab starts from its own environment: pass on the round id, the log path and model settings.
passenv=()
while IFS= read -r v; do passenv+=("$v=${!v}"); done < <(compgen -e | grep -E '^(ORACLE_ROUND|ORACLE_LOG|GPT_|GEMINI_|DEEPSEEK_)' || true)

last=${*: -1}
question=$(tr -s '[:space:]' ' ' <<<"$last")
id=$(herdr-job run --name "ask $skill: ${question:0:40}" --why "${question:0:200}" --notify never --cwd "$PWD" -- \
  env ORACLE_IN_JOB=1 ORACLE_STDIN="$dir/stdin" D="$dir" ${passenv[@]+"${passenv[@]}"} \
  bash -c 'set -o pipefail; "$@" 2>"$D/err" | tee "$D/out"; rc=$?; cat "$D/err" >&2; exit $rc' _ "$@")
rc=0; herdr-job wait --quiet "$id" >/dev/null || rc=$?
[ -f "$dir/out" ] && cat "$dir/out"
[ -f "$dir/err" ] && cat "$dir/err" >&2
[ -f "$dir/out" ] || echo "herdr-job $id: no answer (exit $rc); tab closed or job lost? log: $(herdr-job log "$id")" >&2
exit $rc
