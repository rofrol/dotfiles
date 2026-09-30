#!/usr/bin/env bash
# Remember the files a session edited, then remind that session before it ends.
#
# Motivation: several agent sessions share one checkout. A session that stops
# before committing leaves edits nobody can attribute, and another session can
# sweep them into its own commit. The rule "commit your notes right away" is in
# the repository instructions; this hook is the deterministic net under it.
#
# Usage as Claude Code hooks (see ~/.claude/settings.json):
#   PostToolUse (Edit|Write|MultiEdit|NotebookEdit) -> uncommitted-notes.sh record
#   Stop                                            -> uncommitted-notes.sh check
#
# record appends the edited path to a per-session ledger. check looks only at
# that ledger, so it never nags a session about another session's files. It
# blocks the stop once with a reminder; the model either commits, or says in
# its final message that it left the files behind. No commit happens here:
# committing on the model's behalf would misattribute hunks in shared files.
#
# Known limits: edits made through Bash (sed, >>) are not recorded, because the
# hook only sees tool calls; a session killed without a Stop leaves its ledger
# on disk, which is deliberate - the next session can inspect it.

set -euo pipefail

state_dir=${XDG_STATE_HOME:-$HOME/.local/state}/claude-uncommitted-notes
mkdir -p "$state_dir"

payload=$(cat)
mode=${1:-}

json() { printf '%s' "$payload" | jq -r "$1" 2>/dev/null || true; }

session_id=$(json '.session_id // ""')
[ -n "$session_id" ] || exit 0
ledger=$state_dir/$session_id

case $mode in
record)
	case $(json '.tool_name // ""') in
	Edit | Write | MultiEdit | NotebookEdit) ;;
	*) exit 0 ;;
	esac
	path=$(json '.tool_input.file_path // .tool_input.path // ""')
	[ -n "$path" ] || exit 0
	cwd=$(json '.cwd // ""')
	case $path in
	/*) ;;
	*) path=${cwd:-$PWD}/$path ;;
	esac
	printf '%s\n' "$path" >>"$ledger"
	;;
check)
	# The reminder already ran for this stop; do not loop.
	[ "$(json '.stop_hook_active // false')" = "true" ] && exit 0
	[ -f "$ledger" ] || exit 0
	cwd=$(json '.cwd // ""')
	dirty=()
	while IFS= read -r path; do
		[ -n "$path" ] || continue
		if [ -n "$(git -C "${cwd:-$PWD}" status --porcelain -- "$path" 2>/dev/null)" ]; then
			dirty+=("$path")
		fi
	done < <(sort -u "$ledger")
	if [ ${#dirty[@]} -eq 0 ]; then
		rm -f "$ledger"
		exit 0
	fi
	reason="You edited these files in this session and they are still uncommitted: ${dirty[*]}. Check 'git diff -- <path>' shows only your hunks, then commit them with an explicit path ('git commit -m \"...\" -- <path>'), or, if this repository forbids agent commits, name these files in your final message instead."
	jq -n --arg reason "$reason" '{decision: "block", reason: $reason}'
	;;
esac

exit 0
