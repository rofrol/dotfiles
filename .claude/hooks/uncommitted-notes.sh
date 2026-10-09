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
#   PreToolUse  (Bash)                              -> uncommitted-notes.sh snapshot
#   PostToolUse (Bash)                              -> uncommitted-notes.sh compare
#   Stop                                            -> uncommitted-notes.sh check
#
# record appends the edited path to a per-session ledger. snapshot and compare
# catch files a shell command changed (sed, echo >>, tee), which the edit tools
# never see: the snapshot holds `git status --porcelain` from before the command,
# and the difference afterwards names candidate changes, not proven ownership.
# Concurrent sessions can change status during the same interval. check looks
# only at that ledger; it can still include another session's files.
# It blocks the stop with a one-line reminder; the model either commits, or says
# in its final message that it left the files behind. It stays quiet when the
# final message (the Stop input's last_assistant_message, else the transcript's
# last assistant text) names every dirty file by repo-relative path or basename,
# and it blocks at most once per dirty set: the set and each file's content hash
# go to <ledger>.acked, and only a new dirty path or changed content blocks
# again. No commit happens here: committing on the model's behalf would
# misattribute hunks in shared files.
#
# Known limits: a command that changes a file which was already dirty cannot be
# attributed (the difference does not move), and a session killed without a Stop
# leaves its ledger on disk, which is deliberate - the next session can inspect it.

set -euo pipefail

state_dir=${XDG_STATE_HOME:-$HOME/.local/state}/claude-uncommitted-notes
mkdir -p "$state_dir"

payload=$(cat)
mode=${1:-}

json() { printf '%s' "$payload" | jq -r "$1" 2>/dev/null || true; }

# Content hash of a file, of a directory's files, or "gone" for a deleted path.
content_hash() {
	if [ -f "$1" ]; then
		git hash-object -- "$1" 2>/dev/null || echo unreadable
	elif [ -d "$1" ]; then
		find "$1" -type f 2>/dev/null | LC_ALL=C sort | while IFS= read -r f; do
			printf '%s %s\n' "$(git hash-object -- "$f" 2>/dev/null)" "$f"
		done | git hash-object --stdin
	else
		echo gone
	fi
}

# Text of the last main-chain assistant message in a JSONL transcript. One
# message can span several entries (one per content block) sharing message.id.
last_assistant_text() {
	jq -c 'select(.type == "assistant" and ((.isSidechain // false) | not))
		| {id: (.message.id // ""),
		   text: ([.message.content | if type == "string" then . else (.[]? | select(.type == "text") | .text) end] | join("\n"))}' \
		"$1" 2>/dev/null |
		jq -rs 'if length == 0 then "" else (last.id) as $id | map(select(.id == $id) | .text) | join("\n") end' 2>/dev/null || true
}

# Whether text $1 mentions $2 as a whole name, not inside a longer word.
names() {
	local rest=$1 needle=$2 before after
	[ -n "$needle" ] || return 1
	while [[ $rest == *"$needle"* ]]; do
		before=${rest%%"$needle"*}
		rest=${rest#*"$needle"}
		after=${rest:0:1}
		before=${before: -1}
		[[ $before == [A-Za-z0-9_-] || $after == [A-Za-z0-9_-] ]] || return 0
	done
	return 1
}

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
snapshot)
	# Remember what is dirty just before a shell command runs.
	[ "$(json '.tool_name // ""')" = "Bash" ] || exit 0
	cwd=$(json '.cwd // ""')
	root=$(git -C "${cwd:-$PWD}" rev-parse --show-toplevel 2>/dev/null) || exit 0
	git -C "$root" status --porcelain 2>/dev/null >"$ledger.shell" || : >"$ledger.shell"
	;;
compare)
	# Whatever this command made dirty that was not dirty before it.
	[ "$(json '.tool_name // ""')" = "Bash" ] || exit 0
	[ -f "$ledger.shell" ] || exit 0
	cwd=$(json '.cwd // ""')
	root=$(git -C "${cwd:-$PWD}" rev-parse --show-toplevel 2>/dev/null) || exit 0
	after=$(git -C "$root" status --porcelain 2>/dev/null) || after=""
	new=$(comm -13 <(sort "$ledger.shell") <(printf '%s\n' "$after" | sort) |
		sed -e 's/^...//' -e 's/^.* -> //' -e 's:/$::')
	rm -f "$ledger.shell"
	while IFS= read -r path; do
		[ -n "$path" ] || continue
		case $path in
		/*) printf '%s\n' "$path" >>"$ledger" ;;
		*) printf '%s\n' "$root/$path" >>"$ledger" ;;
		esac
	done <<<"$new"
	;;
check)
	# The reminder already ran for this stop; do not loop.
	[ "$(json '.stop_hook_active // false')" = "true" ] && exit 0
	[ -f "$ledger" ] || exit 0
	cwd=$(json '.cwd // ""')
	root=$(git -C "${cwd:-$PWD}" rev-parse --show-toplevel 2>/dev/null) || root=""
	dirty=()
	while IFS= read -r path; do
		[ -n "$path" ] || continue
		if [ -n "$(git -C "${cwd:-$PWD}" status --porcelain -- "$path" 2>/dev/null)" ]; then
			dirty+=("$path")
		fi
	done < <(sort -u "$ledger")
	if [ ${#dirty[@]} -eq 0 ]; then
		rm -f "$ledger" "$ledger.acked"
		exit 0
	fi

	# The dirty set with each file's content hash, one "hash<TAB>path" per line.
	current=""
	for path in "${dirty[@]}"; do
		current="$current$(content_hash "$path")	$path
"
	done

	# Paths not in the acknowledged set, or whose content changed since then.
	changed=0
	if [ -f "$ledger.acked" ]; then
		while IFS= read -r line; do
			[ -n "$line" ] || continue
			grep -qxF -- "$line" "$ledger.acked" || changed=1
		done <<<"$current"
	else
		changed=1
	fi
	printf '%s' "$current" >"$ledger.acked.tmp" && mv -f "$ledger.acked.tmp" "$ledger.acked"
	[ $changed -eq 1 ] || exit 0

	# The final message already names every dirty file: nothing to remind.
	message=$(json '.last_assistant_message // ""')
	if [ -z "$message" ]; then
		transcript=$(json '.transcript_path // ""')
		[ -f "$transcript" ] && message=$(last_assistant_text "$transcript")
	fi
	shown=()
	unnamed=0
	for path in "${dirty[@]}"; do
		rel=$path
		[ -n "$root" ] && case $path in "$root"/*) rel=${path#"$root"/} ;; esac
		shown+=("$rel")
		names "$message" "$rel" || names "$message" "${path##*/}" || unnamed=1
	done
	[ $unnamed -eq 1 ] || exit 0

	# Shell-observed files are candidates; other sessions may own some hunks.
	reason="Uncommitted (possibly other sessions' edits too): ${shown[*]} - name them as dirty in your final message; this grants no permission to commit, push or continue stopped work, so commit them by path only if committing is authorized."
	jq -n --arg reason "$reason" '{decision: "block", reason: $reason}'
	;;
esac

exit 0
