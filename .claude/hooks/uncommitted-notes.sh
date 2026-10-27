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
# record appends the edited path to a per-session ledger, with the file's content
# hash after the edit and the time ("path<TAB>hash<TAB>epoch"). snapshot and
# compare catch files a shell command changed (sed, echo >>, tee), which the edit
# tools never see: the snapshot holds `git status --porcelain` and the hash of
# each dirty file from before the command; afterwards every path that became
# dirty or whose content changed is recorded with its new hash. These are
# candidates, not proven ownership: concurrent sessions can change files during
# the same command.
# check reports a dirty path of the ledger only when it may still hold this
# session's change (the user, 2026-10-27, after a session was told about files
# another session had dirtied; round 20261027-063006-dde5): its content still is
# what the session left, or it changed since and no commit since holds exactly
# the content the session left (its hunk may be in there; the reason says
# another session changed it too). A path whose session content was committed
# and which was then dirtied again is someone else's. A line without a hash
# counts as before.
# It blocks the stop with a one-line reminder; the model either commits, or says
# in its final message that it left the files behind. It stays quiet when the
# final message (the Stop input's last_assistant_message, else the transcript's
# last assistant text) names every dirty file by repo-relative path or basename,
# and it blocks at most once per dirty set: the set and each file's content hash
# go to <ledger>.acked, and only a new dirty path or changed content blocks
# again. No commit happens here: committing on the model's behalf would
# misattribute hunks in shared files.
#
# Known limits: a path with a tab or newline, or one git status quotes, is not
# followed; a file added inside an already untracked directory does not show;
# equal bytes are not proof of authorship; a session killed without a
# Stop leaves its ledger on disk, which is deliberate - the next session can
# inspect it.

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

# The paths of `git status --porcelain` lines on stdin, both sides of a rename.
status_paths() {
	awk '{ line = substr($0, 4); sub(/\/$/, "", line); n = split(line, side, / -> /); for (i = 1; i <= n; i++) print side[i] }'
}

# A path with its directory's symlinks resolved (/tmp is /private/tmp on macOS), so one file has one ledger key;
# the path itself when its directory is gone.
physical() {
	local dir
	dir=$(cd "$(dirname "$1")" 2>/dev/null && pwd -P) || { printf '%s' "$1"; return 0; }
	printf '%s/%s' "$dir" "${1##*/}"
}

# Whether a commit since epoch $3 holds path $1 with exactly blob $2: the session's change to it was committed.
committed_since() {
	local commit
	while IFS= read -r commit; do
		[ "$(git -C "$(dirname "$1")" rev-parse -q --verify "$commit:./${1##*/}" 2>/dev/null)" = "$2" ] && return 0
	done < <(git -C "$(dirname "$1")" log --format=%H --since="@$(($3 - 1))" -- "$1" 2>/dev/null)
	return 1
}

# A path relative to the repository root when it is inside it (through symlinks too, /tmp on macOS).
relative() {
	local path=$1 real
	[ -n "$root" ] || { printf '%s' "$path"; return; }
	case $path in "$root"/*) printf '%s' "${path#"$root"/}"; return ;; esac
	real=$(physical "$path")
	case $real in "$root"/*) printf '%s' "${real#"$root"/}" ;; *) printf '%s' "$path" ;; esac
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
	path=$(physical "$path")
	printf '%s\t%s\t%s\n' "$path" "$(content_hash "$path")" "$(date +%s)" >>"$ledger"
	;;
snapshot)
	# Remember what is dirty just before a shell command runs.
	[ "$(json '.tool_name // ""')" = "Bash" ] || exit 0
	cwd=$(json '.cwd // ""')
	root=$(git -C "${cwd:-$PWD}" rev-parse --show-toplevel 2>/dev/null) || exit 0
	# One snapshot per call (parallel Bash calls of one session), published whole: status lines, then
	# "hash<TAB>path" of each dirty path after a "--" line, so a change to an already dirty file shows.
	snap=$ledger.shell.$(json '.tool_use_id // "call"')
	{
		git -C "$root" status --porcelain 2>/dev/null || :
		echo --
		git -C "$root" status --porcelain 2>/dev/null | status_paths | while IFS= read -r path; do
			printf '%s\t%s\n' "$(content_hash "$root/$path")" "$path"
		done
	} >"$snap.tmp" && mv -f "$snap.tmp" "$snap"
	;;
compare)
	# Whatever this command made dirty that was not dirty before it.
	[ "$(json '.tool_name // ""')" = "Bash" ] || exit 0
	snap=$ledger.shell.$(json '.tool_use_id // "call"')
	[ -f "$snap" ] || exit 0
	cwd=$(json '.cwd // ""')
	root=$(git -C "${cwd:-$PWD}" rev-parse --show-toplevel 2>/dev/null) || exit 0
	root=$(physical "$root/.")
	root=${root%/.}
	after=$(git -C "$root" status --porcelain 2>/dev/null) || after=""
	new=$(comm -13 <(sed '/^--$/,$d' "$snap" | sort) <(printf '%s\n' "$after" | sort) | status_paths)
	# Already dirty before the command, and changed by it (or meanwhile).
	touched=$(sed '1,/^--$/d' "$snap" | while IFS=$'\t' read -r hash path; do
		[ -n "$path" ] || continue
		[ "$(content_hash "$root/$path")" = "$hash" ] || printf '%s\n' "$path"
	done)
	rm -f "$snap"
	now=$(date +%s)
	while IFS= read -r path; do
		[ -n "$path" ] || continue
		printf '%s\t%s\t%s\n' "$root/$path" "$(content_hash "$root/$path")" "$now" >>"$ledger"
	done < <(printf '%s\n%s\n' "$new" "$touched" | sort -u)
	;;
check)
	# The reminder already ran for this stop; do not loop.
	[ "$(json '.stop_hook_active // false')" = "true" ] && exit 0
	[ -f "$ledger" ] || exit 0
	cwd=$(json '.cwd // ""')
	root=$(git -C "${cwd:-$PWD}" rev-parse --show-toplevel 2>/dev/null) || root=""
	dirty=()
	shared=()
	# The last line per path: the session's latest change to it.
	while IFS=$'\t' read -r path hash when; do
		[ -n "$path" ] || continue
		[ -n "$(git -C "${cwd:-$PWD}" status --porcelain -- "$path" 2>/dev/null)" ] || continue
		if [ -z "$hash" ] || [ "$(content_hash "$path")" = "$hash" ]; then
			dirty+=("$path")
		elif ! committed_since "$path" "$hash" "${when:-0}"; then
			dirty+=("$path") # changed since, and its content never committed: the session's hunk may be in it
			shared+=("$path")
		fi # a commit since holds exactly what the session left, so the change now is someone else's
	done < <(awk -F'\t' '{last[$1] = $0} END {for (p in last) print last[p]}' "$ledger" | LC_ALL=C sort)
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
	also=()
	unnamed=0
	for path in "${dirty[@]}"; do
		rel=$(relative "$path")
		shown+=("$rel")
		names "$message" "$rel" || names "$message" "${path##*/}" || unnamed=1
	done
	for path in ${shared[@]+"${shared[@]}"}; do
		also+=("$(relative "$path")")
	done
	[ $unnamed -eq 1 ] || exit 0

	# Shell-observed files are candidates; other sessions may own some hunks.
	reason="Uncommitted (possibly other sessions' edits too): ${shown[*]} - name them as dirty in your final message; this grants no permission to commit, push or continue stopped work, so commit them by path only if committing is authorized."
	[ ${#also[@]} -eq 0 ] || reason="$reason Another session also changed ${also[*]} after your last change to it, so part of it may not be yours."
	jq -n --arg reason "$reason" '{decision: "block", reason: $reason}'
	;;
esac

exit 0
