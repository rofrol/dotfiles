#!/usr/bin/env bash
# Tests for uncommitted-notes.sh check, with sample Stop inputs in temp git repos.
# Usage: bash uncommitted-notes-test.sh [path/to/bash]
set -euo pipefail

shell=${1:-bash}
hook=$(cd "$(dirname "$0")" && pwd)/uncommitted-notes.sh
work=$(mktemp -d "${TEST_TMPDIR:-/tmp}/uncommitted-notes-test.XXXXXX")
trap 'rm -rf "$work"' EXIT
export XDG_STATE_HOME=$work/state
failures=0

repo=$work/repo
git init -q "$repo"
git -C "$repo" -c user.name=t -c user.email=t@t commit -q --allow-empty -m base
mkdir -p "$repo/docs"

run() { "$shell" "$hook" "$@"; }

record() {
	jq -n --arg s "$1" --arg p "$2" --arg c "$repo" \
		'{session_id: $s, cwd: $c, tool_name: "Write", tool_input: {file_path: $p}}' | run record
}

# check <session> <last_assistant_message or ""> [transcript]
check() {
	jq -n --arg s "$1" --arg m "$2" --arg t "${3:-}" --arg c "$repo" \
		'{session_id: $s, cwd: $c, hook_event_name: "Stop", stop_hook_active: false, transcript_path: $t}
		 + (if $m == "" then {} else {last_assistant_message: $m} end)' | run check
}

expect() { # expect <block|pass> <name> <output>
	local got=pass
	[ -z "$3" ] || got=block
	if [ "$got" = "$1" ]; then
		echo "ok   $2"
	else
		echo "FAIL $2: expected $1, got $got: $3"
		failures=$((failures + 1))
	fi
}

# Files named in the final message: no block.
echo one >"$repo/docs/notes.md"
echo two >"$repo/plan.txt"
record s1 "$repo/docs/notes.md"
record s1 "$repo/plan.txt"
# shellcheck disable=SC2016 # the backticks are Markdown in the message
expect pass "files named -> no block" "$(check s1 'Left dirty: `docs/notes.md` and plan.txt.')"
expect pass "named set again -> no block" "$(check s1 'Done.')"

# Not named: block once, with a one-line reason listing repo-relative paths.
echo one >"$repo/a.md"
record s2 "$repo/a.md"
out=$(check s2 'Here is the answer.')
expect block "not named -> block" "$out"
reason=$(printf '%s' "$out" | jq -r .reason)
want="Uncommitted (possibly other sessions' edits too): a.md - name them as dirty in your final message; this grants no permission to commit, push or continue stopped work, so commit them by path only if committing is authorized."
if [ "$reason" = "$want" ]; then
	echo "ok   reason line"
else
	echo "FAIL reason: $reason"
	failures=$((failures + 1))
fi
expect pass "same set again -> no block" "$(check s2 'Here is the answer.')"

# A recorded file's content changes: block again.
echo changed >>"$repo/a.md"
expect block "file changed -> block" "$(check s2 'Here is the answer.')"
expect pass "changed set acknowledged -> no block" "$(check s2 'Here is the answer.')"

# A new path becomes dirty: block again.
echo new >"$repo/b.md"
record s2 "$repo/b.md"
expect block "new file -> block" "$(check s2 'Here is the answer.')"

# A basename inside a longer word does not count as naming it.
echo x >"$repo/a"
record s3 "$repo/a"
expect block "basename inside a word -> block" "$(check s3 'I edited the data.')"

# No last_assistant_message: read the transcript's last main-chain assistant text.
echo y >"$repo/c.md"
record s4 "$repo/c.md"
transcript=$work/s4.jsonl
cat >"$transcript" <<'EOF'
{"type":"user","message":{"role":"user","content":"what should I do?"}}
{"type":"assistant","message":{"id":"m1","content":[{"type":"text","text":"Run the test; c.md stays dirty."}]}}
{"type":"assistant","message":{"id":"m1","content":[{"type":"tool_use","id":"t","name":"Bash","input":{}}]}}
{"type":"assistant","isSidechain":true,"message":{"id":"m2","content":[{"type":"text","text":"subagent text"}]}}
EOF
expect pass "transcript names file -> no block" "$(check s4 '' "$transcript")"

echo z >"$repo/d.md"
record s5 "$repo/d.md"
transcript=$work/s5.jsonl
cat >"$transcript" <<'EOF'
{"type":"assistant","message":{"id":"m1","content":[{"type":"text","text":"d.md is dirty"}]}}
{"type":"assistant","message":{"id":"m2","content":[{"type":"text","text":"Answer only."}]}}
{"type":"assistant","isSidechain":true,"message":{"id":"m3","content":[{"type":"text","text":"d.md"}]}}
EOF
expect block "transcript's earlier or sidechain text does not count -> block" "$(check s5 '' "$transcript")"

shell() { # shell <session> <snapshot|compare> [tool_use_id]
	jq -n --arg s "$1" --arg c "$repo" --arg t "${3:-t1}" \
		'{session_id: $s, cwd: $c, tool_name: "Bash", tool_use_id: $t, tool_input: {command: "x"}}' | run "$2"
}
commit_all() { # commit_all <message> [committer date]
	git -C "$repo" add -A
	GIT_COMMITTER_DATE=${2:-} git -C "$repo" -c user.name=t -c user.email=t@t commit -q -m "$1"
}
commit_all "before the ownership cases"

# Committed after the session's change, then dirtied by someone else: not this session's
# (the user, 2026-10-27; round 20261027-063006-dde5).
echo mine >"$repo/e.md"
record s6 "$repo/e.md"
commit_all "someone commits e.md" "@$(($(date +%s) + 60))" # dated after the recorded change
echo theirs >>"$repo/e.md"
expect pass "committed since, dirtied by another -> no block" "$(check s6 'Answer.')"

# Changed by another session but never committed since: the session's hunk may be in it.
echo mine >"$repo/f.md"
record s7 "$repo/f.md"
echo theirs >>"$repo/f.md"
out=$(check s7 'Answer.')
expect block "changed since, not committed -> block" "$out"
case $(printf '%s' "$out" | jq -r .reason) in
*"Another session also changed f.md"*) echo "ok   reason names the shared file" ;;
*) echo "FAIL shared reason: $out"; failures=$((failures + 1)) ;;
esac

# Another session commits its own hunk of the file the session also changed: the session's hunk stays
# uncommitted, so it is still reported (round 20261027-063407-6080).
printf 'base\n' >"$repo/h.md"
commit_all "h.md"
printf 'base\nmine\n' >"$repo/h.md"
record s10 "$repo/h.md"
printf 'theirs\nbase\n' >"$repo/h.md"
commit_all "their hunk only" "@$(($(date +%s) + 60))"
printf 'theirs\nbase\nmine\n' >"$repo/h.md"
expect block "another session's partial commit -> still block" "$(check s10 'Answer.')"

# A deleted directory does not break the report.
mkdir -p "$repo/gone"
echo z >"$repo/gone/z.md"
record s11 "$repo/gone/z.md"
commit_all "gone/z.md"
rm -rf "$repo/gone"
out=$(check s11 'Answer.')
case $(printf '%s' "$out" | jq -r .reason) in
*"gone/z.md"*) echo "ok   deleted directory reported by its relative path" ;;
*) echo "FAIL deleted directory: $out"; failures=$((failures + 1)) ;;
esac

# A shell command changes an already dirty file: recorded as the session's change.
echo dirty >"$repo/g.md"
shell s8 snapshot
echo more >>"$repo/g.md"
shell s8 compare
out=$(check s8 'Answer.')
expect block "shell edit of a dirty file -> block" "$out"
case $(printf '%s' "$out" | jq -r .reason) in
*"Another session"*) echo "FAIL own shell edit called shared: $out"; failures=$((failures + 1)) ;;
*) echo "ok   own shell edit not called shared" ;;
esac

# A rename records both sides.
echo r >"$repo/old.md"
commit_all "old.md"
shell s9 snapshot
git -C "$repo" mv old.md new.md
shell s9 compare
out=$(check s9 'Answer.')
case $(printf '%s' "$out" | jq -r .reason) in
*old.md*new.md* | *new.md*old.md*) echo "ok   rename records both paths" ;;
*) echo "FAIL rename: $out"; failures=$((failures + 1)) ;;
esac

# Clean files: no block, ledger and acknowledged set removed.
git -C "$repo" add -A
git -C "$repo" -c user.name=t -c user.email=t@t commit -q -m all
expect pass "all committed -> no block" "$(check s2 'Here is the answer.')"
if [ -e "$XDG_STATE_HOME/claude-uncommitted-notes/s2" ] || [ -e "$XDG_STATE_HOME/claude-uncommitted-notes/s2.acked" ]; then
	echo "FAIL ledger left behind"
	failures=$((failures + 1))
else
	echo "ok   ledger removed"
fi

if [ $failures -ne 0 ]; then
	echo "$failures failed"
	exit 1
fi
echo "all passed"
