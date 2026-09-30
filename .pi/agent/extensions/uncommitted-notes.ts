// Remind a Pi session, before it settles, about its own uncommitted edits.
//
// Motivation: several agent sessions share one checkout. A session that stops
// before committing leaves edits nobody can attribute, and another session can
// sweep them into its own commit. The repository rules say "commit your notes
// right away"; this extension is the deterministic net under that rule, and the
// Pi counterpart of ~/.claude/hooks/uncommitted-notes.sh.
//
// How it works:
// - `tool_call` records every file the session edits, with a baseline: whether
//   the file was already dirty when first touched, so the reminder can warn
//   that committing the whole file would take another session's hunks.
// - A `bash` call is bracketed by a `git status --porcelain` snapshot before it
//   and a comparison after it, so files changed by `sed`, `echo >>` or `tee`
//   (which never go through the edit tools) are attributed to this session too.
// - `agent_before_settle` (Pi's equivalent of a stop) checks only those files.
//   It appends one reminder and forces one more request, guarded by a signature
//   of the current dirty set: the same dirty set never reminds twice, and a
//   clean set ends the episode. Without that guard the continuation could loop.
//
// No auto-commit: committing for the model would misattribute hunks in shared
// files, which is worse than leaving them visible. A file that a shell command
// changed while it was already dirty cannot be attributed either.

import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";

const EDIT_TOOLS = new Set(["edit", "write"]);

/** The path of one `git status --porcelain` line, renames included. */
function path_of(line: string): string {
	const body = line.includes(" -> ") ? line.slice(line.indexOf(" -> ") + 4) : line.slice(3);
	return body.trim().replace(/\/$/, "");
}

interface Touch {
	dirtyBefore: boolean;
}

export default function (pi: any) {
	const touched = new Map<string, Touch>();
	let reminded = "";
	const shellSnapshots = new Map<string, string>();

	function git(cwd: string, args: string[]): string | null {
		try {
			// Strip only trailing newlines: porcelain's two status columns start
			// with a space for worktree-only changes, and trimming it would shift
			// every path by one character.
			return execFileSync("git", args, {
				cwd,
				encoding: "utf8",
				stdio: ["ignore", "pipe", "ignore"],
				timeout: 3000,
			}).replace(/[\r\n]+$/, "");
		} catch {
			return null; // not a repository, or git unavailable
		}
	}

	function status(cwd: string, paths: string[]): string | null {
		if (paths.length === 0) return "";
		return git(cwd, ["status", "--porcelain", "--", ...paths]);
	}

	pi.on("tool_call", (event: any, ctx: any) => {
		const cwd = typeof ctx?.cwd === "string" && ctx.cwd.length > 0 ? ctx.cwd : process.cwd();
		if (event?.toolName === "bash") {
			// Bracket the command: what is dirty before it is not its doing.
			const before = git(cwd, ["status", "--porcelain"]);
			if (before !== null && typeof event?.toolCallId === "string") {
				shellSnapshots.set(event.toolCallId, before);
			}
			return;
		}
		if (!EDIT_TOOLS.has(event?.toolName)) return;
		const raw = event?.input?.path ?? event?.input?.file_path;
		if (typeof raw !== "string" || raw.length === 0) return;
		const absolute = raw.startsWith("/") ? raw : `${cwd}/${raw}`;
		if (touched.has(absolute)) return;
		const before = status(cwd, [absolute]);
		touched.set(absolute, { dirtyBefore: before !== null && before.length > 0 });
	});

	pi.on("tool_result", (event: any, ctx: any) => {
		const id = event?.toolCallId;
		if (typeof id !== "string") return;
		const before = shellSnapshots.get(id);
		if (before === undefined) return;
		shellSnapshots.delete(id);
		const cwd = typeof ctx?.cwd === "string" && ctx.cwd.length > 0 ? ctx.cwd : process.cwd();
		const after = git(cwd, ["status", "--porcelain"]);
		if (after === null) return;
		const seen = new Set(before.split("\n"));
		const root = git(cwd, ["rev-parse", "--show-toplevel"]);
		if (root === null) return;
		for (const line of after.split("\n")) {
			if (line.length === 0 || seen.has(line)) continue;
			const name = path_of(line);
			if (name === "") continue;
			const absolute = name.startsWith("/") ? name : `${root}/${name}`;
			if (!touched.has(absolute)) touched.set(absolute, { dirtyBefore: false });
		}
	});

	pi.on("agent_before_settle", (event: any, ctx: any) => {
		if (touched.size === 0) return;
		const cwd = typeof ctx?.cwd === "string" && ctx.cwd.length > 0 ? ctx.cwd : process.cwd();
		const paths = [...touched.keys()];
		const dirty = status(cwd, paths);
		if (dirty === null) return; // not a repository: nothing to say
		if (dirty.length === 0) {
			reminded = "";
			return;
		}
		const signature = createHash("sha1").update(dirty).digest("hex");
		if (signature === reminded) return;
		reminded = signature;
	const files = dirty
			.split("\n")
			.map(path_of)
			.filter(Boolean);
		const preexisting = [...touched.entries()]
			.filter(([name, touch]) => touch.dirtyBefore && files.includes(name))
			.map(([name]) => name);
		let content =
			`You have your own uncommitted edits: ${files.join(", ")}. ` +
			"Check `git diff -- <path>` shows only your hunks, then commit them with an explicit path " +
			'(`git commit -m "..." -- <path>`), or name them in your final message if this repository forbids agent commits.';
		if (preexisting.length > 0) {
			content +=
				` ${preexisting.join(", ")} was already dirty before your first edit, so another session may have ` +
				"hunks in it: commit only your own parts (`git add -p`) or name the file instead of committing it whole.";
		}
		return {
			entries: [
				{
					type: "custom_message",
					customType: "uncommitted_notes",
					content,
					display: false,
				},
			],
			continue: true,
		};
	});
}
