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
// - `agent_before_settle` (Pi's equivalent of a stop) checks only those files.
//   It appends one reminder and forces one more request, guarded by a signature
//   of the current dirty set: the same dirty set never reminds twice, and a
//   clean set ends the episode. Without that guard the continuation could loop.
//
// No auto-commit: committing for the model would misattribute hunks in shared
// files, which is worse than leaving them visible. Edits made through bash
// (sed, >>) are not recorded, because they do not go through the edit tools.

import { execFileSync } from "node:child_process";
import { createHash } from "node:crypto";

const EDIT_TOOLS = new Set(["edit", "write"]);

interface Touch {
	dirtyBefore: boolean;
}

export default function (pi: any) {
	const touched = new Map<string, Touch>();
	let reminded = "";

	function git(cwd: string, args: string[]): string | null {
		try {
			return execFileSync("git", args, {
				cwd,
				encoding: "utf8",
				stdio: ["ignore", "pipe", "ignore"],
				timeout: 3000,
			}).trim();
		} catch {
			return null; // not a repository, or git unavailable
		}
	}

	function status(cwd: string, paths: string[]): string | null {
		if (paths.length === 0) return "";
		const raw = git(cwd, ["status", "--porcelain", "--", ...paths]);
		if (raw === null) return null;
		// Keep leading spaces: the two status columns of porcelain start with
		// one for worktree-only changes, and trimming it shifts every path.
		return raw.replace(/[\r\n]+$/, "");
	}

	pi.on("tool_call", (event: any, ctx: any) => {
		if (!EDIT_TOOLS.has(event?.toolName)) return;
		const raw = event?.input?.path ?? event?.input?.file_path;
		if (typeof raw !== "string" || raw.length === 0) return;
		const cwd = typeof ctx?.cwd === "string" && ctx.cwd.length > 0 ? ctx.cwd : process.cwd();
		const absolute = raw.startsWith("/") ? raw : `${cwd}/${raw}`;
		if (touched.has(absolute)) return;
		const before = status(cwd, [absolute]);
		touched.set(absolute, { dirtyBefore: before !== null && before.length > 0 });
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
			.map((line) => (line.includes(" -> ") ? line.slice(line.indexOf(" -> ") + 4) : line.slice(3)))
			.map((name) => name.trim())
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
