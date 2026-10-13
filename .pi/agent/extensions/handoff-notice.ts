// Read-only startup hint; discovery never claims a note.
import { execFile } from "node:child_process";
import { homedir } from "node:os";
import { join } from "node:path";
import { promisify } from "node:util";
import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";

const exec = promisify(execFile);

export default function (pi: ExtensionAPI) {
	pi.on("session_start", async (_event, ctx) => {
		if (!ctx.hasUI) return;
		try {
			// Match ~/.zshenv even when the launching shell did not read it.
			const { stdout } = await exec("roagent", ["handoff", "show", "--repo", "."], {
				cwd: ctx.cwd,
				env: { ...process.env, PATH: `${join(homedir(), "personal_projects/roagent/bin")}:${process.env.PATH ?? ""}` },
			});
			if (/^  pending; unclaimed; owner /m.test(stdout)) {
				ctx.ui.notify("Pending handoff notes for this repository: use /skill:handoff --take.", "info");
			}
		} catch {
			// No notes, no repository, or missing roagent: startup stays silent.
		}
	});
}
