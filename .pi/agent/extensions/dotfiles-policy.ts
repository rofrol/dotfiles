// Loads the dotfiles development policy for Pi, but only when the working
// repository is the dotfiles repository: its git worktree is HOME, so its git
// root is HOME itself.
//
// Why not ~/AGENTS.md: Pi loads AGENTS.md from the working directory and every
// parent directory, so a file with that name in HOME would apply to every
// unrelated repository below HOME. The policy therefore has a different name
// (~/AGENTS.policy.md) and this extension injects it as a named prompt section,
// so it survives compaction and is not appended as a message.
//
// This replaced a shim in PATH (~/scripts/dotfiles-shim/pi): Herdr launches Pi
// with ~/.pi/agent/bin first in PATH, so agents never went through that shim.
// A user-dir extension loads in every Pi session regardless of PATH.

import { execFileSync } from "node:child_process";
import { readFileSync, realpathSync } from "node:fs";
import { homedir } from "node:os";
import path from "node:path";

const policyPath = path.join(homedir(), "AGENTS.policy.md");
const section = "dotfiles_policy";
const marker = "[Dotfiles policy]";

function canonical(target: string): string {
	try {
		return realpathSync(target);
	} catch {
		return target;
	}
}

const home = canonical(homedir());

/** The policy text when cwd belongs to the dotfiles repository, else null. */
function policyFor(cwd: string): string | null {
	let root: string;
	try {
		// One short-lived git call per agent run, never per token.
		root = execFileSync("git", ["rev-parse", "--show-toplevel"], {
			cwd,
			encoding: "utf8",
			stdio: ["ignore", "pipe", "ignore"],
			timeout: 2000,
		}).trim();
	} catch {
		return null; // not a repository, git missing, or the call timed out
	}
	if (canonical(root) !== home) {
		return null;
	}
	try {
		const policy = readFileSync(policyPath, "utf8").trim();
		return policy.length > 0 ? policy : null;
	} catch {
		return null; // missing or unreadable policy: run without it
	}
}

export default function (pi: any) {
	pi.on("before_agent_start", (event: any, ctx: any) => {
		const cwd = typeof ctx?.cwd === "string" && ctx.cwd.length > 0 ? ctx.cwd : process.cwd();
		const policy = policyFor(cwd);
		if (policy === null) {
			return;
		}
		const text = `${marker}\n${policy}`;
		if (event?.systemPromptOptions?.sections) {
			event.systemPromptOptions.sections[section] = text;
			return;
		}
		// Older Pi releases expose only systemPrompt; preserve all existing text.
		if (typeof event?.systemPrompt === "string" && !event.systemPrompt.includes(marker)) {
			return { systemPrompt: `${event.systemPrompt}\n\n${text}` };
		}
	});
}
