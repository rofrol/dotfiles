/**
 * bg-bash: answer the user at once while a long bash command runs, like
 * Claude Code.
 *
 * pi delivers a message typed while a tool runs only after that tool returns,
 * so a blocking `herdr-job wait` keeps the user waiting minutes for an answer.
 * This extension replaces the bash tool with the built-in one running on
 * wrapped operations: when the user sends a steering message while bash
 * commands run, each running command is detached into the background (it keeps
 * running, its output goes to a log), the tool call returns at once, and pi
 * delivers the message. When a detached command exits (the child's exit, an
 * event, not a timer), the agent is woken with a `bg-job-done` message carrying
 * the exit code and the log tail. Abort (Esc) still kills attached commands;
 * detached ones end with their command or at session shutdown.
 */

import { createWriteStream, mkdirSync, readFileSync, renameSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import {
	type BashOperations,
	createBashToolDefinition,
	createLocalBashOperations,
	type ExtensionAPI,
} from "@earendil-works/pi-coding-agent";

const DIR = join(process.env.TMPDIR ?? "/tmp", "pi-bg");
const TAIL_LINES = 40;

interface Run {
	id: string;
	command: string;
	log: string;
	started: number;
	detach: () => void;
	kill: () => void;
	detached: boolean;
	done: boolean;
}

function tail(path: string, lines: number): string {
	try {
		const all = readFileSync(path, "utf8").trimEnd().split("\n");
		const out = all.slice(-lines).join("\n");
		return all.length > lines ? `[... ${all.length - lines} earlier lines in the log]\n${out}` : out;
	} catch {
		return "(log unreadable)";
	}
}

function writeAtomic(path: string, data: string) {
	const tmp = `${path}.tmp-${process.pid}`;
	writeFileSync(tmp, data);
	renameSync(tmp, path);
}

export default function (pi: ExtensionAPI) {
	const runs = new Map<string, Run>();
	let seq = 0;
	let shuttingDown = false;
	const local = createLocalBashOperations();

	function wake(run: Run, status: string) {
		run.done = true;
		const secs = Math.round((Date.now() - run.started) / 1000);
		writeAtomic(join(DIR, `${run.id}.status`), JSON.stringify({ id: run.id, command: run.command, status, secs }) + "\n");
		if (shuttingDown) return;
		pi.sendMessage(
			{
				customType: "bg-job-done",
				content:
					`Background command ${run.id} finished after ${secs}s: ${status}.\n` +
					`Command: ${run.command}\nLog: ${run.log}\nLast lines:\n${tail(run.log, TAIL_LINES)}`,
				display: true,
				details: { id: run.id, status, secs, log: run.log },
			},
			{ triggerTurn: true, deliverAs: "steer" },
		);
	}

	const operations: BashOperations = {
		exec(command, cwd, { onData, signal, timeout, env }) {
			mkdirSync(DIR, { recursive: true });
			const id = `${process.pid}-${++seq}`;
			const log = join(DIR, `${id}.log`);
			const logStream = createWriteStream(log);
			const inner = new AbortController();
			let attached = true;
			let timedOut = false;
			const onAbort = () => {
				if (attached) inner.abort();
			};
			signal?.addEventListener("abort", onAbort, { once: true });
			// The model's own timeout (an external deadline it chose) applies only
			// while the command is attached to the tool call.
			const timer =
				timeout !== undefined
					? setTimeout(() => {
							if (!attached) return;
							timedOut = true;
							inner.abort();
						}, timeout * 1000)
					: undefined;
			const release = () => {
				attached = false;
				if (timer) clearTimeout(timer);
				signal?.removeEventListener("abort", onAbort);
			};

			return new Promise((resolve, reject) => {
				const run: Run = {
					id,
					command,
					log,
					started: Date.now(),
					detached: false,
					done: false,
					detach: () => {
						if (!attached) return;
						release();
						run.detached = true;
						onData(
							Buffer.from(
								`\n[bg-bash: the user sent a message, so this command was moved to the background. It has NOT finished: ` +
									`the output above is partial and its exit status is unknown, so do not assume it succeeded. It keeps ` +
									`running as job ${id} (log ${log}). Answer the user; you will get a bg-job-done message with the exit ` +
									`status when it exits. Do not re-run, wait for or poll it.]\n`,
							),
						);
						resolve({ exitCode: 0 });
					},
					kill: () => inner.abort(),
				};
				runs.set(id, run);

				local
					.exec(command, cwd, {
						onData: (data) => {
							logStream.write(data);
							if (attached) onData(data);
						},
						signal: inner.signal,
						env,
					})
					.then(
						(result) => {
							logStream.end();
							if (attached) {
								release();
								runs.delete(id);
								resolve(result);
							} else {
								wake(run, result.exitCode === null ? "failed (no exit code)" : `exit code ${result.exitCode}`);
							}
						},
						(err: unknown) => {
							logStream.end();
							if (attached) {
								release();
								runs.delete(id);
								reject(timedOut ? new Error(`timeout:${timeout}`) : err);
							} else {
								wake(run, `ended: ${err instanceof Error ? err.message : String(err)}`);
							}
						},
					);
			});
		},
	};

	const definitions = new Map<string, ReturnType<typeof createBashToolDefinition>>();
	const definitionFor = (cwd: string) => {
		let def = definitions.get(cwd);
		if (!def) {
			def = createBashToolDefinition(cwd, { operations });
			definitions.set(cwd, def);
		}
		return def;
	};
	const base = definitionFor(process.cwd());
	pi.registerTool({
		...base,
		execute: (toolCallId, params, signal, onUpdate, ctx) =>
			definitionFor(ctx.cwd).execute(toolCallId, params, signal, onUpdate, ctx),
	});

	pi.on("input", (event) => {
		if (event.streamingBehavior !== "steer" || event.source === "extension") return { action: "continue" };
		for (const run of runs.values()) if (!run.detached) run.detach();
		return { action: "continue" };
	});

	pi.registerCommand("bg", {
		description: "List bash commands moved to the background",
		handler: async (_args, ctx) => {
			const list = [...runs.values()].filter((r) => r.detached);
			ctx.ui.notify(
				list.length === 0
					? "No background commands"
					: list.map((r) => `${r.id} ${r.done ? "done" : "running"} — ${r.command.slice(0, 80)}`).join("\n"),
				"info",
			);
		},
	});

	pi.on("session_shutdown", async () => {
		shuttingDown = true;
		for (const run of runs.values()) if (!run.done) run.kill();
	});
}
