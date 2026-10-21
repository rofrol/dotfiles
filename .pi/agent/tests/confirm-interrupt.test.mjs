import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { homedir } from "node:os";
import { join, resolve } from "node:path";
import { createRequire } from "node:module";
import test from "node:test";

const install = join(homedir(), ".pi/agent/install");
const version = readFileSync(join(install, "current-version"), "utf8").trim();
const modules = join(install, "releases", version, "node_modules");
const require = createRequire(join(modules, "jiti/package.json"));
const { createJiti } = require("jiti");
const agent = join(modules, "@earendil-works/pi-coding-agent/dist/index.js");
const jiti = createJiti(import.meta.url, { alias: { "@earendil-works/pi-coding-agent": agent } });
const extension = await jiti.import(resolve(import.meta.dirname, "../extensions/confirm-interrupt.ts"));
const {
	default: register,
	askToStop,
	installInterruptGate,
	wrapEditorFactory,
	WRAPPED_EDITOR_FACTORIES,
	KEEP_OPTION,
	STOP_OPTION,
} = extension;

const ESC = "\x1b";
const matches = (data, action) => data === ESC && action === "app.interrupt";
const tick = () => new Promise((resolve) => setImmediate(resolve));

function editorFixture() {
	const seen = [];
	const editor = {
		seen,
		autocomplete: false,
		shortcut: false,
		handleInput(data) {
			seen.push(data);
		},
		isShowingAutocomplete() {
			return this.autocomplete;
		},
		onEscape() {
			seen.push("escape-action");
		},
		onExtensionShortcut() {
			return this.shortcut;
		},
		actionHandlers: new Map(),
	};
	return editor;
}

function stopDialogFixture() {
	const state = { choice: KEEP_OPTION, busy: true, generation: 0, registered: undefined, shown: 0 };
	return {
		state,
		select() {
			state.shown += 1;
			return Promise.resolve(state.choice);
		},
		isBusy: () => state.busy,
		generation: () => state.generation,
		setDialog: (controller) => {
			state.registered = controller;
		},
	};
}

test("askToStop runs the stop only for the work it was opened for", async () => {
	const keep = stopDialogFixture();
	assert.equal(await askToStop(keep), false);
	assert.equal(keep.state.shown, 1);
	assert.equal(keep.state.registered, undefined);

	const cancel = stopDialogFixture();
	cancel.state.choice = undefined;
	assert.equal(await askToStop(cancel), false);

	const stop = stopDialogFixture();
	stop.state.choice = STOP_OPTION;
	assert.equal(await askToStop(stop), true);

	const ended = stopDialogFixture();
	ended.state.choice = STOP_OPTION;
	ended.state.busy = false;
	assert.equal(await askToStop(ended), false);
	assert.equal(ended.state.shown, 0, "nothing to stop means no dialog");

	// The work ended or was replaced while the dialog was open: a late
	// confirmation must not run Escape's idle behavior and must not abort a
	// replacement run.
	const late = stopDialogFixture();
	late.select = async () => {
		late.state.generation += 1;
		return STOP_OPTION;
	};
	assert.equal(await askToStop(late), false);

	const replaced = stopDialogFixture();
	replaced.select = async () => {
		replaced.state.busy = false;
		return STOP_OPTION;
	};
	assert.equal(await askToStop(replaced), false);
});

test("askToStop dismisses without stopping when its dialog is aborted", async () => {
	const dialog = stopDialogFixture();
	dialog.select = (_title, _options, opts) =>
		new Promise((resolve) => {
			opts.signal.addEventListener("abort", () => resolve(undefined), { once: true });
		});
	const pending = askToStop(dialog);
	assert.ok(dialog.state.registered, "the open dialog is registered for dismissal");
	dialog.state.registered.abort();
	assert.equal(await pending, false);
});

test("the interrupt key opens the dialog while work runs", async () => {
	const editor = editorFixture();
	const state = { busy: true, stop: true, asked: 0 };
	installInterruptGate(editor, matches, {
		isBusy: () => state.busy,
		confirmStop: async () => {
			state.asked += 1;
			return state.stop;
		},
	});

	editor.handleInput(ESC);
	assert.equal(state.asked, 1);
	assert.deepEqual(editor.seen, [], "the key is consumed until the user answers");
	await tick();
	assert.deepEqual(editor.seen, ["escape-action"], "stopping runs the action Escape would run");

	state.stop = false;
	editor.handleInput(ESC);
	await tick();
	assert.deepEqual(editor.seen, ["escape-action"], "keeping working runs nothing");
});

test("everything else Escape does is left alone", async () => {
	const editor = editorFixture();
	const state = { busy: true, asked: 0 };
	installInterruptGate(editor, matches, {
		isBusy: () => state.busy,
		confirmStop: async () => {
			state.asked += 1;
			return true;
		},
	});

	state.busy = false;
	editor.handleInput(ESC);
	assert.equal(state.asked, 0, "idle Escape keeps its double-Esc and bash-mode behavior");
	assert.deepEqual(editor.seen, [ESC]);

	state.busy = true;
	editor.autocomplete = true;
	editor.handleInput(ESC);
	assert.equal(state.asked, 0, "Escape cancels autocomplete, it does not stop work");
	assert.deepEqual(editor.seen, [ESC, ESC]);

	editor.autocomplete = false;
	editor.shortcut = true;
	editor.handleInput(ESC);
	assert.equal(state.asked, 0, "extension shortcuts win over the interrupt binding");
	assert.deepEqual(editor.seen, [ESC, ESC]);

	const paste = editorFixture();
	let pasted = 0;
	paste.handleInput = () => {
		pasted += 1;
	};
	installInterruptGate(
		paste,
		(data, action) => data === "paste" && action === "app.clipboard.pasteImage",
		{ isBusy: () => true, confirmStop: async () => true },
	);
	paste.handleInput("paste");
	assert.equal(pasted, 1, "the clipboard paste key is passed through");
});

test("a broken check degrades to Pi's own handling", async () => {
	const editor = editorFixture();
	installInterruptGate(editor, matches, {
		isBusy: () => {
			throw new Error("stale context");
		},
		confirmStop: async () => true,
	});
	editor.handleInput(ESC);
	assert.deepEqual(editor.seen, [ESC]);
});

test("without onEscape the app.interrupt handler runs", async () => {
	const editor = editorFixture();
	editor.onEscape = undefined;
	let ran = 0;
	editor.actionHandlers.set("app.interrupt", () => {
		ran += 1;
	});
	installInterruptGate(editor, matches, { isBusy: () => true, confirmStop: async () => true });
	editor.handleInput(ESC);
	await tick();
	assert.equal(ran, 1);
});

test("editor factories compose and stay idempotent", () => {
	const decorations = [];
	let installed;
	let notifyType;
	const ctx = {
		ui: {
			getEditorComponent: () => installed,
			setEditorComponent: (factory) => {
				installed = factory;
			},
			notify: (_message, type) => {
				notifyType = type;
			},
		},
	};
	const tui = { requestRender() {} };
	const theme = { borderColor: (x) => x, selectList: {} };
	const keybindings = { matches: () => false };
	const built = [];
	const first = (t, th, kb) => {
		const editor = editorFixture();
		built.push([editor, t, th, kb]);
		return editor;
	};
	installed = first;

	wrapEditorFactory(ctx, "one", (editor) => decorations.push(["one", editor]));
	assert.deepEqual(installed[WRAPPED_EDITOR_FACTORIES], ["one"]);
	const editorA = installed(tui, theme, keybindings);
	assert.deepEqual(decorations, [["one", editorA]]);
	assert.equal(built.length, 1, "the previous factory builds the editor");

	wrapEditorFactory(ctx, "two", (editor) => decorations.push(["two", editor]));
	assert.deepEqual(installed[WRAPPED_EDITOR_FACTORIES], ["one", "two"], "markers carry through composition");
	const editorB = installed(tui, theme, keybindings);
	assert.deepEqual(decorations.slice(-2), [["one", editorB], ["two", editorB]], "both decorate the same instance");

	const factoryAfterTwo = installed;
	wrapEditorFactory(ctx, "one", (editor) => decorations.push(["one", editor]));
	assert.equal(installed, factoryAfterTwo, "a kind registers once, so session_start cannot stack wrappers");

	wrapEditorFactory(ctx, "boom", () => {
		throw new Error("nope");
	});
	assert.equal(notifyType, undefined);
	const editorC = installed(tui, theme, keybindings);
	assert.ok(editorC, "a broken decoration still yields a working editor");
	assert.equal(notifyType, "error");
});

test("registration wires the gate and binds the dialog to the targeted work", async () => {
	const handlers = {};
	const pi = {
		on(event, handler) {
			handlers[event] = handler;
		},
	};
	register(pi);

	const editor = editorFixture();
	let resolveChoice;
	const ui = {
		installed: (_t, _th, _kb) => editor,
		getEditorComponent() {
			return this.installed;
		},
		setEditorComponent(factory) {
			this.installed = factory;
		},
		notify() {},
		select: () => new Promise((resolve) => {
			resolveChoice = resolve;
		}),
	};
	const session = { busy: true };
	const ctx = { mode: "tui", isIdle: () => !session.busy, ui };

	handlers.session_start({}, ctx);
	const keybindings = { matches };
	assert.equal(ui.installed({ requestRender() {} }, {}, keybindings), editor, "the built editor is the previous factory's");

	editor.handleInput(ESC);
	assert.equal(typeof resolveChoice, "function", "the dialog is open");
	resolveChoice(STOP_OPTION);
	await tick();
	assert.deepEqual(editor.seen, ["escape-action"], "confirming stops the work");

	// Regression: the work ends while the dialog is open. The late confirmation
	// must not fall through to Escape's idle behavior and must not abort a
	// replacement run.
	session.busy = true;
	editor.handleInput(ESC);
	const late = resolveChoice;
	handlers.agent_end({});
	session.busy = false;
	late(STOP_OPTION);
	await tick();
	assert.deepEqual(editor.seen, ["escape-action"], "a confirmation for work that already ended does nothing");
});
