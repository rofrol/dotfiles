import assert from "node:assert/strict";
import { readFileSync, mkdtempSync, mkdirSync, writeFileSync, rmSync } from "node:fs";
import { homedir, tmpdir } from "node:os";
import { join, resolve } from "node:path";
import { createRequire } from "node:module";
import { pathToFileURL } from "node:url";
import test from "node:test";

const install = join(homedir(), ".pi/agent/install");
const version = readFileSync(join(install, "current-version"), "utf8").trim();
const modules = join(install, "releases", version, "node_modules");
const require = createRequire(join(modules, "jiti/package.json"));
const { createJiti } = require("jiti");
const agent = join(modules, "@earendil-works/pi-coding-agent/dist/index.js");
const tuiPath = join(modules, "@earendil-works/pi-tui/dist/index.js");
const jiti = createJiti(import.meta.url, { alias: { "@earendil-works/pi-coding-agent": agent } });
const extension = await jiti.import(resolve(import.meta.dirname, "../extensions/shell-path-completion.ts"));
const { CombinedAutocompleteProvider } = await import(pathToFileURL(tuiPath));
const signal = new AbortController().signal;

function fixture() {
	const root = mkdtempSync(join(tmpdir(), "pi-path-completion-"));
	for (const name of ["project-one", "project-two", "space one", "space two", "unique"]) mkdirSync(join(root, name));
	writeFileSync(join(root, "project-note.txt"), "");
	return { root, provider: new CombinedAutocompleteProvider([{ name: "help" }], root) };
}

async function suggest(provider, text, force = true) {
	return provider.getSuggestions([text], 0, text.length, { signal, force });
}

test("paths expand only their common prefix, with no automatic selection", async () => {
	const { root, provider } = fixture();
	try {
		const shell = extension.shellPathProvider(provider);
		assert.equal(await suggest(shell, "./pro", false), null);
		assert.deepEqual((await suggest(shell, "./pro")).items, [{ value: "./project-", label: "./project-" }]);
		assert.equal((await suggest(shell, "./project-")).items.length, 3);
		assert.equal((await suggest(shell, "./unique")).items[0].value, "./unique/");
		assert.equal(await suggest(shell, "./missing"), null);
		assert.deepEqual(await suggest(shell, "/he", false), await suggest(provider, "/he", false));
		assert.equal((await suggest(shell, '"./spa')).items[0].value, '"./space ');
		assert.equal((await suggest(shell, './spa')).items[0].value, '"./space ');
		for (const prefix of ["./p", "./pr", "./pro", "./proj", "./project-"]) {
			const original = await suggest(provider, prefix);
			const result = await suggest(shell, prefix);
			if (result.items.length === 1 && original.items.length > 1) {
				assert.ok(result.items[0].value.length > prefix.length);
				assert.ok(original.items.every(item => item.value.startsWith(result.items[0].value)));
			}
		}
		const partial = await suggest(shell, '"./spa');
		const applied = shell.applyCompletion(['"./spa'], 0, 6, partial.items[0], partial.prefix);
		assert.equal(applied.lines[0], '"./space ');
		assert.equal(applied.cursorCol, 9);
	} finally { rmSync(root, { recursive: true }); }
});

test("@ search and command argument menus are passed through", async () => {
	const suggestions = { prefix: "@pro", items: [{ value: "@project-one/", label: "project-one/" }, { value: "@project-two/", label: "project-two/" }] };
	const provider = { getSuggestions: async () => suggestions, applyCompletion() {} };
	const shell = extension.shellPathProvider(provider);
	assert.equal(await suggest(shell, "@pro"), suggestions);
	assert.equal(await suggest(shell, "@pro", false), suggestions);
	suggestions.prefix = "pro";
	assert.equal(await suggest(shell, "/command pro", false), suggestions);
});

test("real editor: Tab expands prefix, then opens menu, then accepts selection; undo works", async () => {
	const { root, provider } = fixture();
	try {
		let sessionStart;
		extension.default({ on(_event, handler) { sessionStart = handler; } });
		let editor;
		let rendered;
		const tui = { requestRender() { rendered?.(); } };
		sessionStart({}, { mode: "tui", ui: { setEditorComponent(factory) {
			editor = factory(tui, { borderColor: x => x, selectList: {} }, { matches: () => false });
		} } });
		editor.setAutocompleteProvider(provider);
		editor.setText("./pro");
		async function tab() {
			const result = new Promise(resolve => { rendered = resolve; });
			editor.handleInput("\t");
			await result;
			rendered = undefined;
		}
		await tab();
		assert.equal(editor.getText(), "./project-");
		assert.equal(editor.isShowingAutocomplete(), false);
		await tab();
		assert.equal(editor.getText(), "./project-");
		assert.equal(editor.isShowingAutocomplete(), true);
		editor.handleInput("\t");
		assert.equal(editor.getText(), "./project-one/");
		assert.equal(editor.isShowingAutocomplete(), false);
		editor.handleInput("\x1f");
		assert.equal(editor.getText(), "./project-");
	} finally { rmSync(root, { recursive: true }); }
});
