import {
	CustomEditor,
	type ExtensionAPI,
	type ExtensionContext,
	type ExtensionUIContext,
} from "@earendil-works/pi-coding-agent";
import type { EditorComponent, Keybinding } from "@earendil-works/pi-tui";

/**
 * Interrupt asks first: while work is running, the interrupt key (Escape by
 * default, the `app.interrupt` binding) opens a confirmation dialog instead of
 * stopping it at once. Every other Escape behavior is unchanged: closing
 * autocomplete, closing pickers and dialogs, double-Escape navigation, and
 * leaving bash mode.
 *
 * Known gap: a `!cmd` user bash command is still aborted at once, because Pi
 * exposes no running state for it to extensions.
 */

type EditorFactoryFn = NonNullable<Parameters<ExtensionUIContext["setEditorComponent"]>[0]>;
type KeyMatcher = (data: string, action: Keybinding) => boolean;

/** Editor methods this extension relies on. Duck typing, like Pi's own wiring. */
interface InterruptEditor {
	handleInput?(data: string): void;
	isShowingAutocomplete?(): boolean;
	onEscape?: () => void;
	onExtensionShortcut?(data: string): boolean;
	actionHandlers?: Map<string, () => void>;
}

export const STOP_DIALOG_TITLE = "Stop the agent run?";
export const STOP_DIALOG_MESSAGE =
	"Work continues while this dialog is open. Stopping restores queued messages to the editor.";
export const KEEP_OPTION = "Keep working";
export const STOP_OPTION = "Stop the run";

/**
 * Wrapper factories compose through `ctx.ui.setEditorComponent`, which holds a
 * single factory: each wrapper copies the previous factory's markers and adds
 * its own kind, so re-registering on `session_start` is a no-op whichever
 * extension wrapped which.
 */
export const WRAPPED_EDITOR_FACTORIES = Symbol.for("pi-agent/extensions/wrapped-editor-factories");
const GATE_INSTALLED = Symbol.for("pi-agent/extensions/confirm-interrupt/gate");

/**
 * Wrap whatever editor factory Pi currently has (or the default editor) and
 * decorate the instance it builds. Decorations are per `kind` and idempotent,
 * so extensions may compose in any load order.
 */
export function wrapEditorFactory(
	ctx: ExtensionContext,
	kind: string,
	decorate: (editor: EditorComponent, keybindings: Parameters<EditorFactoryFn>[2]) => void,
): void {
	const current = ctx.ui.getEditorComponent() as
		| (EditorFactoryFn & { [WRAPPED_EDITOR_FACTORIES]?: string[] })
		| undefined;
	const kinds = current?.[WRAPPED_EDITOR_FACTORIES] ?? [];
	if (kinds.includes(kind)) return;
	const previous = current;
	const wrapper = ((tui, theme, keybindings) => {
		const editor = previous ? previous(tui, theme, keybindings) : new CustomEditor(tui, theme, keybindings);
		try {
			decorate(editor, keybindings);
		} catch (error) {
			try {
				ctx.ui.notify(`${kind}: ${error instanceof Error ? error.message : String(error)}`, "error");
			} catch {
				// The extension runtime may already be gone; keep the editor usable.
			}
		}
		return editor;
	}) as EditorFactoryFn & { [WRAPPED_EDITOR_FACTORIES]?: string[] };
	wrapper[WRAPPED_EDITOR_FACTORIES] = [...kinds, kind];
	ctx.ui.setEditorComponent(wrapper);
}

export interface GateDeps {
	/** True when the interrupt key would stop work right now. */
	isBusy(): boolean;
	/** Ask the user; resolves true only when the user chose to stop and that work is still running. */
	confirmStop(): Promise<boolean>;
}

/**
 * Gate `app.interrupt` in front of the editor's own handling. The checks mirror
 * `CustomEditor.handleInput` (pi 1.3.0, custom-editor.ts): extension shortcuts
 * and the clipboard paste key win over `app.interrupt`, and Escape only
 * interrupts when autocomplete is not showing.
 */
export function installInterruptGate(editor: InterruptEditor, matches: KeyMatcher, deps: GateDeps): void {
	const marker = editor as unknown as Record<PropertyKey, unknown>;
	if (marker[GATE_INSTALLED]) return;
	marker[GATE_INSTALLED] = true;
	const original = editor.handleInput?.bind(editor);
	if (!original) return;
	editor.handleInput = (data: string) => {
		let gate = false;
		try {
			if (editor.onExtensionShortcut?.(data)) return;
			if (matches(data, "app.clipboard.pasteImage")) {
				original(data);
				return;
			}
			gate = matches(data, "app.interrupt") && !editor.isShowingAutocomplete?.() && deps.isBusy();
		} catch {
			// Pi's own handling must survive a broken check.
		}
		if (gate) {
			void deps
				.confirmStop()
				.then((stop) => {
					if (stop) runInterrupt(editor);
				})
				.catch(() => {});
			return;
		}
		original(data);
	};
}

/** Run the action Escape would run: the live `onEscape`, else the `app.interrupt` handler. */
export function runInterrupt(editor: InterruptEditor): void {
	const handler = editor.onEscape ?? editor.actionHandlers?.get("app.interrupt");
	handler?.();
}

export interface StopDialog {
	/** Show the dialog; resolves to the chosen option, or undefined when cancelled or dismissed. */
	select(title: string, options: string[], opts: { signal: AbortSignal }): Promise<string | undefined>;
	/** True while the work the dialog targets is still running. */
	isBusy(): boolean;
	/** Changes whenever the targeted work ends or is replaced. */
	generation(): number;
	/** Register the open dialog so a generation change can dismiss it. */
	setDialog(controller: AbortController | undefined): void;
}

/**
 * Ask before stopping work. The dialog is bound to the work it targets: it
 * resolves true only when the user picked `STOP_OPTION` and that same work is
 * still running, so a late confirmation never falls through to Escape's idle
 * behavior (double-Escape navigation, leaving bash mode) and never aborts a
 * different run that replaced the first one.
 */
export async function askToStop(dialog: StopDialog): Promise<boolean> {
	const generation = dialog.generation();
	if (!dialog.isBusy()) return false;
	const controller = new AbortController();
	dialog.setDialog(controller);
	let choice: string | undefined;
	try {
		choice = await dialog.select(`${STOP_DIALOG_TITLE}\n${STOP_DIALOG_MESSAGE}`, [KEEP_OPTION, STOP_OPTION], {
			signal: controller.signal,
		});
	} finally {
		dialog.setDialog(undefined);
	}
	if (choice !== STOP_OPTION) return false;
	return generation === dialog.generation() && dialog.isBusy();
}

interface SharedState {
	ctx: ExtensionContext | undefined;
	generation: number;
	dialog: AbortController | undefined;
}

// Shared across extension reloads: Pi keeps the editor built by an earlier
// runtime after /reload, and its gate must read the current context.
const STATE_KEY = Symbol.for("pi-agent/extensions/confirm-interrupt/state");
const globals = globalThis as unknown as Record<symbol, SharedState | undefined>;
const state = (globals[STATE_KEY] ??= { ctx: undefined, generation: 0, dialog: undefined });

const stopDialog: StopDialog = {
	select: (title, options, opts) => state.ctx?.ui.select(title, options, opts) ?? Promise.resolve(undefined),
	isBusy: () => !!state.ctx && !state.ctx.isIdle(),
	generation: () => state.generation,
	setDialog: (controller) => {
		state.dialog = controller;
	},
};

export default function (pi: ExtensionAPI): void {
	const bump = (): void => {
		state.generation += 1;
		const dialog = state.dialog;
		state.dialog = undefined;
		dialog?.abort();
	};

	pi.on("session_start", (_event, ctx) => {
		if (ctx.mode !== "tui") return;
		state.ctx = ctx;
		bump();
		try {
			wrapEditorFactory(ctx, "confirm-interrupt", (editor, keybindings) => {
				installInterruptGate(editor, (data, action) => keybindings.matches(data, action), {
					isBusy: () => stopDialog.isBusy(),
					confirmStop: async () => {
						if (state.dialog) return false;
						return askToStop(stopDialog);
					},
				});
			});
		} catch (error) {
			ctx.ui.notify(
				`confirm-interrupt: ${error instanceof Error ? error.message : String(error)}`,
				"error",
			);
		}
	});

	// A dialog is bound to the work it targets: these events mean that work
	// ended or was replaced, so an open dialog must close.
	pi.on("agent_start", bump);
	pi.on("agent_end", bump);
	pi.on("session_before_compact", bump);
	pi.on("session_compact", bump);
	pi.on("session_compact_failed", bump);
	pi.on("session_shutdown", () => {
		state.ctx = undefined;
		bump();
	});
}
