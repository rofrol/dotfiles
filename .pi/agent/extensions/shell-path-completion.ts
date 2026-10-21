import type { ExtensionAPI } from "@earendil-works/pi-coding-agent";
import type { AutocompleteProvider } from "@earendil-works/pi-tui";
import { wrapEditorFactory } from "./confirm-interrupt.ts";

/** Keep Pi's command and @ search menus; ordinary paths open only on Tab. */
export function shellPathProvider(provider: AutocompleteProvider): AutocompleteProvider {
	return {
		triggerCharacters: provider.triggerCharacters,
		shouldTriggerFileCompletion: (lines, line, col) =>
			provider.shouldTriggerFileCompletion?.(lines, line, col) ?? true,
		applyCompletion: (lines, line, col, item, prefix) =>
			provider.applyCompletion(lines, line, col, item, prefix),
		async getSuggestions(lines, line, col, options) {
			const suggestions = await provider.getSuggestions(lines, line, col, options);
			if (!suggestions) return null;
			const beforeCursor = (lines[line] ?? "").slice(0, col).trimStart();
			if (suggestions.prefix.startsWith("@") || (!options.force && beforeCursor.startsWith("/"))) {
				return suggestions;
			}
			// An automatic menu would make the first Tab accept its selected path.
			if (!options.force) return null;
			if (suggestions.items.length < 2) return suggestions;

			let common = Array.from(suggestions.items[0].value);
			for (const item of suggestions.items.slice(1)) {
				const value = Array.from(item.value);
				let length = 0;
				while (length < common.length && common[length] === value[length]) length++;
				common = common.slice(0, length);
			}
			const value = common.join("");
			const comparable = value.startsWith('"') && !suggestions.prefix.startsWith('"') ? value.slice(1) : value;
			if (!comparable.startsWith(suggestions.prefix) || comparable.length <= suggestions.prefix.length) {
				return suggestions;
			}
			// Pi auto-applies a single forced result. This is a partial path, not
			// a file: do not close quotes or append whitespace.
			return { prefix: suggestions.prefix, items: [{ value, label: value }] };
		},
	};
}

export default function (pi: ExtensionAPI): void {
	pi.on("session_start", (_event, ctx) => {
		if (ctx.mode !== "tui") return;
		// Compose with whatever editor factory is installed (see confirm-interrupt.ts),
		// so both extensions survive in either load order.
		wrapEditorFactory(ctx, "shell-path-completion", (editor) => {
			const setAutocompleteProvider = editor.setAutocompleteProvider?.bind(editor);
			if (!setAutocompleteProvider) return;
			editor.setAutocompleteProvider = (provider) => setAutocompleteProvider(shellPathProvider(provider));
		});
	});
}
