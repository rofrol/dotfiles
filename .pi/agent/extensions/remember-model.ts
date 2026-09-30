import { getSupportedThinkingLevels } from "@earendil-works/pi-ai";
import { SettingsManager, type ExtensionAPI } from "@earendil-works/pi-coding-agent";

/** Save interactive model choices as Pi's default for /new and future launches. */
export default function (pi: ExtensionAPI) {
	pi.registerCommand("reasoning", {
		description: "Choose a thinking level and save it as the global default",
		handler: async (args, ctx) => {
			if (ctx.mode !== "tui") return;
			const model = ctx.model;
			if (!model) {
				ctx.ui.notify("No model selected", "error");
				return;
			}
			const levels = getSupportedThinkingLevels(model);
			const requested = args.trim().toLowerCase() || await ctx.ui.select(
				`Reasoning (current: ${pi.getThinkingLevel()}; saves global default)`, levels,
			);
			if (requested === undefined) return;
			const level = levels.find((candidate) => candidate === requested);
			if (!level) {
				ctx.ui.notify(`Unknown or unsupported level. Available: ${levels.join(", ")}`, "error");
				return;
			}
			if (ctx.model?.provider !== model.provider || ctx.model?.id !== model.id) {
				ctx.ui.notify("Model changed; run /reasoning again", "error");
				return;
			}
			pi.setThinkingLevel(level);
			const effectiveLevel = pi.getThinkingLevel();
			const settings = SettingsManager.create(ctx.cwd, undefined, { projectTrusted: false });
			settings.setDefaultThinkingLevel(effectiveLevel);
			await settings.flush();
			const errors = settings.drainErrors();
			for (const { error } of errors) {
				ctx.ui.notify(`Thinking level changed, but could not save default: ${error.message}`, "error");
			}
			if (errors.length === 0) ctx.ui.notify(`Default reasoning: ${effectiveLevel}`, "info");
		},
	});

	pi.on("model_select", async (event, ctx) => {
		// Restoring sessions and non-interactive workers must not change user defaults.
		if (ctx.mode !== "tui" || event.source === "restore") return;

		const settings = SettingsManager.create(ctx.cwd, undefined, { projectTrusted: false });
		settings.setDefaultModelAndProvider(event.model.provider, event.model.id);
		await settings.flush();
		for (const { error } of settings.drainErrors()) {
			ctx.ui.notify(`Could not save default model: ${error.message}`, "error");
		}
	});
}
