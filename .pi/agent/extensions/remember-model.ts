import { SettingsManager, type ExtensionAPI } from "@earendil-works/pi-coding-agent";

/** Save interactive model choices as Pi's default for /new and future launches. */
export default function (pi: ExtensionAPI) {
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
