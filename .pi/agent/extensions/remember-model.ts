import { getSupportedThinkingLevels } from "@earendil-works/pi-ai";
import {
	DynamicBorder,
	SettingsManager,
	getSelectListTheme,
	type ExtensionAPI,
	type ExtensionCommandContext,
} from "@earendil-works/pi-coding-agent";
import {
	Container,
	Input,
	SelectList,
	Spacer,
	Text,
	fuzzyFilter,
	getKeybindings,
	type Focusable,
	type SelectItem,
} from "@earendil-works/pi-tui";

type ModelLike = NonNullable<ExtensionCommandContext["model"]>;
type Level = Parameters<ExtensionAPI["setThinkingLevel"]>[0];

const LEVEL_DESCRIPTIONS: Record<string, string> = {
	off: "No reasoning",
	minimal: "Very brief reasoning (~1k tokens)",
	low: "Light reasoning (~2k tokens)",
	medium: "Moderate reasoning (~8k tokens)",
	high: "Deep reasoning (~16k tokens)",
	xhigh: "Extra-high reasoning (~32k tokens)",
	max: "Maximum reasoning",
};

// /m applies its own model change; the model_select handler must not persist twice.
let applyingFromPicker = false;
let pickerOpen = false;

interface Choice<T> {
	value: string;
	label: string;
	description?: string;
	payload: T;
}

/**
 * Single-stage picker: type to filter, arrows to move, Enter to confirm, Esc to cancel.
 * Levels and models render through the same component, like the built-in selectors do.
 */
class FuzzyPicker<T> extends Container implements Focusable {
	private readonly input = new Input({ placeholder: "Type to filter" });
	private readonly choices: Choice<T>[];
	private readonly onPick: (payload: T | undefined) => void;
	private list: SelectList;
	private readonly listIndex: number;
	private settled = false;
	private _focused = false;

	constructor(
		title: string,
		footer: string,
		choices: Choice<T>[],
		preselect: string | undefined,
		onPick: (payload: T | undefined) => void,
	) {
		super();
		this.choices = choices;
		this.onPick = onPick;
		this.addChild(new DynamicBorder());
		this.addChild(new Spacer(1));
		this.addChild(new Text(title, 1, 0));
		this.addChild(new Spacer(1));
		this.addChild(this.input);
		this.addChild(new Spacer(1));
		this.list = this.buildList(choices, preselect);
		this.listIndex = this.children.length;
		this.addChild(this.list);
		this.addChild(new Spacer(1));
		this.addChild(new Text(footer, 1, 0));
		this.addChild(new DynamicBorder());
		this.input.onSubmit = () => this.confirm();
		this.input.onEscape = () => this.cancel();
	}

	get focused(): boolean {
		return this._focused;
	}

	set focused(value: boolean) {
		this._focused = value;
		this.input.focused = value;
	}

	handleInput(data: string): void {
		const keybindings = getKeybindings();
		const isNavigation =
			keybindings.matches(data, "tui.select.up") ||
			keybindings.matches(data, "tui.select.down") ||
			keybindings.matches(data, "tui.select.confirm") ||
			keybindings.matches(data, "tui.select.cancel");
		if (isNavigation) {
			this.list.handleInput(data);
			return;
		}
		this.input.handleInput(data);
		this.filter(this.input.getValue());
	}

	private buildList(choices: Choice<T>[], preselect: string | undefined): SelectList {
		const items: SelectItem[] = choices.map(({ value, label, description }) => ({
			value,
			label,
			description,
		}));
		const list = new SelectList(items, Math.max(1, Math.min(items.length, 12)), getSelectListTheme());
		const index = preselect === undefined ? -1 : choices.findIndex((choice) => choice.value === preselect);
		if (index >= 0) {
			list.setSelectedIndex(index);
		}
		list.onSelect = () => this.confirm();
		list.onCancel = () => this.cancel();
		return list;
	}

	private filter(query: string): void {
		const filtered = query.trim()
			? fuzzyFilter(this.choices, query, (choice) => `${choice.label} ${choice.value} ${choice.description ?? ""}`)
			: this.choices;
		const selected = this.list.getSelectedItem()?.value;
		const list = this.buildList(filtered, selected);
		this.children[this.listIndex] = list;
		this.list = list;
	}

	private confirm(): void {
		if (this.settled) {
			return;
		}
		const selected = this.list.getSelectedItem();
		const choice = selected ? this.choices.find((item) => item.value === selected.value) : undefined;
		if (!choice) {
			return;
		}
		this.settled = true;
		this.onPick(choice.payload);
	}

	private cancel(): void {
		if (this.settled) {
			return;
		}
		this.settled = true;
		this.onPick(undefined);
	}
}

function sameModel(a: ModelLike | undefined, b: ModelLike | undefined): boolean {
	return !!a && !!b && a.provider === b.provider && a.id === b.id;
}

function availableModels(ctx: ExtensionCommandContext): ModelLike[] {
	const available = ctx.modelRegistry.getAvailable();
	if (ctx.scopedModels.length === 0) {
		return available;
	}
	const scoped = new Set(ctx.scopedModels.map(({ model }) => `${model.provider}/${model.id}`));
	return available.filter((model) => scoped.has(`${model.provider}/${model.id}`));
}

/** Saved level wins, then the global default, then the session level; all clamped to the model. */
function preselectLevel(
	levels: readonly Level[],
	saved: Level | undefined,
	globalDefault: Level | undefined,
	sessionLevel: Level | undefined,
): string | undefined {
	for (const candidate of [saved, globalDefault, sessionLevel]) {
		if (candidate !== undefined && levels.includes(candidate)) {
			return candidate;
		}
	}
	return levels[0];
}

async function applyChoice(
	pi: ExtensionAPI,
	ctx: ExtensionCommandContext,
	model: ModelLike,
	level: Level | undefined,
): Promise<void> {
	if (!sameModel(ctx.model, model)) {
		applyingFromPicker = true;
		let switched = false;
		try {
			switched = await pi.setModel(model);
		} finally {
			applyingFromPicker = false;
		}
		if (!switched) {
			ctx.ui.notify(`Cannot switch to ${model.provider}/${model.id}: no credentials configured`, "error");
			return;
		}
	}
	if (level !== undefined) {
		pi.setThinkingLevel(level);
	}
	const effective = pi.getThinkingLevel();

	const settings = SettingsManager.create(ctx.cwd, undefined, { projectTrusted: false });
	settings.setDefaultModelAndProvider(model.provider, model.id);
	if (model.reasoning) {
		settings.setModelThinkingLevel(model.provider, model.id, effective);
	}
	await settings.flush();
	const errors = settings.drainErrors();
	for (const { error } of errors) {
		ctx.ui.notify(
			`Applied ${model.provider}/${model.id} (reasoning ${effective}) for this session; saving failed: ${error.message}`,
			"error",
		);
	}
	if (errors.length === 0) {
		ctx.ui.notify(`Model ${model.provider}/${model.id}, reasoning ${effective} — remembered`, "info");
	}
}

/** Save interactive model choices as Pi's default for /new and future launches. */
export default function (pi: ExtensionAPI) {
	pi.registerCommand("m", {
		description: "Pick a model and its reasoning level, then remember both",
		handler: async (_args, ctx) => {
			if (ctx.mode !== "tui" || pickerOpen) {
				return;
			}
			pickerOpen = true;
			try {
				const startModel = ctx.model;
				const models = availableModels(ctx);
				if (models.length === 0) {
					ctx.ui.notify("No models available", "error");
					return;
				}
				const chosenModel = await ctx.ui.custom<ModelLike | undefined>((_tui, _theme, _keybindings, done) =>
					new FuzzyPicker<ModelLike>(
						"Model",
						"Enter selects · Esc cancels",
						models.map((model) => ({
							value: `${model.provider}/${model.id}`,
							label: model.name && model.name !== model.id ? `${model.id} — ${model.name}` : model.id,
							description: model.provider,
							payload: model,
						})),
						startModel ? `${startModel.provider}/${startModel.id}` : undefined,
						(payload) => done(payload),
					),
				);
				if (!chosenModel) {
					return;
				}

				let chosenLevel: Level | undefined;
				if (chosenModel.reasoning) {
					const levels = getSupportedThinkingLevels(chosenModel) as Level[];
					const settings = SettingsManager.create(ctx.cwd, undefined, { projectTrusted: false });
					chosenLevel = await ctx.ui.custom<Level | undefined>((_tui, _theme, _keybindings, done) =>
						new FuzzyPicker<Level>(
							`Reasoning for ${chosenModel.provider}/${chosenModel.id}`,
							"Enter applies and remembers this level for the model · Esc cancels",
							levels.map((level) => ({
								value: level,
								label: level,
								description: LEVEL_DESCRIPTIONS[level],
								payload: level,
							})),
							preselectLevel(
								levels,
								settings.getModelThinkingLevel(chosenModel.provider, chosenModel.id),
								settings.getDefaultThinkingLevel(),
								pi.getThinkingLevel(),
							),
							(payload) => done(payload),
						),
					);
					if (chosenLevel === undefined) {
						return;
					}
				}

				if (!sameModel(ctx.model, startModel)) {
					ctx.ui.notify("Model changed while picking; nothing applied", "error");
					return;
				}
				await applyChoice(pi, ctx, chosenModel, chosenLevel);
			} catch (error) {
				ctx.ui.notify(`Model picker failed: ${error instanceof Error ? error.message : String(error)}`, "error");
			} finally {
				pickerOpen = false;
			}
		},
	});

	pi.on("model_select", async (event, ctx) => {
		// Restoring sessions, non-interactive workers and /m's own switch must not change user defaults.
		if (applyingFromPicker || ctx.mode !== "tui" || event.source === "restore") return;

		const settings = SettingsManager.create(ctx.cwd, undefined, { projectTrusted: false });
		settings.setDefaultModelAndProvider(event.model.provider, event.model.id);
		await settings.flush();
		for (const { error } of settings.drainErrors()) {
			ctx.ui.notify(`Could not save default model: ${error.message}`, "error");
		}
	});
}
