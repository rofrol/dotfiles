# TODO

Follow-ups for the tools configured in this dotfiles repository.

## pi

- [ ] Return pi-web-search to npm once a release contains the search thinking
  level (ttttmr/pi-web-search#40, opened 2026-10-06). Until then
  `.pi/agent/settings.json` pins the fork `rofrol/pi-web-search@31f2564`.
  Upstream publishes only to npm (no GitHub releases or tags), so a merged PR
  is not the trigger; check the latest npm tarball instead:
  ```bash
  T=$(mktemp -d); curl -sL "$(npm view pi-web-search@latest dist.tarball)" | tar -xz -C "$T"
  grep -q 'parsed.thinking' "$T/package/src/utils.ts" && npm view pi-web-search@latest version
  ```
  When it prints a version: `pi remove git:github.com/rofrol/pi-web-search@31f2564a71f61c78142522b7bd781aa91fe935ab`,
  `pi install npm:pi-web-search@<version>`, run one search with
  `"thinking": "low"` in `.pi/agent/web-search.json`, commit.
- [ ] Maybe, only if the ChatGPT quota starts blocking work or raw search
  results are needed: try Serper (https://serper.dev, Google results,
  $1 per 1000 queries, 2,500 free, $50 minimum purchase) as pi's search
  backend instead of Luna. Not a task before then.

## scripts

- [ ] wh-speakers-diarize: detect the language automatically (the user,
  2026-10-12, via todo-rormpc-tools: "it needs to be added to
  ~/scripts/wh-speakers-diarize"). Today `--language` defaults to `en`
  (wh-speakers-diarize:98-102) and goes straight to mlx_whisper.transcribe,
  so non-English audio is transcribed as English (Whisper may translate).
  Make `--language auto` the default (an explicit code still wins): Silero
  VAD finds speech regions; Whisper's language detection runs on 5 windows
  of 30 s starting at speech regions; majority vote, ties broken by summed
  probability (Whisper alone reads only the first 30 s and fails on a long
  silent intro). No demucs for speech recordings. Print the detected code,
  the votes and probabilities, store them in the .diarized.json, and key the
  cached .whisper.json by language so a cached English run is not reused.
  Reference: rormpc's transcription backend (`speech_regions`,
  `window_starts`, the ASR class's `detect` and `vote`; its plan doc,
  "Language detection is the biggest failure"), which picked pl, fr and en
  correctly on library songs.

## Completed

- **todo: require quota-gated worker dispatch** (2026-10-14). Approved by the
  user: "Tell that agent to stop and write a handoff note, and do the TODO
  task about limits. Ask the models." Updated `.agents/skills/todo/SKILL.md`
  and `.claude/CLAUDE.md` (the existing Pi instructions symlink points here).
  Coordinators must use the existing `herdr todo run` provider gate, not
  direct worker/interactive starts; high/unknown usage never admits a worker,
  pending run ids survive handoff, missing gate/task/check support blocks
  dispatch, and manual diagnostics cannot execute TODO work. No new API,
  dependency, timer, threshold or paid fallback. MiMo/DeepSeek design round
  `20261014-020657-a1da`, diff review `20261014-020945-0f83`; verified against
  Herdr gate, pending-start and CLI sources, static instruction checks and
  whitespace checks. No real worker/provider calls or binary changes were
  tested. Generic launch APIs remain ungated; this fixes coordinator routing,
  not enforcement against an agent that ignores its instructions.

## Next, in order

- [ ] Show the previous handoff note to the user after a takeover (skills
  `todo` and `handoff`). Approved by the user 2026-10-16 (herdr-bend
  decisions d-f5cs3xm5, d-3t7iup5f, d-hlrc3pjw; consult round
  `20261016-101519-311d`), to run now. The roagent CLI opens the note in
  a herdr pane after a claim (roagent TODO t-w3wtzqyw); the skills add the
  fallback: when the claim reports the note was not shown, the
  successor's first message quotes the note's open questions, next action
  and "what not to repeat" verbatim, labeled as the previous agent's text,
  never a summary; and `done` archives rather than deletes. Depends on
  t-w3wtzqyw for the CLI's report.

- [ ] skills `macbook-2010`: check a file's users before moving it off the
  disk. Approved by the user 2026-10-16 ("give it to a worker"). Lesson
  from the herdr coordinator on 2026-10-16: it archived
  `~/personal_projects/guix/images` to a pendrive and deleted the
  original after checking only qemu's command line; try-roguix's
  `builder.qcow2` used one of those images as its qcow2 backing file, so
  the builder broke until the image was restored. Add to the skill's
  freeing-disk steps: before moving or deleting, check open files
  (`lsof +D <dir>`) and, for VM images, every backing chain that may
  point into the directory (`qemu-img info --backing-chain` on the images
  of running and known VMs), and keep base images in place. A worker
  edits `.agents/skills/macbook-2010/SKILL.md`; text only.

## Proposed

- **pi: answer while working (plan, awaiting the user's choices).** The user
  (2026-10-13): pi should answer a message typed while it works fairly
  quickly, as Claude Code does, not when the model finishes; inside herdr or
  herdr-tov maybe in a separate tab; herdr jobs must keep working, and
  herdr-tov must support it too. Facts: pi delivers a steer only after the
  current turn's tool calls all finish; Claude Code does the same, and adds
  Ctrl+B (move a running Bash to the background by hand), run_in_background,
  exit notifications and `/btw` (a side answer with the conversation as
  context, no tools, not added to the conversation). bg-bash is disabled
  (renamed to `.bak` by the user): pi 1.2.0 counts a second `registerTool("bash")`
  (herdr-activity also registers it) as an extension load error. Consult
  round `20261013-170505-9b5f` (sol, mimo, deepseek). Plan:
  1. One bash owner: herdr-activity stops registering `bash` and draws rows
     with `registerToolRenderer` (composes through `next()`); bg-bash
     publishes versioned `pi.events` (detach, exit with id, pid, log, exit
     code). Lands in the herdr fork first, through its TODO coordinator;
     bg-bash is re-enabled only after that.
  2. Detach only the tool call blocking the current turn (by toolCallId),
     not every running command; the result says the command still runs and
     may still write files; completion carries the exit code and log tail.
     Codemode: a nested bash detaches back into the script, which still
     blocks; documented as a gap.
  3. `/btw <question>` extension: one `ctx.modelRegistry.streamSimple()`
     call on a snapshot of the conversation at the last finished turn, no
     tools, answer in a widget labelled with that turn, nothing written to
     the session.
  4. Durable job records (see "durable completion records" below), with
     reconciliation at session_start, detach, exit and `/bg`.
  5. Inside herdr with herdr-job: `herdr-job watch --pid` on detach (sidebar
     entry). Elsewhere, herdr-tov included: one shared, reused "bg jobs" tab
     (tab.create + pane.run, no focus) showing the logs, or only `/bg`.
  6. Tests driven by events (child blocks on a FIFO): steer mid-bash, exit
     racing detach, both extensions loaded, `/btw` leaves the session file
     unchanged, restart reconciles.
  Also: report upstream to pi that a duplicate tool name blocks startup and
  ask to separate implementation from renderer registration.
- **pi bg-bash: race stress test.** `.pi/agent/extensions/bg-bash.ts`
  (commit 2d025f3) detaches running bash commands on a steering message.
  Consult round `20261013-155214-a3cd` (sol, deepseek) asked for a test
  driven by events, not elapsed time: a child prints READY and blocks on a
  pipe; steer after READY, release the pipe, assert one answer and exactly
  one bg-job-done; also a steer landing as the command exits and two
  commands exiting together. Cost: a test script plus a few cents of model
  calls; no build.
- **pi bg-bash: durable completion records.** Today a detached command's
  completion lives only in memory and a `$TMPDIR/pi-bg/<id>.status` file;
  if pi crashes the wake is lost, and quitting pi kills detached commands
  (as Claude Code does). Proposed by mimo and deepseek in the same round:
  persist records with an announced marker, reconcile at `session_start`.
  Cost: moderate extension code; no build. Only worth it if wakes go missing
  in practice.

## Agent rules and skills

- [ ] Remove roagent from the global agent rules (`~/.claude/CLAUDE.md`, "Context size and handoff", "Working through TODO.md") and the `todo` and `handoff` skills, in the same step that removes roagent: the coordinator lease becomes herdr's coordinator tenure, handoff notes and `todo start` move to herdr, `context` to a skill script, and the supervisor in `~/personal_projects/agentic-workflow-bend-claude` runs queues instead of herdr's TODO driver. Decided by the user 2026-10-16 (d-ytigfuq7); plan in that repository's `docs/decisions/0007-one-workflow-engine.md` (its item t-onengine) and roagent's TODO. Waits for those items.
