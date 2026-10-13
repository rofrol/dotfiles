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

## Next, in order

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

