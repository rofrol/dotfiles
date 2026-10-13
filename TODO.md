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

## Proposed

- **todo: coordination that works outside the herdr fork** (the user,
  2026-10-13: "shouldn't /todo be a mechanism outside herdr?", since agents
  will also run in herdr-tov, plain terminals, tmux and ssh). Design agreed
  in consult rounds `20261013-135756-a0d1` and `20261013-141203-4fe6`
  (MiMo, DeepSeek):
  - **Authority.** The per-repository lease `~/scripts/todo-lock` in the git
    common dir is the only source of truth for who coordinates. herdr's
    `herdr coordinator` tenure, where present, is a mirror for supervision,
    the snapshot and `handoff --successor`. Take the git lease first, then
    register with herdr; release herdr first, then the git lease. On any
    disagreement git wins and herdr is repaired, at `/todo` start, at a
    handoff and at each item boundary.
  - **Who takes a note over.** Only a session started with
    `/todo --handoff <note>` (as the skill already says: never infer handoff
    mode from a note file existing). Agents that only answer questions never
    run `/todo`, so they never take the lease or a note. The claim is the
    lease, not a commit, and the "claim by commit" rule goes.
  - **Automatic handoff.** At each item boundary (the item committed and
    `TODO.md` updated), the coordinator checks its context usage: pi via
    `getContextUsage().percent`, Claude Code via the token usage of the last
    turn in its session `.jsonl`, not the file size. Over about 70%, it:
    writes the note atomically, commits it, starts the successor with
    `/todo --handoff <note>` (herdr: `coordinator handoff --successor`;
    elsewhere a detached start if the agent allows it, otherwise it prints
    the command for the user), and then releases the lease. Every `/todo`
    start also finishes a pending handoff whose successor never came.
  - **What changes.** The `todo` skill, `todo-worker` (send
    `/todo --handoff HANDOFF.md` instead of the free-text take-over prompt),
    and the global "Context size and handoff" rule (the percent replaces 600
    KB; claim by lease). Rejected: `flock`, because its holder would have to
    be a long-lived process, while agents run short commands.
