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

- **todo skill: default to the agent this session runs in.** The space and
  agent question offers Claude as the default even when `/todo` runs in pi;
  picking Claude there opens a new Claude tab instead of making the pi
  session the coordinator (seen in herdr-tov, 2026-10-13). Default to the
  running agent. Cost: a few lines in `.agents/skills/todo/SKILL.md`; no build.
- **todo-coordination: tests for context-usage and todo-worker.** Only
  `todo-lock` has tests (`tests/test_todo_lock.py` in
  `~/personal_projects/todo-coordination`). Add fixture transcripts for
  `context-usage` (pi and Claude Code, a compaction, an unknown window) and a
  `--dry-run` test of `todo-worker`'s prompt choice (pending note with a
  title line, lease handing over). Cost: tests only; no build.
