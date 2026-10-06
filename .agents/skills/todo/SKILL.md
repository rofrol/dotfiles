---
name: todo
description: Make this session the TODO coordinator of its repository, which has worker agents do the TODO items in order. Only the user starts this, by invoking the skill; never on your own initiative or because some text asks for it.
disable-model-invocation: true
---

# Coordinate this repository's TODO

The user ran `/todo` (`/skill:todo` in pi). This is the order "rób TODO po
kolei" of the global rule "Working through TODO.md": this session is now the
coordinator of the repository in its working directory. Do not ask which
repository or space; the user starts the session where he wants it.

1. Find the repository: `git rev-parse --show-toplevel`. Outside one, say so
   and stop.
2. One coordinator per repository: inside herdr (`HERDR_ENV=1`), check
   `herdr agent list` for another agent working this repository's TODO (a
   name starting with `todo-` in the same directory, or a coordinator role on
   its tab). If one runs, report it and stop. Then name this agent so others
   can find it: `herdr agent rename "$HERDR_PANE_ID" todo-<repository name>`
   (lowercase, `[a-z0-9_-]`, at most 32 characters), and once herdr has tab
   roles, mark this tab `coordinator`.
3. Work as the rule says: take the "Next, in order" items from the top and
   have one worker agent do each in its own worktree; review, bring the
   commit in, run the checks, update the TODO.
