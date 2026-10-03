# Testing Hammerspoon from an agent session

Observed with Hammerspoon 1.1.1 on 2026-10-03. Treat them as hazards of this
setup, not documented Hammerspoon contracts.

## Do not drive the GUI

Do not call `chooser:show()`, `hs.eventtap.keyStroke(s)` or anything else
that opens windows or types keys without the user's explicit consent. The user
types in other terminal panes while agents run; the chooser took keyboard
focus and swallowed their keystrokes. A synthetic F18 from
`hs.eventtap.keyStroke` also did not trigger the `hs.hotkey.bind` F18 handler,
so it does not test the hotkey anyway.

Test logic headlessly instead, against the loaded functions:

```sh
timeout 5 hs -q -c 'local t={} for i,c in ipairs(appLauncher.rank("ter")) do
  if i>4 then break end t[#t+1]=c.text end return table.concat(t," | ")'
```

## Bound every `hs` call

Wrap every `hs -c` in `timeout 5`. A plain `hs -c` returning
`hs.console.getConsole()` once hung forever while Hammerspoon's main thread was
idle; the cause is unknown.

## Reload by restarting the app

Do not reload through IPC. `hs -c 'hs.reload()'` hangs the CLI, because the
reload tears down the IPC port before the reply. Deferring it with
`hs.timer.doAfter(0, hs.reload)` returns, but the next `hs` call after the
reload crashed Hammerspoon (EXC_BREAKPOINT in `CFMessagePortIsValid` from
`CFMessagePortSendRequest` via libipc); killing a hung CLI crashed it the same
way. Quit the app and start a fresh process instead; IPC to a fresh process
was stable:

```sh
pgrep -x Hammerspoon && osascript -e 'tell application "Hammerspoon" to quit'
while pgrep -x Hammerspoon >/dev/null; do :; done
open -g -a Hammerspoon
for i in $(seq 100); do
  n=$(timeout 5 hs -q -c 'return #appLauncher.rank("")' 2>/dev/null)
  [ -n "$n" ] && break
done
```

Then assert behavior (the `rank` example above), not just that it answers.

## After a hang or crash

Check `pgrep -x Hammerspoon` and `ls -t ~/Library/Logs/DiagnosticReports/ |
grep Hammerspoon` before continuing: a dead Hammerspoon makes every `hs` call
fail or hang, not the config under test. Keep the `.ips` report, then restart
with `open -g -a Hammerspoon`.
