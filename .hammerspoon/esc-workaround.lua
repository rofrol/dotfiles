-- Temporary workaround, not loaded by init.lua: forwards plain Escape
-- straight to the frontmost application.
--
-- On 2026-10-03 at about 14:45 Escape stopped reaching every app (Ghostty,
-- Terminal.app, Chrome) while other keys worked. Karabiner-EventViewer and
-- listen-only CGEventTaps at the HID and session level still saw keycode 53
-- down/up with no modifiers, so something after the session tap swallowed it.
-- Not the cause: Karabiner rules, Ghostty, AltTab, Hammerspoon's F18 binding,
-- Gemini launcher, Siri AI, Secure Input, Services or symbolic hotkeys.
-- Posting the event to the app's process (event:post(app)) bypasses that
-- routing and works.
--
-- Load it only while the bug is back (it lasts until Hammerspoon reloads or
-- you log out):
--   hs -c 'dofile(os.getenv("HOME") .. "/.hammerspoon/esc-workaround.lua")'
-- Remove it now:
--   hs -c 'escFix.hk:delete(); escFix = nil'

if escFix then escFix.hk:delete() end
escFix = {}

local function send(down)
  local app = escFix.target or hs.application.frontmostApplication()
  -- Disabled while posting so the event cannot re-trigger this hotkey.
  escFix.hk:disable()
  hs.eventtap.event.newKeyEvent({}, "escape", down):post(app)
  escFix.hk:enable()
end

-- The release and repeats go to the app that got the press, even if Escape
-- moved the focus.
escFix.hk = hs.hotkey.new({}, "escape",
  function() escFix.target = hs.application.frontmostApplication(); send(true) end,
  function() send(false); escFix.target = nil end,
  function() send(true) end)
escFix.hk:enable()
