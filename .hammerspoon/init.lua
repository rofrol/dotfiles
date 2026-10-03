-- Cmd+Space (F18 via Karabiner): index-free application launcher.
-- Scans only application directories, keeps the list in memory and rebuilds
-- it when one of those directories changes. No Spotlight, no file indexing.

hs.autoLaunch(true)
hs.allowAppleScript(true)
require("hs.ipc")

local appDirs = {
  "/Applications",
  "/System/Applications",
  "/System/Library/CoreServices/Applications",
  os.getenv("HOME") .. "/Applications",
}
local extraApps = {
  "/System/Library/CoreServices/Finder.app",
}
local maxDepth = 3

-- Global table so watchers and timers are not garbage collected.
appLauncher = { watchers = {} }
local L = appLauncher

L.chooser = hs.chooser.new(function(choice)
  if choice then
    hs.application.launchOrFocus(choice.path)
  end
end)
L.chooser:placeholderText("Open application")
L.chooser:searchSubText(false)

local function addApp(path, out, seen)
  local info = hs.application.infoForBundlePath(path)
  local id = (info and info.CFBundleIdentifier) or path
  if seen[id] then return end
  seen[id] = true
  local name = path:match("([^/]+)%.app$")
  out[#out + 1] = { text = name, subText = path, path = path }
end

local function scan(dir, depth, out, seen)
  local ok, iter, state = pcall(hs.fs.dir, dir)
  if not ok or not iter then return end
  for name in iter, state do
    if name:sub(1, 1) ~= "." then
      local path = dir .. "/" .. name
      -- symlinkAttributes does not follow links, which avoids directory cycles.
      local attr = hs.fs.symlinkAttributes(path)
      if attr and attr.mode == "directory" then
        if name:sub(-4) == ".app" then
          addApp(path, out, seen)
        elseif depth < maxDepth then
          scan(path, depth + 1, out, seen)
        end
      end
    end
  end
end

local function rebuild()
  local out, seen = {}, {}
  for _, dir in ipairs(appDirs) do scan(dir, 1, out, seen) end
  for _, path in ipairs(extraApps) do
    if hs.fs.attributes(path, "mode") == "directory" then addApp(path, out, seen) end
  end
  table.sort(out, function(a, b) return a.text:lower() < b.text:lower() end)
  L.chooser:choices(out)
end

L.rebuildTimer = hs.timer.delayed.new(1, rebuild)
for _, dir in ipairs(appDirs) do
  if hs.fs.attributes(dir, "mode") == "directory" then
    L.watchers[#L.watchers + 1] =
      hs.pathwatcher.new(dir, function() L.rebuildTimer:start() end):start()
  end
end
rebuild()

-- Karabiner maps Cmd+Space to F18, except in Emacs and the Try Roguix VM:
-- macOS keeps Cmd+Space registered even with Spotlight's shortcuts off, so
-- binding it directly fails (hs.hotkey.assignable({"cmd"}, "space") was
-- false on 2026-10-03).
-- TODO: test without the workaround. After a logout, if
-- `hs -c 'return hs.hotkey.assignable({"cmd"}, "space")'` is true, bind
-- {"cmd"}, "space" here, drop the Karabiner F18 rule, and check that
-- Cmd+Space still reaches Emacs and the Try Roguix VM.
hs.hotkey.bind({}, "f18", function()
  if L.chooser:isVisible() then
    L.chooser:hide()
  else
    L.chooser:query("")
    L.chooser:show()
  end
end)
