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
  -- Without an image hs.chooser draws a generic arrow; ~0.2 s for 156 apps.
  out[#out + 1] = {
    text = name, subText = path, path = path, key = name:lower(),
    image = hs.image.iconForFile(path),
  }
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

-- Ranks matches of the query in name prefixes first, then at word starts,
-- then anywhere, keeping alphabetical order within each group. Case folding
-- is ASCII-only: Lua has no Unicode lowercase and hs.utf8 offers none.
local function rank(query)
  local q = query:match("^%s*(.-)%s*$"):lower()
  if q == "" then return L.all end
  local groups = { {}, {}, {} }
  for _, choice in ipairs(L.all) do
    local key, group = choice.key, nil
    local i = key:find(q, 1, true)
    while i do
      if i == 1 then group = 1; break end
      if not key:sub(i - 1, i - 1):match("%w") then group = 2 end
      group = group or 3
      i = key:find(q, i + 1, true)
    end
    if group then table.insert(groups[group], choice) end
  end
  local out = groups[1]
  for g = 2, 3 do table.move(groups[g], 1, #groups[g], #out + 1, out) end
  return out
end

local function showRanked(query)
  local choices = rank(query)
  L.chooser:choices(choices)
  if #choices > 0 then L.chooser:selectedRow(1) end
end

-- With this callback set, hs.chooser does no filtering of its own.
L.chooser:queryChangedCallback(showRanked)

local function rebuild()
  local out, seen = {}, {}
  for _, dir in ipairs(appDirs) do scan(dir, 1, out, seen) end
  for _, path in ipairs(extraApps) do
    if hs.fs.attributes(path, "mode") == "directory" then addApp(path, out, seen) end
  end
  table.sort(out, function(a, b) return a.key < b.key end)
  L.all = out
  showRanked(L.chooser:query())
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
    -- query() sets the text without calling queryChangedCallback.
    L.chooser:query("")
    L.chooser:show()
    showRanked("")
  end
end)
