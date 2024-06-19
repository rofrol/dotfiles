#!/usr/bin/env bun

// To add system shortcut
// 1. Create service
// Automator > Run Shell Script
// Workflow receives: no input
// /opt/homebrew/bin/bun ~/bin/karabiner_toggle_internal_keyboard_when_nuphy_air75_v2_is_connected.js
// Save as karabiner_toggle_internal_keyboard_when_nuphy_air75_v2_is_connected.js
// 2. Add shortcut
// System Settings > Keyboard > Keyboard Shortcuts > Services > General
// Search for karabiner_toggle_internal_keyboard_when_nuphy_air75_v2_is_connected.js
// On the right side click and set shortcut ctrl+shift+f12
// https://apple.stackexchange.com/questions/24063/create-global-shortcut-to-run-command-line-applications/40887#40887

// https://stackoverflow.com/questions/10685998/how-to-update-a-value-in-a-json-file-and-save-it-through-node-js/28175137#28175137
import { writeFile } from "fs";
const debug = false;
// https://stackoverflow.com/questions/21077670/expanding-resolving-in-node-js/54183941#54183941
const fileName = "~/.config/karabiner/karabiner.json".replace(
  /^~/,
  require("os").homedir,
);
if (debug) console.log(fileName);
const file = require(fileName);

const profileIndex = file.profiles.findIndex((profile) => profile.selected);
if (debug) console.log("profileIndex", profileIndex);
if (profileIndex !== -1) {
  const deviceIndex = file.profiles[profileIndex].devices.findIndex(
    (device) => device.identifiers.device_address === "d7-91-93-56-e5-a9",
  );
  if (debug) console.log("deviceIndex", deviceIndex);
  if (deviceIndex !== -1) {
    const device = file.profiles[profileIndex].devices[deviceIndex];
    if (debug) console.log(device);
    console.log(device.disable_built_in_keyboard_if_exists);
    device.disable_built_in_keyboard_if_exists =
      !device.disable_built_in_keyboard_if_exists;
  }
}

writeFile(fileName, JSON.stringify(file, null, 2), function writeJSON(err) {
  if (err) return console.log(err);
  if (debug) console.log(JSON.stringify(file));
  if (debug) console.log("writing to " + fileName);
});
