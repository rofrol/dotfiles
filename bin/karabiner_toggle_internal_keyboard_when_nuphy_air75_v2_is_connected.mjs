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
// On the right side click and set shortcut cmd+shift+0
// Also I have created macro cmd+shift+0 and mapped it to screenshot key
// https://apple.stackexchange.com/questions/24063/create-global-shortcut-to-run-command-line-applications/40887#40887

// https://stackoverflow.com/questions/10685998/how-to-update-a-value-in-a-json-file-and-save-it-through-node-js/28175137#28175137

import { writeFile } from "fs";
import { exec } from "child_process";

const debug = false;
// https://stackoverflow.com/questions/21077670/expanding-resolving-in-node-js/54183941#54183941
import { homedir } from "os";
const fileName = "~/.config/karabiner/karabiner.json".replace(/^~/, homedir);
if (debug) console.log(fileName);
import file from "/Users/roman.frolow/.config/karabiner/karabiner.json" with { type: "json" };

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
    device.disable_built_in_keyboard_if_exists =
      !device.disable_built_in_keyboard_if_exists;
    console.log(
      "disable_built_in_keyboard_if_exists",
      device.disable_built_in_keyboard_if_exists,
    );

    // https://gist.github.com/LukaszWiktor/80d2423072a7b88ec14d7688ff9b7433
    exec(
      `osascript -e 'display notification "disable_built_in_keyboard_if_exists: ${device.disable_built_in_keyboard_if_exists}"'`,
      (error, stdout, stderr) => {
        if (error) {
          console.log(`error: ${error.message}`);
          return;
        }
        if (stderr) {
          console.log(`stderr: ${stderr}`);
          return;
        }
        console.log(`stdout: ${stdout}`);
      },
    );
  }
}

writeFile(fileName, JSON.stringify(file, null, 2), function writeJSON(err) {
  if (err) return console.log(err);
  if (debug) console.log(JSON.stringify(file));
  if (debug) console.log("writing to " + fileName);
});
