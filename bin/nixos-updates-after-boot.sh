#!/usr/bin/env bash

sudo nix store diff-closures /run/booted-system /run/current-system | awk '/[0-9] →|→ [0-9]/ && !/nixos/' || echo

# https://discourse.nixos.org/t/how-to-get-this-pending-updates-notification-in-gnome/16344
