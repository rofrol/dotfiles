#!/usr/bin/env bash

sudo nix-channel --update
sudo nixos-rebuild build
#sudo ls -l result
sudo nix store diff-closures /run/current-system ./result | awk '/[0-9] →|→ [0-9]/ && !/nixos/' || echo

# https://discourse.nixos.org/t/how-to-get-this-pending-updates-notification-in-gnome/16344
