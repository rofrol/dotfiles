#!/usr/bin/env bash
VERSION="$1"
if [ -z "${VERSION}" ]; then
	echo "Please set VERSION"
	exit 1
fi
# fd pkg /opt/homebrew/Caskroom/wireshark/${VERSION}/Wireshark.app
ln -s /opt/homebrew/Caskroom/wireshark/${VERSION}/Wireshark.app/Contents/Resources/Extras/Remove\ Wireshark\ from\ the\ system\ path.pkg /opt/homebrew/Caskroom/wireshark/${VERSION}/
ln -s /opt/homebrew/Caskroom/wireshark/${VERSION}/Wireshark.app/Contents/Resources/Extras/Uninstall\ ChmodBPF.pkg /opt/homebrew/Caskroom/wireshark/${VERSION}/
brew reinstall --cask wireshark

# https://github.com/orgs/Homebrew/discussions/4190#discussioncomment-8075275
