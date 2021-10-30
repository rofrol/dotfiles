#!/bin/bash
 
set -e

# run it like this:
# sudo -s ./install-robo3t-on-debian.sh

# sudo apt install jq desktop-file-utils

# clean
rm -rf robo3t /usr/localbin/robo3t /usr/share/applications/robo3t/usr/share/applications/robo3t.desktop

JSON="$(curl -s https://api.github.com/repos/Studio3T/robomongo/releases/latest | jq -r '.assets[] | select(.name | select(endswith(".tar.gz")))')"
URL="$(echo $JSON | jq -r '.browser_download_url')"
FILENAME="$(echo $JSON | jq -r '.name')"
NAME="$(basename "$FILENAME" .tar.gz)"
echo "URL=$URL"
echo "FILENAME=$FILENAME"
echo "NAME=$NAME"

curl -sOJLN "$URL"
tar xf "$FILENAME"
rm -rf "$FILENAME"
sudo mv "$NAME" /usr/local/bin/robo3t
cd /usr/local/bin/robo3t/bin
sudo curl -s "https://raw.githubusercontent.com/Studio3T/robomongo/48f7dfde82b9c4233ca0133044114dca239f5239/install/macosx/robomongo.iconset/icon_128x128.png" -o icon.png
cat > robo3t.desktop <<EOL
[Desktop Entry]
Name=Robo3T
Icon=/usr/local/bin/robo3t/bin/icon.png
Exec=/usr/local/bin/robo3t/bin/robo3t
Terminal=false
Type=Application
Categories=Development;
StartupNotify=true
EOL

desktop-file-validate robo3t.desktop
sudo desktop-file-install robo3t.desktop
rm robo3t.desktop

# Now, we can find the `icon` in application launcher menu by search for `robo3t`

# https://askubuntu.com/questions/739297/how-to-install-robomongo-on-ubuntu/974766#974766
# https://stackoverflow.com/questions/24987542/is-there-a-link-to-github-for-downloading-a-file-in-the-latest-release-of-a-repo#comment49446532_29360657
# https://unix.stackexchange.com/questions/156892/how-to-validate-verify-desktop-files
# https://stackoverflow.com/questions/821396/aborting-a-shell-script-if-any-command-returns-a-non-zero-value/821419#821419
# https://askubuntu.com/questions/739297/how-to-install-robomongo-on-ubuntu/1372127#1372127
