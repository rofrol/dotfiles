#Because I had problem with space in directory name, I have done this:
#on OS X: mv ~/Library/Application\ Support/Terraria ~/Terraria
#rsync [OPTION...] SRC... [USER@]HOST:DEST
#rsync -azvvcW -e ssh romanfroow@192.168.0.18:/Users/romanfroow/Terraria /media/c/Users/roman/Documents/My\ Games/
HOST="$1"
rsync -azvvcW -e ssh /home/roman/.local/share/Terraria/{Worlds,Players} romanfroow@${HOST}:/Users/romanfroow/Terraria/
