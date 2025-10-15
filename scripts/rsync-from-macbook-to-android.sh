adb devices
adb forward tcp:8022 tcp:8022
time rsync -xWrctzva --delete --info=progress2 -e 'ssh -p 8022' --exclude .DS_Store ~/Downloads/Xiaomi\ Poco\ X3\ Pro/Download/ localhost:~/storage/shared/Download/ 2>~/rsync-from-macbook-to-android-errors.txt
