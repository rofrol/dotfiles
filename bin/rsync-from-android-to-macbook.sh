adb devices
adb forward tcp:8022 tcp:8022
time rsync -xWrctzv --info=progress2 -e 'ssh -p 8022' -azv localhost:~/storage/shared/Download . 2>rsync_errors_reverse.txt
