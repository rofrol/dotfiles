# run this:
# $ sudo -s ./bin/maps_capslock_to_escape.sh

# to get scancode for capslock

# $ sudo apt install evtest
# $ evtest
# ...
# Event: time 1635332652.595399, type 4 (EV_MSC), code 4 (MSC_SCAN), value 70039
# Event: time 1635332652.595399, type 1 (EV_KEY), code 58 (KEY_CAPSLOCK), value 1

# to get name string for escape:

# $ grep KEY_ESC /usr/include/linux/input-event-codes.h
# #define KEY_ESC

sudo tee /etc/udev/hwdb.d/10-my-modifiers.hwdb > /dev/null <<EOT
# all usb kbds
evdev:input:b0003v*p*
#caps -> esc
 KEYBOARD_KEY_70039=esc
EOT

sudo udevadm hwdb --update

# check timestamp that it was rebuilt
ls -l /etc/udev/hwdb.bin

sudo udevadm trigger --sysname-match="event*"

# should show:
# E: KEYBOARD_KEY_70039=esc
sudo udevadm info /dev/input/by-path/*-usb-*-kbd | grep KEYBOARD_KEY

# https://unix.stackexchange.com/questions/156985/keyboard-hard-remap-keys/170357#170357
# https://wiki.archlinux.org/title/Map_scancodes_to_keycodes
# https://github.com/10ne1/carpalx-keyboard/blob/master/linux/systemd/60-qwerty.hwdb
# https://unix.stackexchange.com/questions/31970/remap-capslock-to-escape-and-control-system-wide/50176#50176
# https://unix.stackexchange.com/questions/40719/stay-at-same-working-directory-when-changing-to-sudo/40722#40722

## Old

# localectl does not work in Debian. Maybe bc of this:
# and the locale D-BUS service goes and rewrites /etc/vconsole.conf with its best guess as to the nearest equivalent map for the kernel virtual terminal. This nearest equivalent may not be exactly equivalent, though, and you may find it just outright ignoring options and suchlike.
# https://unix.stackexchange.com/questions/326651/swapping-keys-in-a-virtual-terminal/326804#326804

# works in terminal and X.org
# https://superuser.com/questions/566871/how-to-map-the-caps-lock-key-to-escape-key-in-arch-linux/1378825#1378825
# https://superuser.com/questions/566871/how-to-map-the-caps-lock-key-to-escape-key-in-arch-linux/1378825#1378825
# https://unix.stackexchange.com/questions/283558/remapping-caps-lock-for-every-session
# sudo localectl set-x11-keymap pl pc105 '' caps:escape
# localectl set-x11-keymap us,de "" "" caps:escape,grp:alt_caps_toggle,grp_led:caps,lv3:ralt_switch_multikey,terminate:ctrl_alt_bksp,eurosign:e,rupeesign:4
# localectl set-x11-keymap pl "" "" caps:escape
# https://askubuntu.com/questions/982863/change-caps-lock-to-control-in-virtual-console-on-ubuntu-17/982865#982865
# localectl status | grep caps
#     X11 Options: ctrl:nocaps
# To make it work in Xorg, you need to logout.
# To make it work in terminal (ctrl+alt+f4), you need to restart computer

# This needs root:
# $ cat .keystrings 
# keycode 58 = Escape 
# $ sudo loadkeys .keystrings 

# https://martin.hoppenheit.info/blog/2014/mapping-caps-lock-to-escape-in-debian/
# https://wiki.debian.org/Keyboard/
# https://wiki.archlinux.org/title/Linux_console/Keyboard_configuration#Persistent_configuration
# https://unix.stackexchange.com/questions/57085/setting-console-font-in-vconsole-conf-does-not-work-systemd/118345#118345
# https://unix.stackexchange.com/questions/85374/loadkeys-gives-permission-denied-for-normal-user
# https://unix.stackexchange.com/questions/96463/detect-if-running-in-a-virtual-terminal
# https://unix.stackexchange.com/questions/266817/how-to-reverse-esc-and-caps-lock-on-tty/346988#346988

# this is only for xorg
# $ cat ~/.xsessionrc
# setxkbmap -option caps:escape

# this only for gnome and xorg https://askubuntu.com/questions/363346/how-to-permanently-switch-caps-lock-and-esc#comment2229733_1095629
# gsettings set org.gnome.desktop.input-sources xkb-options "['caps:escape']"
