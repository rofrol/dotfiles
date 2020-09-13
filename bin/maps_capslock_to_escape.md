# works in terminal and X.org
# https://superuser.com/questions/566871/how-to-map-the-caps-lock-key-to-escape-key-in-arch-linux/1378825#1378825
# https://superuser.com/questions/566871/how-to-map-the-caps-lock-key-to-escape-key-in-arch-linux/1378825#1378825
# https://unix.stackexchange.com/questions/283558/remapping-caps-lock-for-every-session
# sudo localectl set-x11-keymap pl pc105 '' caps:escape
# localectl set-x11-keymap us,de "" "" caps:escape,grp:alt_caps_toggle,grp_led:caps,lv3:ralt_switch_multikey,terminate:ctrl_alt_bksp,eurosign:e,rupeesign:4
localectl set-x11-keymap pl "" "" caps:escape
# https://askubuntu.com/questions/982863/change-caps-lock-to-control-in-virtual-console-on-ubuntu-17/982865#982865
localectl status | grep caps
#     X11 Options: ctrl:nocaps
# To make it work in Xorg, you need to logout.
# To make it work in terminal (ctrl+alt+f4), you need to restart computer
