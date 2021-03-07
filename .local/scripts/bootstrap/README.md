# Arch Install

## To do:
- Video makes backlight work and wheel gives you sudo privileges.

```bash
usermod -a -G video pol
usermod -a -G wheel pol
usermod -a -G input pol
```
- Activate [REISUB](https://forum.manjaro.org/t/howto-reboot-turn-off-your-frozen-computer-reisub-reisuo/3855) for safe reboots if computer freezes.

## Video Card
Configure nvidia nvidia-xconfig.

## LightDM permissions "Wallpaper error"
Wallpaper error when background is set in desktop, it will also be used by LightDM unless the picture isn't coming from /usr/share/backgrounds.

## Connect to upclink VPN
```bash
f5fpc -s -x -t https://upclink.upc.edu
f5fpc --stop
```

## How to format USB
Format as [exFAT](https://www.howtogeek.com/73178/what-file-system-should-i-use-for-my-usb-drive/#:~:text=File%20systems%20are%20the%20sort,run%20into%20others%20on%20occasion.) for most compatibility with every OS and over 4GB files compatible.

## Swap escape and caps

View file:
```
vim /etc/X11/xorg.conf.d/00-keyboard.conf
```

https://wiki.archlinux.org/index.php/Xorg/Keyboard_configuration#Frequently_used_XKB_options
See mappings:
```bash
setxkbmap -print -verbose 10
```

https://man.archlinux.org/man/xkeyboard-config.7
https://wiki.archlinux.org/index.php/Xorg/Keyboard_configuration#Using_localectl
Swap caps spanish keyboard:
```bash
localectl --no-convert set-x11-keymap es pc104 cat caps:swapescape
localectl --no-convert set-x11-keymap es pc105 "" caps:swapescape
```

https://wiki.archlinux.org/index.php/Linux_console/Keyboard_configuration
https://wiki.archlinux.org/index.php/Xorg/Keyboard_configuration
USE ONLY WITH US KEYBOARD  
altgr to type á
```
setxkbmap -rules evdev -model evdev -layout us -variant altgr-intl
```
