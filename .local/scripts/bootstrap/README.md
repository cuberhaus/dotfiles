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

## Wallpaper error
Wallpaper error when background is set in desktop, it will also be used by LightDM unless the picture isn't coming from /usr/share/backgrounds.

## Connect to upclink
```bash
f5fpc -s -x -t https://upclink.upc.edu
f5fpc --stop
```

## How to format USB
Format as [exFAT](https://www.howtogeek.com/73178/what-file-system-should-i-use-for-my-usb-drive/#:~:text=File%20systems%20are%20the%20sort,run%20into%20others%20on%20occasion.) for most compatibility with every OS and over 4GB files compatible.

## Swap escape and bloq mayus on xfce
If using xfce add /usr/bin/setxkbmap -option "caps:swapescape" to the startup applications to swap keys.
