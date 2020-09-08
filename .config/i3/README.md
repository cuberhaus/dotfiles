# i3
### Config:
- Vim Mode: Keys changed from jkl, to hjkl
### Edit:
vim /usr/share/X11/xorg.conf.d/40-libinput.conf

```
#Match on all types of devices but joysticks :
Section "InputClass"
        Identifier "libinput pointer catchall"
        MatchIsPointer "on"
        MatchDevicePath "/dev/input/event*"
        Driver "libinput"
        Option "NaturalScrolling" "True"
EndSection

Section "InputClass"
        Identifier "libinput touchpad catchall"
        MatchIsTouchpad "on"
        MatchDevicePath "/dev/input/event*"
        Driver "libinput"
        Option "NaturalScrolling" "True"
EndSection
```
