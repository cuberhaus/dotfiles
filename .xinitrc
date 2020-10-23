#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

source ~/.config/distro

# merge in defaults and keymaps
if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

# altgr to type á
setxkbmap -rules evdev -model evdev -layout us -variant altgr-intl

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs
if [ -d /etc/X11/xinit/xinitrc.d ]; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi


# Set environmental variables before starting i3
# .xprofile just sources .zshenv, sourcing it from here just to be consistent with LightDM
[ -f /etc/xprofile ] && . /etc/xprofile
[ -f ~/.xprofile ] && . ~/.xprofile

# i3 will open apps itself, no need to start them from .zshenv or .xprofile,
if [[ "$DISTRO" == "arch" ]]; then
    ## Starts i3
    i3
fi

if [[ "$DISTRO" == "manjaro" ]]; then
    DEFAULT_SESSION=gnome-session
	get_session(){
		local dbus_args=(--sh-syntax --exit-with-session)
		case $1 in
			awesome) dbus_args+=(awesome) ;;
			bspwm) dbus_args+=(bspwm-session) ;;
			budgie) dbus_args+=(budgie-desktop) ;;
			cinnamon) dbus_args+=(cinnamon-session) ;;
			deepin) dbus_args+=(startdde) ;;
			enlightenment) dbus_args+=(enlightenment_start) ;;
			fluxbox) dbus_args+=(startfluxbox) ;;
			gnome) dbus_args+=(gnome-session) ;;
			i3|i3wm) dbus_args+=(i3 --shmlog-size 0) ;;
			jwm) dbus_args+=(jwm) ;;
			kde) dbus_args+=(startkde) ;;
			lxde) dbus_args+=(startlxde) ;;
			lxqt) dbus_args+=(lxqt-session) ;;
			mate) dbus_args+=(mate-session) ;;
			xfce) dbus_args+=(xfce4-session) ;;
			openbox) dbus_args+=(openbox-session) ;;
			*) dbus_args+=($DEFAULT_SESSION) ;;
		esac

		echo "dbus-launch ${dbus_args[*]}"
    }
    #exec dbus-launch --sh-syntax --exit-with-session gnome-session 
    exec $(get_session)
fi

