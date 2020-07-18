#! /usr/bin/env bash
# YOU NEED TO ADD YOURSELF TO WHEEL AND TO VIDEO
# video makes backlight work and wheel gives you sudo privilege
# usermod -a -G wheel video
arch_install () {
    sudo pacman -S xorg
    # Drivers
    # Add force composition pipeline in nvidia-settings
    sudo pacman -S nvidia nvidia-settings xf86-video-intel
    # Theme
    sudo pacman -S lxappearance gtk2 gtk3 arc-gtk-theme cmatrix arc-icon-theme
    # Notifications
    # Image viewer
    sudo pacman -S nomacs
    # Audio | Equalizer qpaeq
    sudo pacman -S pulseaudio pulseaudio-equalizer pulseaudio-bluetooth
    # Video player
	sudo pacman -S vlc
    mkdir ~/.cache/vlc
    # Apps
    sudo pacman -S thunderbird chromium thunar albert

    # Terminal
    sudo pacman -S rxvt-unicode gvim exa ranger
    # Tmux
    sudo pacman -S tmux
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    # Manage storage
    sudo pacman -S ncdu
    # System tools
    sudo pacman -S base-devel dhcpcd git python3 stow
    # AUR
    sudo pacman -S --needed base-devel
    # Display manager
    #systemctl enable lightdm.service
    sudo pacman -S lightdm-gtk-greeter lightdm
    # Fonts
    sudo pacman -S ttf-font-awesome ttf-dejavu ttf-liberation ttf-droid ttf-ubuntu-font-family ttf-roboto noto-fonts
    # Doxygen
    sudo pacman -S doxygen texlive-most graphviz gcc

}

vim_install () {
    cd ~/
    vim +PlugInstall +qall
    # Vim YouCompleteMe plugin install
    bash ~/scripts/ycm.sh
}

i3_install () {

    sudo pacman -S i3-gaps compton rofi i3blocks python-dbus
    # Wallpaper
    sudo pacman -S feh
    # Brightness
    sudo pacman -S light
    # battery plus
    sudo pacman -S awk bc upower
    sudo pacman -S acpi
    # calendar
    sudo pacman -S yad xdotool
    # Audio
    sudo pacman -S pulseaudio alsa-utils
}

snap_install () {
    # https://wiki.archlinux.org/index.php/Snap

	git clone https://aur.archlinux.org/snapd.git
	cd snapd
	makepkg -si
	sudo systemctl enable --now apparmor.service
	sudo systemctl enable --now snapd.apparmor.service
	sudo systemctl enagle --now snapd.socket
}

spotify_install () {
    # https://wiki.archlinux.org/index.php/Spotify
    git clone https://aur.archlinux.org/spotify.git
    cd spotify
    curl -sS https://download.spotify.com/debian/pubkey.gpg | gpg --import -
    makepkg -si
}

Network_manager_install () {
    # Network Manager and enable daemon on startup
    sudo pacman -S networkmanager
    #systemctl enable NetworkManager.service
    # Gui
    sudo pacman -S nm-connection-editor
    # Applet
    sudo pacman -S nm-applet
}

Auto-mounting () {
# https://wiki.archlinux.org/index.php/USB_storage_devices
    # Daemon to manipulate storage devices
    sudo pacman -S udisks2
    # Automater
    sudo pacman -S udiskie
}

base16 () {
    git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell
}

### Installation starts here

cd ~/
# Update
sudo pacman -Syu
# Base install
#Network_manager_install
arch_install
i3_install
vim_install
spotify_install
Auto-mounting
base16
sudo pacman -S discord
yay -S skypeforlinux-stable-bin
# configure nvidia
nvidia-xconfig
