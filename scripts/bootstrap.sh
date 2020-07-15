#! /usr/bin/env bash

arch_install () {
    # Theme
    sudo pacman -S LXAppearance gtk-2 gtk-3 arc-gtk-theme cmatrix
    # Image viewer
    sudo pacman -S nomacs
    # Audio
    sudo pacman -S pulseaudio
    # Video player
	sudo pacman -S vlc
    mkdir ~/.cache/vlc
    # Apps
    sudo pacman -S thunderbird virtualbox-qt transmission chromium thunar albert

    # Terminal
    sudo pacman -S rxvt-unicode gvim exa
    # Tmux
    sudo pacman -S tmux
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    # Manage storage
    sudo pacman -S ncdu
    # System tools
    sudo pacman -S dhcpcd git nvidia python3 stow
    # AUR
    sudo pacman -S --needed base-devel
    # Display manager
    sudo pacman -S lightdm-gtk-greeter lightdm
    # Fonts
    sudo pacman -S ttf-dejavu ttf-liberation ttf-droid ttf-ubuntu-font-family ttf-roboto noto-fonts ttf-ms-fonts
    # Doxygen
    sudo pacman -S doxygen texlive-most graphviz gcc

}

vim_install () {
    cd ~/
    vim +PlugInstall +qall
    # Vim YouCompleteMe plugin install
    bash ~/scripts/ycm
}

i3_install () {

    sudo pacman -S i3-gaps compton rofi i3blocks python-dbus
    # Wallpaper
    sudo pacman -S feh
    # Brightness
    sudo pacman -S light
    # battery plus
    sudo pacman -S awk bc upower
    # calendar
    sudo pacman -S yad xdotool
}

### Installation starts here

cd ~/
# Update
sudo pacman -Syu
# Base install
arch_install
i3_install
vim_install
