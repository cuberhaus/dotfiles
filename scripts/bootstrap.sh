#! /usr/bin/env bash
arch_install () {
    # Doxygen
    sudo pacman -S doxygen texlive-most graphviz gcc
    # System tools
    sudo pacman -S dhcpcd git tmux nvidia python3 stow
    # Terminal
    sudo pacman -S rxvt-unicode gvim exa
    # Manage storage
    sudo pacman -S ncdu
    # Apps
    sudo pacman -S thunderbird virtualbox-qt transmission chromium thunar
    # Fonts
    sudo pacman -S ttf-dejavu ttf-liberation ttf-droid ttf-ubuntu-font-family ttf-roboto noto-fonts ttf-ms-fonts
    # Theme
    sudo pacman -S LXAppearance gtk-2 gtk-3 arc-gtk-theme
    # Tmux
    git clone https://github.com/tmux-plugins/tpm ~/.tmux/plugins/tpm
    # AUR
    sudo pacman -S --needed base-devel
    # Display manager
    sudo pacman -S lightdm-gtk-greeter lightdm
    # Image viewer
    sudo pacman -S nomacs
    # Audio
    sudo pacman -S pulseaudio
    # Video player
	sudo pacman -S vlc
    mkdir ~/.cache/vlc

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
}

cd ~/
# Update
sudo pacman -Syu

arch_install
i3_install
vim_install
