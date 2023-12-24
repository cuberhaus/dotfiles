#! /usr/bin/env bash
# Author: https://github.com/cuberhaus
flags="-y"
apt="sudo apt-get $flags install"

function Help () {
    echo "This script installs YouCompleteMe and its dependancies "
    echo "Usage: ycm.sh"
}

arch_based_install () {
    sudo pacman -S patchelf go mono cmake llvm --noconfirm
    ### This ones are already on the main install (faster!)
    #sudo pacman -S npm python3 python 
}

ubuntu_based_install () {
    $apt build-essential 
    $apt clangd
    $apt cmake 
    $apt cmake 
    $apt default-jdk 
    $apt golang 
    $apt golang-go 
    $apt mono-complete 
    $apt mono-devel 
    $apt nodejs 
    $apt npm 
    $apt openjdk-8-jre 
    $apt python-dev 
    $apt python3-dev 
    $apt vim-nox 
}

mac_install () {
    brew install macvim mono cmake python go nodejs llvm
}

ycm_ubuntu_install () {
    cd ~/.vim/bundle/YouCompleteMe
    python3 install.py --all
}

ycm_install () {
    cd $HOME
    mkdir ycm_build
    cd ycm_build
    cmake -G "Unix Makefiles" . $HOME/.vim/plugged/YouCompleteMe/third_party/ycmd/cpp
    cmake --build . --target ycm_core --config Release
    cd $HOME/.vim/plugged/YouCompleteMe
    python3 install.py --all
    cd $HOME
    sudo rm -r ycm_build # Clean Up
}

if [[ $1 == "-h" ]]; then
    Help
    exit
fi

# Install dependancies
if [[ "$OSTYPE" == "darwin"* ]]; then
    mac_install
    ycm_install # Builds
elif [[ "$DISTRO" == "arch" ]]; then
    arch_based_install
    ycm_install # Builds
elif [[ "$DISTRO" == "ubuntu" ]]; then
    ubuntu_based_install
    ycm_install # Builds
elif [[ "$DISTRO" == "ubuntu_windows" ]]; then
    ubuntu_based_install
    ycm_install # Builds
elif [[ "$DISTRO" == "manjaro" ]]; then
    arch_based_install
    ycm_install # Builds
fi
rm -r $HOME/ycm_build # Cleanup
