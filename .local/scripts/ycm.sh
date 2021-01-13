#! /usr/bin/env bash

arch_based_install () {
    sudo pacman -S patchelf go mono cmake llvm --noconfirm
    ### This ones are already on the main install (faster!)
    #sudo pacman -S npm python3 python 
}

ubuntu_based_install () {
    sudo apt-get install python3-dev python-dev python3-dev cmake build-essential golang-go npm mono-devel openjdk-8-jre clangd
}

mac_install () {
    brew install macvim mono cmake python go nodejs llvm
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

# Install dependancies
if [[ "$OSTYPE" == "darwin"* ]]; then
    mac_install
elif [[ "$DISTRO" == "arch" ]]; then
    arch_based_install
elif [[ "$DISTRO" == "ubuntu" ]]; then
    ubuntu_based_install
elif [[ "$DISTRO" == "manjaro" ]]; then
    arch_based_install
fi
ycm_install # Builds
rm -r $HOME/ycm_build # Cleanup
