#! /usr/bin/env bash

arch_based_install () {
    sudo pacman -S patchelf go npm mono python python3 cmake llvm
}

ubuntu_based_install () {
    sudo apt-get install python3-dev python-dev python3-dev cmake build-essential golang-go npm mono-devel openjdk-8-jre clangd
}

mac_install () {
    brew install macvim mono cmake python go nodejs llvm
}

ycm_install () {
    cd ~
    mkdir ycm_build
    cd ycm_build
    cmake -G "Unix Makefiles" . ~/.vim/plugged/YouCompleteMe/third_party/ycmd/cpp
    cmake --build . --target ycm_core --config Release
    cd ~/.vim/plugged/YouCompleteMe
    python3 install.py --all
    rm -r ycm_build
}

if [[ "$OSTYPE" == "darwin"* ]]; then
    mac_install
elif [[ "$DISTRO" == "arch" ]]; then
    arch_based_install
elif [[ "$DISTRO" == "ubuntu" ]]; then
    ubuntu_based_install
elif [[ "$DISTRO" == "manjaro" ]]; then
    arch_based_install
fi
# Start
ycm_install # Builds
rm -r ~/ycm_build # Cleanup
