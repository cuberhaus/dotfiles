arch_based_install () {
    sudo pacman -S patchelf go npm mono python python3 cmake llvm
}
ycm_install () {
    mkdir ycm_build
    cd ycm_build
    cmake -G "Unix Makefiles" . ~/.vim/plugged/YouCompleteMe/third_party/ycmd/cpp
    cmake --build . --target ycm_core --config Release
    cd ~/.vim/plugged/YouCompleteMe
    python3 install.py --all
    rm -r ycm_build
}
cd ~
arch_based_install
ycm_install
cd ~/
rm -r ycm_build
