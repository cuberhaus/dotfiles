windows_install () {
    choco install visualstudio2017buildtools
    choco install golang
    choco install nodejs Rem npm
    choco install cmake --installargs 'ADD_CMAKE_TO_PATH=System' Rem cmake
}

windows_install Rem Installs dependancies
cd ~\.vim\plugged\YouCompleteMe\
python3 install.py --all
