windows_install () {
    choco install visualstudio2017buildtools
    choco install golang
    choco install nodejs Rem npm
    choco install cmake --installargs 'ADD_CMAKE_TO_PATH=System' Rem cmake
    Rem vim plug neovim
    iwr -useb https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim |`
    ni "$env:LOCALAPPDATA/nvim-data/site/autoload/plug.vim" -Force
    Rem npm
    npm install -g neovim
    Rem Python3
    pip install --upgrade neovim
}
