set uncap=~\WinDotfiles\dotfiles\uncap.exe Rem uncap location
CALL :windows_install
CALL :vim_install
cd "~\dotfiles\.local\bin\
CALL ycm.bat
CALL :swap
REM CALL :games_install
EXIT /B %ERRORLEVEL% 

:vim_install
choco install vim -y
choco install neovim -y
Rem vim plug neovim
iwr -useb https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim |`
ni "$env:LOCALAPPDATA/nvim-data/site/autoload/plug.vim" -Force
Rem vim plug for PowerShell
iwr -useb https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim |`
    ni $HOME/vimfiles/autoload/plug.vim -Force
Rem npm
npm install -g neovim
Rem Python3
pip install --upgrade neovim
EXIT /B 0

:swap
mv %uncap% C:\Windows\
Rem Swap caps with escape https://github.com/susam/uncap#readme
EXIT /B 0

:windows_install
Install-Module git-aliases -Scope CurrentUser -AllowClobber Rem https://github.com/gluons/powershell-git-aliases
Install-Module PSWindowsUpdate
Get-WindowsUpdate
Install-WindowsUpdate
choco install fzf               Rem Fuzzy finder
choco install r.project         Rem Probabilitat i estadística
choco install r.studio          Rem Probabilitat i estadística IDE
choco install poshgit           Rem git bar
choco install adobereader -y 	Rem Pdf viewer
choco install chromium -y 	    Rem Open source Web browser
choco install firefox -y        Rem Open source web browser
choco install skype -y		    Rem Skype
choco install ccleaner -y 	    Rem Cleanup
choco install 7zip.install -y   Rem Archiver
choco install vlc -y		    Rem Media player
choco install git.install -y    Rem Git
choco install openssh -y	    Rem SSH client
choco install malwarebytes -y   Rem Anti-virus
choco install curl -y 		    Rem cUrL is a command line tool and library for transferring data with URLs
choco install wget -y		    Rem A command-line utility for retrieving files using HTTP protocols
choco install thunderbird -y    Rem Email client
choco install atom -y           Rem GUI Editor
choco install vscode -y         Rem GUI Editor
choco install yarn -y           Rem Packages
choco install spotify -y        Rem Spotify
choco install discord -y        Rem Discord
choco install telegram -y       Rem Telegram
choco install calibre -y        Rem Books manager
choco install microsoft-windows-terminal -y	Rem Windows terminal
choco install toastify -y 		Rem Toastify adds some missing functionallity to the Spotify client.
choco install rufus -y          Rem burn iso's on usb
choco install powershell-core   Rem Updated powershell
choco install linkshellextension -y Rem Make links from explorer
choco install wsl2 -y           Rem Windows subsystem for linux 2
choco install gimp -y           Rem Photoshop
choco install audacity          Rem Audio editor
choco install transmission -y   Rem Simple torrent client
REM choco install qbittorrent -y    Rem Torrent client
Rem choco install slack         Rem Slack
Rem choco install autohotkey.portable -y Rem Automation software
Rem choco install git-lfs
Rem choco install googlechrome 	Rem Web browser
Rem choco install jre8
Rem choco install teamviewer 	
Rem choco install virtualbox
Rem choco install chocolateygui -y  Rem A gui for chocolatey package manager
Rem choco install autoruns -y		Rem What programs are configured to startup automatically
EXIT /B 0
Rem office?
Rem Deezloader Remix?
Rem Maple

:games_install
choco install steam -y
choco install leagueoflegends -y
choco install goggalaxy -y
EXIT /B 0
