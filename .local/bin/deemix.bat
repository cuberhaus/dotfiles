::::::::::::::::::::::::::::::::::::::::::::::::::::::
::    Installation script for Deemix                ::
::                                                  ::
::  This script will test if python3 is installed,  ::
::  install it on demand and then download and run  ::
::  Deemix from source.                             ::
::                                                  ::
::  Installer Version 1.7                           ::
::::::::::::::::::::::::::::::::::::::::::::::::::::::
@ECHO OFF
setlocal enabledelayedexpansion

CLS
:: Check for admin permissions. Needed for python requirement installation
:check_Permissions
    echo [93m[Deemix Installer]   Installer V1.7 started[0m
    echo [93m[Deemix Installer]   Administrative permissions required. Detecting permissions...[0m
    echo %DATE% %TIME% - Installer Version 1.7 started > "%~dp0log.txt"
    echo %DATE% %TIME% - Checking permissions >> "%~dp0log.txt"
    net session >nul 2>&1
    if %errorLevel% == 0 (
        echo [92m[Deemix Installer]   Success: Administrative permissions confirmed.[0m
        echo %DATE% %TIME% - Checking permissions - Admin rights >> "%~dp0log.txt"
    ) else (
        echo [91m[Deemix Installer]   Failed: Please start the install_windows.bat with 'Run as Administrator'[0m
        echo %DATE% %TIME% - Checking permissions - No Admin rights >> "%~dp0log.txt"
        TITLE Please run as admin
        pause
        exit /b
    )

:: Check for Python Installation
:pyTest
CLS
TITLE Deemix Installer
echo [93m[Deemix Installer]   Testing for Python Installation[0m
python --version 2>NUL
if errorlevel 1 (
    echo [91m[Deemix Installer]   Error^: Python not correctly installed[0m
    echo %DATE% %TIME% - Python Installation - Python not correctly installed >> "%~dp0log.txt"
    goto checkBadPathInstalls
)
if errorlevel 0 (
    python --version > "%~dp0pythonversion.txt"
    set /p pyVersion= < "%~dp0pythonversion.txt"
    echo %DATE% %TIME% - Python Installation - Python Version %pyVersion% installed >> "%~dp0log.txt"
    if !pyVersion:~7^,1! EQU 3 (
        goto dlDeemix
    ) else (
        echo [91m[Deemix Installer]   Warning: Old Python Version detected[0m
        goto checkBadPathInstalls
    )
)
:: No Python Installation found
:noPython
set /p answer=[93m[Deemix Installer]   Do you want to download and install Python 3.7? (Y/N)?[0m
if /i "%answer:~,1%" EQU "Y" goto dlPython
if /i "%answer:~,1%" EQU "N" exit /b

:: Download and install Python
:dlPython
echo [93m[Deemix Installer]   Downloading Python[0m
TITLE Deemix Installer - Downloading Python
bitsadmin /transfer PythonDownload /download /priority normal https://www.python.org/ftp/python/3.7.9/python-3.7.9-amd64.exe "%~dp0python-3.7.9-amd64.exe"
echo %DATE% %TIME% - Python Installation - Python Downloaded >> "%~dp0log.txt"
echo [93m[Deemix Installer]   Installing. Please wait a while![0m
TITLE Deemix Installer - Installing Python
"%~dp0python-3.7.9-amd64.exe" /quiet InstallAllUsers=1 PrependPath=1 Include_test=0
echo %DATE% %TIME% - Python Installation - Python installed >> "%~dp0log.txt"
echo [92m[Deemix Installer]   Python 3.7 installed[0m

:: Download and unpack Deemix. Then run the requirements install
:dlDeemix
echo [93m[Deemix Installer]   Downloading Deemix[0m
TITLE Deemix Installer - Downloading Deemix
bitsadmin /transfer DeemixBase /download /priority normal https://codeberg.org/RemixDev/deemix-pyweb/archive/main.zip "%~dp0pyweb.zip"
bitsadmin /transfer DeemixWeb /download /priority normal https://codeberg.org/RemixDev/deemix-webui/archive/main.zip "%~dp0webui.zip"
echo %DATE% %TIME% - Deemix Installation - Deemix downloaded >> "%~dp0log.txt"
echo [92m[Deemix Installer]   Deemix downloaded, unpacking[0m
TITLE Deemix Installer - Unpacking
mkdir "%~dp0deemixfiles"
echo Set objShell = CreateObject("Shell.Application") > "%~dp0unzip.vbs"
echo Set FilesInZip=objShell.NameSpace("%~dp0pyweb.zip").Items() >> "%~dp0unzip.vbs"
echo objShell.NameSpace("%~dp0deemixfiles").copyHere FilesInZip, 16 >> "%~dp0unzip.vbs"
echo Set FilesInZip=objShell.NameSpace("%~dp0webui.zip").Items() >> "%~dp0unzip.vbs"
echo objShell.NameSpace("%~dp0deemixfiles").copyHere FilesInZip, 16 >> "%~dp0unzip.vbs"
wscript "%~dp0unzip.vbs"
move "%~dp0deemixfiles\deemix-pyweb" "%~dp0deemixfiles\deemix"
rmdir /S /Q "%~dp0deemixfiles\deemix\webui"
move "%~dp0deemixfiles\deemix-webui" "%~dp0deemixfiles\deemix\webui"
echo %DATE% %TIME% - Deemix Installation - Deemix unzipped >> "%~dp0log.txt"
echo [92m[Deemix Installer]   Unpacked, installing requirements[0m
TITLE Deemix Installer - Installing python requirements
python -m pip install -U -r "%~dp0deemixfiles\deemix\requirements.txt" --user
echo [92m[Deemix Installer]   Requirements installed[0m
echo %DATE% %TIME% - Deemix Installation - Requirements installed >> "%~dp0log.txt"

:: Create start.bat
echo [93m[Deemix Installer]   Creating start.bat[0m
echo start python "%~dp0deemixfiles\deemix\server.py" > "%~dp0start.bat"
echo start http://localhost:6595 >> "%~dp0start.bat"
CLS
TITLE Deemix Installer - Done
echo %DATE% %TIME% - Deemix Installation - All done >> "%~dp0log.txt"
echo [42m[Deemix Installer]   Please start Deemix by executing the start.bat[0m
pause
exit

:checkBadPathInstalls
set pyInstalls=0
:: Find all python 3 installs in the default folders
FOR /D %%d IN ("%ProgramFiles%\Python*") DO (
  set /a "pyInstalls+=1"
  set pythonInstall[!pyInstalls!]=%%d
)
FOR /D %%d IN ("%ProgramFiles(x86)%\Python*") DO (
  set /a "pyInstalls+=1"
  set pythonInstall[!pyInstalls!]=%%d
)
FOR /D %%d IN ("%LocalAppData%\Programs\Python*") DO (
  set /a "pyInstalls+=1"
  set pythonInstall[!pyInstalls!]=%%d
)
echo [93m[Deemix Installer]   Found !pyInstalls! installations of Python[0m

:: if no installs are found, ask if python should be downloaded
if !pyInstalls! EQU 0 goto noPython

set pyOptions=0
FOR /l %%n in (1,1,%pyInstalls%) do (
    set /a "pyOptions+=1"
    echo [93m !pyOptions! : [0m !pythonInstall[%%n]!
)
:: ask the user which folder he wants added to the Path variable
set /p pyAnswer=[93m[Deemix Installer]   Which Install do you want to add to your PATH variable? (type Q to install a new Python Version)[0m
if /i %pyAnswer% LEQ %pyInstalls% (
    :: Add to path
    setx path "%PATH%;!pythonInstall[%pyAnswer%]!;" /M
    echo [42m[Deemix Installer]   Please restart the install for the new Path to be identified[0m
    pause
    exit
) else (
    if /I "%pyAnswer:~,1%" EQU "Q" (
        echo Aborting
        goto noPython
    ) else (
        echo Please select a valid number
        goto checkBadPathInstalls
    )
)
