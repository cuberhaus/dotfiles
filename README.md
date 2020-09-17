# Cuberhaus's dotfiles 
#### WORKING 
- ![Arch_icon][Arch_icon] Arch
- ![Manjaro_icon][Manjaro_icon] Manjaro
- ![Ubuntu_icon][Ubuntu_icon] Ubuntu
- ![MacOS_icon][MacOS_icon] MacOS
#### WIP
- ![Gentoo_icon][Gentoo_icon]Gentoo
- Openbox

## ![Imgur](https://i.imgur.com/nnJ6y17.png) Git
#### Install this repo:
```
git clone --recurse-submodules https://github.com/cuberhaus/dotfiles 
```
#### Add new submodule
```
git submodule add URL   DIRECTORY
```
#### Submodules info
```
git submodule status
```
#### Update all submodules:
```
git submodule update --recursive --remote
```
#### Clone newly added submodules:
[Source 1](https://stackoverflow.com/questions/1030169/easy-way-to-pull-latest-of-all-git-submodules), [Source 2](https://stackoverflow.com/questions/38208748/how-do-i-clone-a-local-repo-with-submodules)
```
git submodule sync
git submodule update --init --recursive
```
#### Move Submodule
[Explanation](https://stackoverflow.com/questions/4604486/how-do-i-move-an-existing-git-submodule-within-a-git-repository)
```
git mv old/submod new/submod
```
#### View submodules differences
```
git diff --submodule=diff
```
#### Download only a part of a repository:
> https://github.com/lodash/lodash/tree/master/test ➜  https://github.com/lodash/lodash/trunk/test
```
svn checkout https://github.com/lodash/lodash/trunk/test
```
#### Make the current commit the [only commit][only commit] in a git repo
```
git checkout --orphan newBranch
git add -A  # Add all files and commit them
git commit
git branch -D master  # Deletes the master branch
git branch -m master  # Rename the current branch to master
git push -f origin master  # Force push master branch to github
git gc --aggressive --prune=all     # remove the old files
```
> Doing this didn't reduce the repo size at all

# ![Arch icon][Arch_icon] Arch
## Pacman
Remove a package and its _dependancies_:
```
sudo pacman -Rs
```
Remove all **orphan** packages:
```
pacman -Rns $(pacman -Qtdq)
```
List all AUR packages:
```
pacman -Qm
```
List installed packages
```
pacman -Qqe > pkglist.txt
```
Remove cache:
```
yay -Sc
```
## Usb
Remove Usb safely: 

_Option 1_:
```
sync
umount /dev/sdX
```

_Option 2_:
```
udisksctl unmount -b /dev/sdXY
udisksctl power-off -b /dev/sdX
```

## Usage
Print PATH:
```
path
```
Print environment variables:
```
printenv
```
Print out installed packages by [Homebrew][brew page]:
```
brew leaves
```
Find out class of an app:
```
xprop
```
View resources usage:
```
htop
```
View disk usage:
```
ncdu
```
Show all attached devices:
```
lsblk
```
To copy files from one place to another place:
```
dd
```
Show swap memory:
```
swapon --show
```
Gives information about a disk:
```
fdisk -l /dev/sdx
```
List blocks with label and [UUID](https://wiki.archlinux.org/index.php/Persistent_block_device_naming)

```
lsblk -f
```
List currently running services:
```
systemctl --type=service
```
Show systemd Journal:
```
journalctl
```

## Disk Partition
Use cfdisk instead of fdisk
```
cfdisk
```
## Pacstrap:
```
pacstrap /mnt base linux linux-firmware man-db man-pages texinfo vim sudo
```

# ![Mac][MacOS_icon] MacOS
## Brew
#### Fix brew
```
brew doctor
```
#### Update
```
brew update
brew upgrade
```

# ![Ubuntu][Ubuntu_icon] Ubuntu
#### Update
```
sudo apt-get update
sudo apt-get full-upgrade
```
#### CleanUp
```
sudo apt-get autoclean
sudo apt-get clean
sudo apt-get autoremove
```
#### Print out manually installed packages
```
comm -23 <(apt-mark showmanual | sort -u) <(gzip -dc /var/log/installer/initial-status.gz | sed -n 's/^Package: //p' | sort -u)
```

# ![Windows][Windows 10 icon] Windows
## Chocolatey
#### Upgrade packages
```
choco upgrade all
```
#### List packages
```
choco list --local-only
```

## ![GNU icon][GNU icon] Stow 
#### Adding files to stow symlink:

1. Create files with the same name of those we want to add to the repo
(_in ~/dotfiles/dotfiles_)

    ```
    mkdir folder_structure_that_contains_the_file     # skip if it's inside ~/
    touch file_name
    ```
2. Move files with the same name than those we created with "touch" to our dotfiles folder replacing our "touch" files and create links that replace mgces from the home directory

    ```
    stow --adopt -nvt ~ dotfiles/ 

    ```
#### Flags: 
* -n Do nothing, just to see the effects of the command beforehand
* -v Verbose 
* -t Target folder where links will be created "~"
* --adopt Moves the existing files to the -d "destination" folder, without it, just links will be created

#### Cloning the repo and creating symlinks to the files 

Steps:
```
cd ~
mkdir dotfiles
cd dotfiles
git clone https://github.com/Pol-Gil/ArchDotfiles
stow -nvt ~ dotfiles/
```

## ![SSH icon2][SSH icon2] SSH
[Github](https://docs.github.com/en/github/authenticating-to-github/connecting-to-github-with-ssh)
#### Create the key
```
ssh-keygen -t rsa -b 2048 -C "some name"
```
#### Open the program that holds private keys and then add the key
Do not add the .pub (public key)  
[Source](https://stackoverflow.com/questions/17846529/could-not-open-a-connection-to-your-authentication-agent)
```
eval `ssh-agent -s`
ssh-add KEYFILE
```
#### List remotes to see their protocol
[Source](https://help.github.jp/enterprise/2.11/user/articles/changing-a-remote-s-url/)
```
git remote -v
```
#### Change from Https to SSH
```
git remote set-url origin git@hostname:USERNAME/REPOSITORY.git
```
#### Change from SSH to Https
```
git remote set-url origin https://hostname/USERNAME/REPOSITORY.git
```

###### Other comments:
> In Unix-like operating systems, a device file or special file is an interface to a device driver that appears in a file system as if it were an ordinary file.  
> On Linux they are in the /dev directory, according to the Filesystem Hierarchy Standard.

> "Double space is a soft break"  
> You can use any html syntax inside a markdown file

> About [OVA][OVA] files:  
> An OVA file is an Open Virtualization Appliance that contains a compressed, "installable" version of a virtual machine. When you open an OVA file it extracts the VM and imports it into whatever virtualization software you have installed on your computer.

[SSH icon2]: https://i.imgur.com/RY2Xk5O.png?1
[SSH icon]: https://i.imgur.com/Jtz8Dma.png?1
[GNU icon]: https://i.imgur.com/dc4F2u2.png?1
[Windows 10 icon]: https://i.imgur.com/b3co2Zl.png
[OVA]: https://wikis.utexas.edu/display/MSBTech/Installing+OVA+files+using+VirtualBox#:~:text=An%20OVA%20file%20is%20an,have%20installed%20on%20your%20computer.
[brew page]: https://brew.sh/
[Manjaro_icon]: https://i.imgur.com/rfuvfYo.png 
[Arch_icon]: https://upload.wikimedia.org/wikipedia/commons/a/a5/Archlinux-icon-crystal-64.svg
[Ubuntu_icon]: https://i.imgur.com/EX9n2Ib.png?1
[MacOS_icon]: https://i.imgur.com/olG7ewE.png?1 
[Gentoo_icon]: https://i.imgur.com/cKReKS2.png 
[only commit]: https://stackoverflow.com/questions/9683279/make-the-current-commit-the-only-initial-commit-in-a-git-repository
