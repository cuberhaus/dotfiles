

 
# Arch
# Stow
### Adding files to stow symlink:

1. Create files with the same name of those we want to add to the repo
(in ~/dotfiles/dotfiles)
```
mkdir folder_structure_that_contains_the_file     # skip if it's inside ~/
touch file_name
```

2. Move files with the same name than those we created with "touch" to our dotfiles folder replacing our "touch" files and create links that replace mgces from the home directory
```
stow --adopt -nvt ~ dotfiles/ 

```
### Flags: 
* -n Do nothing, just to see the effects of the command beforehand
* -v Verbose 
* -t Target folder where links will be created "~"
* --adopt Moves the existing files to the -d "destination" folder, without it, just links will be created
### Cloning the repo and creating symlinks to the files 

Steps:
```
cd ~
mkdir dotfiles
cd dotfiles
git clone https://github.com/polthelost/dotfiles
stow -nvt ~ dotfiles/
```

# Usage

Print out installed packages by Homebrew:
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
List blocks with label and UUID
https://wiki.archlinux.org/index.php/Persistent_block_device_naming
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
List installed packages
```
pacman -Qqe > pkglist.txt
```

# Git
To update all submodules:
```
git fetch --recurse-submodules
git merge --recurse-submodules
```
### Disk Partition
Use cfdisk instead of fdisk
```
cfdisk
```

In Unix-like operating systems, a device file or special file is an interface to a device driver that appears in a file system as if it were an ordinary file.
On Linux they are in the /dev directory, according to the Filesystem Hierarchy Standard.


# Pacstrap:
```
pacstrap /mnt base linux linux-firmware man-db man-pages texinfo vim sudo
```
