# Cuberhaus's dotfiles

## ![Imgur](https://i.imgur.com/nnJ6y17.png) Git

Install this repo:

```bash
git clone --recurse-submodules https://github.com/cuberhaus/dotfiles ~/dotfiles/dotfiles
```

Add new submodule

```bash
git submodule add URL   DIRECTORY
```

Submodules info

```bash
git submodule status
```

Update all submodules:

```bash
git submodule update --recursive --remote
```

Clone newly added submodules  
_See:_ [1](https://stackoverflow.com/questions/1030169/easy-way-to-pull-latest-of-all-git-submodules) and [2](https://stackoverflow.com/questions/38208748/how-do-i-clone-a-local-repo-with-submodules)

```bash
git submodule sync
git submodule update --init --recursive
```

[Move Submodule](https://stackoverflow.com/questions/4604486/how-do-i-move-an-existing-git-submodule-within-a-git-repository)

```bash
git mv old/submod new/submod
```

View submodules differences

```bash
git diff --submodule=diff
```

Download only a part of a repository:

> https://github.com/lodash/lodash/tree/master/test ➜ https://github.com/lodash/lodash/trunk/test

```bash
svn checkout https://github.com/lodash/lodash/trunk/test
```

Make the current commit the [only commit][only commit] in a git repo

```bash
git checkout --orphan newBranch
git add -A  # Add all files and commit them
git commit
git branch -D master  # Deletes the master branch
git branch -m master  # Rename the current branch to master
git push -f origin master  # Force push master branch to github
git gc --aggressive --prune=all     # remove the old files
```

> Doing this didn't reduce the repo size at all

Remove file from all [commits](https://docs.github.com/en/free-pro-team@latest/github/authenticating-to-github/removing-sensitive-data-from-a-repository):

```
bfg --delete-files FILE_WITH_SENSITIVE_DATA PATH_TO_REPO
cd some-big-repo.git
git reflog expire --expire=now --all && git gc --prune=now --aggressive
git push
```

Remove [folder](https://rtyley.github.io/bfg-repo-cleaner/) from all commits:

```
bfg --delete-folders FOLDER_WITH_DATA PATH_TO_REPO
cd some-big-repo.git
git reflog expire --expire=now --all && git gc --prune=now --aggressive
git push
```

## ![Rclone_icon][rclone_icon] Rclone

To sync a remote to a local folder

> remote = folder  
> REMOTENAME: drive:

```bash
rclone sync SOURCEDIRECTORY drive:DIRECTORYBEINGCOPIEDTO
```

To sync a folder to a remote

> folder = remote

```bash
rclone sync drive:SOURCEDIRECTORY DIRECTORYBEINGCOPIEDTO 
```

Configure rclone:

```bash
rclone config
```

Quota about a remote:

```bash
rclone about drive:
```

To list remotes:

```bash
rclone listremotes
```

### Flags

- -P Shows progress in real time

## ![Arch icon][arch_icon] Arch

### Pacman

Print all environment variables:

```bash
env
```

Remove a package and its _dependancies_:

```bash
sudo pacman -Rs
```

Remove all **orphan** packages:

```bash
pacman -Rns $(pacman -Qtdq)
```

List all AUR packages:

```bash
pacman -Qm
```

List installed packages

```bash
pacman -Qqe > pkglist.txt
```

Remove cache:

```bash
yay -Sc
```

Identify which package owns a file

```bash
pacman -Qo /path/to/file
```

[Pacman failed to commit transaction:](https://forum.manjaro.org/t/update-or-package-installation-returns-failed-to-commit-transaction-conflicting-files-filename-exists-in-filesystem/3598)

```bash
sudo pacman -S $PACKAGE --overwrite '*'
```

### Usb

Remove Usb safely:

_Option 1_:

```bash
sync
umount /dev/sdX
```

_Option 2_:

```bash
udisksctl unmount -b /dev/sdXY
udisksctl power-off -b /dev/sdX
```

### Usage

Print PATH:

```bash
path
```

Print environment variables:

```bash
printenv
```

Print out installed packages by [Homebrew][brew page]:

```bash
brew leaves
```

Find out class of an app:

```bash
xprop
```

View resources usage:

```bash
htop
```

View disk usage:

```bash
ncdu
```

Show all attached devices:

```bash
lsblk
```

To copy files from one place to another place:

```bash
dd
```

Show swap memory:

```bash
swapon --show
```

Gives information about a disk:

```bash
fdisk -l /dev/sdx
```

List blocks with label and [UUID](https://wiki.archlinux.org/index.php/Persistent_block_device_naming)

```bash
lsblk -f
```

List currently running services:

```bash
systemctl --type=service
```

Show systemd Journal:

```bash
journalctl
```

Change user owner of a file:

```bash
chown $user $file
```

Change group of a file:

```bash
chgrp $group $file
```

### Disk Partition

Use cfdisk instead of fdisk

```bash
cfdisk
```

### Pacstrap

```bash
pacstrap /mnt base linux linux-firmware man-db man-pages texinfo vim sudo
```

### GPG keys

Install a [key](https://www.reddit.com/r/linuxquestions/comments/9fxddu/please_help_me_resolve_aur_signing_key_issue/)

```bash
gpg --keyserver hkp://pgp.mit.edu:80 --recv-keys CB9387521E1EE0127DA804843FDBB55084CC5D84
```

- hkp://pgp.mit.edu:80
- hkps://pgp.mit.edu
- hkps://keyserver.ubuntu.com
- hkp://p80.pool.sks-keyservers.net:80

### Jobs

View all background jobs

```bash
jobs
```

Take a job from the background to the foreground

> Takes job number 1 to the foreground

```bash
fg %1
```

Kill a specific job

> Kills job number 2

```bash
kill %2
```

Pressing ‘CTRL+Z’ will suspend the current foreground job.

Making a command execute in background when suspended

```bash
bg %1
```

## ![Mac][macos_icon] MacOS

### Brew

Fix brew

```bash
brew doctor
```

# ![Ubuntu][ubuntu_icon] Ubuntu

Print out manually installed packages

```bash
comm -23 <(apt-mark showmanual | sort -u) <(gzip -dc /var/log/installer/initial-status.gz | sed -n 's/^Package: //p' | sort -u)
```

## ![GNU icon][gnu icon] Stow

Adding files to stow symlink:

1.  Create files with the same name of those we want to add to the repo
    (_in ~/dotfiles/dotfiles_)

    ```bash
    mkdir folder_structure_that_contains_the_file     # skip if it's inside ~/
    touch file_name
    ```

2.  Move files with the same name than those we created with "touch" to our dotfiles folder replacing our "touch" files and create links that replace mgces from the home directory

    ```bash
    stow --adopt -nvt ~ dotfiles/

    ```

Flags:

- -n Do nothing, just to see the effects of the command beforehand
- -v Verbose
- -t Target folder where links will be created "~"
- --adopt Moves the existing files to the -d "destination" folder, without it, just links will be created

Cloning the repo and creating symlinks to the files

Steps:

```bash
cd ~
mkdir dotfiles
cd dotfiles
git clone https://github.com/Pol-Gil/ArchDotfiles
stow -nvt ~ dotfiles/
```
## Wine 

Uninstall a wine application 

```bash
wine uninstaller
```
## ![SSH icon2][ssh icon2] SSH

Create the [key](https://docs.github.com/en/github/authenticating-to-github/connecting-to-github-with-ssh)

```bash
ssh-keygen -t rsa -b 2048 -C "some name"
```

[Open the program that holds private keys and then add the key](https://stackoverflow.com/questions/17846529/could-not-open-a-connection-to-your-authentication-agent)  
_Tip:_ Do not add the .pub (public key)

```bash
eval `ssh-agent -s`
ssh-add KEYFILE
```

List [remotes](https://help.github.jp/enterprise/2.11/user/articles/changing-a-remote-s-url/) to see their protocol

```bash
git remote -v
```

Change from Https to SSH

```bash
git remote set-url origin git@hostname:USERNAME/REPOSITORY.git
```

Change from SSH to Https

```bash
git remote set-url origin https://hostname/USERNAME/REPOSITORY.git
```

### WORKING

- ![Arch_icon][arch_icon] Arch
- ![Manjaro_icon][manjaro_icon] Manjaro
- ![Ubuntu_icon][ubuntu_icon] Ubuntu
- ![MacOS_icon][macos_icon] MacOS

### WIP

- ![Gentoo_icon][gentoo_icon]Gentoo
- Openbox

Other comments

> In Unix-like operating systems, a device file or special file is an interface to a device driver that appears in a file system as if it were an ordinary file.  
> On Linux they are in the /dev directory, according to the Filesystem Hierarchy Standard.

> "Double space is a soft break"  
> You can use any html syntax inside a markdown file

> About [OVA][ova] files:  
> An OVA file is an Open Virtualization Appliance that contains a compressed, "installable" version of a virtual machine. When you open an OVA file it extracts the VM and imports it into whatever virtualization software you have installed on your computer.

[rclone_icon]: https://i.imgur.com/2S75O8C.png?1
[ssh icon2]: https://i.imgur.com/RY2Xk5O.png?1
[ssh icon]: https://i.imgur.com/Jtz8Dma.png?1
[gnu icon]: https://i.imgur.com/dc4F2u2.png?1
[windows 10 icon]: https://i.imgur.com/b3co2Zl.png
[ova]: https://wikis.utexas.edu/display/MSBTech/Installing+OVA+files+using+VirtualBox#:~:text=An%20OVA%20file%20is%20an,have%20installed%20on%20your%20computer.
[brew page]: https://brew.sh/
[manjaro_icon]: https://i.imgur.com/rfuvfYo.png
[arch_icon]: https://upload.wikimedia.org/wikipedia/commons/a/a5/Archlinux-icon-crystal-64.svg
[ubuntu_icon]: https://i.imgur.com/EX9n2Ib.png?1
[macos_icon]: https://i.imgur.com/olG7ewE.png?1
[gentoo_icon]: https://i.imgur.com/cKReKS2.png
[only commit]: https://stackoverflow.com/questions/9683279/make-the-current-commit-the-only-initial-commit-in-a-git-repository
