

 
# Arch
## Disk Partition
Use cfdisk instead of fdisk
```
cfdisk
```
Show swap memory:
```
swapon --show
```
Gives information about a disk:
```
fdisk -l /dev/sdx
```
In Unix-like operating systems, a device file or special file is an interface to a device driver that appears in a file system as if it were an ordinary file.
On Linux they are in the /dev directory, according to the Filesystem Hierarchy Standard.

### List blocks with label and UUID
https://wiki.archlinux.org/index.php/Persistent_block_device_naming
```
lsblk -f
```
### Pacstrap:
```
pacstrap /mnt base linux linux-firmware man-db man-pages texinfo vim sudo
```
### List currently running services:
```
systemctl --type=service
```
### Show systemd Journal:
```
journalctl
```
### List installed packages
```
pacman -Qqe > pkglist.txt
```
Grub
Microcode
