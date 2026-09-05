# pol-server

Source of truth for the ASUS VivoBook X560UD home NAS/server. This directory
contains configuration that is safe to track and scripts that apply privileged
system configuration. It must not contain passwords, private keys, API tokens,
or generated machine state.

## Current inventory

Recorded on 2026-09-05 from the active SSH session:

- Hostname: `pol-server`
- Debian: 13 (trixie), kernel `6.12.107+deb13-amd64`
- SSH user: `pol`
- Current LAN address: `192.168.1.34` on `wlp2s0`
- Current LAN: `192.168.1.0/24`
- System disk: `/dev/sdb`, Micron 1100, approximately 238.5 GB
    - `/dev/sdb1`: EFI, approximately 976 MB
    - `/dev/sdb2`: ext4 root, approximately 225.2 GB
    - `/dev/sdb3`: swap, approximately 12.3 GB
- Data candidate: `/dev/sda`, Kingston SA400S37960G, approximately 894.3 GB
    - `/dev/sda1`: approximately 16 MB
    - `/dev/sda2`: NTFS, approximately 894.2 GB
- The Kingston disk has not been formatted or repartitioned.

The LAN address is currently DHCP-assigned. Reserve `192.168.1.34` for this
laptop in the router before relying on it for SSH or WireGuard.

## Changes applied

- OpenSSH is enabled and active.
- Public-key SSH access is authorized for the dedicated local client key:
    `SHA256:TmBy3/5PjJhHwNh+EDs96ZJvajK0Gj8xfqGrwBfE13Y`.
- `smartmontools` is installed for disk inspection.
- `ufw` is installed but has not been enabled or given firewall rules.
- The tracked no-sleep policy is installed and active. All four systemd sleep
    targets report `masked` and the effective `logind` actions report `ignore`.
- No disk contents or partition tables have been changed.

Connect from the managed workstation with:

```bash
ssh home-nas
```

The tracked SSH host entry references `~/.ssh/home-nas-client_ed25519`. The
private key remains local and ignored; never add it to this repository.

## Power policy

The tracked source is
[`etc/systemd/logind.conf.d/90-pol-server.conf`](etc/systemd/logind.conf.d/90-pol-server.conf).
It prevents lid-close, idle, power-key, suspend-key, and hibernate-key actions
from suspending the server. The application script also masks the four systemd
sleep targets so an ordinary suspend or hibernate request cannot stop services.

Apply it from the repository root on the server:

```bash
sudo ./server/pol-server/apply-power-policy
```

The script is idempotent. It installs the tracked drop-in, masks
`sleep.target`, `suspend.target`, `hibernate.target`, and `hybrid-sleep.target`,
reloads systemd, and restarts `systemd-logind` so the policy is active.

Verify it with:

```bash
systemctl is-enabled sleep.target suspend.target hibernate.target hybrid-sleep.target
systemctl status systemd-logind --no-pager
```

The four targets should report `masked`.

## Rollback

Only use this if suspend is deliberately required again:

```bash
sudo rm /etc/systemd/logind.conf.d/90-pol-server.conf
for unit in sleep.target suspend.target hibernate.target hybrid-sleep.target; do
    sudo systemctl unmask "$unit"
done
sudo systemctl daemon-reload
sudo systemctl restart systemd-logind
```

## Safety gates

Before formatting `/dev/sda2` for `/srv/storage`:

1. Complete and verify an independent backup of all required data.
2. Record the complete SMART output for `/dev/sda` and `/dev/sdb` here.
3. Confirm the Kingston model and device path again with `lsblk` immediately before partitioning.
4. Use a separate, explicitly approved step for partitioning and formatting.
5. Mount the resulting filesystem by UUID, not by `/dev/sda2`.

## Reproducibility log

The following commands were run during the initial server setup:

```text
hostname
id -un
uname -sr
hostname -I
lsblk -o NAME,SIZE,MODEL,FSTYPE,MOUNTPOINTS
sudo apt-get update
sudo apt-get install -y smartmontools ufw
sudo smartctl -x /dev/sda
sudo smartctl -x /dev/sdb
ssh-keygen -t ed25519 -N '' -f ~/.ssh/home-nas-client_ed25519 -C dotfiles-client@pol-server
ssh-copy-id -i ~/.ssh/home-nas-client_ed25519.pub pol@192.168.1.34
sudo /tmp/pol-server-power-policy/apply-power-policy
systemctl is-enabled sleep.target suspend.target hibernate.target hybrid-sleep.target
systemd-analyze cat-config systemd/logind.conf
```

Record future commands and resulting state changes in this file or in a
machine-specific script before applying them to the server. Never record
passwords, private keys, or full secret-bearing environment files.
