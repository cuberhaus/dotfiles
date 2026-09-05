# pol-server

Source of truth for the ASUS VivoBook X560UD home NAS/server. This directory
contains configuration that is safe to track and scripts that apply privileged
system configuration. It must not contain passwords, private keys, API tokens,
or generated machine state.

See [PLAN.md](PLAN.md) for the phased implementation roadmap, current gates,
and acceptance criteria derived from the Obsidian Home NAS design note.

## Bootstrap

The tracked bootstrap is the only supported way to converge routine server
configuration. Enroll or update its root-owned copy from this repository on the
managed workstation:

```bash
make enroll-pol-server
```

Enrollment validates and installs sudoers rules for a small set of exact,
root-owned commands. It requires the server user's sudo password in a visible
terminal. It does not grant `NOPASSWD: ALL`, accept arbitrary device paths, or
enable root SSH login.

For a supervised build session that needs to install new root-owned phases
without repeated password prompts, enroll an eight-hour maintenance window:

```bash
make enroll-pol-server-maintenance
```

This opt-in mode is intentionally broader: until the UTC `NOTAFTER` timestamp,
the `pol` account and any process running as that account can execute any command
as root without a password. It still does not enable root SSH. The installer
prints the expiry, validates both sudoers files with `visudo`, and preserves the
narrow permanent rules that remain after expiry.

Revoke the window as soon as the supervised work is done:

```bash
make revoke-pol-server-maintenance
```

The revoker itself is a permanent exact-command rule, so early rollback does not
need a password. The expired file is inert, but the revoke target also removes
it from `/etc/sudoers.d`.

During the active window, reboot through the tracked noninteractive target:

```bash
make reboot-pol-server
```

This target deliberately stops working after maintenance access expires or is
revoked; reboot is not part of the permanent passwordless sudo policy.

Apply current Debian updates noninteractively during the same window:

```bash
make upgrade-pol-server
```

Review `/var/run/reboot-required` afterward and use the tracked reboot target if
needed. Package upgrades are not part of routine baseline convergence.

After enrollment, audit with:

```bash
make audit-pol-server
```

Apply the enrolled safe baseline without a password:

```bash
make bootstrap-pol-server
```

Normal apply mode invokes only the installed root-owned bootstrap. The deploy
script transfers code only during enrollment and removes the temporary directory
afterward. Re-run enrollment after changing bootstrap code or tracked server
configuration. The bootstrap is idempotent and currently owns:

- The packages declared in `packages.txt`.
- The tracked administrative public key and SSH hardening configuration.
- Automatic security-update scheduling.
- Active SSH, SMART monitoring, SSD trimming, and APT upgrade timers/services.
- Hostname, timezone, locale, time synchronization, latest trim trigger, and
    failed-unit checks.
- The no-sleep policy and masked sleep targets.
- Disabled Samba services until authenticated shares are tracked and approved.

It deliberately does not partition disks, enable UFW, configure Samba shares,
install Docker, or deploy applications. Those capabilities enter the bootstrap
only after their phase-specific gate and rollback have been reviewed.

The selected future restic destination is a 1.5 TB WD Elements disk. Its
read-only workstation inventory found a 1 TB exFAT volume with approximately
712 GiB free, existing backups, and the Immich copy intended for later import.
Its SMART media counters are clean, but the observed 54 C temperature and lack
of a completed long self-test still block backup use. It is now attached
unmounted to `pol-server`; a 232-minute test started at 52 C and is expected to
finish at 20:12:46 CEST on 2026-09-05. Preserve its contents and do not format or
repartition it; see
`reports/2026-09-05-external-backup-baseline.md`.

## GitHub recovery mirror

The server keeps bare Git mirrors and all referenced Git LFS objects for every
repository owned by `cuberhaus`. The fine-grained read-only token is stored at
`/etc/cuberhaus/github-mirror-token`, outside Git and readable only by root.

```bash
make configure-pol-server-github-mirrors
make sync-pol-server-github-mirrors
make audit-pol-server-github-mirrors
```

The initial 2026-09-05 sync and post-sync audit verified 54 healthy active
repositories, zero retained mirrors, and a complete success marker. The
persistent timer is enabled and schedules a daily run at 03:30 with up to 30
minutes of randomized delay. A failed repository or LFS fetch prevents the
success marker from advancing while allowing the remaining repositories to be
processed.

## RSS email notifications

The server can replace the GrabFreeGames IFTTT applet with a small Python
command and a hardened systemd service. It checks the Steam Community RSS feed
at 08:00 and 20:00, with up to 15 minutes of randomized delay. Each unseen RSS
`guid` produces one plain-text email; the `guid` is recorded only after Gmail
accepts that message, so failed deliveries remain pending for the next run.

Configure it with a Gmail app password after enabling two-step verification:

```bash
make configure-pol-server-rss-email
```

Enter the Gmail sender, an optional recipient (blank means the same address),
and the app password directly in the hidden server prompt. The command validates
the SMTP login without sending an email, stores the settings root-only at
`/etc/cuberhaus/rss-email.json`, runs the service once to record the current feed
as a silent baseline, and then enables the persistent timer. The secret must
never be added to Git or pasted into chat.

Run an additional check manually and inspect its status with:

```bash
make run-pol-server-rss-email
make test-pol-server-rss-email
ssh home-nas 'systemctl status pol-server-rss-email.service --no-pager'
ssh home-nas 'systemctl list-timers pol-server-rss-email.timer --no-pager'
```

The test target sends one confirmation message to the configured recipient
without fetching the feed or changing its recorded GUIDs.

After a WD extended test finishes, email its current redacted SMART report with:

```bash
make email-pol-server-wd-report
```

The report command is model-pinned to `wd-backup`, includes command failures in
the message, and does not read or change RSS feed state.

The service uses a dynamic user, a private state directory, a read-only system,
and a systemd credential copy. Its first run intentionally sends nothing; all
items already present become the baseline. No container or n8n instance is
required for this single low-frequency workflow.

## Hardware qualification

The same enrollment installs a separate root-owned hardware command. It locates
the expected disks by exact model and capacity, so device-letter changes cannot
redirect a test. Reports omit disk serial numbers and other unique identifiers.

Print the current inventory, relevant SMART attributes, self-test history,
battery values, network addresses, and failed units:

```bash
make audit-pol-server-hardware
```

Start one nondestructive SMART long test at a time:

```bash
make start-pol-server-smart-long-kingston
# Wait for the reported completion time, then audit again.
make audit-pol-server-hardware

make start-pol-server-smart-long-micron
# Wait for the reported completion time, then audit again.
make audit-pol-server-hardware

make audit-pol-server-wd-backup
make start-pol-server-smart-long-wd-backup
# Wait for the reported completion time, then audit the WD disk again.
make audit-pol-server-wd-backup
```

These targets can only select `kingston`, `micron`, or `wd-backup`; the
root-owned command rejects unknown models, duplicate matches, and unexpected
capacities. The external WD path also requires SAT pass-through. These commands
do not mount, format, repartition, or erase any disk; the start commands request
only a drive-managed nondestructive SMART self-test.

## Kingston storage setup

Audit the destructive preconditions without changing the Kingston disk:

```bash
make audit-pol-server-storage
```

The root-owned command resolves the Kingston by exact model and capacity,
rejects a mounted or root-containing target, and requires the expected legacy
NTFS layout. After separately reviewing and approving the destructive change,
run:

```bash
make prepare-pol-server-storage
```

That command erases the Kingston partition table, creates one GPT ext4
filesystem labeled `nas-data`, records it in `/etc/fstab` by UUID, mounts it at
`/srv/storage`, and creates the initial ownership boundaries. It cannot target
the Micron system SSD or the external WD disk, and it refuses to run without
the exact destructive confirmation token embedded in the reviewed deploy path.

## Samba setup

The initial Samba deployment uses the current Wi-Fi interface by explicit
decision. Ethernet remains recommended for sustained transfer reliability, but
is not a prerequisite. Service access is limited to `192.168.1.0/24`, TCP 445,
and authenticated SMB2/SMB3 clients; guest access, SMB1, NetBIOS discovery, and
router port forwarding remain disabled.

Use the supervised maintenance workflow and keep configuration, password
enrollment, and auditing separate:

```bash
make enroll-pol-server-maintenance
make configure-pol-server-samba
make set-pol-server-samba-password
make audit-pol-server-samba
make audit-pol-server
make revoke-pol-server-maintenance
```

The password command creates the Samba credential interactively for the
non-login `pol-files` identity. Enter that password only in the terminal. The
tracked shares are authenticated read/write `shared` and `incoming`, plus
`private`, which is restricted to `pol-files`. The storage root, `immich`,
`appdata`, and the administrator's home are not shared.

After physical inspection, run the fixed moderate CPU thermal check:

```bash
make test-pol-server-thermals
```

It uses two workers at 60% load for two minutes and prints thermal telemetry
every ten seconds. Its duration and load are fixed in the root-owned command.

## Current inventory

Initial device letters were recorded on 2026-09-05, but they changed across
reboots and are not identifiers:

- Hostname: `pol-server`
- Debian: 13 (trixie), kernel `6.12.107+deb13-amd64`
- SSH user: `pol`
- Current LAN address: `192.168.1.34` on `wlp2s0`
- Current LAN: `192.168.1.0/24`
- System disk: `/dev/sdb`, Micron 1100, approximately 238.5 GB
    - `/dev/sdb1`: EFI, approximately 976 MB
    - `/dev/sdb2`: ext4 root, approximately 225.2 GB
    - `/dev/sdb3`: swap, approximately 12.3 GB
- Data disk: Kingston SA400S37960G, approximately 894.3 GB
    - One ext4 filesystem labeled `nas-data`
    - UUID `ba435bde-4f44-44f4-9f74-a6c55c59ab86`
    - Mounted at `/srv/storage` through `/etc/fstab`

The LAN address is currently DHCP-assigned. Reserve `192.168.1.34` for this
laptop in the router before relying on it for SSH or WireGuard.

## Changes applied

- OpenSSH is enabled and active.
- Public-key SSH access is authorized for the dedicated local client key:
    `SHA256:TmBy3/5PjJhHwNh+EDs96ZJvajK0Gj8xfqGrwBfE13Y`.
- `smartmontools` is installed for disk inspection.
- `ufw` is installed but has not been enabled or given firewall rules.
- `unattended-upgrades` and the tracked daily APT policy are installed and
    active.
- The tracked no-sleep policy is installed and active. All four systemd sleep
    targets report `masked` and the effective `logind` actions report `ignore`.
- The explicitly approved Kingston migration replaced its legacy NTFS layout
    with one GPT ext4 filesystem labeled `nas-data`.
- The baseline bundle was enrolled and its passwordless apply and audit paths
    passed on 2026-09-05.
- Two unattended reboot cycles restored SSH and passed the full baseline audit;
    the subsequent Debian full upgrade reported no pending packages.
- Temporary broad maintenance access was revoked after the supervised build;
    arbitrary passwordless sudo is denied and the exact root-owned commands remain.
- The Kingston SMART long test completed without error on 2026-09-05. Its old
    interface counters remained stable during the test at `524353` and `13`;
    future audits must compare against that baseline.
- The `nas-data` UUID mount and its acceptance file survived two additional
    reboots even though the Kingston device letter changed from `/dev/sdb` to
    `/dev/sdc`.
- The GitHub mirror completed its initial authenticated Git and LFS sync. All 54
    active repositories passed the post-sync integrity audit, and its persistent
    daily timer is enabled.
- PCI inventory exposes only the Qualcomm Wi-Fi controller, so wired service
    requires a Linux-compatible USB 3 gigabit Ethernet adapter.

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

Apply it from the tracked checkout on the managed workstation:

```bash
make bootstrap-pol-server
```

Enrollment transfers and invokes the installer; paths under `/tmp` are staging
details and are removed after each enrollment, so they must never be called
manually.
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

Before formatting the model-verified Kingston disk for `/srv/storage`:

1. Complete and verify an independent backup of all required data.
2. Record the complete SMART output for `/dev/sda` and `/dev/sdb` here.
3. Confirm the Kingston model and device path again with `lsblk` immediately before partitioning.
4. Review `make audit-pol-server-storage`, then explicitly approve the separate
    `make prepare-pol-server-storage` step for partitioning and formatting.
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
