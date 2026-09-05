# pol-server implementation plan

This is the executable roadmap for turning the ASUS VivoBook into the home
server designed in the Obsidian note `2 Areas/Hogar/Home NAS.md`. The Obsidian
note owns the architecture and rationale; this file owns machine-specific
status, sequencing, acceptance gates, and tracked implementation artifacts.

## Target outcome

- Debian 13 runs from the 256 GB Micron SSD.
- The 960 GB Kingston SSD provides one ext4 data filesystem at `/srv/storage`.
- Samba serves authenticated household shares only on trusted private networks.
- Restic creates encrypted, versioned backups to an independent destination.
- Immich and OpenClaw run as separate Docker Compose projects after backup and
  restore are proven.
- Plain WireGuard provides remote access later; SSH, SMB, Immich, and OpenClaw
  are never directly forwarded from the router.
- Configuration, scripts, redacted templates, and verification commands live
  under `server/pol-server/`. Secrets and private keys never enter Git.

Do not create RAID, span LVM across both SSDs, expose application ports to the
Internet, or store Immich PostgreSQL on network storage.

## Change-control rules

Every implementation batch follows the same sequence:

1. Record a read-only baseline and identify the exact device or service.
2. Add the proposed configuration, bootstrap convergence logic, verification,
   and rollback to this directory. Manual command sequences are not an accepted
   final implementation.
3. Add or update deterministic tests and validate the tracked artifacts locally.
4. Run `make audit-pol-server`, then apply one bounded change with
   `make bootstrap-pol-server` or a separately reviewed phase command.
5. Run the documented acceptance check and update this plan with the result.

The baseline bootstrap must remain idempotent: a second apply on converged state
must make no changes. `make enroll-pol-server` is the only path that updates its
root-owned code and requires visible sudo approval; routine
`make bootstrap-pol-server` is passwordless but can execute only that installed
code with the exact `--apply` argument. Destructive storage operations and secret
provisioning are never hidden inside the baseline target.

During an explicitly supervised build session,
`make enroll-pol-server-maintenance` may install a broad passwordless sudo rule
with an eight-hour `NOTAFTER` timestamp. This is equivalent to temporary root
access for every process running as `pol`, so use it only while actively
configuring the host. Revoke it early with
`make revoke-pol-server-maintenance`; after expiry or revocation, only the
tracked exact-command rules remain. Root SSH stays disabled in all modes.

Formatting `/dev/sda`, changing firewall policy, disabling SSH passwords, and
configuring backup retention each require an explicit review immediately before
application. Never infer a destructive device path from its current letter;
verify model and capacity again at execution time.

## Current state

Baseline recorded on 2026-09-05:

| Area | State | Evidence or remaining issue |
| --- | --- | --- |
| Debian base | Complete | Debian 13 boots from the Micron SSD; root filesystem is 3% used |
| SSH administration | Complete | `ssh home-nas` authenticates with the dedicated local key |
| Sleep prevention | Complete | Effective `logind` actions are `ignore`; four sleep targets are `masked` |
| Service health | Complete | No failed systemd units at the baseline check |
| SSD trimming | Complete | `fstrim.timer` is enabled and active |
| Host identity and time | Complete | Audit enforces `pol-server`, `Europe/Madrid`, `es_ES.UTF-8`, synchronized NTP, and no failed units |
| Security updates | Complete | `unattended-upgrades`, APT periodic policy, and both upgrade services/timers are active |
| Cooling baseline | Complete | Fixed two-minute moderate load passed with a 54 C observed maximum and no service or SMART regressions |
| Battery | Complete | User confirmed no swelling or abnormal heat; telemetry reports 68.4% retained capacity after 630 cycles |
| Charger | Complete | User confirmed physical stability and kernel reports AC adapter online |
| Network | Blocked | PCI inventory contains only Qualcomm Wi-Fi and no Ethernet controller; a Linux-compatible USB 3 gigabit adapter is required |
| SMART qualification | Complete | Both long tests passed; media-error counters are zero and Kingston CRC counters stayed stable |
| Kingston data disk | Blocked | User confirmed no data must survive; NTFS remains untouched pending full Phase 0 gate and explicit format approval |
| Firewall | Pending | UFW is installed but intentionally not enabled or configured |
| Samba | Pending | Package is installed; users, directories, shares, and restrictions are not configured |
| Backup | Blocked | Existing external disk selected for `pol-server-restic`, but it is attached to another PC and must be inventoried before use |
| Baseline bootstrap | Complete | Root-owned bundle enrolled; narrow apply/audit pass and temporary broad maintenance access is revoked |
| Containers | Pending | Docker Engine and Compose plugin are not installed |
| Immich | Pending | Must wait for storage, Samba isolation, backup, and monitoring |
| OpenClaw | Pending | Must wait for backup and Docker; use a cloud model initially |
| Remote access | Pending | WireGuard waits until LAN operation and recovery are proven |

## Phase 0: close hardware and data gates

Status: **in progress**.

Read-only inventory on 2026-09-05 confirmed that Debian root is on the Micron,
the Kingston remains an unmounted 894.2 GB NTFS partition, no systemd units are
failed, and Wi-Fi is still the only network interface. The battery reports
24.615 Wh full-charge capacity versus 36 Wh design capacity (68.4%) after 630
cycles. Serial numbers are intentionally omitted from tracked reports.

The enrolled hardware command resolves disks by exact model and expected
capacity rather than trusting `/dev/sdX`. Its report redacts unique disk
identifiers. The only passwordless diagnostic mutations are fixed commands to
start a SMART long test on the named Kingston or Micron model.

The Kingston long test completed without error at 999 power-on hours. Health
remained `PASSED`, reallocated and reported-uncorrectable counters remained zero,
and the historical interface counters stayed unchanged during the test:
`SATA_CRC_Error_Count=524353` and `CRC_Error_Count=13`. Treat those as a
baseline, not as repaired history; any increase requires stopping and inspecting
the internal connection. The Micron long test completed without error at 2,707
power-on hours. Its reallocated, pending, uncorrectable, and UDMA CRC counters
are all zero. The redacted details are saved in
`reports/2026-09-05-smart-baseline.md` outside the server.

1. Confirm whether the Kingston NTFS volume contains anything that must survive.
2. Copy all required data to an independent device or remote destination and
   open a representative sample from that copy.
3. Run fresh SMART long tests on the Micron and Kingston SSDs separately. Save
   redacted reports outside the server and record health, uncorrectable errors,
   interface errors, wear, temperature, and self-test results here.
4. Investigate the preliminary Kingston SMART report's old CRC/interface error
   counters. Accept the disk only if the new test passes and counters do not
   increase; otherwise stop and inspect the internal connection or replace it.
5. Inspect the battery physically for swelling or heat and record design versus
   full-charge capacity. The reported cycle count alone is not a health verdict.
6. Confirm a usable RJ45 port or obtain a Linux-compatible USB 3 gigabit
   Ethernet adapter. Test sustained transfer and link stability.
7. Confirm charger stability, ventilation, and temperatures under a controlled
   CPU and disk load.

User confirmations on 2026-09-05:

- No data on the Kingston needs to be preserved, so the old-data copy step is
   not applicable. This is not yet authorization to format it.
- Battery, charger, and ventilation passed physical inspection without swelling,
   abnormal heat, or instability.
- The tracked two-minute moderate CPU load completed with zero stressor failures
   and a 54 C maximum; post-load service and SMART audits passed.
- No USB Ethernet adapter is available yet.
- The existing external backup disk was selected as the restic destination. It
   remains attached to another PC and contains backups plus an Immich copy that
   must be preserved and later imported. When connected, inventory it read-only
   before creating a dedicated `pol-server-restic` directory; do not reformat it.

Tracked commands:

```bash
make audit-pol-server-hardware
make start-pol-server-smart-long-kingston
make start-pol-server-smart-long-micron
```

Run the two long tests separately. Re-run the hardware audit after each test has
finished and compare the Kingston CRC counter with the recorded starting value.
The physical battery, charger, ventilation, old-data backup, and wired Ethernet
checks still require human inspection or external equipment.

Acceptance gate:

- Required old data is independently backed up and sampled successfully.
- Both SSDs complete fresh long tests without media or uncorrectable errors.
- CRC/interface counters remain stable during testing.
- Battery, charger, and cooling are physically safe.
- A reliable wired-network path is available.

All items except the wired-network path are complete. Phase 3 remains blocked
until a compatible USB 3 gigabit Ethernet adapter is connected and proven.

Stop here if any item fails. Do not repartition the Kingston SSD.

## Phase 1: preserve recovery material

Status: **partial** because Debian is already installed.

1. Keep any required Windows recovery material and activation notes outside the
   laptop if returning to Windows matters.
2. Keep the Debian installer and its verified checksum available on another
   machine.
3. Record the hardware map, firmware boot settings, SSH public-key fingerprint,
   and recovery procedure outside the NAS.
4. Export this repository or ensure its remote contains every accepted server
   change before the NAS becomes the primary copy of any data.

Acceptance gate: the operating system can be reinstalled without relying on
files stored only on `pol-server`.

## Phase 2: finish the Debian base

Status: **complete**.

Package convergence, automatic upgrades, SSH, SMART monitoring, trimming, and
the no-sleep policy are complete. The baseline auditor now enforces hostname,
timezone, locale, synchronized time, a recorded `fstrim` trigger, and no failed
systemd units. On 2026-09-05, two ordinary unattended reboots produced distinct
boot IDs; SSH key access and the full baseline audit passed after each boot and
time synchronization. A noninteractive APT refresh and full upgrade then
reported zero packages upgraded, installed, removed, or held back.

1. Apply current Debian security and point-release updates, then reboot and
   verify SSH key access.
2. Install and configure `unattended-upgrades`; confirm that automatic security
   upgrades and their timer actually run.
3. Keep `fstrim.timer` enabled and verify its last successful execution.
4. Record locale, timezone, time synchronization, hostname, and failed-unit
   checks in a tracked audit script.
5. Test two ordinary reboots without a monitor or keyboard.

Acceptance gate: two unattended boots succeed, `ssh home-nas` works afterward,
package upgrades are clean, time is synchronized, and no systemd units fail.

## Phase 3: qualify and prepare the Kingston SSD

Status: **blocked by Phase 0 and explicit approval**.

1. Recheck model, serial privately, capacity, filesystem, mount state, and SMART
   immediately before making changes.
2. Present the exact destructive plan and obtain confirmation that the NTFS
   partitions may be erased.
3. Create one GPT ext4 filesystem labeled `nas-data` on the Kingston SSD.
4. Mount it at `/srv/storage` by filesystem UUID through `/etc/fstab`.
5. Create `shared`, `private`, `incoming`, `immich`, and `appdata` directories
   with separate ownership boundaries. Never expose the entire mount as one
   share.
6. Keep at least 10-15% free and set the operational warning threshold at 80%.

Acceptance gate: the filesystem mounts by UUID after two reboots, a disposable
test file survives both reboots, ownership is correct, and SMART remains clean.

## Phase 4: finish power, cooling, network, and firewall

Status: **partial**; no-sleep is complete.

1. Move normal service traffic from Wi-Fi to wired gigabit Ethernet.
2. Reserve the wired adapter's address in the router. Update `Host home-nas` in
   `.ssh/config` only after the reserved address is proven.
3. Ensure there is one intended default route; keep Wi-Fi disabled or explicitly
   configured as fallback rather than an equal default path.
4. Configure UFW default-deny inbound. Allow SSH from the actual trusted LAN,
   then allow Samba only when its shares are ready. Review IPv4 and IPv6 rules.
5. Verify SSH through a second session before enabling UFW. Never expose TCP 22
   or TCP 445 through the router.
6. Test lid closure, console blanking, temperatures, and service reachability.
7. If firmware supports restore-on-AC-power, enable and test it only after backup
   and clean-shutdown procedures exist.

Acceptance gate: wired SSH survives a reboot and lid closure, the router
reservation is stable, only one default route exists, UFW permits only intended
private traffic, and no guest-network client can reach administration services.

## Phase 5: configure authenticated Samba shares

Status: **pending**.

1. Define the household users, private-directory ownership, and one `nasusers`
   group before writing configuration.
2. Add separate non-login identities where practical; do not share the
   administrator's home directory.
3. Start with one authenticated `shared` share and one private share per
   approved person. Keep guest access and SMB1 disabled.
4. Bind and firewall Samba to trusted private networks.
5. Store the complete `smb.conf`, directory-creation script, verification, and
   rollback under this directory before applying them.

Acceptance gate: `testparm -s` passes, SMB2/SMB3 is negotiated, approved users
can create/read/rename/delete test files, and guest or unauthorized users fail.

## Phase 6: implement backup and prove restore

Status: **blocked until the selected external disk is connected; required before
applications**.

The selected destination is an existing external backup disk. Preserve all
current content, especially the Immich copy intended for later import. The plan
is to add a dedicated `pol-server-restic` directory using free space, without
repartitioning or reformatting. Before any write, record its model, capacity,
filesystem, mount state, free space, SMART availability, and a top-level
read-only inventory.

1. Choose an independent backup destination with enough capacity for
   irreplaceable data and version history. A second internal SSD is not backup.
2. Install restic and define tracked include/exclude rules. Keep repository
   credentials in a root-readable environment file or password manager, never
   in Git.
3. Add tracked systemd service and timer units for daily backup.
4. Start with 7 daily, 5 weekly, 12 monthly, and 3 yearly snapshots. Preview
   retention with `--dry-run`; never prune after a failed backup.
5. Schedule repository checks and rotating `--read-data-subset` verification.
6. Restore to a different path while the source is unavailable and open several
   representative files.
7. Add an off-site copy for irreplaceable documents and photos.

Acceptance gate: unattended backup succeeds, failure is visible, `restic check`
passes, and a real restore works without access to the original source.

## Phase 7: monitoring and maintenance

Status: **pending**.

1. Configure `smartd` and scheduled monthly short and quarterly long tests.
2. Alert on backup failure, SMART failure, failed units, and 80% disk usage.
3. Add a tracked health-check command covering mounts, usage, SMART, services,
   backup freshness, updates, temperatures, and swap activity.
4. Schedule monthly restore sampling and quarterly ventilation/battery review.
5. Measure idle and busy wall power rather than relying on the planning estimate.

Acceptance gate: intentionally triggered test failures reach the chosen alert
channel and the maintenance checklist can be executed from the tracked runbook.

## Phase 8: add private remote access

Status: **deferred until LAN and restore acceptance pass**.

1. Determine public IPv4, usable inbound IPv6, or CGNAT from the router and a
   mobile-network test.
2. Prefer plain WireGuard on the router when its implementation is maintained
   and exportable; otherwise run it on Debian.
3. Under CGNAT, request a public address before paying for a small external
   WireGuard hub.
4. Assign one key and least-privilege route set per client. Store private keys
   in the password manager or encrypted backup, not in this repository.
5. Expose only the WireGuard UDP listener. Keep SSH, SMB, Immich, OpenClaw, and
   administration panels private.

Acceptance gate: an approved mobile peer reaches only allowed services, a
revoked peer fails, and an external scan finds no application ports exposed.

## Phase 9: deploy Immich

Status: **deferred until Phases 3, 6, and 7 pass**.

1. Install Docker Engine and the Compose plugin from Docker's Debian repository.
2. Keep the reviewed Compose project under a tracked server directory and keep
   its real `.env` file on the server with restrictive permissions.
3. Put Immich media under `/srv/storage/immich`; keep PostgreSQL on the Micron
   system SSD under `/var/lib/immich/postgres`.
4. Never expose the Immich-managed media tree through Samba.
5. Import a small test set and prove upload, retrieval, mobile background sync,
   paired media/database backup, and clean restore before the main library.
6. Measure RAM, swap, temperature, free space, and responsiveness during initial
   indexing. Test Intel Quick Sync separately before enabling it broadly.

Acceptance gate: a phone round trip and a clean paired restore pass while Samba
remains responsive and the server does not sustain heavy swap or unsafe heat.

## Phase 10: deploy OpenClaw

Status: **last**.

1. Use the reproducible Docker deployment from the local `openclaw-ai`
   repository with `OPENCLAW_WITH_OLLAMA=0` initially.
2. Use a dedicated service identity and separate Compose project. Mount only an
   explicitly approved workspace, never the NAS root, Immich media, Docker
   socket, or host root filesystem.
3. Keep the dashboard on loopback and access it through an SSH tunnel over
   WireGuard. Do not publish port 18789 to the LAN or Internet.
4. Configure provider spending limits and usage alerts. Keep tokens and OAuth
   state out of Git and normal Samba shares.
5. Prove backup, restore, restart behavior, and `openclaw security audit --deep`
   before enabling startup at boot.
6. Test one Immich upload, one Samba transfer, and one cloud-backed OpenClaw
   request concurrently. Record load, RAM, swap, temperature, and latency.

Acceptance gate: OpenClaw cannot access unrelated data, is unreachable from
unapproved clients, restores successfully, and leaves the server responsive
during representative concurrent use.

## Immediate next batch

Do not install more services yet. The next session should do only the following:

1. Confirm whether every required file on the Kingston NTFS volume is backed up
   and whether that disk may eventually be erased.
2. Confirm the independent backup destination and available capacity.
3. Confirm the physical Ethernet option and obtain router access for a DHCP
   reservation.
4. Add and run a tracked, read-only qualification script for SMART long tests,
   battery capacity, temperatures, network, and storage identity.
5. Review the resulting report and decide whether Phase 0 passes.

Only after those five items pass should the plan prepare a separately reviewed
script for the destructive Kingston migration.
