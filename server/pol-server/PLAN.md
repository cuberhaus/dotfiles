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

For a complete build, recovery, or change spanning multiple phases, use
`make bootstrap-pol-server-full`. It is the resumable top-level lifecycle from
the first SSH-reachable Debian host onward and owns local validation, bounded
maintenance, all tracked phase commands, interactive acceptance, and cleanup.
Use `make status-pol-server-full` for its read-only aggregate audit. The phase
targets remain available for focused diagnosis and repair.

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
| Network | Wi-Fi accepted temporarily | Samba may use trusted LAN `192.168.1.0/24` on `wlp2s0`; USB 3 gigabit Ethernet remains the recommended reliability upgrade |
| SMART qualification | Complete | Both long tests passed; media-error counters are zero and Kingston CRC counters stayed stable |
| Kingston data disk | Complete | Approved model-pinned migration created ext4 `nas-data`; its UUID mount and acceptance file survived two reboots with changing device letters |
| Firewall | Pending | UFW is installed but intentionally not enabled or configured |
| Samba | Complete for Wi-Fi rollout | Dedicated `pol-files` access passed SMB3 file lifecycle tests on all three shares; guest access failed and `nmbd` remains disabled |
| Backup | Operational; policy follow-up pending | User accepted the incomplete SMART-test risk; encrypted Restic backup, rotating integrity check, and restore test passed on the preserved WD exFAT volume |
| GitHub mirrors | Complete | All 54 active repositories passed Git integrity; LFS fetches completed and the persistent daily timer is enabled |
| RSS email | Pending lifecycle acceptance | The full bootstrap audits and reuses healthy Gmail settings, configures only when needed, and requires receipt of its delivery test and WD SMART report before completion |
| Baseline bootstrap | Complete | Root-owned bundle enrolled; routine operations use exact narrow commands and supervised broad access remains bounded by `NOTAFTER` |
| Containers | Complete | Official Docker Engine 29.8.0 and Compose 5.5.1 are installed; `pol` remains outside the Docker group |
| Immich | Complete | Tracked v3.1.0 stack is LAN-bound with media on Kingston and PostgreSQL on Micron; preserved WD recovery and application acceptance passed |
| OpenClaw | Pending | Must wait for backup and Docker; use a cloud model initially |
| Monitoring | Local monitoring complete | Native LAN-only Netdata 2.11.0 with three-month/2 GiB bounded history; external Healthchecks.io email acceptance remains pending |
| Remote access | Tracked implementation complete; deployment pending | NAS-hosted `wg0`, fixed iPhone peer, pre-NAT nftables restriction, local key generation, revocation, and optional DuckDNS timer have hermetic tests; router and mobile acceptance remain |
| Complete lifecycle | Implemented; live acceptance pending | One resumable interactive entrypoint composes every tracked phase; full local validation and router/iPhone acceptance remain |

## Phase 0: close hardware and data gates

Status: **complete for the accepted Wi-Fi rollout**.

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

- No data on the Kingston needed preservation. Its separately approved,
   model-pinned migration is complete and recorded in Phase 3.
- Battery, charger, and ventilation passed physical inspection without swelling,
   abnormal heat, or instability.
- The tracked two-minute moderate CPU load completed with zero stressor failures
   and a 54 C maximum; post-load service and SMART audits passed.
- No USB Ethernet adapter is available yet. The user explicitly accepted Wi-Fi
   for the initial Samba rollout; Ethernet remains a later reliability upgrade.
- The existing WD Elements backup disk was inventoried read-only on the Ubuntu
   workstation. Its 1 TB exFAT volume contains backups plus an Immich copy that
   must be preserved and later imported, with approximately 712 GiB free. SMART
   reports clean media and interface counters. It is now attached unmounted to
   `pol-server`; repeated long tests were interrupted by host or bridge resets.
   The user chose not to retry them and explicitly accepted that residual risk
   for unattended Restic use. Do not reformat or repartition it.

Tracked commands:

```bash
make audit-pol-server-hardware
make start-pol-server-smart-long-kingston
make start-pol-server-smart-long-micron
```

Run the two long tests separately. Re-run the hardware audit after each test has
finished and compare the Kingston CRC counter with the recorded starting value.
The physical battery, charger, ventilation, and local-storage gates passed.
Wired Ethernet remains an optional reliability upgrade requiring external
equipment.

Acceptance gate:

- Required old data is independently backed up and sampled successfully.
- Both SSDs complete fresh long tests without media or uncorrectable errors.
- CRC/interface counters remain stable during testing.
- Battery, charger, and cooling are physically safe.
- The active private network is explicitly accepted for the current service
   rollout; wired networking remains the preferred final transport.

All hardware items needed for local storage preparation are complete. The
initial Samba rollout may proceed over Wi-Fi within the trusted LAN.

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

Status: **complete**.

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

Tracked commands:

```bash
make audit-pol-server-storage
make prepare-pol-server-storage
```

The second command is intentionally separate from the baseline and contains the
exact confirmation token accepted by the root-owned, model-pinned script.

Acceptance gate: the filesystem mounts by UUID after two reboots, a disposable
test file survives both reboots, ownership is correct, and SMART remains clean.

Completed on 2026-09-05 after explicit approval. The Kingston was resolved by
model and capacity, its legacy NTFS partition table was replaced with one ext4
filesystem labeled `nas-data`, and UUID
`ba435bde-4f44-44f4-9f74-a6c55c59ab86` was added to `/etc/fstab`. The hierarchy
uses `nasusers` only for `shared`, `private`, and `incoming`; `immich` and
`appdata` remain root-only. Acceptance file persistence and automatic mounting
passed across boot IDs `76ff9747-041a-4a1b-945b-f928bd9928c2` and
`bc736cea-2901-41c8-93a1-0d1c2315faa8`. The device letter changed across those
boots, confirming that no persistent configuration depends on `/dev/sdX`.

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

Status: **complete for the accepted Wi-Fi LAN**.

The tracked configuration is installed, `pol-files` is enrolled, `smbd` is
enabled and active, and `nmbd` is disabled and inactive. A reusable loopback
SMB3 acceptance test passed create/read/rename/delete on `shared`, `incoming`,
and `private`, then confirmed that guest access is denied. TCP 445 listens on
loopback and the current Wi-Fi addresses. UFW hardening and an additional test
from a separate LAN client remain Phase 4 follow-up work rather than Samba
configuration blockers.

Confirmed initial access model:

- Dedicated non-login identity: `pol-files`.
- `shared` and `incoming`: authenticated read/write for `nasusers`.
- `private`: authenticated read/write for `pol-files` only.
- `immich`, `appdata`, `/srv/storage`, and administrator homes: not shared.
- TCP 445 only on `lo` and `wlp2s0`; clients restricted to `192.168.1.0/24`.
- Guest access, SMB1, NetBIOS discovery, and router port forwarding: disabled.

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
The tracked audit and `make test-pol-server-samba-access` now enforce this gate.

## Phase 6: implement backup and prove restore

Status: **operational; retention and off-site policy remain pending**.

The selected destination is an existing external backup disk. Preserve all
current content, especially the Immich copy intended for later import. The plan
is to add a dedicated `pol-server-restic` directory using free space, without
repartitioning or reformatting. Before any write, record its model, capacity,
filesystem, mount state, free space, SMART availability, and a top-level
read-only inventory. That inventory completed on the Ubuntu workstation on
2026-09-05 and is recorded in
`reports/2026-09-05-external-backup-baseline.md`. Repeated long tests were
interrupted, while every observed critical counter remained clean. On
2026-09-05 the user explicitly accepted that residual risk and selected the
disk for unattended Restic use without formatting or repartitioning it.

The existing exFAT partition is mounted by UUID `5EEB-7DF6` at
`/mnt/pol-server-backup`; the encrypted repository is isolated under
`pol-server-restic/`. The first backup processed 755 MiB into snapshot
`1d2c4c00`. A restore to a separate temporary path matched the source probe,
the repository metadata and rotating data-subset check reported no errors, and
both hardened systemd services completed with status 0. Daily backup and weekly
check timers are enabled. Approximately 711 GiB remained free after acceptance.
When Immich is configured, the production backup service first stops its server
container, creates and verifies an atomic compressed PostgreSQL dump alongside
the media on Kingston, and only then starts Restic. Dump failure blocks the
snapshot, preserving the database/media consistency boundary.

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

Acceptance gate: the production backup service succeeds, failure is visible in
systemd, `restic check` passes, and a real restore works without access to the
original source. Those functional checks pass; observe the first timer-triggered
run before calling scheduling fully proven. Retention pruning and an off-site
copy remain separate follow-up gates.

## Phase 7: monitoring and maintenance

Status: **local monitoring complete; external alerting pending**.

The selected minimal local layer is native Netdata rather than a
Prometheus/Grafana stack. Netdata 2.11.0 is installed from its official stable
Trixie repository. Its tracked configuration binds only to loopback and
`192.168.1.34`, keeps one high-resolution tier for at most three months and
roughly 2 GiB, disables anonymous telemetry, and exposes no Docker socket. Live
acceptance on 2026-09-06 verified the LAN API, exact listener ownership,
effective access and retention settings, package signing fingerprint, Immich
health, and the full NAS baseline. The local dashboard is intentionally
separate from the alert transport: Healthchecks.io will provide the off-box
dead-man signal and email delivery after its account URLs are enrolled outside
Git.

Tracked commands:

```bash
make install-pol-server-monitoring
make audit-pol-server-monitoring
```

1. Configure `smartd` and scheduled monthly short and quarterly long tests.
2. Alert on backup failure, SMART failure, failed units, and 80% disk usage.
3. Add a tracked health-check command covering mounts, usage, SMART, services,
   backup freshness, updates, temperatures, and swap activity.
4. Schedule monthly restore sampling and quarterly ventilation/battery review.
5. Measure idle and busy wall power rather than relying on the planning estimate.

Acceptance gate: intentionally triggered test failures reach the chosen alert
channel and the maintenance checklist can be executed from the tracked runbook.

## Phase 8: add private remote access

Status: **tracked implementation complete; live deployment and acceptance in
progress**.

Read-only inventory found a dynamic Telefonica public IPv4 address, no usable
public IPv6, and no strong CGNAT evidence. The Movistar Askey router does not
provide the selected VPN implementation, so Debian owns plain WireGuard. The
confirmed server and first-peer contract is:

- `wg0` at `10.77.0.1/24`, listening on UDP `51820`.
- Fixed peer `pol-iphone` at `10.77.0.2/32`.
- Split route `192.168.1.34/32`; no default route through the NAS.
- Pre-NAT nftables access only to Immich TCP `2283` and Netdata TCP `19999`.
- SSH, SMB, every other NAS port, and forwarded Internet traffic denied.
- Endpoint `pol-home-nas.duckdns.org:51820`; router-managed DuckDNS when
  explicitly supported, otherwise the approved root-only five-minute timer.
- Client private key and profile generated only on the managed workstation,
  stored mode `0600`, shown as a QR only in an interactive local terminal, and
  backed up to the password manager.
- One redoable `make bootstrap-pol-server-wireguard` entrypoint owns local tool
   installation, baseline audits, bounded maintenance, deployment, external
   setup prompts, mobile acceptance, revocation proof, final audits, and cleanup.
   Its exit trap revokes maintenance, and reruns reuse the existing phone key.

Remaining live steps are:

1. Reserve NAS MAC `dc:f5:05:65:af:e1` as `192.168.1.34` in the router.
2. Reserve `pol-home-nas` in DuckDNS and choose router DDNS or the NAS fallback.
3. Deploy the root-owned bundle, install WireGuard, and enroll `pol-iphone`.
4. Forward only UDP `51820` to `192.168.1.34`; do not forward application or
   administration ports.
5. Configure iOS on demand for cellular and away Wi-Fi while excluding
   `MOVISTAR_PLUS_9460`.
6. From mobile data, prove Immich and Netdata work while SSH and SMB fail.
7. Revoke the peer and prove access stops, then generate a replacement profile.
8. Confirm an external scan exposes no application ports and close temporary
   maintenance access.

Acceptance gate: an approved mobile peer reaches only allowed services, a
revoked peer fails, and an external scan finds no application ports exposed.

## Phase 9: deploy Immich

Status: **deployed; preserved-library restore acceptance in progress**.

Docker Engine and its Compose plugin are installed from Docker's official
Debian repository. The tracked four-service project is pinned to Immich
`v3.1.0`; its root-only environment is stored outside Git. Media is on Kingston
at `/srv/storage/immich`, while PostgreSQL is on Micron at
`/var/lib/immich/postgres`. Only `192.168.1.34:2283` is published, the media tree
is excluded from Samba, and `pol` has no Docker-group access.

The preserved WD copy is a fixed read-only recovery source, not primary
storage. Its restore validates Immich markers and the newest database dump,
requires a dry run and an exact apply token, renames both fresh SSD targets to
timestamped rollback paths, and restores those paths on any failed copy, import,
or startup. The WD partition and approximately 500 GB unallocated tail remain
unchanged so backup and primary data stay in separate physical failure domains.

Remaining acceptance steps are:

1. Complete the active media copy and database import without interrupting it.
2. Prove all four containers healthy and the exact LAN-only listener.
3. Verify login, library counts, representative assets, and upload/retrieval.
4. Create a fresh paired PostgreSQL/media Restic snapshot and pass repository
   and isolated-restore checks.
5. Reboot and prove service, storage, Samba, backup, and Immich convergence.
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

Complete and prove the active Immich recovery before adding another application:

1. Finish Immich application, paired-backup, restore, and reboot acceptance.
2. Confirm the first timer-triggered backup and weekly check freshness.
3. Review the proposed 7 daily, 5 weekly, 12 monthly, and 3 yearly retention
   policy with a Restic dry run before authorizing any prune.
4. Preserve the Restic password and recovery procedure outside the NAS, then
   choose an off-site copy for irreplaceable data.
5. Reserve the current address in the router and configure tested LAN-only UFW
   rules for SSH, Samba, and Immich; Ethernet remains an optional reliability
   upgrade.
6. Add backup, SMART, failed-unit, temperature, and 80% storage alerts.
