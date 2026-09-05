# External backup disk baseline: 2026-09-05

Source commands:

```bash
lsblk -b -o NAME,PATH,MODEL,SIZE,TYPE,FSTYPE,LABEL,UUID,FSAVAIL,FSUSE%,MOUNTPOINTS,TRAN /dev/sda
df -hT /media/pol/Pol-HDD
sudo smartctl -d sat -H -A -l selftest /dev/sda
make audit-pol-server-wd-backup
make start-pol-server-smart-long-wd-backup
```

Serial numbers and other unique hardware identifiers are intentionally omitted.
The disk was inspected on the Ubuntu workstation, not on `pol-server`.

## Inventory

- Device: WD Elements 25A2 containing a WDC WD15SMRW-11YNDS0 1.5 TB HDD.
- Partition table: GPT.
- Existing EFI partition: approximately 200 MiB, FAT.
- Existing data partition: approximately 1 TB, exFAT, label `Pol-HDD`.
- Data usage: approximately 221 GiB used and 712 GiB available (24% used).
- Unpartitioned tail: approximately 500 GB decimal (465 GiB).
- Inspection mount: `/media/pol/Pol-HDD`, explicitly remounted read-only.
- Existing content includes PARA folders, FreeFileSync version history, and an
  Immich tree with media directories and database dumps through 2026-09-02.

The current content, especially the Immich media and matching database dumps,
must be preserved. Do not reformat, repartition, or reuse the existing data
partition for another filesystem.

## SMART snapshot

- Overall health: `PASSED`.
- Power-on hours: `6,895`.
- Start/stop count: `32,436`.
- Load-cycle count: `59,797`.
- Reallocated sectors: `0`.
- Reallocation events: `0`.
- Current pending sectors: `0`.
- Offline uncorrectable sectors: `0`.
- UDMA CRC errors: `0`.
- Spin retries: `0`.
- Observed temperature: `54 C`.
- Initial self-test history: no self-tests logged.

The first extended test was started on the workstation and deliberately aborted
at 90% remaining before disconnecting the disk. After connection to
`pol-server`, the model-and-capacity-pinned audit resolved it as `/dev/sdc`,
confirmed that neither partition was mounted, and reported `52 C`. The first
server-side test was interrupted by a host or USB bridge reset with 80%
remaining. No matching kernel USB disconnect, I/O error, or server reboot was
recorded after the disk attached, and the critical SMART counters remained at
zero. A third extended test was also interrupted after reaching 60% remaining.
Every observed critical media and interface counter remained at zero. The user
chose not to run further extended tests; preserve the disk in its current layout
and record this incomplete qualification rather than treating it as a pass.

## Result

The read-only inventory gate passes and the snapshot contains no reported media
or interface errors. The disk is not qualified by a completed extended test and
must not be reformatted or repartitioned. Its use as an unattended restic target
requires a separate risk decision or replacement with a qualified destination.
