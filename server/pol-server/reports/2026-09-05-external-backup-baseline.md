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
confirmed that neither partition was mounted, and reported `52 C`. A new
232-minute extended test started successfully and is expected to finish at
2026-09-05 20:12:46 CEST. Keep the disk powered and connected until then.

## Result

The read-only inventory gate passes and the snapshot contains no reported media
or interface errors. The disk is not yet qualified for unattended backups: its
temperature remains elevated and the server-side long test is still in
progress. Verify a `Completed without error` result and unchanged critical
counters before creating `pol-server-restic` or relying on the disk as a backup
target.
