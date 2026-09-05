# External backup disk baseline: 2026-09-05

Source commands:

```bash
lsblk -b -o NAME,PATH,MODEL,SIZE,TYPE,FSTYPE,LABEL,UUID,FSAVAIL,FSUSE%,MOUNTPOINTS,TRAN /dev/sda
df -hT /media/pol/Pol-HDD
sudo smartctl -d sat -H -A -l selftest /dev/sda
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
- Self-test history: no self-tests logged.

## Result

The read-only inventory gate passes and the snapshot contains no reported media
or interface errors. The disk is not yet qualified for unattended backups: its
observed temperature is elevated and it has no logged self-test. Improve its
ventilation, allow it to cool, then run and verify one SMART long self-test
before creating `pol-server-restic` or relying on the disk as a backup target.
