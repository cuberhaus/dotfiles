# SMART baseline: 2026-09-05

Source command:

```bash
make audit-pol-server-hardware
```

Serial numbers and network addresses are intentionally omitted. Both extended
self-tests were started separately through model-pinned commands and completed
without error.

## Kingston SA400S37960G

- Role: candidate data SSD; 960,197,124,096 bytes; NTFS partition unmounted.
- SMART overall health: `PASSED`.
- Extended self-test: `Completed without error` at 999 power-on hours.
- Reallocated events: `0`.
- Reported uncorrectable errors: `0`.
- SATA PHY errors: `0`.
- SATA CRC errors: `524353`; unchanged before and after the long test.
- Secondary CRC errors: `13`; unchanged before and after the long test.
- SSD life-left normalized value: `98`.
- Observed temperature: 27-28 C; recorded lifetime maximum: 50 C.
- Unsafe shutdown count: `72`.

The CRC values are historical counters, not proof of a current media fault.
They remain a watch condition: do not accept the disk if either counter rises
during later transfer and load testing.

## Micron 1100 MTFDDAV256TBN

- Role: Debian system SSD; 256,060,514,304 bytes.
- SMART overall health: `PASSED`.
- Extended self-test: `Completed without error` at 2,707 power-on hours.
- Reallocated NAND blocks: `0`.
- Program and erase failures: `0`.
- Reported uncorrectable errors: `0`.
- Pending ECC errors: `0`.
- Offline uncorrectable errors: `0`.
- UDMA CRC errors: `0`.
- Lifetime indicator: normalized value `92`, raw value `8`.
- Observed temperature: 34 C; recorded lifetime maximum: 70 C.
- Unexpected power-loss count: `301`.

## Result

Both SSDs pass the 2026-09-05 media-health gate. This does not clear the other
Phase 0 gates: cooling needs a supervised load test and a stable wired network
path is still required. The user subsequently confirmed that no Kingston data
must survive and that battery, charger, and ventilation are physically safe.
