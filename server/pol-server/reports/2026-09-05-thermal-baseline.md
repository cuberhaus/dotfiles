# Thermal baseline: 2026-09-05

Source command:

```bash
make test-pol-server-thermals
```

Test conditions:

- Debian CPU governor remained `powersave`.
- Two `stress-ng` CPU workers ran at 60% load for two minutes.
- Thermal telemetry was sampled every ten seconds.
- The AC adapter remained online.

Results:

- `stress-ng` completed successfully with two workers passed and zero failed.
- The observed CPU/package thermal readings peaked at 54 C.
- The system baseline audit passed after the test with no failed units.
- Both SSDs retained `PASSED` SMART health and successful extended self-tests.
- Kingston CRC counters remained unchanged at `524353` and `13`.
- Micron UDMA CRC, pending, reallocated, and uncorrectable counters remained zero.

This establishes a moderate-load baseline, not a guarantee for blocked vents or
high ambient temperatures. Keep the laptop ventilated and repeat the test after
material hardware, placement, or cooling changes.
