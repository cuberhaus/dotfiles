# Ubuntu GRUB Resolution and Performance

## Observation

Ubuntu 24.04 performed poorly on this machine with a very high-DPI GRUB
resolution. On Ubuntu 26.04, GRUB used a much lower resolution and the
machine ran fast. The lower GRUB resolution was the meaningful difference.

This is a machine-specific observation, but it is a useful first
troubleshooting step when poor performance appears after an Ubuntu upgrade:
check whether GRUB is using an unnecessarily high resolution. This refers to
the GRUB boot-menu resolution, not the desktop display resolution.
