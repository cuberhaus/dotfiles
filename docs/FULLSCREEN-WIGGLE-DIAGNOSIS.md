# Fullscreen Wiggle on the External Monitor Diagnosis

## Summary

On the ASUS ROG Strix SCAR 16 (G635LX) running GNOME on Wayland, Chromium-based
apps shook in fullscreen on the external monitor: text and buttons jittered,
and the pointer flipped between the text cursor and the arrow. The user had
seen it on earlier days too.

The trigger is a fractional-scale monitor next to a 100% one: the laptop panel
at 133% beside the external monitor at 100%. With both monitors at the same
scale it does not happen. An A-B-A test confirmed it (see
[Confirmed failure path](#confirmed-failure-path)): the wiggle came, went and
came back with nothing changed but the scale.

The fix is to keep every monitor at one scale. It was saved on 2026-10-05 with
both monitors at 133%, and the command `match-monitor-scales` re-applies equal
scales after a dock reconnect or a change in GNOME Settings. Which side has
the bug, Chromium's Wayland backend or mutter, is not known.

## Environment

| Item | Value |
| --- | --- |
| Machine | ASUS ROG Strix SCAR 16 (G635LX), GPU MUX in dGPU mode |
| OS | Ubuntu 26.04.1, kernel 7.0.0-38 |
| Session | GNOME Shell 50.1 on Wayland (mutter 50.1) |
| GPU driver | NVIDIA open kernel module 595.91.07 |
| Built-in monitor | `eDP-1`, BOE NE160QDM-NZC, 2560x1600 at 240 Hz |
| External monitor | `DP-3`, SKG M27T6, 2560x1440, through the dock; its modes are 119.998 Hz and 59.951 Hz |
| Affected apps | Chromium-based: Cursor, VS Code, Chrome and Obsidian, all started as native Wayland clients (`--ozone-platform=wayland`) |

The kernel parameters `nvidia-drm.modeset=1 acpi=force pcie_port_pm=off
acpi_osi=Linux acpi_backlight=native` are deliberate and unrelated. Do not drop
`acpi_osi=Linux`: the shutdown fix depends on it.

## Confirmed failure path

Only the scale was changed between steps, with `gdctl` (see [Fix](#fix)). Each
result was judged by eye, with Chromium-based apps fullscreen on `DP-3`.

| Step | `eDP-1` scale | `DP-3` scale | Wiggle |
| --- | --- | --- | --- |
| 1 (original layout) | 133% | 100% | yes |
| 2 | 100% | 100% | no |
| 3 (back to step 1) | 133% | 100% | yes, again |
| 4 | 133% | 133% | no |

Steps 1 and 3 are the same layout and behaved the same, so the wiggle follows
the layout, not the time of day or what happened to be running. GNOME's own
shell and apps were not reported shaking; the reports are all about
Chromium-based windows.

## Hypotheses tested

Each test changed one thing and was reverted before the next.

| Hypothesis | Test | Result |
| --- | --- | --- |
| The 120 Hz mode of the external monitor | `DP-3` at 59.951 Hz, temporary | Same wiggle |
| GPU memory pressure from the local vLLM server | vLLM stopped, 21 GB of VRAM free | Same wiggle |
| The dock link or the cable | Not tested directly: the A-B-A changed only the scale and the wiggle still came and went | Not the cause |
| A one-pixel size renegotiation loop, as in a [GNOME Discourse report](#related-reports) | `journalctl -g 'exceeds allowed maximum size'` over the four boots since 2026-10-01; gnome-shell logged 1,277 lines in the test window | The message never appears; not shown to be the same mechanism |
| Mixed scales | The A-B-A table | Confirmed |

The GPU is a separate nuisance. While vLLM runs, VRAM sits near 96% and
Chromium logs bursts of `Cannot create bo ... Scanout` failures
(`nv_drm_gem_alloc_nvkms_memory_ioctl`). That did not cause the wiggle.
Likewise, after a hot-plug the dock logs `Cannot enable. Maybe the USB cable
is bad?` about once a second, and attaching the dock before boot avoids it. It
is not the trigger of the wiggle, which came and went with the scale alone.

## Fix

Give every monitor the same scale and save it. The layout that was saved on
2026-10-05, in `~/.config/monitors.xml` (GNOME's own file, per machine, not in
this repository):

| Monitor | Mode | Scale | Position |
| --- | --- | --- | --- |
| `DP-3` | 2560x1440 at 119.998 Hz | 133% | 0,0 |
| `eDP-1` (primary) | 2560x1600 at 240 Hz | 133% | 0,1080 |

`gdctl` is mutter's command-line tool for this. It applies a layout
temporarily by default, `--persistent` also writes `monitors.xml` (mutter does
that about two seconds later, so a file read straight after the command still
shows the old layout), and `--verify` only asks mutter whether it accepts the
layout.

```bash
gdctl set --persistent \
  --logical-monitor --monitor DP-3  --mode 2560x1440@119.998 --scale 1.3333333 --x 0 --y 0 \
  --logical-monitor --monitor eDP-1 --mode 2560x1600@240.000 --scale 1.3333333 --primary --x 0 --y 1080
```

The old file was copied to `~/.config/monitors.xml.bak-2026-10-05` first.

### `match-monitor-scales`

Changing a scale changes the logical size of a monitor (the external one goes
from 2560x1440 at 100% to 1920x1080 at 133%), so the positions have to be
worked out again. Doing that by hand for every dock reconnect is the reason for
the command.

```bash
match-monitor-scales --dry-run       # show the plan; mutter checks it; nothing is applied
match-monitor-scales                 # apply it until the monitors change or you log out
match-monitor-scales --persistent    # also save ~/.config/monitors.xml (old file copied first)
match-monitor-scales --scale 125%    # pick the scale instead of using the primary monitor's
```

How it decides:

- The target is the primary monitor's scale unless `--scale` says otherwise,
  and it must be a scale that every monitor offers for its current mode.
- The primary monitor is the anchor and the others are placed around it. Every
  other monitor keeps the side of its neighbour it is on and how it is
  aligned: flush at the start, flush at the end, centered, or the same
  fraction of the way along the shared edge. Rotated monitors swap width and
  height.
- The layout is shifted so its corner is 0,0, which mutter requires, and
  `gdctl set --verify` has the last word before anything is applied.
- Modes, rotation, the primary flag, color mode and RGB range are carried over.
  It refuses flipped or leased monitors, and a monitor that touches no other.
- It prints the `gdctl` command that goes back to the layout it started from.
- With `--persistent` it copies `monitors.xml` to `monitors.xml.bak-DATE-TIME`
  first (never over an existing copy), then waits for mutter to write the
  file and checks that the file really holds the new scales.

It is `.local/scripts/bin/match-monitor-scales` over
`.local/scripts/match_monitor_scales.py`; `tests/test_match_monitor_scales.py`
covers it, including the exact layout from this diagnosis.

### Limits

- Keep the scales equal. Changing one scale in GNOME Settings brings the
  wiggle back; run `match-monitor-scales` afterwards.
- GNOME matches a saved layout by connector name and monitor identity. The
  external monitor on another dock port (`DP-4` instead of `DP-3`) or another
  monitor has no saved layout, and GNOME may build a default one with the
  external monitor at 100%. Run `match-monitor-scales --persistent` once for
  that combination.
- At 133% the 27-inch external monitor has 1920x1080 logical pixels, less room
  than at 100%. That was the user's choice (both screens used equally).
- 100%/100% and 133%/133% were looked at. 125%/125% was only checked by mutter
  with `--verify`, not by eye.

## Verification

- The A-B-A table above, by eye, on the live session.
- After the fix: both monitors at 133%, no wiggle in the Chromium apps.
- `~/.config/monitors.xml` holds the new layout; mutter's own previous copy
  `monitors.xml~` and `monitors.xml.bak-2026-10-05` hold the old one.
- `tests/test_match_monitor_scales.py` runs the real wrapper against a fake
  `busctl` and `gdctl`. A throwaway copy of the code with ten deliberate bugs
  (ignored alignment, unswapped rotation, no `0,0` shift, no `--verify`, a
  `--dry-run` that applies, no backup, a single read of `monitors.xml`, a
  `--layout-mode` flag, a lost primary, a skipped re-save) failed the tests every
  time.
- Against the real mutter, `match-monitor-scales --dry-run` for 100%, 125% and
  200% was accepted by `gdctl set --verify`, and so was the exact command the
  tool builds for the original mixed layout (laptop at 190,1080, external at
  0,0, both 133%). A deliberately broken layout was refused with `Logical
  monitors not adjacent`. The display state was unchanged afterwards (the same
  fingerprint of `GetCurrentState` before and after).

## Rollback

The live way back is the layout from before the change (laptop 133% at
190,1440 as primary, external 100% at 0,0). It brings the wiggle back:

```bash
gdctl set --persistent \
  --logical-monitor --monitor eDP-1 --mode 2560x1600@240.000 --scale 1.3333333 --primary --x 190 --y 1440 \
  --logical-monitor --monitor DP-3  --mode 2560x1440@119.998 --scale 1 --x 0 --y 0
```

Restoring the file (`cp -p ~/.config/monitors.xml.bak-2026-10-05
~/.config/monitors.xml`) is the fallback; prefer `gdctl` for a change in the
running session. `match-monitor-scales` prints the exact command that undoes
each of its own runs.

## If it does not work

1. `gdctl show` lists the scale of each monitor. If they differ, run
   `match-monitor-scales` (or `--dry-run` first).
2. If the scales are equal and the window still shakes, it is not this cause.
   Note the refresh rate of each monitor, whether the app is a native Wayland
   client (`--ozone-platform=wayland` in its command line), and whether GNOME's
   own apps shake too.
3. With vLLM running, check VRAM with `nvidia-smi`. A full GPU produced
   Chromium errors in this setup, but it did not cause the wiggle.

## Related reports

Found on 2026-10-06 while preparing an upstream report. None matches this one
exactly and none is a confirmed duplicate:

- [GNOME Discourse, 2026-04-18](https://discourse.gnome.org/t/wayland-multi-monitor-bug-chromium-electron-windows-jitter-when-maximized-on-an-external-monitor-positioned-to-the-left/34752):
  GNOME Shell 50.1, internal display at scale 2 and external at scale 1.
  Chromium and Electron windows jitter when *maximized* on the external
  monitor, but only while it is left of the internal one. The author says true
  fullscreen is fine and sees `size 1921x1081 exceeds allowed maximum size
  1920x1080` in the log. Here the windows are fullscreen, the 100% monitor
  sits above the laptop, and that message never appears.
- [mutter #3080](https://gitlab.gnome.org/GNOME/mutter/-/work_items/3080):
  Electron apps are blurry on a fractional monitor beside a non-scaled one,
  depending on where the monitors sit relative to each other (GNOME 45). Same
  family, different symptom.
- [Fedora Discussion](https://discussion.fedoraproject.org/t/windows-maximize-past-edge-of-external-monitor/137602):
  A user reports that Chromium and Electron windows maximize wrongly on an
  external monitor at 100% beside a scaled built-in one, and that equal
  scales on both fix it.

## Not investigated

- Which component owns the bug. No upstream report was filed. A report needs a
  small reproduction: two monitors at different scales and one Chromium window
  fullscreen on the 100% one.
- Whether the Chromium apps stop shaking under XWayland or in a newer Chromium
  or Electron release while the scales stay mixed.
- Whether mixed scales that are both fractional (125% beside 133%) shake.
- Whether the position of the monitors matters. The 100% monitor was above the
  laptop in every test, and the Discourse report and mutter #3080 above both
  found that position changes the result.
- Whether Chromium's own switches change it. `--disable-features=WaylandPerSurfaceScale`
  and `--disable-features=WaylandFractionalScaleV1` helped other reports of
  scale glitches and were not tried here.
