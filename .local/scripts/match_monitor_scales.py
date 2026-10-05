#!/usr/bin/env python3
"""match-monitor-scales: give every connected monitor the same display scale on GNOME.

Why: a fractional-scale monitor (the laptop at 133%) next to a 100% one makes Chromium-based apps
(Cursor, VS Code, Chrome, Obsidian) jitter in fullscreen on the other monitor; text, buttons and
the pointer shape flicker. Equal scales stop it. docs/FULLSCREEN-WIGGLE-DIAGNOSIS.md has the evidence.

How: the layout is read from mutter (GNOME's compositor) with `busctl`, the positions are
recomputed so the monitors keep sitting next to each other the way they do now, mutter checks the
result (`gdctl set --verify`), and only then is it applied (`gdctl set`). Modes, rotation, the
primary monitor, color mode and RGB range are carried over. Standard library only.
"""

from __future__ import annotations

import argparse
import json
import math
import os
import re
import shlex
import shutil
import subprocess
import sys
import time
import xml.etree.ElementTree as ET
from collections.abc import Callable
from dataclasses import dataclass
from itertools import combinations
from pathlib import Path
from typing import NamedTuple, NoReturn

PROG = "match-monitor-scales"

# The same text as the `-h` of the wrapper in .local/scripts/bin (the wrapper answers it before it
# needs Python); tests/test_match_monitor_scales.py fails when the two differ.
USAGE = """\
Usage: match-monitor-scales [--scale SCALE] [--persistent] [--dry-run]

Give every connected monitor the same display scale on GNOME. A fractional-scale
monitor next to a 100% one makes Chromium-based apps (Cursor, VS Code, Chrome,
Obsidian) jitter in fullscreen on the other monitor; equal scales stop it.

It reads the layout from mutter, recomputes the positions so the monitors keep
sitting next to each other as they do now, asks mutter to check the result, and
only then applies it. Modes, rotation, the primary monitor, color mode and RGB
range are carried over. It prints the gdctl command that goes back.

Options:
  -s, --scale SCALE  Scale for every monitor, as 1.25 or 125%
                     (default: the primary monitor's scale)
  -P, --persistent   Also save the layout in ~/.config/monitors.xml; the old
                     file is copied to monitors.xml.bak-DATE first
  -n, --dry-run      Only ask mutter whether it accepts the layout; change nothing
  -h, --help         Show this help

Without --persistent the change is temporary: GNOME forgets it when the monitors
change or you log out. MATCH_MONITOR_SCALES_WAIT sets how many seconds to wait
for mutter to confirm (default 8).
Needs gdctl (part of mutter) and busctl in a GNOME session.
Exit status: 0 done or nothing to do, 1 refused or failed, 2 usage or setup.
"""

# mutter's display configuration API on the session bus.
BUS_NAME = "org.gnome.Mutter.DisplayConfig"
OBJECT_PATH = "/org/gnome/Mutter/DisplayConfig"

LOGICAL, PHYSICAL = 1, 2
LAYOUT_MODES = {LOGICAL: "logical", PHYSICAL: "physical"}
# Rotations only. The numbers of the flipped transforms differ between gdctl and mutter's own
# enumeration, and nobody mirrors a monitor, so a flipped monitor is left to GNOME Settings.
TRANSFORMS = {0: "normal", 1: "90", 2: "180", 3: "270"}
PORTRAIT = frozenset({1, 3})
COLOR_MODES = {0: "default", 1: "bt2100", 2: "sdr-native"}
RGB_RANGES = {1: "auto", 2: "full", 3: "limited"}
DEFAULT_COLOR_MODE, DEFAULT_RGB_RANGE = 0, 1

SCALE_EPSILON = 1e-3  # two supported scales are never closer than 0.05
REQUEST_TOLERANCE = 0.02  # how far a typed scale may be from a supported one (133% is 1.3333)
EDGE_TOLERANCE = 1  # logical pixels: sizes such as 2560 / 1.3333 are rounded
DEFAULT_WAIT = 8.0
POLL_INTERVAL = 0.25

Rect = tuple[int, int, int, int]  # x, y, width, height in layout pixels


class Unavailable(Exception):
    """This machine cannot run the tool (no gdctl, no GNOME session); the exit status is 2."""


class Refused(Exception):
    """The layout cannot be read or changed safely, or mutter said no; the exit status is 1."""


# ---------------------------------------------------------------------------------------------
# What mutter reports
# ---------------------------------------------------------------------------------------------


@dataclass(frozen=True)
class Mode:
    id: str  # what `gdctl set --mode` takes, such as 2560x1440@119.998
    width: int
    height: int
    scales: tuple[float, ...]  # the scales mutter offers for this mode
    current: bool


@dataclass(frozen=True)
class Monitor:
    connector: str
    modes: tuple[Mode, ...]
    color_mode: int
    rgb_range: int
    for_lease: bool

    @property
    def mode(self) -> Mode:
        for mode in self.modes:
            if mode.current:
                return mode
        raise Refused(f"{self.connector} reports no current mode")


@dataclass(frozen=True)
class LogicalMonitor:
    """What GNOME calls a screen: one monitor, or several mirroring each other."""

    x: int
    y: int
    scale: float
    transform: int
    primary: bool
    monitors: tuple[Monitor, ...]

    @property
    def label(self) -> str:
        return "+".join(monitor.connector for monitor in self.monitors)

    def size(self, layout_mode: int | None, scale: float | None = None) -> tuple[int, int]:
        """Width and height in layout pixels at `scale` (default: the scale it has now)."""
        mode = self.monitors[0].mode
        width, height = (mode.height, mode.width) if self.transform in PORTRAIT else (mode.width, mode.height)
        if layout_mode == PHYSICAL:
            return width, height
        factor = self.scale if scale is None else scale
        return half_up(width / factor), half_up(height / factor)


@dataclass(frozen=True)
class DisplayState:
    layout_mode: int | None
    logical: tuple[LogicalMonitor, ...]
    monitors: tuple[Monitor, ...]


class Placement(NamedTuple):
    x: int
    y: int
    scale: float


def half_up(value: float) -> int:
    return math.floor(value + 0.5)


def percent(scale: float) -> str:
    return f"{half_up(scale * 100)}%"


def unwrap(value: object) -> object:
    """busctl --json wraps every value of an a{sv} as {"type": "b", "data": true}."""
    return value["data"] if isinstance(value, dict) and "data" in value else value


def parse_state(reply: object) -> DisplayState:
    """Turn the JSON of `GetCurrentState` into a DisplayState."""
    try:
        _serial, raw_monitors, raw_logical, raw_props = reply["data"]
        props = {key: unwrap(value) for key, value in raw_props.items()}
        monitors: dict[str, Monitor] = {}
        for spec, raw_modes, raw_monitor_props in raw_monitors:
            monitor_props = {key: unwrap(value) for key, value in raw_monitor_props.items()}
            modes = []
            for mode_id, width, height, _refresh, _preferred, scales, raw_flags in raw_modes:
                flags = {key: unwrap(value) for key, value in raw_flags.items()}
                modes.append(Mode(mode_id, int(width), int(height), tuple(float(s) for s in scales),
                                  bool(flags.get("is-current", False))))
            monitors[spec[0]] = Monitor(spec[0], tuple(modes),
                                        int(monitor_props.get("color-mode", DEFAULT_COLOR_MODE)),
                                        int(monitor_props.get("rgb-range", DEFAULT_RGB_RANGE)),
                                        bool(monitor_props.get("is-for-lease", False)))
        logical = tuple(
            LogicalMonitor(int(x), int(y), float(scale), int(transform), bool(primary),
                           tuple(monitors[spec[0]] for spec in specs))
            for x, y, scale, transform, primary, specs, _props in raw_logical
        )
        layout = props.get("layout-mode")
        return DisplayState(int(layout) if layout is not None else None, logical, tuple(monitors.values()))
    except (KeyError, IndexError, TypeError, ValueError, AttributeError) as error:
        raise Refused(f"cannot read mutter's reply ({error!r}); is GNOME newer than this tool?") from error


def run(command: list[str], timeout: float) -> subprocess.CompletedProcess[str]:
    return subprocess.run(command, capture_output=True, text=True, errors="replace", timeout=timeout, check=False)


def read_state() -> DisplayState:
    request = ["busctl", "--user", "--json=short", "call", BUS_NAME, OBJECT_PATH, BUS_NAME, "GetCurrentState"]
    try:
        done = run(request, 10)
    except FileNotFoundError as error:
        raise Unavailable("busctl (systemd) is not installed") from error
    except subprocess.TimeoutExpired as error:
        raise Unavailable("mutter did not answer within 10 seconds") from error
    if done.returncode != 0:
        reason = done.stderr.strip() or f"busctl exited with status {done.returncode}"
        raise Unavailable(f"cannot reach mutter's display configuration on the session bus: {reason}\n"
                          "This needs a graphical GNOME session.")
    try:
        reply = json.loads(done.stdout)
    except json.JSONDecodeError as error:
        raise Refused(f"cannot read the reply of busctl: {error}") from error
    return parse_state(reply)


def check_supported(state: DisplayState) -> None:
    """Refuse a layout the tool could not carry over unchanged."""
    for monitor in state.monitors:
        if monitor.for_lease:
            raise Refused(f"{monitor.connector} is leased to another program; leaving the layout alone")
    for screen in state.logical:
        if screen.transform not in TRANSFORMS:
            raise Refused(f"{screen.label} is flipped; change its scale in GNOME Settings instead")
        for monitor in screen.monitors:
            monitor.mode  # noqa: B018 - raises when mutter reports no current mode
            if monitor.color_mode not in COLOR_MODES or monitor.rgb_range not in RGB_RANGES:
                raise Refused(f"{monitor.connector} uses a color setting this tool does not know")


# ---------------------------------------------------------------------------------------------
# Which scale
# ---------------------------------------------------------------------------------------------


def parse_scale(text: str) -> float:
    """A scale typed as 1.25 or 125%."""
    match = re.fullmatch(r"\s*(\d+(?:\.\d+)?)\s*(%?)\s*", text)
    value = float(match[1]) / (100 if match[2] else 1) if match else 0.0
    if value <= 0:
        raise argparse.ArgumentTypeError(f"{text!r} is not a scale; write 1.25 or 125%")
    return value


def common_scales(state: DisplayState) -> list[float]:
    """The scales every active monitor offers for the mode it is in, smallest first."""
    offered = [monitor.mode.scales for screen in state.logical for monitor in screen.monitors]
    first, *others = offered
    return [scale for scale in sorted(first)
            if all(any(abs(scale - other) < SCALE_EPSILON for other in rest) for rest in others)]


def choose_scale(state: DisplayState, requested: float | None) -> float:
    """The scale to give everything: the one asked for, else the primary monitor's."""
    primary = next((screen for screen in state.logical if screen.primary), state.logical[0])
    wanted = primary.scale if requested is None else requested
    options = common_scales(state)
    tolerance = SCALE_EPSILON if requested is None else REQUEST_TOLERANCE
    near = [scale for scale in options if abs(scale - wanted) <= tolerance]
    if not near:
        if requested is None:
            source = f"the primary monitor's scale ({primary.label} at {percent(wanted)})"
        else:
            source = f"a scale of {percent(wanted)}"
        listed = ", ".join(f"{percent(scale)} ({scale:.4g})" for scale in options) or "none"
        raise Refused(f"{source} is not offered by every monitor. Scales that all of them offer: {listed}.\n"
                      f"Pick one with --scale.")
    return min(near, key=lambda scale: abs(scale - wanted))


# ---------------------------------------------------------------------------------------------
# Where everything goes
# ---------------------------------------------------------------------------------------------


def overlap(start_a: int, end_a: int, start_b: int, end_b: int) -> int:
    return min(end_a, end_b) - max(start_a, start_b)


def side_of(a: Rect, b: Rect) -> str | None:
    """The side of `a` that `b` touches ("right", "left", "below", "above"), or None."""
    ax, ay, aw, ah = a
    bx, by, bw, bh = b
    shares_rows = overlap(ay, ay + ah, by, by + bh) > 0
    shares_columns = overlap(ax, ax + aw, bx, bx + bw) > 0
    if shares_rows and abs(ax + aw - bx) <= EDGE_TOLERANCE:
        return "right"
    if shares_rows and abs(bx + bw - ax) <= EDGE_TOLERANCE:
        return "left"
    if shares_columns and abs(ay + ah - by) <= EDGE_TOLERANCE:
        return "below"
    if shares_columns and abs(by + bh - ay) <= EDGE_TOLERANCE:
        return "above"
    return None


def cross_offset(a_start: int, a_len: int, b_start: int, b_len: int, new_a_len: int, new_b_len: int) -> int:
    """Where B starts, counted from where A starts, once both have been resized.

    B keeps its place along the shared edge: flush at the start, flush at the end, centered, or
    else the same fraction of the way along A.
    """
    if abs(b_start - a_start) <= EDGE_TOLERANCE:
        return 0
    if abs((b_start + b_len) - (a_start + a_len)) <= EDGE_TOLERANCE:
        return new_a_len - new_b_len
    if abs((2 * b_start + b_len) - (2 * a_start + a_len)) <= 2 * EDGE_TOLERANCE:
        return half_up((new_a_len - new_b_len) / 2)
    return half_up((b_start - a_start) * new_a_len / a_len)


def place_beside(side: str, old_a: Rect, old_b: Rect, new_a: Rect, size_b: tuple[int, int]) -> Rect:
    """The new rectangle of B, on the same side of the resized A it was on before."""
    ax, ay, aw, ah = old_a
    bx, by, bw, bh = old_b
    nx, ny, nw, nh = new_a
    width, height = size_b
    if side in ("right", "left"):
        y = ny + cross_offset(ay, ah, by, bh, nh, height)
        return (nx + nw if side == "right" else nx - width, y, width, height)
    x = nx + cross_offset(ax, aw, bx, bw, nw, width)
    return (x, ny + nh if side == "below" else ny - height, width, height)


def plan_layout(state: DisplayState, scale: float) -> list[Placement]:
    """Positions that give every screen `scale` while keeping how they sit next to each other.

    The primary screen stays where it is. Each screen reachable from it is placed on the same
    side of its neighbour as before, flush or centered the way it was, and the result is shifted
    so the top left corner is 0,0, which mutter requires.
    """
    screens = state.logical
    if state.layout_mode == PHYSICAL:  # sizes do not depend on the scale, so nothing moves
        return [Placement(screen.x, screen.y, scale) for screen in screens]
    old: list[Rect] = [(screen.x, screen.y, *screen.size(state.layout_mode)) for screen in screens]
    sizes = [screen.size(state.layout_mode, scale) for screen in screens]
    anchor = next((i for i, screen in enumerate(screens) if screen.primary), 0)
    placed: dict[int, Rect] = {anchor: (old[anchor][0], old[anchor][1], *sizes[anchor])}
    queue = [anchor]
    while queue:
        a = queue.pop(0)
        for b in range(len(screens)):
            side = None if b in placed else side_of(old[a], old[b])
            if side is not None:
                placed[b] = place_beside(side, old[a], old[b], placed[a], sizes[b])
                queue.append(b)
    stray = [screens[i].label for i in range(len(screens)) if i not in placed]
    if stray:
        raise Refused(f"{', '.join(stray)} touches no other monitor in the current layout; "
                      "not guessing where it goes")
    rects = [placed[i] for i in range(len(screens))]
    for i, j in combinations(range(len(rects)), 2):
        (ax, ay, aw, ah), (bx, by, bw, bh) = rects[i], rects[j]
        if overlap(ax, ax + aw, bx, bx + bw) > 0 and overlap(ay, ay + ah, by, by + bh) > 0:
            raise Refused(f"at {percent(scale)} {screens[i].label} and {screens[j].label} would overlap; "
                          "not guessing a different arrangement")
    left = min(rect[0] for rect in rects)
    top = min(rect[1] for rect in rects)
    return [Placement(x - left, y - top, scale) for x, y, _width, _height in rects]


def current_layout(state: DisplayState) -> list[Placement]:
    return [Placement(screen.x, screen.y, screen.scale) for screen in state.logical]


def gdctl_command(state: DisplayState, layout: list[Placement], *, persistent: bool = False,
                  verify: bool = False) -> list[str]:
    """The `gdctl set` that puts `layout` on the screens of `state`.

    There is no --layout-mode: without it gdctl keeps the current one, and with it gdctl refuses
    on a mutter that cannot change the mode, even to the mode it already has.
    """
    command = ["gdctl", "set"]
    if persistent:
        command.append("--persistent")
    if verify:
        command.append("--verify")
    for screen, place in zip(state.logical, layout):
        command.append("--logical-monitor")
        for monitor in screen.monitors:
            command += ["--monitor", monitor.connector, "--mode", monitor.mode.id]
            if monitor.color_mode != DEFAULT_COLOR_MODE:
                command += ["--color-mode", COLOR_MODES[monitor.color_mode]]
            if monitor.rgb_range != DEFAULT_RGB_RANGE:
                command += ["--rgb-range", RGB_RANGES[monitor.rgb_range]]
        command += ["--scale", repr(place.scale), "--x", str(place.x), "--y", str(place.y)]
        if screen.primary:
            command.append("--primary")
        if screen.transform:
            command += ["--transform", TRANSFORMS[screen.transform]]
    return command


# ---------------------------------------------------------------------------------------------
# Saving and checking
# ---------------------------------------------------------------------------------------------


def monitors_xml_path() -> Path:
    config = os.environ.get("XDG_CONFIG_HOME")
    return Path(config) / "monitors.xml" if config else Path.home() / ".config" / "monitors.xml"


def backup_file(path: Path, stamp: str | None = None) -> Path | None:
    """Copy `path` to NAME.bak-YYYYMMDD-HHMMSS without ever replacing a file; None if it is absent."""
    if not path.is_file():
        return None
    stamp = stamp or time.strftime("%Y%m%d-%H%M%S")
    for attempt in range(100):
        copy = path.with_name(f"{path.name}.bak-{stamp}" + (f"-{attempt}" if attempt else ""))
        try:
            with path.open("rb") as source, copy.open("xb") as target:
                shutil.copyfileobj(source, target)
        except FileExistsError:
            continue
        except OSError:
            copy.unlink(missing_ok=True)
            raise
        shutil.copystat(path, copy)
        if copy.read_bytes() != path.read_bytes():
            copy.unlink(missing_ok=True)
            raise OSError(f"the copy {copy} differs from {path}")
        return copy
    raise OSError(f"no free backup name for {path}")


def saved_layout_matches(path: Path, connectors: frozenset[str], scale: float) -> bool:
    """Whether monitors.xml has a configuration for exactly these connectors, all at `scale`.

    mutter writes the file about two seconds after it applies a persistent layout, so a caller
    polls this instead of reading the file once.
    """
    try:
        root = ET.parse(path).getroot()
    except (OSError, ET.ParseError):
        return False
    for configuration in root.iter("configuration"):
        names: set[str] = set()
        scales: list[float] = []
        for logical in configuration.iter("logicalmonitor"):
            try:
                scales.append(float(logical.findtext("scale") or ""))
            except ValueError:
                scales.append(math.nan)
            names.update(node.text for node in logical.iter("connector") if node.text)
        if names == connectors and scales and all(abs(s - scale) < SCALE_EPSILON for s in scales):
            return True
    return False


def wait_until(check: Callable[[], bool], seconds: float) -> bool:
    deadline = time.monotonic() + seconds
    while True:
        if check():
            return True
        if time.monotonic() >= deadline:
            return False
        time.sleep(POLL_INTERVAL)


def wait_seconds() -> float:
    try:
        return max(0.0, float(os.environ.get("MATCH_MONITOR_SCALES_WAIT", DEFAULT_WAIT)))
    except ValueError:
        return DEFAULT_WAIT


# ---------------------------------------------------------------------------------------------
# Talking to the user
# ---------------------------------------------------------------------------------------------


class Style:
    def __init__(self, enabled: bool) -> None:
        self.enabled = enabled

    @classmethod
    def for_stream(cls, stream: object) -> Style:
        isatty = getattr(stream, "isatty", lambda: False)()
        return cls(bool(isatty) and not os.environ.get("NO_COLOR") and os.environ.get("TERM") != "dumb")

    def _wrap(self, code: str, text: str) -> str:
        return f"\033[{code}m{text}\033[0m" if self.enabled else text

    def ok(self, text: str) -> str:
        return self._wrap("32", text)

    def warn(self, text: str) -> str:
        return self._wrap("33", text)

    def info(self, text: str) -> str:
        return self._wrap("34", text)


def tilde(path: Path) -> str:
    try:
        return "~/" + path.relative_to(Path.home()).as_posix()
    except ValueError:
        return str(path)


def print_layout(state: DisplayState, style: Style) -> None:
    label_width = max(len(screen.label) for screen in state.logical)
    print(style.info(f"Current layout ({LAYOUT_MODES.get(state.layout_mode or LOGICAL, 'logical')}):"))
    for screen in state.logical:
        modes = "+".join(monitor.mode.id for monitor in screen.monitors)
        print(f"  {screen.label:<{label_width}}  {modes}  {percent(screen.scale):>4}  at {screen.x},{screen.y}"
              + ("  primary" if screen.primary else ""))


def print_plan(state: DisplayState, layout: list[Placement], scale: float, style: Style, *,
               from_primary: bool) -> None:
    source = " (the primary monitor's scale)" if from_primary else ""
    print(style.info(f"Giving all {len(state.logical)} monitors {percent(scale)}{source}:"))
    label_width = max(len(screen.label) for screen in state.logical)
    for screen, place in zip(state.logical, layout):
        print(f"  {screen.label:<{label_width}}  {percent(screen.scale):>4} -> {percent(place.scale):<4}"
              f"  at {screen.x},{screen.y} -> {place.x},{place.y}")


def output_of(done: subprocess.CompletedProcess[str]) -> str:
    return (done.stderr or done.stdout or f"exit status {done.returncode}").strip()


def scales_are(scale: float) -> bool:
    try:
        return all(abs(screen.scale - scale) < SCALE_EPSILON for screen in read_state().logical)
    except (Unavailable, Refused):
        return False


def run_tool(args: argparse.Namespace, out: Style, err: Style) -> int:
    if shutil.which("gdctl") is None:
        raise Unavailable("gdctl (part of mutter) is not installed, so there is nothing to change the layout with")
    state = read_state()
    check_supported(state)
    if len(state.logical) < 2:
        print("Only one monitor is active; there is nothing to match.")
        return 0
    print_layout(state, out)

    scale = choose_scale(state, args.scale)
    if all(abs(screen.scale - scale) < SCALE_EPSILON for screen in state.logical) and not args.persistent:
        print(out.ok(f"All {len(state.logical)} monitors are already at {percent(scale)}. Nothing to change."))
        return 0

    layout = plan_layout(state, scale)
    print_plan(state, layout, scale, out, from_primary=args.scale is None)
    checked = run(gdctl_command(state, layout, verify=True), 60)
    if checked.returncode != 0:
        raise Refused("mutter does not accept this layout; nothing was changed.\n" + output_of(checked))
    print("mutter accepts the layout.")

    apply = gdctl_command(state, layout, persistent=args.persistent)
    if args.dry_run:
        print(out.ok("Dry run: nothing was changed."))
        print("It would run: " + shlex.join(apply))
        return 0

    config = monitors_xml_path()
    if args.persistent:
        try:
            backup = backup_file(config)
        except OSError as error:
            raise Refused(f"cannot back up {tilde(config)}, so nothing was changed: {error}") from error
        if backup:
            print(f"Backed up {tilde(config)} to {tilde(backup)}")
    applied = run(apply, 60)
    if applied.returncode != 0:
        raise Refused("gdctl failed: " + output_of(applied))

    undo = "To go back: " + shlex.join(gdctl_command(state, current_layout(state), persistent=args.persistent))
    wait = wait_seconds()
    if not wait_until(lambda: scales_are(scale), wait):
        print(err.warn(f"gdctl said yes, but mutter does not report {percent(scale)} on every monitor after "
                       f"{wait:g} s. Check GNOME Settings > Displays."), file=sys.stderr)
        print(undo)
        return 1
    print(out.ok(f"Done: all {len(state.logical)} monitors are at {percent(scale)}."))

    status = 0
    if args.persistent:
        connectors = frozenset(monitor.connector for screen in state.logical for monitor in screen.monitors)
        if wait_until(lambda: saved_layout_matches(config, connectors, scale), wait):
            print(out.ok(f"Saved in {tilde(config)}."))
        else:
            print(err.warn(f"The layout is applied, but {tilde(config)} does not show it after {wait:g} s, so "
                           "GNOME may forget it. Look at the file, or set it again in GNOME Settings."),
                  file=sys.stderr)
            status = 1
    else:
        print("This is temporary: GNOME forgets it when the monitors change or you log out. "
              "Add --persistent to keep it.")
    print(out.info(undo))
    return status


class Parser(argparse.ArgumentParser):
    def error(self, message: str) -> NoReturn:
        self.exit(2, f"{PROG}: {message}\nTry '{PROG} --help'.\n")


def build_parser() -> Parser:
    parser = Parser(prog=PROG, add_help=False, allow_abbrev=False)
    parser.add_argument("-s", "--scale", type=parse_scale)
    parser.add_argument("-P", "--persistent", action="store_true")
    parser.add_argument("-n", "--dry-run", action="store_true")
    return parser


def main(argv: list[str] | None = None) -> int:
    argv = sys.argv[1:] if argv is None else argv
    if any(arg in ("-h", "--help") for arg in argv):
        sys.stdout.write(USAGE)
        return 0
    args = build_parser().parse_args(argv)
    out, err = Style.for_stream(sys.stdout), Style.for_stream(sys.stderr)
    try:
        return run_tool(args, out, err)
    except Unavailable as error:
        print(f"{PROG}: {error}", file=sys.stderr)
        return 2
    except Refused as error:
        print(f"{PROG}: {error}", file=sys.stderr)
        return 1
    except KeyboardInterrupt:
        print(f"\n{PROG}: interrupted; the layout may be half applied, check GNOME Settings", file=sys.stderr)
        return 130


if __name__ == "__main__":
    raise SystemExit(main())
