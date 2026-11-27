#!/usr/bin/env python3
"""Keep GNOME and X11/XWayland in agreement about the Escape/Caps Lock swap."""

from __future__ import annotations

import ast
import json
import os
import re
import subprocess
import sys


SCHEMA = "org.gnome.desktop.input-sources"
OPTION = "caps:swapescape"
USAGE = """\
Usage: keyboard-swap toggle|on|off|status

Toggle, enable, disable, or report the persistent Escape/Caps Lock swap in a
graphical GNOME session. Other keyboard options are preserved. Run toggle again
to switch back.
"""


class KeyboardSwapError(Exception):
    pass


def gnome_session() -> bool:
    return any(
        "gnome" in os.environ.get(name, "").lower()
        for name in ("XDG_CURRENT_DESKTOP", "XDG_SESSION_DESKTOP", "DESKTOP_SESSION")
    )


def run(command: list[str]) -> str:
    try:
        result = subprocess.run(command, capture_output=True, text=True, timeout=5, check=False)
    except (OSError, subprocess.TimeoutExpired) as error:
        raise KeyboardSwapError(f"could not run {command[0]}: {error}") from error
    if result.returncode:
        detail = result.stderr.strip()
        raise KeyboardSwapError(f"{command[0]} failed{': ' + detail if detail else ''}")
    return result.stdout


def gnome_options() -> list[str]:
    value = run(["gsettings", "get", SCHEMA, "xkb-options"]).strip()
    if value.startswith("@as "):
        value = value[4:]
    try:
        options = ast.literal_eval(value)
    except (SyntaxError, ValueError) as error:
        raise KeyboardSwapError("GNOME returned an invalid xkb-options value") from error
    if not isinstance(options, list) or not all(isinstance(option, str) for option in options):
        raise KeyboardSwapError("GNOME returned an invalid xkb-options value")
    return options


def x11_state() -> tuple[str, list[str]] | None:
    if not os.environ.get("DISPLAY"):
        return None
    values = {}
    for line in run(["setxkbmap", "-query"]).splitlines():
        match = re.match(r"^\s*(layout|options):\s*(.*?)\s*$", line)
        if match:
            values[match.group(1)] = match.group(2)
    if not values.get("layout"):
        raise KeyboardSwapError("could not read the X11 keyboard layout")
    options = [option.strip() for option in values.get("options", "").split(",") if option.strip()]
    return values["layout"], options


def sync_x11(
    enabled: bool, state: tuple[str, list[str]] | None, preserve: tuple[str, ...] | list[str] = ()
) -> None:
    if state is None:
        return
    layout, options = state
    wanted = list(options) if enabled else [option for option in options if option != OPTION]
    for option in preserve:
        if option != OPTION and option not in wanted:
            wanted.append(option)
    if enabled and OPTION not in wanted:
        wanted.append(OPTION)
    if layout == "es" and wanted == options:
        return
    command = ["setxkbmap", "-layout", "es", "-option", ""]
    if wanted:
        command.extend(["-option", ",".join(wanted)])
    run(command)
    updated = x11_state()
    if updated is None or updated[0] != "es" or set(updated[1]) != set(wanted):
        raise KeyboardSwapError("X11 did not retain the requested keyboard options")


def set_swap(action: str) -> bool:
    options = gnome_options()
    x11 = x11_state()
    enabled = action == "on" or (action == "toggle" and OPTION not in options)
    wanted = list(options) if enabled else [option for option in options if option != OPTION]
    if enabled and OPTION not in wanted:
        wanted.append(OPTION)
    if wanted != options:
        run(["gsettings", "set", SCHEMA, "xkb-options", repr(wanted) if wanted else "@as []"])
        if gnome_options() != wanted:
            raise KeyboardSwapError("GNOME did not persist the keyboard options")
    try:
        sync_x11(enabled, x11_state(), x11[1] if x11 is not None else ())
    except KeyboardSwapError as error:
        raise KeyboardSwapError(f"the GNOME setting was saved, but {error}") from error
    return enabled


def status() -> dict[str, bool | None]:
    enabled = OPTION in gnome_options()
    try:
        x11 = x11_state()
    except KeyboardSwapError:
        x11 = None
    return {"gnome": enabled, "x11": OPTION in x11[1] if x11 is not None else None}


def say(message: str, color: str) -> None:
    if sys.stdout.isatty() and not os.environ.get("NO_COLOR") and os.environ.get("TERM") != "dumb":
        message = f"{color}{message}\033[0m"
    print(message)


def main(arguments: list[str]) -> int:
    if "-h" in arguments or "--help" in arguments:
        print(USAGE, end="")
        return 0
    if len(arguments) != 1 or arguments[0] not in {"toggle", "on", "off", "status", "--status-json", "--sync-x11"}:
        print(USAGE, end="", file=sys.stderr)
        return 2
    action = arguments[0]
    try:
        if action == "--sync-x11":
            if os.environ.get("DISPLAY"):
                if gnome_session():
                    sync_x11(OPTION in gnome_options(), x11_state())
                else:
                    run(["setxkbmap", "-layout", "es", "-option", OPTION])
            return 0
        if not gnome_session() or not (os.environ.get("DISPLAY") or os.environ.get("WAYLAND_DISPLAY")):
            raise KeyboardSwapError("run this command in a graphical GNOME session")
        if action in {"status", "--status-json"}:
            current = status()
            if action == "--status-json":
                print(json.dumps(current))
            else:
                say(f"GNOME: {'swapped' if current['gnome'] else 'not swapped'}", "\033[34m")
                if os.environ.get("DISPLAY"):
                    x11 = current["x11"]
                    say(
                        f"X11: {'unknown' if x11 is None else 'swapped' if x11 else 'not swapped'}",
                        "\033[33m" if x11 is None else "\033[34m",
                    )
        else:
            enabled = set_swap(action)
            say(f"Escape/Caps Lock swap {'enabled' if enabled else 'disabled'}.", "\033[32m")
    except KeyboardSwapError as error:
        print(f"keyboard-swap: {error}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
