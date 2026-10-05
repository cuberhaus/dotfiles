"""match-monitor-scales: one scale for every monitor, without moving the monitors around.

A fractional-scale monitor next to a 100% one makes Chromium-based apps jitter in fullscreen on
the other monitor (docs/FULLSCREEN-WIGGLE-DIAGNOSIS.md). The command changes the scale of the
monitors that differ, which changes their logical size, so every position has to be worked out
again. These tests guard what that must get right:

  - The layout that wiggled on this machine (laptop 133% at 190,1440 under a 100% monitor at 0,0)
    becomes 133% on both with the laptop still 190 pixels right of the other monitor's edge.
  - A monitor keeps its place along the shared edge: flush at the start, flush at the end,
    centered, or the same fraction of the way along. Rotated monitors swap width and height.
  - Nothing is changed before mutter has checked the layout, `--dry-run` changes nothing, and a
    refusal from mutter, an unsupported scale or a layout the tool does not understand ends with
    no `gdctl set` that applies anything.
  - `--persistent` copies monitors.xml first, never over a copy that exists, and waits for mutter
    to write the file (it does so about two seconds after it applies) instead of reading it once.
  - Help works without Python or a desktop, the wrapper and the module print the same text, and a
    word the command does not know is refused instead of being taken as data.

The CLI cases run the real wrapper in a `PATH` that holds only a fake busctl and a fake gdctl, so
no case can reach the real display. The fake gdctl keeps its state in a file shaped like the
reply of `GetCurrentState` and refuses what mutter refuses (positions offset from 0,0, overlap,
monitors that touch nothing, an unknown mode or scale), so a wrong plan fails the case.
"""

import importlib.util
import json
import os
import pathlib
import shutil
import stat
import subprocess
import sys
import tempfile
import unittest
from unittest import mock

REPO_ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = REPO_ROOT / ".local" / "scripts" / "match_monitor_scales.py"
WRAPPER = REPO_ROOT / ".local" / "scripts" / "bin" / "match-monitor-scales"

spec = importlib.util.spec_from_file_location("match_monitor_scales", SCRIPT)
mms = importlib.util.module_from_spec(spec)
sys.modules["match_monitor_scales"] = mms  # dataclasses resolve their string annotations through sys.modules
spec.loader.exec_module(mms)

# The scales mutter offers on this machine for both of its monitors.
SCALES = [1.0, 1.25, 1.3333333730697632, 1.6666666269302368, 2.0, 2.5, 2.6666667461395264]
S133 = SCALES[2]


###############################################################
# => Fixtures: what mutter's GetCurrentState says
###############################################################


def variant(kind, data):
    return {"type": kind, "data": data}


def mode(mode_id, width, height, rate, *, current=False, scales=SCALES):
    flags = {"is-current": variant("b", True)} if current else {"is-preferred": variant("b", True)}
    return [mode_id, width, height, rate, 1.0, list(scales), flags]


def monitor(connector, vendor, product, modes, *, color_mode=0, rgb_range=1, lease=False):
    props = {
        "display-name": variant("s", connector),
        "is-builtin": variant("b", connector.startswith("eDP")),
        "is-for-lease": variant("b", lease),
        "min-refresh-rate": variant("i", 48),
        "color-mode": variant("u", color_mode),
        "supported-color-modes": variant("au", [0, 2]),
        "rgb-range": variant("u", rgb_range),
    }
    return [[connector, vendor, product, "0"], modes, props]


def laptop(**kwargs):
    return monitor("eDP-1", "BOE", "NE160QDM-NZC", [
        mode("2560x1600@240.000", 2560, 1600, 240.0, current=True),
        mode("2560x1600@60.000", 2560, 1600, 60.0),
    ], **kwargs)


def external(connector="DP-3", scales=SCALES, **kwargs):
    return monitor(connector, "SKG", "M27T6", [
        mode("2560x1440@119.998", 2560, 1440, 119.99758911132812, current=True, scales=scales),
        mode("2560x1440@59.951", 2560, 1440, 59.9505500793457, scales=scales),
    ], **kwargs)


def uhd(connector="HDMI-1"):
    return monitor(connector, "LEN", "UHD", [mode("3840x2160@60.000", 3840, 2160, 60.0, current=True)])


def screen(x, y, scale, *monitors, transform=0, primary=False):
    """One logical monitor: where it is, its scale, and the monitors it shows."""
    return [x, y, scale, transform, primary, [m[0] for m in monitors], {}]


def reply(monitors, screens, *, layout_mode=1):
    return {
        "type": "ua((ssss)a(siiddada{sv})a{sv})a(iiduba(ssss)a{sv})a{sv}",
        "data": [8, monitors, screens, {
            "layout-mode": variant("u", layout_mode),
            "supports-changing-layout-mode": variant("b", True),
        }],
    }


def mixed_reply():
    """The layout of this machine when the wiggle happened: laptop 133%, external 100%."""
    edp, dp = laptop(), external()
    return reply([edp, dp], [screen(190, 1440, S133, edp, primary=True), screen(0, 0, 1.0, dp)])


def equal_reply():
    """What the fix saved: both at 133%, the external above the laptop."""
    edp, dp = laptop(), external()
    return reply([edp, dp], [screen(0, 1080, S133, edp, primary=True), screen(0, 0, S133, dp)])


def single_reply():
    edp = laptop()
    return reply([edp], [screen(0, 0, S133, edp, primary=True)])


def parsed(raw):
    return mms.parse_state(raw)


def plan(raw, scale):
    """The planned positions as plain tuples, in the order of the logical monitors."""
    return [(place.x, place.y) for place in mms.plan_layout(parsed(raw), scale)]


###############################################################
# => Reading the state
###############################################################


class ParseTests(unittest.TestCase):
    def test_reads_what_busctl_prints(self):
        state = parsed(mixed_reply())
        self.assertEqual(state.layout_mode, mms.LOGICAL)
        self.assertEqual([s.label for s in state.logical], ["eDP-1", "DP-3"])
        edp, dp = state.logical
        self.assertTrue(edp.primary)
        self.assertFalse(dp.primary)
        self.assertEqual((edp.x, edp.y, edp.scale), (190, 1440, S133))
        self.assertEqual(edp.monitors[0].mode.id, "2560x1600@240.000")
        self.assertEqual(edp.size(state.layout_mode), (1920, 1200))
        self.assertEqual(dp.size(state.layout_mode), (2560, 1440))

    def test_a_reply_it_cannot_read_is_refused_not_guessed(self):
        for bad in ({"data": []}, {"data": [1, 2, 3, 4]}, "text", None, {"data": [8, [["x"]], [], {}]}):
            with self.subTest(reply=bad), self.assertRaises(mms.Refused):
                mms.parse_state(bad)

    def test_unsupported_setups_are_refused(self):
        edp, dp = laptop(), external()
        cases = {
            "a flipped monitor": reply([edp], [screen(0, 0, S133, edp, transform=4, primary=True)]),
            "a leased monitor": reply([laptop(), external(lease=True)], [screen(0, 0, S133, edp, primary=True)]),
            "an unknown color mode": reply([laptop(color_mode=9)], [screen(0, 0, S133, edp, primary=True)]),
            "no current mode": reply([monitor("eDP-1", "B", "P", [mode("1x1@60.000", 1, 1, 60.0)])],
                                     [screen(0, 0, 1.0, edp, primary=True)]),
        }
        for name, raw in cases.items():
            with self.subTest(name), self.assertRaises(mms.Refused):
                mms.check_supported(parsed(raw))
        mms.check_supported(parsed(mixed_reply()))  # the normal case passes


###############################################################
# => Which scale
###############################################################


class ScaleTests(unittest.TestCase):
    def test_a_scale_is_typed_as_a_number_or_a_percentage(self):
        self.assertEqual(mms.parse_scale("1.25"), 1.25)
        self.assertEqual(mms.parse_scale("125%"), 1.25)
        self.assertAlmostEqual(mms.parse_scale(" 133 % "), 1.33)
        self.assertEqual(mms.parse_scale("2"), 2.0)
        for bad in ("", "abc", "0", "-1", "1,25", "1.2.3", "%", "12x"):
            with self.subTest(bad), self.assertRaises(Exception) as caught:
                mms.parse_scale(bad)
            self.assertEqual(type(caught.exception).__name__, "ArgumentTypeError")

    def test_the_default_is_the_primary_monitors_scale(self):
        self.assertEqual(mms.choose_scale(parsed(mixed_reply()), None), S133)

    def test_a_typed_scale_becomes_the_exact_value_mutter_offers(self):
        state = parsed(mixed_reply())
        self.assertEqual(mms.choose_scale(state, 1.33), S133)  # 133% is 1.3333, not 1.33
        self.assertEqual(mms.choose_scale(state, 1.25), 1.25)
        self.assertEqual(mms.choose_scale(state, 2.0), 2.0)

    def test_a_scale_between_two_offered_ones_is_refused_and_the_choices_are_listed(self):
        with self.assertRaises(mms.Refused) as caught:
            mms.choose_scale(parsed(mixed_reply()), 1.3)
        message = str(caught.exception)
        self.assertIn("not offered by every monitor", message)
        self.assertIn("100% (1)", message)
        self.assertIn("133% (1.333)", message)
        self.assertIn("--scale", message)

    def test_a_scale_one_monitor_lacks_is_refused(self):
        edp, dp = laptop(), external(scales=[1.0, 2.0])
        raw = reply([edp, dp], [screen(0, 0, S133, edp, primary=True), screen(1920, 0, 1.0, dp)])
        with self.assertRaises(mms.Refused) as caught:
            mms.choose_scale(parsed(raw), None)
        self.assertIn("primary monitor's scale (eDP-1 at 133%)", str(caught.exception))
        self.assertIn("100% (1), 200% (2)", str(caught.exception))
        self.assertEqual(mms.choose_scale(parsed(raw), 2.0), 2.0)  # a common scale still works


###############################################################
# => Where everything goes
###############################################################


class PlanTests(unittest.TestCase):
    def test_the_layout_that_wiggled_keeps_the_laptop_190_pixels_right(self):
        # DP-3 at 100% was 2560x1440 at 0,0; at 133% it is 1920x1080, still above the laptop, and the
        # laptop stays 190 pixels right of its left edge. The corner of the layout is 0,0 again.
        self.assertEqual(plan(mixed_reply(), S133), [(190, 1080), (0, 0)])

    def test_a_layout_that_already_matches_does_not_move(self):
        self.assertEqual(plan(equal_reply(), S133), [(0, 1080), (0, 0)])

    def test_side_by_side_and_flush_at_the_top_stays_flush_at_the_top(self):
        edp, dp = laptop(), external()
        raw = reply([edp, dp], [screen(0, 0, S133, edp, primary=True), screen(1920, 0, 1.0, dp)])
        self.assertEqual(plan(raw, S133), [(0, 0), (1920, 0)])

    def test_flush_at_the_bottom_stays_flush_at_the_bottom(self):
        # Laptop 1920x1200 at 0,240 and the external 2560x1440 at 1920,0 share the line y=1440. At 133%
        # the external is 1080 high, so it must start 120 lower than the laptop's corner to end there.
        edp, dp = laptop(), external()
        raw = reply([edp, dp], [screen(0, 240, S133, edp, primary=True), screen(1920, 0, 1.0, dp)])
        self.assertEqual(plan(raw, S133), [(0, 0), (1920, 120)])

    def test_centered_stays_centered_when_the_sizes_change_differently(self):
        # A 4K monitor (3840 wide) centered above the laptop (1920 wide at 133%). At 133% it is 2880
        # wide, so it sticks out 480 pixels on each side instead of 960.
        edp, hdmi = laptop(), uhd()
        raw = reply([edp, hdmi], [screen(960, 2160, S133, edp, primary=True), screen(0, 0, 1.0, hdmi)])
        self.assertEqual(plan(raw, S133), [(480, 1620), (0, 0)])

    def test_any_other_offset_keeps_its_fraction_along_the_edge(self):
        # The external stays primary at 100% and the laptop goes from 133% to 200% for both (--scale 2):
        # the laptop was 200 of the external's 2560 pixels in, so it is 100 of its new 1280 in.
        edp, dp = laptop(), external()
        raw = reply([edp, dp], [screen(0, 0, 1.0, dp, primary=True), screen(200, 1440, S133, edp)])
        self.assertEqual(plan(raw, 2.0), [(0, 0), (100, 720)])

    def test_three_monitors_in_a_row_close_ranks_around_the_primary(self):
        left, edp, right = external("DP-3"), laptop(), external("DP-4")
        raw = reply([left, edp, right], [screen(2560, 0, S133, edp, primary=True),
                                         screen(0, 0, 1.0, left), screen(4480, 0, 1.0, right)])
        self.assertEqual(plan(raw, S133), [(1920, 0), (0, 0), (3840, 0)])

    def test_a_rotated_monitor_swaps_width_and_height(self):
        # The portrait monitor is 1440x2560 at 100% and 1080x1920 at 133%; the monitor to its right
        # must start 1080 pixels after it, not 1920.
        edp, portrait, dp = laptop(), external("DP-3"), external("DP-4")
        raw = reply([edp, portrait, dp], [
            screen(0, 0, S133, edp, primary=True),
            screen(1920, 0, 1.0, portrait, transform=1),
            screen(3360, 0, 1.0, dp),
        ])
        self.assertEqual(plan(raw, S133), [(0, 0), (1920, 0), (3000, 0)])

    def test_in_the_physical_layout_mode_nothing_moves(self):
        # Sizes there are pixels of the panel, whatever the scale, so the positions stay as they are.
        edp, dp = laptop(), external()
        raw = reply([edp, dp], [screen(0, 0, S133, edp, primary=True), screen(2560, 0, 1.0, dp)], layout_mode=2)
        self.assertEqual(plan(raw, S133), [(0, 0), (2560, 0)])
        self.assertEqual(plan(raw, 2.0), [(0, 0), (2560, 0)])

    def test_a_monitor_that_touches_no_other_is_refused_not_guessed(self):
        edp, dp = laptop(), external()
        raw = reply([edp, dp], [screen(0, 0, S133, edp, primary=True), screen(5000, 5000, 1.0, dp)])
        with self.assertRaises(mms.Refused) as caught:
            mms.plan_layout(parsed(raw), S133)
        self.assertIn("DP-3 touches no other monitor", str(caught.exception))

    def test_an_arrangement_that_would_overlap_is_refused(self):
        # The laptop stays at 190,1440; a monitor placed at 200,1500 sits on top of it.
        with mock.patch.object(mms, "place_beside", return_value=(200, 1500, 100, 100)):
            with self.assertRaises(mms.Refused) as caught:
                mms.plan_layout(parsed(mixed_reply()), S133)
        self.assertIn("would overlap", str(caught.exception))

    def test_sides_are_found_from_the_edges(self):
        a = (0, 0, 100, 100)
        self.assertEqual(mms.side_of(a, (100, 20, 50, 50)), "right")
        self.assertEqual(mms.side_of(a, (-50, 20, 50, 50)), "left")
        self.assertEqual(mms.side_of(a, (20, 100, 50, 50)), "below")
        self.assertEqual(mms.side_of(a, (20, -50, 50, 50)), "above")
        self.assertIsNone(mms.side_of(a, (100, 100, 50, 50)), "touching at a corner shares no edge")
        self.assertIsNone(mms.side_of(a, (110, 20, 50, 50)), "a gap is not touching")


###############################################################
# => The gdctl command
###############################################################

MIXED_ARGS = [
    "--logical-monitor", "--monitor", "eDP-1", "--mode", "2560x1600@240.000",
    "--scale", "1.3333333730697632", "--x", "190", "--y", "1080", "--primary",
    "--logical-monitor", "--monitor", "DP-3", "--mode", "2560x1440@119.998",
    "--scale", "1.3333333730697632", "--x", "0", "--y", "0",
]
MIXED_UNDO_ARGS = [
    "--logical-monitor", "--monitor", "eDP-1", "--mode", "2560x1600@240.000",
    "--scale", "1.3333333730697632", "--x", "190", "--y", "1440", "--primary",
    "--logical-monitor", "--monitor", "DP-3", "--mode", "2560x1440@119.998",
    "--scale", "1.0", "--x", "0", "--y", "0",
]


class CommandTests(unittest.TestCase):
    def build(self, raw, scale=S133, **options):
        state = parsed(raw)
        return mms.gdctl_command(state, mms.plan_layout(state, scale), **options)

    def test_the_command_for_the_layout_that_wiggled(self):
        self.assertEqual(self.build(mixed_reply()), ["gdctl", "set", *MIXED_ARGS])
        self.assertEqual(self.build(mixed_reply(), verify=True), ["gdctl", "set", "--verify", *MIXED_ARGS])
        self.assertEqual(self.build(mixed_reply(), persistent=True), ["gdctl", "set", "--persistent", *MIXED_ARGS])

    def test_the_way_back_is_the_layout_as_it_was(self):
        state = parsed(mixed_reply())
        self.assertEqual(mms.gdctl_command(state, mms.current_layout(state)), ["gdctl", "set", *MIXED_UNDO_ARGS])

    def test_the_layout_mode_is_left_to_gdctl(self):
        # gdctl refuses --layout-mode on a mutter that cannot change it, even to the mode it has.
        for layout_mode in (1, 2):
            with self.subTest(layout_mode=layout_mode):
                raw = mixed_reply()
                raw["data"][3]["layout-mode"] = variant("u", layout_mode)
                self.assertNotIn("--layout-mode", self.build(raw))

    def test_color_settings_are_carried_over_only_when_they_are_not_the_defaults(self):
        edp, dp = laptop(), external(color_mode=1, rgb_range=3)
        raw = reply([edp, dp], [screen(0, 1080, S133, edp, primary=True), screen(0, 0, S133, dp)])
        command = self.build(raw)
        at = command.index("DP-3")
        self.assertEqual(command[at:at + 7], ["DP-3", "--mode", "2560x1440@119.998", "--color-mode", "bt2100",
                                              "--rgb-range", "limited"])
        self.assertNotIn("--color-mode", command[:at], "the laptop has the default color settings")
        self.assertNotIn("--rgb-range", command[:at])
        self.assertEqual(command.count("--color-mode"), 1)
        self.assertEqual(command.count("--rgb-range"), 1)

    def test_a_rotation_is_carried_over(self):
        edp, portrait = laptop(), external("DP-3")
        raw = reply([edp, portrait], [screen(0, 0, S133, edp, primary=True),
                                      screen(1920, 0, 1.0, portrait, transform=3)])
        command = self.build(raw)
        self.assertEqual(command[command.index("--transform") + 1], "270")

    def test_mirrored_monitors_stay_one_logical_monitor(self):
        edp, dp = laptop(), external()
        raw = reply([edp, dp], [screen(0, 0, S133, edp, dp, primary=True)])
        command = self.build(raw)
        self.assertEqual(command.count("--logical-monitor"), 1)
        self.assertEqual(command.count("--monitor"), 2)
        self.assertEqual(mms.parse_state(raw).logical[0].label, "eDP-1+DP-3")


###############################################################
# => monitors.xml
###############################################################


def config_xml(*configurations):
    """monitors.xml as mutter writes it. Each configuration is a list of (connector, scale)."""
    body = ""
    for configuration in configurations:
        body += "  <configuration>\n    <layoutmode>logical</layoutmode>\n"
        for index, (connector, scale) in enumerate(configuration):
            body += (f"    <logicalmonitor>\n      <x>0</x>\n      <y>{index * 1080}</y>\n"
                     f"      <scale>{scale}</scale>\n      <monitor>\n        <monitorspec>\n"
                     f"          <connector>{connector}</connector>\n          <vendor>V</vendor>\n"
                     f"          <product>P</product>\n          <serial>0</serial>\n        </monitorspec>\n"
                     f"        <mode>\n          <width>2560</width>\n          <height>1440</height>\n"
                     f"          <rate>60.000</rate>\n        </mode>\n      </monitor>\n"
                     f"    </logicalmonitor>\n")
        body += "  </configuration>\n"
    return f'<monitors version="2">\n{body}</monitors>\n'


class SavedLayoutTests(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory(prefix="match-monitor-scales-xml-")
        self.addCleanup(tmp.cleanup)
        self.path = pathlib.Path(tmp.name) / "monitors.xml"

    def matches(self, connectors=("eDP-1", "DP-3"), scale=S133):
        return mms.saved_layout_matches(self.path, frozenset(connectors), scale)

    def test_a_configuration_for_these_monitors_at_this_scale_matches(self):
        self.path.write_text(config_xml([("eDP-1", "1.3333333730697632"), ("DP-3", "1.3333333730697632")]))
        self.assertTrue(self.matches())

    def test_any_matching_configuration_counts(self):
        self.path.write_text(config_xml([("DP-3", "1")],
                                        [("eDP-1", "1.3333333730697632"), ("DP-3", "1.3333333730697632")]))
        self.assertTrue(self.matches())

    def test_other_scales_other_monitors_and_no_file_do_not_match(self):
        self.assertFalse(self.matches(), "no file yet")
        self.path.write_text(config_xml([("eDP-1", "1.3333333730697632"), ("DP-3", "1")]))
        self.assertFalse(self.matches(), "the external is still at 100%")
        self.path.write_text(config_xml([("eDP-1", "1.3333333730697632")]))
        self.assertFalse(self.matches(), "no configuration for both monitors")
        self.path.write_text(config_xml([("eDP-1", "1.3333333730697632"), ("DP-3", "1.3333333730697632")]))
        self.assertFalse(self.matches(connectors=("eDP-1", "DP-4")), "a different monitor")
        self.assertFalse(self.matches(scale=1.25), "a different scale")

    def test_a_half_written_file_does_not_match_and_does_not_crash(self):
        self.path.write_text(config_xml([("eDP-1", "1.3333333730697632"), ("DP-3", "1.3333333730697632")])[:120])
        self.assertFalse(self.matches())
        self.path.write_text("<monitors><configuration><logicalmonitor><scale>big</scale></logicalmonitor>"
                             "</configuration></monitors>")
        self.assertFalse(self.matches())


class BackupTests(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory(prefix="match-monitor-scales-backup-")
        self.addCleanup(tmp.cleanup)
        self.dir = pathlib.Path(tmp.name)
        self.path = self.dir / "monitors.xml"

    def test_there_is_nothing_to_back_up_when_there_is_no_file(self):
        self.assertIsNone(mms.backup_file(self.path))
        self.assertEqual(list(self.dir.iterdir()), [])

    def test_the_copy_has_the_same_bytes_and_time_and_the_original_stays(self):
        self.path.write_bytes(b"<monitors version='2'/>\n")
        os.utime(self.path, (1_700_000_000, 1_700_000_000))
        copy = mms.backup_file(self.path, stamp="20261005-235959")
        self.assertEqual(copy.name, "monitors.xml.bak-20261005-235959")
        self.assertEqual(copy.read_bytes(), self.path.read_bytes())
        self.assertEqual(int(copy.stat().st_mtime), 1_700_000_000)
        self.assertTrue(self.path.exists())

    def test_an_existing_copy_is_never_replaced(self):
        self.path.write_bytes(b"new\n")
        taken = self.dir / "monitors.xml.bak-20261005-235959"
        taken.write_bytes(b"older copy\n")
        copy = mms.backup_file(self.path, stamp="20261005-235959")
        self.assertEqual(copy.name, "monitors.xml.bak-20261005-235959-1")
        self.assertEqual(taken.read_bytes(), b"older copy\n")
        self.assertEqual(copy.read_bytes(), b"new\n")

    def test_the_default_name_carries_the_date_and_time(self):
        self.path.write_bytes(b"x\n")
        self.assertRegex(mms.backup_file(self.path).name, r"^monitors\.xml\.bak-\d{8}-\d{6}$")


class WaitTests(unittest.TestCase):
    def test_returns_as_soon_as_the_check_passes(self):
        calls = []
        self.assertTrue(mms.wait_until(lambda: calls.append(1) or len(calls) >= 3, 5))
        self.assertEqual(len(calls), 3)

    def test_gives_up_after_the_time_is_up(self):
        self.assertFalse(mms.wait_until(lambda: False, 0.3))

    def test_the_wait_comes_from_the_environment_and_falls_back_on_nonsense(self):
        with mock.patch.dict(os.environ, {"MATCH_MONITOR_SCALES_WAIT": "2.5"}):
            self.assertEqual(mms.wait_seconds(), 2.5)
        with mock.patch.dict(os.environ, {"MATCH_MONITOR_SCALES_WAIT": "soon"}):
            self.assertEqual(mms.wait_seconds(), mms.DEFAULT_WAIT)
        with mock.patch.dict(os.environ, {"MATCH_MONITOR_SCALES_WAIT": "-4"}):
            self.assertEqual(mms.wait_seconds(), 0.0)


###############################################################
# => The command line, against a fake busctl and a fake gdctl
###############################################################

FAKE_BUSCTL = '''\
#!/usr/bin/env python3
"""Serves the state file the way `busctl --user --json=short call ... GetCurrentState` does."""
import json, os, sys

with open(os.environ["FAKE_LOG"], "a") as log:
    log.write(json.dumps(["busctl", *sys.argv[1:]]) + "\\n")
if os.environ.get("FAKE_BUSCTL_FAIL"):
    print(os.environ["FAKE_BUSCTL_FAIL"], file=sys.stderr)
    sys.exit(1)
expected = ["--user", "--json=short", "call", "org.gnome.Mutter.DisplayConfig",
            "/org/gnome/Mutter/DisplayConfig", "org.gnome.Mutter.DisplayConfig", "GetCurrentState"]
if sys.argv[1:] != expected:
    print("fake busctl: unexpected request", file=sys.stderr)
    sys.exit(1)
sys.stdout.write(open(os.environ["FAKE_STATE"]).read())
'''

FAKE_GDCTL = '''\
#!/usr/bin/env python3
"""`gdctl set` as far as the tool uses it, on a state file shaped like GetCurrentState.

It refuses what mutter refuses, so a wrong plan fails a case: positions offset from 0,0, monitors that
overlap or touch nothing, no primary, an unknown monitor, mode or scale. FAKE_VERIFY_FAIL=text makes
--verify fail with that text. FAKE_SAVE=now|late|never decides when a persistent apply writes
monitors.xml (late: FAKE_SAVE_DELAY seconds later, from a detached process, as mutter does).
"""
import json, math, os, shutil, subprocess, sys

argv = sys.argv[1:]
with open(os.environ["FAKE_LOG"], "a") as log:
    log.write(json.dumps(["gdctl", *argv]) + "\\n")


def die(message, status=1):
    print(message, file=sys.stderr)
    sys.exit(status)


if not argv or argv[0] != "set":
    die("fake gdctl: only set is supported", 2)

TRANSFORMS = {"normal": 0, "90": 1, "180": 2, "270": 3}
persistent = verify = False
screens = []
i = 1


def value():
    global i
    i += 1
    if i >= len(argv):
        die("fake gdctl: missing value", 2)
    return argv[i]


while i < len(argv):
    arg = argv[i]
    if arg in ("-P", "--persistent"):
        persistent = True
    elif arg in ("-V", "--verify"):
        verify = True
    elif arg in ("-L", "--logical-monitor"):
        screens.append({"monitors": [], "scale": None, "x": None, "y": None, "primary": False, "transform": "normal"})
    elif not screens:
        die(f"fake gdctl: {arg} comes before --logical-monitor", 2)
    elif arg in ("-M", "--monitor"):
        screens[-1]["monitors"].append({"connector": value(), "mode": None, "color_mode": None, "rgb_range": None})
    elif arg in ("-m", "--mode"):
        screens[-1]["monitors"][-1]["mode"] = value()
    elif arg in ("-c", "--color-mode"):
        screens[-1]["monitors"][-1]["color_mode"] = value()
    elif arg in ("-r", "--rgb-range"):
        screens[-1]["monitors"][-1]["rgb_range"] = value()
    elif arg in ("-p", "--primary"):
        screens[-1]["primary"] = True
    elif arg in ("-s", "--scale"):
        screens[-1]["scale"] = float(value())
    elif arg in ("-t", "--transform"):
        screens[-1]["transform"] = value()
    elif arg in ("-x", "--x"):
        screens[-1]["x"] = int(value())
    elif arg in ("-y", "--y"):
        screens[-1]["y"] = int(value())
    else:
        die(f"fake gdctl: unknown option {arg}", 2)
    i += 1

with open(os.environ["FAKE_STATE"]) as handle:
    state = json.load(handle)
_serial, monitors, _current, props = state["data"]
layout_mode = props["layout-mode"]["data"]
known = {m[0][0]: m for m in monitors}

if verify and os.environ.get("FAKE_VERIFY_FAIL"):
    die("Error: " + os.environ["FAKE_VERIFY_FAIL"])

rects = []
for s in screens:
    if not s["monitors"] or None in (s["scale"], s["x"], s["y"]):
        die("Error: incomplete logical monitor")
    mode_of_screen = None
    for m in s["monitors"]:
        if m["connector"] not in known:
            die(f"Error: unknown monitor {m['connector']}")
        modes = {md[0]: md for md in known[m["connector"]][1]}
        if m["mode"] not in modes:
            die(f"Error: unknown mode {m['mode']} for {m['connector']}")
        if not any(abs(s["scale"] - scale) < 1e-6 for scale in modes[m["mode"]][5]):
            die(f"Error: scale {s['scale']} is not supported by mode {m['mode']}")
        mode_of_screen = mode_of_screen or modes[m["mode"]]
    width, height = mode_of_screen[1], mode_of_screen[2]
    if s["transform"] in ("90", "270"):
        width, height = height, width
    if layout_mode == 1:
        width, height = math.floor(width / s["scale"] + 0.5), math.floor(height / s["scale"] + 0.5)
    rects.append((s["x"], s["y"], width, height))

if sum(1 for s in screens if s["primary"]) != 1:
    die("Error: exactly one logical monitor must be primary")
if min(r[0] for r in rects) != 0 or min(r[1] for r in rects) != 0:
    die("Error: Logical monitors positions are offset")


def overlap(a0, a1, b0, b1):
    return min(a1, b1) - max(a0, b0)


def touches(a, b):
    rows = overlap(a[1], a[1] + a[3], b[1], b[1] + b[3]) > 0
    columns = overlap(a[0], a[0] + a[2], b[0], b[0] + b[2]) > 0
    return ((rows and (a[0] + a[2] == b[0] or b[0] + b[2] == a[0]))
            or (columns and (a[1] + a[3] == b[1] or b[1] + b[3] == a[1])))


for n, a in enumerate(rects):
    for b in rects[n + 1:]:
        if overlap(a[0], a[0] + a[2], b[0], b[0] + b[2]) > 0 and overlap(a[1], a[1] + a[3], b[1], b[1] + b[3]) > 0:
            die("Error: Logical monitors overlap")
if len(rects) > 1 and not all(any(touches(a, b) for b in rects if b is not a) for a in rects):
    die("Error: Logical monitors not adjacent")
if verify:
    sys.exit(0)

chosen = {m["connector"]: m["mode"] for s in screens for m in s["monitors"]}
for monitor in monitors:
    for md in monitor[1]:
        md[6].pop("is-current", None)
        if chosen.get(monitor[0][0]) == md[0]:
            md[6]["is-current"] = {"type": "b", "data": True}
state["data"][2] = [
    [s["x"], s["y"], s["scale"], TRANSFORMS[s["transform"]], s["primary"],
     [known[m["connector"]][0] for m in s["monitors"]], {}]
    for s in screens
]
with open(os.environ["FAKE_STATE"] + ".new", "w") as handle:
    json.dump(state, handle)
os.replace(os.environ["FAKE_STATE"] + ".new", os.environ["FAKE_STATE"])

if persistent and os.environ.get("FAKE_SAVE", "now") != "never":
    xml = "<monitors version=\\"2\\">\\n  <configuration>\\n    <layoutmode>logical</layoutmode>\\n"
    for s in screens:
        xml += f"    <logicalmonitor>\\n      <x>{s['x']}</x>\\n      <y>{s['y']}</y>\\n      <scale>{s['scale']}</scale>\\n"
        for m in s["monitors"]:
            xml += (f"      <monitor>\\n        <monitorspec>\\n          <connector>{m['connector']}</connector>\\n"
                    f"        </monitorspec>\\n      </monitor>\\n")
        xml += "    </logicalmonitor>\\n"
    xml += "  </configuration>\\n</monitors>\\n"
    target = os.path.join(os.environ["HOME"], ".config", "monitors.xml")
    if os.environ.get("FAKE_SAVE", "now") == "late":
        writer = "import sys, time; time.sleep(float(sys.argv[2])); open(sys.argv[1], 'w').write(sys.argv[3])"
        subprocess.Popen([shutil.which("python3"), "-c", writer, target, os.environ.get("FAKE_SAVE_DELAY", "0.6"), xml],
                         stdin=subprocess.DEVNULL, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL,
                         start_new_session=True)
    else:
        with open(target, "w") as handle:
            handle.write(xml)
'''


def python_command():
    """The interpreter running this test, without trusting `sys.executable`.

    zsh uses an exported ARGV0 as argv[0] of every external command. An editor installed as an AppImage
    exports ARGV0 (the path of the AppImage) to its terminals, so a Python started there reports the
    editor as `sys.executable`, and `subprocess.run([sys.executable, ...])` starts the editor instead.
    """
    executable = pathlib.Path(sys.executable)
    if executable.name.startswith("python"):
        return str(executable)
    return shutil.which(f"python{sys.version_info.major}") or "python3"


class Sandbox:
    """A throwaway HOME, a state file and a PATH that holds nothing but the fakes and a few basics."""

    # The wrapper prints its help with cat and finds the module with dirname; the fakes are Python behind env.
    BASICS = ("bash", "env", "dirname", "cat")

    def __init__(self, case, raw, *, fakes=("busctl", "gdctl"), basics=BASICS, python=True):
        tmp = tempfile.TemporaryDirectory(prefix="match-monitor-scales-cli-")
        case.addCleanup(tmp.cleanup)
        self.root = pathlib.Path(tmp.name)
        self.home = self.root / "home"
        (self.home / ".config").mkdir(parents=True)
        self.bin = self.root / "bin"
        self.bin.mkdir()
        self.state_file = self.root / "state.json"
        self.log_file = self.root / "calls.log"
        self.log_file.touch()
        self.write_state(raw)
        for tool in basics:
            os.symlink(shutil.which(tool), self.bin / tool)
        if python:
            os.symlink(shutil.which(python_command()) or python_command(), self.bin / "python3")
        for name, source in (("busctl", FAKE_BUSCTL), ("gdctl", FAKE_GDCTL)):
            if name in fakes:
                path = self.bin / name
                path.write_text(source)
                path.chmod(path.stat().st_mode | stat.S_IXUSR)

    @property
    def config(self):
        return self.home / ".config" / "monitors.xml"

    def write_state(self, raw):
        self.state_file.write_text(json.dumps(raw))

    def state(self):
        return json.loads(self.state_file.read_text())

    def screens(self):
        """(x, y, scale) of each logical monitor now, in the order mutter lists them."""
        return [(s[0], s[1], s[2]) for s in self.state()["data"][2]]

    def calls(self, tool):
        lines = [json.loads(line) for line in self.log_file.read_text().splitlines()]
        return [line[1:] for line in lines if line[0] == tool]

    def run(self, *args, env=None, command=None):
        """Run the wrapper (or `command`) with only the sandbox on PATH and a stdin that is empty."""
        environment = {
            "PATH": str(self.bin), "HOME": str(self.home), "LC_ALL": "C", "PYTHON_COLORS": "0",
            "FAKE_STATE": str(self.state_file), "FAKE_LOG": str(self.log_file),
            "MATCH_MONITOR_SCALES_WAIT": "3",
        }
        environment.update(env or {})
        argv = (command or [str(WRAPPER)]) + [str(arg) for arg in args]
        return subprocess.run(argv, capture_output=True, text=True, env=environment, cwd=self.home,
                              timeout=60, check=False, input="")


MIXED_APPLIED = [(190, 1080, S133), (0, 0, S133)]


class CommandLineTests(unittest.TestCase):
    def sandbox(self, raw=None, **kwargs):
        return Sandbox(self, raw or mixed_reply(), **kwargs)

    def test_a_dry_run_asks_mutter_and_changes_nothing(self):
        box = self.sandbox()
        before = box.state_file.read_bytes()
        result = box.run("--dry-run")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(box.calls("gdctl"), [["set", "--verify", *MIXED_ARGS]])
        self.assertEqual(box.state_file.read_bytes(), before)
        self.assertFalse(box.config.exists())
        for text in ("Current layout (logical):", "Giving all 2 monitors 133% (the primary monitor's scale):",
                     "100% -> 133%", "at 0,0 -> 0,0", "at 190,1440 -> 190,1080", "mutter accepts the layout.",
                     "Dry run: nothing was changed.", "It would run: gdctl set " + " ".join(MIXED_ARGS)):
            self.assertIn(text, result.stdout)
        self.assertEqual(result.stderr, "")

    def test_it_checks_with_mutter_first_and_then_applies(self):
        box = self.sandbox()
        result = box.run()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(box.calls("gdctl"), [["set", "--verify", *MIXED_ARGS], ["set", *MIXED_ARGS]])
        self.assertEqual(box.screens(), MIXED_APPLIED)
        self.assertIn("Done: all 2 monitors are at 133%.", result.stdout)
        self.assertIn("This is temporary", result.stdout)
        self.assertIn("To go back: gdctl set " + " ".join(MIXED_UNDO_ARGS), result.stdout)
        self.assertFalse(box.config.exists(), "a temporary change does not touch monitors.xml")

    def test_persistent_copies_monitors_xml_first_then_saves(self):
        box = self.sandbox()
        old = config_xml([("eDP-1", "1.3333333730697632"), ("DP-3", "1")])
        box.config.write_text(old)
        result = box.run("--persistent")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(box.calls("gdctl"), [["set", "--verify", *MIXED_ARGS], ["set", "--persistent", *MIXED_ARGS]])
        backups = sorted(p for p in box.config.parent.iterdir() if p.name.startswith("monitors.xml.bak-"))
        self.assertEqual(len(backups), 1)
        self.assertRegex(backups[0].name, r"^monitors\.xml\.bak-\d{8}-\d{6}$")
        self.assertEqual(backups[0].read_text(), old)
        self.assertNotEqual(box.config.read_text(), old)
        self.assertTrue(mms.saved_layout_matches(box.config, frozenset({"eDP-1", "DP-3"}), S133))
        self.assertIn(f"Backed up ~/.config/monitors.xml to ~/.config/{backups[0].name}", result.stdout)
        self.assertIn("Saved in ~/.config/monitors.xml.", result.stdout)
        self.assertNotIn("temporary", result.stdout)
        self.assertIn("To go back: gdctl set --persistent " + " ".join(MIXED_UNDO_ARGS), result.stdout)

    def test_it_waits_for_mutter_to_write_the_file_instead_of_reading_it_once(self):
        # Mutter writes monitors.xml about two seconds after it applies. A read right after gdctl
        # returns sees the old file, which once made a good save look like a failure.
        box = self.sandbox()
        result = box.run("--persistent", env={"FAKE_SAVE": "late", "FAKE_SAVE_DELAY": "0.8"})
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("Saved in ~/.config/monitors.xml.", result.stdout)
        self.assertTrue(box.config.exists())

    def test_it_says_so_when_the_file_never_shows_the_layout(self):
        box = self.sandbox()
        result = box.run("--persistent", env={"FAKE_SAVE": "never", "MATCH_MONITOR_SCALES_WAIT": "1"})
        self.assertEqual(result.returncode, 1)
        self.assertIn("Done: all 2 monitors are at 133%.", result.stdout, "the layout itself was applied")
        self.assertIn("To go back: gdctl set --persistent", result.stdout)
        self.assertIn("~/.config/monitors.xml does not show it after 1 s", result.stderr)
        self.assertNotIn("Saved in", result.stdout)

    def test_matching_monitors_need_no_change_and_no_gdctl(self):
        for options in ((), ("--dry-run",)):
            with self.subTest(options=options):
                box = self.sandbox(equal_reply())
                result = box.run(*options)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn("All 2 monitors are already at 133%. Nothing to change.", result.stdout)
                self.assertEqual(box.calls("gdctl"), [])

    def test_persistent_still_saves_a_layout_that_already_matches(self):
        # A change made without --persistent is temporary; asking to keep it must not be a no-op.
        box = self.sandbox(equal_reply())
        result = box.run("--persistent")
        self.assertEqual(result.returncode, 0, result.stderr)
        calls = box.calls("gdctl")
        self.assertEqual(len(calls), 2)
        self.assertIn("--persistent", calls[1])
        self.assertEqual(box.screens(), [(0, 1080, S133), (0, 0, S133)], "nothing moved")
        self.assertIn("Saved in ~/.config/monitors.xml.", result.stdout)

    def test_when_mutter_refuses_nothing_is_applied_and_nothing_is_backed_up(self):
        box = self.sandbox()
        box.config.write_text(config_xml([("DP-3", "1")]))
        before = box.state_file.read_bytes()
        result = box.run("--persistent", env={"FAKE_VERIFY_FAIL": "Logical monitors not adjacent"})
        self.assertEqual(result.returncode, 1)
        self.assertIn("mutter does not accept this layout; nothing was changed.", result.stderr)
        self.assertIn("Logical monitors not adjacent", result.stderr)
        self.assertEqual([c[:2] for c in box.calls("gdctl")], [["set", "--verify"]])
        self.assertEqual(box.state_file.read_bytes(), before)
        self.assertEqual([p.name for p in box.config.parent.iterdir()], ["monitors.xml"], "no backup was made")

    def test_a_typed_scale_is_applied_to_every_monitor(self):
        box = self.sandbox()
        result = box.run("--scale", "125%")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual([s[2] for s in box.screens()], [1.25, 1.25])
        self.assertIn("Giving all 2 monitors 125%:", result.stdout)
        self.assertNotIn("primary monitor's scale", result.stdout)

    def test_a_scale_between_two_offered_ones_is_refused_before_anything_runs(self):
        box = self.sandbox()
        result = box.run("--scale", "1.3")
        self.assertEqual(result.returncode, 1)
        self.assertIn("a scale of 130% is not offered by every monitor", result.stderr)
        self.assertIn("Pick one with --scale.", result.stderr)
        self.assertEqual(box.calls("gdctl"), [])

    def test_a_single_monitor_has_nothing_to_match(self):
        box = self.sandbox(single_reply())
        result = box.run()
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("nothing to match", result.stdout)
        self.assertEqual(box.calls("gdctl"), [])

    def test_a_setup_it_does_not_understand_is_left_alone(self):
        edp, dp = laptop(), external()
        flipped = reply([edp, dp], [screen(0, 1080, S133, edp, primary=True), screen(0, 0, S133, dp, transform=5)])
        leased = reply([edp, external(lease=True)], [screen(0, 0, S133, edp, primary=True),
                                                     screen(1920, 0, 1.0, dp)])
        for name, raw, words in (("flipped", flipped, "flipped"), ("leased", leased, "leased")):
            with self.subTest(name):
                box = self.sandbox(raw)
                result = box.run()
                self.assertEqual(result.returncode, 1)
                self.assertIn(words, result.stderr)
                self.assertEqual(box.calls("gdctl"), [])

    def test_a_desktop_without_mutter_is_a_setup_error(self):
        box = self.sandbox()
        result = box.run(env={"FAKE_BUSCTL_FAIL": "Failed to connect to bus: No such file or directory"})
        self.assertEqual(result.returncode, 2)
        self.assertIn("cannot reach mutter's display configuration on the session bus", result.stderr)
        self.assertIn("Failed to connect to bus", result.stderr)
        self.assertEqual(box.calls("gdctl"), [])

    def test_missing_tools_are_a_setup_error_that_names_them(self):
        no_gdctl = self.sandbox(fakes=("busctl",)).run()
        self.assertEqual(no_gdctl.returncode, 2)
        self.assertIn("gdctl (part of mutter) is not installed", no_gdctl.stderr)
        no_busctl = self.sandbox(fakes=("gdctl",)).run()
        self.assertEqual(no_busctl.returncode, 2)
        self.assertIn("busctl (systemd) is not installed", no_busctl.stderr)
        no_python = self.sandbox(python=False).run()
        self.assertEqual(no_python.returncode, 2)
        self.assertIn("python3 is required", no_python.stderr)

    def test_words_it_does_not_know_are_refused_not_taken_as_data(self):
        for words in (["now"], ["--persist"], ["--bogus"], ["--scale"], ["--scale", "big"], ["--scale", "0"],
                      ["--dry-run", "extra"]):
            with self.subTest(words=words):
                box = self.sandbox()
                result = box.run(*words)
                self.assertEqual(result.returncode, 2, result.stderr)
                self.assertEqual(result.stdout, "")
                self.assertIn("match-monitor-scales:", result.stderr)
                self.assertIn("--help", result.stderr)
                self.assertEqual(box.calls("gdctl"), [])
                self.assertEqual(box.calls("busctl"), [])

    def test_no_colour_codes_when_the_output_is_not_a_terminal(self):
        box = self.sandbox()
        result = box.run()
        self.assertNotIn("\033", result.stdout + result.stderr)


class HelpTests(unittest.TestCase):
    def test_help_needs_neither_python_nor_the_desktop(self):
        # Only bash, env and cat: no python3, no dirname, no busctl, no gdctl.
        for words in (["-h"], ["--help"], ["--dry-run", "--help"]):
            with self.subTest(words=words):
                box = Sandbox(self, mixed_reply(), fakes=(), basics=("bash", "env", "cat"), python=False)
                result = box.run(*words)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stderr, "")
                self.assertTrue(result.stdout.startswith("Usage: match-monitor-scales "), result.stdout)
                self.assertEqual(box.log_file.read_text(), "")

    def test_the_wrapper_and_the_module_print_the_same_help(self):
        box = Sandbox(self, mixed_reply())
        from_wrapper = box.run("--help")
        from_module = box.run("--help", command=["python3", str(SCRIPT)])
        self.assertEqual(from_wrapper.returncode, 0)
        self.assertEqual(from_module.returncode, 0)
        self.assertEqual(from_module.stdout, from_wrapper.stdout)
        self.assertEqual(from_wrapper.stdout, mms.USAGE)

    def test_the_help_says_what_the_options_do(self):
        for word in ("--scale", "--persistent", "--dry-run", "--help", "monitors.xml", "MATCH_MONITOR_SCALES_WAIT",
                     "Exit status"):
            self.assertIn(word, mms.USAGE)


class WrapperTests(unittest.TestCase):
    def test_both_files_carry_the_description_the_command_catalog_reads(self):
        lines = WRAPPER.read_text().splitlines()
        self.assertTrue(lines[0].startswith("#!"), lines[0])
        self.assertTrue(lines[1].startswith("# Description: "), lines[1])
        self.assertEqual(lines[2], "# Group: Desktop & hardware")
        self.assertTrue(os.access(WRAPPER, os.X_OK), f"{WRAPPER} must be executable")
        self.assertTrue(SCRIPT.read_text().startswith("#!/usr/bin/env python3"))

    def test_the_wrapper_finds_the_module_next_to_it_then_through_dotfiles_then_home(self):
        box = Sandbox(self, mixed_reply())
        lone = box.root / "lone" / "bin"
        lone.mkdir(parents=True)
        wrapper = lone / "match-monitor-scales"
        wrapper.write_text(WRAPPER.read_text())
        wrapper.chmod(0o755)
        # Nothing next to it, nothing in $DOTFILES or $HOME: the wrapper must fail, not pass quietly.
        nowhere = box.run("--dry-run", command=[str(wrapper)])
        self.assertNotEqual(nowhere.returncode, 0)
        self.assertEqual(box.calls("gdctl"), [])
        via_dotfiles = box.run("--dry-run", command=[str(wrapper)], env={"DOTFILES": str(REPO_ROOT)})
        self.assertEqual(via_dotfiles.returncode, 0, via_dotfiles.stderr)
        scripts = box.home / ".local" / "scripts"
        scripts.mkdir(parents=True)
        (scripts / "match_monitor_scales.py").symlink_to(SCRIPT)
        via_home = box.run("--dry-run", command=[str(wrapper)])
        self.assertEqual(via_home.returncode, 0, via_home.stderr)

    def test_the_module_starts_processes_in_one_place_and_never_through_a_shell_or_sudo(self):
        # What it starts is busctl and gdctl (the CLI cases above only have those two on PATH); this guards
        # the way: one call site, a list of words, no shell, no privileges.
        source = SCRIPT.read_text()
        self.assertEqual(source.count("subprocess.run("), 1)
        for forbidden in ("Popen", "os.system", "os.exec", "check_output", "shell=True", "sudo", "pkexec"):
            self.assertNotIn(forbidden, source)


if __name__ == "__main__":
    unittest.main()
