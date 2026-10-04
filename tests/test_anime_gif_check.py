"""anime-gif-check: the legibility filter for the ASUS AniMe Vision lid, and what it writes.

The tool simulates the 810-LED lid, so a GIF that is only recognisable at full resolution fails.
These tests build tiny synthetic GIFs whose verdict is known by construction (a bold moving disc
passes; scrolling letters, a flashing solid block and noise each fail exactly the stage that
should catch them), then check the decoder rules, the files `--out` writes and the command line.
The tool must never start a process (it prints the `asusctl anime gif` command, it does not run
it), so a source scan guards that too. NumPy and Pillow are optional on a dotfiles machine; the
tests are skipped where they are missing, like the tool itself reports.
"""

import importlib.util
import io
import json
import os
import pathlib
import shutil
import site
import subprocess
import sys
import tempfile
import unittest
from unittest import mock

try:
    import numpy as np
    from PIL import Image, ImageDraw
except ImportError as exc:
    if __name__ == "__main__":
        print(f"SKIP test_anime_gif_check: needs NumPy and Pillow ({exc})")
        raise SystemExit(0)
    raise unittest.SkipTest(f"needs NumPy and Pillow ({exc})") from exc


REPO_ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = REPO_ROOT / ".local" / "scripts" / "anime_gif_check.py"
WRAPPER = REPO_ROOT / ".local" / "scripts" / "bin" / "anime-gif-check"

spec = importlib.util.spec_from_file_location("anime_gif_check", SCRIPT)
agc = importlib.util.module_from_spec(spec)
sys.modules["anime_gif_check"] = agc  # dataclasses resolve their string annotations through sys.modules
spec.loader.exec_module(agc)

WHITE = (255, 255, 255)
GIFS: dict = {}


def save_gif(path, frames, duration=100):
    frames[0].save(path, save_all=True, append_images=frames[1:], duration=duration, loop=0)


def disc(frames=12, width=64, height=32, radius=9, black_on_white=False):
    """A disc crossing the picture: big, simple, bold and always moving."""
    background, ink = ((WHITE, (0, 0, 0)) if black_on_white else ((0, 0, 0), WHITE))
    out = []
    for i in range(frames):
        im = Image.new("RGB", (width, height), background)
        x = 12 + (width - 24) * i / (frames - 1)
        ImageDraw.Draw(im).ellipse((x - radius, height / 2 - radius, x + radius, height / 2 + radius), fill=ink)
        out.append(im)
    return out


def scrolling_letters(frames=12, step=5):
    """Six bars of different widths in a row, like the letters of a word, scrolling sideways."""
    out = []
    for i in range(frames):
        im = Image.new("RGB", (120, 24), (0, 0, 0))
        draw = ImageDraw.Draw(im)
        x = 46 - i * step
        for width in (4, 9, 6, 10, 5, 8):
            draw.rectangle((x, 5, x + width - 1, 18), fill=WHITE)
            x += width + 3
        out.append(im)
    return out


def identical_icons(frames=8):
    """Three identical balls in a row (owls, hearts, stars): a row of one icon is not a word."""
    out = []
    for i in range(frames):
        im = Image.new("RGB", (80, 24), (0, 0, 0))
        for j in range(3):
            x = 8 + j * 26 + (i % 2) * 2
            ImageDraw.Draw(im).ellipse((x, 5, x + 13, 18), fill=WHITE)
        out.append(im)
    return out


def flashing_block(frames=8):
    """A square that fills its whole box and blinks: lit LEDs, but no silhouette to read."""
    out = []
    for i in range(frames):
        im = Image.new("RGB", (40, 40), (0, 0, 0))
        ImageDraw.Draw(im).rectangle((0, 0, 8, 8) if i % 4 == 3 else (0, 0, 38, 38), fill=WHITE)
        out.append(im)
    return out


def strobing_disc(frames=10):
    """The same disc on and off every frame: a big whole-lid brightness swing ten times a second."""
    out = []
    for i in range(frames):
        im = Image.new("RGB", (64, 32), (0, 0, 0))
        if i % 2 == 0:
            ImageDraw.Draw(im).ellipse((23, 7, 41, 25), fill=WHITE)
        out.append(im)
    return out


def noise(frames=12):
    """Random pixels: every LED lit or dark, nothing to recognise."""
    rng = np.random.default_rng(7)
    return [Image.fromarray((rng.random((32, 64)) > 0.5).astype(np.uint8) * 255).convert("RGB") for _ in range(frames)]


def transparent_disc(frames=12):
    """Palette index 0 is transparent but its colour is white; asusctl lights opaque pixels only."""
    out = []
    for i in range(frames):
        im = Image.new("P", (64, 32), 0)
        im.putpalette([255, 255, 255, 255, 255, 255] + [0] * 762)
        x = 12 + 40 * i / (frames - 1)
        ImageDraw.Draw(im).ellipse((x - 9, 7, x + 9, 25), fill=1)
        out.append(im)
    return out


def setUpModule():
    work = tempfile.TemporaryDirectory(prefix="anime-gif-check-test-")
    unittest.addModuleCleanup(work.cleanup)
    root = pathlib.Path(work.name)
    for name, frames in {
        "disc": disc(),
        "inverted": disc(black_on_white=True),
        "letters": scrolling_letters(),
        "icons": identical_icons(),
        "block": flashing_block(),
        "strobe": strobing_disc(),
        "noise": noise(),
    }.items():
        GIFS[name] = root / f"{name}.gif"
        save_gif(GIFS[name], frames)
    GIFS["slow"] = root / "slow.gif"
    save_gif(GIFS["slow"], disc(frames=6), duration=5000)
    GIFS["zero-delay"] = root / "zero-delay.gif"
    save_gif(GIFS["zero-delay"], disc(), duration=0)
    GIFS["fast"] = root / "fast.gif"
    save_gif(GIFS["fast"], disc(), duration=10)
    GIFS["transparent"] = root / "transparent.gif"
    frames = transparent_disc()
    frames[0].save(GIFS["transparent"], save_all=True, append_images=frames[1:], duration=100, loop=0,
                   transparency=0, disposal=2)
    GIFS["single"] = root / "single.gif"
    disc(frames=2)[0].save(GIFS["single"])
    GIFS["corrupt"] = root / "corrupt.gif"
    GIFS["corrupt"].write_bytes(b"GIF89a\x00\x01not really a gif")
    GIFS["root"] = root


def analyse(name, **kwargs):
    return agc.analyse(GIFS[name], **kwargs)[0]


def failed_stages(report):
    return {stage["key"] for stage in report.stages if not stage["ok"]}


class GeometryTests(unittest.TestCase):
    def test_lid_has_810_leds_in_the_published_row_pairs(self):
        # ASUS's mask.csv merges row pairs: 2, 4, ... 30 LEDs, then 30 for the rest of the band.
        self.assertEqual(len(agc.LEDS), 810)
        rows = np.bincount(agc.LEDS[:, 1].astype(int))
        pairs = [int(rows[2 * k] + rows[2 * k + 1]) for k in range(34)]
        self.assertEqual(pairs, [min(2 * (k + 1), 30) for k in range(34)])

    def test_white_picture_lights_every_led(self):
        for size, scale in (((60, 81), 1.0), ((120, 162), 1.1), ((48, 65), 1.2)):
            with self.subTest(size=size):
                values = agc.led_values(np.full(size, 255.0), scale=scale)
                self.assertEqual(values.shape, (810,))
                self.assertEqual(int((values == 255).sum()), 810)

    def test_black_picture_lights_nothing(self):
        self.assertEqual(int(agc.led_values(np.zeros((60, 81))).sum()), 0)

    def test_play_command_uses_the_calibrated_strip_mapping(self):
        # The same numbers drive the simulation and the printed command, and they were fitted by hand
        # to lay a 702x160 canvas along the diagonal band: change them deliberately, never by accident.
        self.assertEqual(agc.PLAY_FLAGS, "--scale 1.225 --angle 0.607 --x-pos -2.43 --y-pos 1.49")
        self.assertEqual((agc.CANVAS_W, agc.CANVAS_H), (702, 160))


class ThresholdTests(unittest.TestCase):
    """The thresholds were calibrated on measurements: pin each edge so a change is deliberate."""

    GOOD = dict(loop_s=1.2, lit=0.25, peak=1.0, bold=0.8, motion_ps=1.0, static_ratio=0.0,
                detail_loss=0.05, fg_box=0.4, fill=0.5, text_like=False)

    # (stage, field, value that still passes, value that fails)
    EDGES = (
        ("loop", "loop_s", 0.3, 0.29),
        ("loop", "loop_s", 24.0, 24.1),
        ("lit", "lit", 0.14, 0.139),
        ("lit", "lit", 0.50, 0.501),
        ("peak", "peak", 0.85, 0.849),
        ("bold", "bold", 0.40, 0.399),
        ("motion", "motion_ps", 0.15, 0.149),
        ("motion", "motion_ps", 3.0, 3.01),
        ("motion", "static_ratio", 0.5, 0.51),
        ("detail", "detail_loss", 0.22, 0.221),
        ("flood", "fg_box", 0.80, 0.801),
        ("text", "text_like", False, True),
        ("solid", "fill", 0.88, 0.881),
    )

    def verdicts(self, **overrides):
        report = agc.Report(path="x.gif", **{**self.GOOD, **overrides})
        return {stage.key: stage.ok(report) for stage in agc.STAGES}

    def test_a_good_report_passes_every_stage(self):
        self.assertEqual(set(self.verdicts().values()), {True})

    def test_each_edge_flips_exactly_its_own_stage(self):
        for stage, field, passes, fails in self.EDGES:
            with self.subTest(stage=stage, field=field, passes=passes, fails=fails):
                self.assertTrue(self.verdicts(**{field: passes})[stage])
                result = self.verdicts(**{field: fails})
                self.assertFalse(result.pop(stage))
                self.assertEqual(set(result.values()), {True}, "only the stage under test may change")

    def test_labels_state_the_thresholds_that_are_checked(self):
        self.assertEqual([stage.label for stage in agc.STAGES], [
            "loops in 0.3-24 s",
            "lights 14-50% of the LEDs",
            "reaches at least 85% brightness",
            "at least 40% of the lit LEDs are bold",
            "moves (0.15-3.0 per s) and is not frozen",
            "fine-detail loss at most 0.22",
            "not a full-frame flood",
            "not a text banner",
            "has a silhouette (fill at most 88%)",
        ])


class ReadabilityTests(unittest.TestCase):
    def test_bold_moving_shape_passes_every_stage(self):
        report = analyse("disc")
        self.assertEqual(report.skip, "")
        self.assertEqual(failed_stages(report), set())
        self.assertTrue(report.passed)
        self.assertEqual(report.background, "kept")

    def test_scrolling_letters_fail_only_the_text_stage(self):
        report = analyse("letters")
        self.assertEqual(failed_stages(report), {"text"})
        self.assertTrue(report.text_like)
        self.assertFalse(report.passed)

    def test_a_row_of_identical_icons_is_not_text(self):
        report = analyse("icons")
        self.assertFalse(report.text_like)
        self.assertTrue(report.passed)

    def test_flashing_solid_block_fails_only_the_silhouette_stage(self):
        report = analyse("block")
        self.assertEqual(failed_stages(report), {"solid"})
        self.assertGreater(report.fill, 0.88)

    def test_noise_fails_the_fine_detail_stage(self):
        report = analyse("noise")
        self.assertIn("detail", failed_stages(report))
        self.assertFalse(report.passed)

    def test_light_background_is_inverted_so_the_subject_lights_up(self):
        report = analyse("inverted")
        self.assertEqual(report.background, "inverted")
        self.assertTrue(report.passed)

    def test_loop_longer_than_24_seconds_fails_the_loop_stage(self):
        report = analyse("slow")
        self.assertIn("loop", failed_stages(report))
        self.assertEqual(report.loop_s, 30.0)

    def test_strobing_is_reported_and_a_steady_shape_is_not(self):
        self.assertGreaterEqual(analyse("strobe").flicker_swings_per_s, 3.0)
        self.assertEqual(analyse("disc").flicker_swings_per_s, 0.0)

    def test_verdict_is_deterministic(self):
        self.assertEqual(analyse("noise"), analyse("noise"))


class DecodingTests(unittest.TestCase):
    def test_zero_frame_delay_means_100_ms_like_a_browser(self):
        self.assertEqual(analyse("zero-delay").loop_s, 1.2)

    def test_delays_under_40_ms_are_raised_to_40(self):
        self.assertEqual(analyse("fast").loop_s, 0.48)

    def test_transparent_pixels_are_dark_because_asusctl_lights_opaque_pixels_only(self):
        report = analyse("transparent")
        self.assertEqual(report.skip, "")
        self.assertEqual(report.background, "kept")
        self.assertTrue(report.passed)

    def test_single_frame_is_skipped_with_the_reason(self):
        report = analyse("single")
        self.assertIn("only 1 frame", report.skip)
        self.assertFalse(report.passed)

    def test_too_many_frames_are_skipped(self):
        report = analyse("disc", max_frames=5)
        self.assertIn("12 frames; the limit is 5", report.skip)

    def test_strip_space_needs_the_exact_canvas(self):
        report = analyse("disc", strip_space=True)
        self.assertIn("702x160", report.skip)
        self.assertIn("got 64x32", report.skip)

    def test_corrupt_gif_is_skipped_instead_of_crashing(self):
        report = analyse("corrupt")
        self.assertTrue(report.skip.startswith("unreadable"), report.skip)


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


def run_cli(*args, env=None, unset=(), executable=None):
    """Run the script (or `executable`, e.g. the wrapper) with colour settings that cannot vary by machine."""
    environment = {k: v for k, v in os.environ.items()
                   if k not in ("NO_COLOR", "FORCE_COLOR", "CLICOLOR_FORCE", *unset)}
    environment["PYTHON_COLORS"] = "0"  # Python 3.14 colours argparse help when FORCE_COLOR is set, even to 0
    # Python finds packages installed with `pip install --user` (NumPy, Pillow) through $HOME. A test that
    # gives the child another $HOME keeps this process's user base, so the child imports what this one did.
    environment["PYTHONUSERBASE"] = site.getuserbase()
    environment.update(env or {})
    command = [str(executable)] if executable else [python_command(), str(SCRIPT)]
    return subprocess.run(command + [str(a) for a in args], capture_output=True, text=True, env=environment,
                          timeout=120, check=False)


class CommandLineTests(unittest.TestCase):
    def setUp(self):
        tmp = tempfile.TemporaryDirectory(prefix="anime-gif-check-out-")
        self.addCleanup(tmp.cleanup)
        self.out = pathlib.Path(tmp.name) / "lid"

    def test_exit_status_is_zero_only_when_every_gif_passes(self):
        passing = run_cli(GIFS["disc"])
        self.assertEqual(passing.returncode, 0, passing.stderr)
        self.assertIn("PASS", passing.stdout)
        result = run_cli(GIFS["disc"], GIFS["letters"])
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertIn("PASS", result.stdout)
        self.assertIn("FAIL", result.stdout)
        self.assertIn("looks like text", result.stdout)

    def test_out_writes_a_lid_ready_gif_and_a_preview_and_prints_the_play_command(self):
        result = run_cli(GIFS["disc"], "--out", self.out)
        self.assertEqual(result.returncode, 0, result.stderr)
        lid, preview = self.out / "disc.lid.gif", self.out / "disc.preview.gif"
        self.assertTrue(lid.is_file() and preview.is_file())
        with Image.open(lid) as image:
            self.assertEqual(image.size, (agc.CANVAS_W, agc.CANVAS_H))
            self.assertEqual(image.n_frames, 12)
            self.assertEqual(image.info["duration"], 100)
            for index in range(image.n_frames):
                image.seek(index)
                rgb = np.asarray(image.convert("RGB"))
                self.assertTrue((rgb[..., 0] == rgb[..., 1]).all() and (rgb[..., 1] == rgb[..., 2]).all(),
                                "asusctl reads the average of R, G and B, so the lid copy must be gray")
        with Image.open(preview) as image:
            self.assertEqual(image.n_frames, 12)
        self.assertIn(f"Play one (not run): asusctl anime gif --path {lid} {agc.PLAY_FLAGS}", result.stdout)
        self.assertIn("--enable-powersave-anim true", result.stdout)

    def test_existing_outputs_are_kept_unless_forced(self):
        first = run_cli(GIFS["disc"], "--out", self.out)
        self.assertEqual(first.returncode, 0, first.stderr)
        lid = self.out / "disc.lid.gif"
        lid.write_bytes(b"edited by hand")
        again = run_cli(GIFS["disc"], "--out", self.out)
        self.assertIn("exists", again.stdout)
        self.assertEqual(lid.read_bytes(), b"edited by hand")
        forced = run_cli(GIFS["disc"], "--out", self.out, "--force")
        self.assertIn("wrote", forced.stdout)
        self.assertNotEqual(lid.read_bytes(), b"edited by hand")

    def test_failing_gif_is_written_only_with_all(self):
        result = run_cli(GIFS["letters"], "--out", self.out)
        self.assertEqual(result.returncode, 1)
        self.assertFalse((self.out / "letters.lid.gif").exists())
        self.assertNotIn("Play one", result.stdout)
        everything = run_cli(GIFS["letters"], "--out", self.out, "--all")
        self.assertEqual(everything.returncode, 1, everything.stderr)
        self.assertTrue((self.out / "letters.lid.gif").is_file())
        self.assertNotIn("Play one", everything.stdout, "a failing GIF is never suggested for the lid")

    def test_json_is_machine_readable(self):
        result = run_cli(GIFS["disc"], GIFS["letters"], GIFS["single"], "--json")
        self.assertEqual(result.returncode, 1, result.stderr)
        reports = json.loads(result.stdout)
        self.assertEqual([r["passed"] for r in reports], [True, False, False])
        self.assertEqual({s["key"] for s in reports[0]["stages"]},
                         {"loop", "lit", "peak", "bold", "motion", "detail", "flood", "text", "solid"})
        self.assertIn("only 1 frame", reports[2]["skip"])

    def test_output_has_no_colour_codes_when_it_is_not_a_terminal(self):
        result = run_cli(GIFS["disc"], GIFS["letters"])
        self.assertIn("PASS", result.stdout)
        self.assertIn("FAIL", result.stdout)
        self.assertNotIn("\033", result.stdout)

    def test_strobing_gif_gets_a_flicker_note(self):
        strobe, steady = run_cli(GIFS["strobe"]), run_cli(GIFS["disc"])
        self.assertIn("FAIL", strobe.stdout)
        self.assertIn("flickers", strobe.stdout)
        self.assertIn("PASS", steady.stdout)
        self.assertNotIn("flickers", steady.stdout)

    def test_missing_file_is_reported_and_fails(self):
        result = run_cli(GIFS["root"] / "nothing.gif")
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertIn("not a file", result.stdout)

    def test_missing_libraries_give_install_hints_and_exit_2(self):
        shadow = GIFS["root"] / "shadow"
        (shadow / "numpy").mkdir(parents=True)
        (shadow / "numpy" / "__init__.py").write_text("raise ImportError('numpy is not installed (simulated)')\n")
        result = run_cli(GIFS["disc"], env={"PYTHONPATH": str(shadow)})
        self.assertEqual(result.returncode, 2, result.stderr)
        self.assertIn("simulated", result.stderr)
        self.assertIn("python3-numpy", result.stderr)
        self.assertEqual(result.stdout, "")

    def test_source_never_starts_a_process(self):
        source = SCRIPT.read_text()
        for needle in ("subprocess", "os.system", "os.popen", "os.exec", "os.spawn"):
            self.assertNotIn(needle, source, "the tool prints the asusctl command; it must never run it")


class TerminalStdout(io.StringIO):
    def isatty(self):
        return True


class ColourTests(unittest.TestCase):
    """Colour is for a terminal only: never when piped (see CommandLineTests), under NO_COLOR or with --json."""

    def on_terminal(self, *args, no_color=False):
        out = TerminalStdout()
        with mock.patch.dict(os.environ), mock.patch.object(sys, "stdout", out):
            os.environ.pop("NO_COLOR", None)
            if no_color:
                os.environ["NO_COLOR"] = "1"
            status = agc.main([str(a) for a in args])
        return status, out.getvalue()

    def test_terminal_gets_green_pass_and_red_fail(self):
        status, text = self.on_terminal(GIFS["disc"], GIFS["letters"])
        self.assertEqual(status, 1)
        self.assertIn("\033[32mPASS\033[0m", text)
        self.assertIn("\033[31mFAIL\033[0m", text)

    def test_no_color_is_honoured(self):
        status, text = self.on_terminal(GIFS["disc"], no_color=True)
        self.assertEqual(status, 0)
        self.assertIn("PASS", text)
        self.assertNotIn("\033", text)

    def test_json_is_never_coloured(self):
        status, text = self.on_terminal(GIFS["disc"], "--json")
        self.assertEqual(status, 0)
        self.assertTrue(json.loads(text)[0]["passed"])
        self.assertNotIn("\033", text)


class WrapperTests(unittest.TestCase):
    def test_both_files_carry_the_description_the_command_catalog_reads(self):
        for path in (WRAPPER, SCRIPT):
            with self.subTest(path=path.name):
                lines = path.read_text().splitlines()
                self.assertTrue(lines[0].startswith("#!"), lines[0])
                self.assertTrue(lines[1].startswith("# Description: "), lines[1])
                self.assertTrue(os.access(path, os.X_OK), f"{path} must be executable")

    def test_wrapper_runs_the_script_next_to_it(self):
        result = run_cli("--help", executable=WRAPPER)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("usage: anime-gif-check", result.stdout)

    def test_wrapper_finds_the_script_through_dotfiles_and_then_home(self):
        lone = GIFS["root"] / "lone" / "bin"
        lone.mkdir(parents=True)
        wrapper = lone / "anime-gif-check"
        wrapper.write_text(WRAPPER.read_text())
        wrapper.chmod(0o755)
        empty_home = GIFS["root"] / "empty-home"  # the real $HOME may expose the script through stow
        empty_home.mkdir()
        via_dotfiles = run_cli("--help", executable=wrapper, env={"DOTFILES": str(REPO_ROOT), "HOME": str(empty_home)})
        self.assertEqual(via_dotfiles.returncode, 0, via_dotfiles.stderr)
        home = GIFS["root"] / "home"
        (home / ".local" / "scripts").mkdir(parents=True)
        (home / ".local" / "scripts" / "anime_gif_check.py").symlink_to(SCRIPT)
        via_home = run_cli("--help", executable=wrapper, env={"HOME": str(home)}, unset=("DOTFILES",))
        self.assertEqual(via_home.returncode, 0, via_home.stderr)
        self.assertIn("usage: anime-gif-check", via_home.stdout)
        nowhere = run_cli("--help", executable=wrapper, env={"HOME": str(GIFS["root"])}, unset=("DOTFILES",))
        self.assertNotEqual(nowhere.returncode, 0, "with no script anywhere the wrapper must fail, not pass quietly")


if __name__ == "__main__":
    unittest.main()
