import ast
import contextlib
import importlib.util
import io
import os
import pathlib
import shlex
import subprocess
import sys
import tempfile
import unittest
from unittest import mock


REPO_ROOT = pathlib.Path(__file__).resolve().parents[1]
COMMAND = REPO_ROOT / ".local/scripts/bin/keyboard-swap"
AUDIT = REPO_ROOT / ".local/scripts/audit_installation.py"


def load_audit():
    spec = importlib.util.spec_from_file_location("keyboard_swap_audit", AUDIT)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


@unittest.skipUnless(sys.platform.startswith("linux"), "GNOME keyboard integration is Linux-specific")
class KeyboardSwapTests(unittest.TestCase):
    def setUp(self):
        self.temporary = tempfile.TemporaryDirectory()
        self.addCleanup(self.temporary.cleanup)
        self.root = pathlib.Path(self.temporary.name)
        self.home = self.root / "home"
        self.home.mkdir()
        self.bin = self.root / "bin"
        self.bin.mkdir()
        self.gnome = self.root / "gnome-options"
        self.x11 = self.root / "x11-options"
        self.layout = self.root / "x11-layout"
        self.log = self.root / "calls"
        self.gnome.write_text("['compose:ralt', 'caps:swapescape']\n", encoding="utf-8")
        self.x11.write_text("compose:ralt,caps:swapescape\n", encoding="utf-8")
        self.layout.write_text("es\n", encoding="utf-8")
        self.log.write_text("", encoding="utf-8")
        self.executable(
            "gsettings",
            """\
#!/bin/sh
printf 'gsettings %s\\n' "$*" >> "$FAKE_LOG"
[ "$2" = org.gnome.desktop.input-sources ] && [ "$3" = xkb-options ] || exit 2
case "$1" in
    get) /bin/cat "$FAKE_GNOME" ;;
    set)
        if [ "${FAKE_DROP_GNOME_WRITE:-0}" != 1 ]; then
            printf '%s\\n' "$4" > "$FAKE_GNOME"
            if [ "${FAKE_GNOME_RESETS_X11:-0}" = 1 ]; then
                case "$4" in
                    *caps:swapescape*) printf 'caps:swapescape\\n' > "$FAKE_X11" ;;
                    *) printf '\\n' > "$FAKE_X11" ;;
                esac
            fi
        fi
        ;;
    *) exit 2 ;;
esac
""",
        )
        self.executable(
            "setxkbmap",
            """\
#!/bin/sh
printf 'setxkbmap' >> "$FAKE_LOG"
printf ' <%s>' "$@" >> "$FAKE_LOG"
printf '\\n' >> "$FAKE_LOG"
if [ "$1" = -query ]; then
    [ "${FAKE_X11_QUERY_FAIL:-0}" != 1 ] || exit 1
    printf 'rules: evdev\\nlayout: '
    /bin/cat "$FAKE_LAYOUT"
    printf 'options: '
    /bin/cat "$FAKE_X11"
    exit 0
fi
layout=$(/bin/cat "$FAKE_LAYOUT")
options=$(/bin/cat "$FAKE_X11")
while [ "$#" -gt 0 ]; do
    case "$1" in
        -layout) layout="$2"; shift 2 ;;
        -option)
            if [ -z "$2" ]; then
                options=
            elif [ -n "$options" ]; then
                options="$options,$2"
            else
                options="$2"
            fi
            shift 2
            ;;
        *) exit 2 ;;
    esac
done
if [ "${FAKE_DROP_X11_WRITE:-0}" != 1 ]; then
    printf '%s\\n' "$layout" > "$FAKE_LAYOUT"
    printf '%s\\n' "$options" > "$FAKE_X11"
fi
""",
        )
        self.env = {
            "HOME": str(self.home),
            "PATH": f"{self.bin}{os.pathsep}{os.defpath}",
            "DISPLAY": ":test",
            "WAYLAND_DISPLAY": "wayland-test",
            "XDG_CURRENT_DESKTOP": "ubuntu:GNOME",
            "FAKE_GNOME": str(self.gnome),
            "FAKE_X11": str(self.x11),
            "FAKE_LAYOUT": str(self.layout),
            "FAKE_LOG": str(self.log),
        }

    def executable(self, name, content):
        path = self.bin / name
        path.write_text(content, encoding="utf-8")
        path.chmod(0o755)

    def run_command(self, *arguments, env=None):
        return subprocess.run(
            [str(COMMAND), *arguments],
            env=env or self.env,
            capture_output=True,
            text=True,
            check=False,
        )

    def gnome_options(self):
        value = self.gnome.read_text(encoding="utf-8").strip().removeprefix("@as ")
        return ast.literal_eval(value)

    def x11_options(self):
        value = self.x11.read_text(encoding="utf-8").strip()
        return value.split(",") if value else []

    def audit(self, root=REPO_ROOT, env=None):
        audit = load_audit()
        reporter = audit.Reporter()
        output = io.StringIO()
        with mock.patch.dict(os.environ, env or self.env, clear=True), contextlib.redirect_stdout(output):
            audit.audit_keyboard_swap(root, reporter)
        return reporter, output.getvalue()

    def test_help_and_invalid_arguments_do_not_change_settings(self):
        for arguments in (("-h",), ("--help",), ("toggle", "--help")):
            with self.subTest(arguments=arguments):
                result = self.run_command(*arguments)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertIn("Usage: keyboard-swap", result.stdout)
                self.assertEqual(result.stderr, "")
        result = self.run_command("--unknown")
        self.assertEqual(result.returncode, 2)
        self.assertEqual(self.log.read_text(encoding="utf-8"), "")
        self.assertEqual(self.gnome_options(), ["compose:ralt", "caps:swapescape"])
        self.assertEqual(self.x11_options(), ["compose:ralt", "caps:swapescape"])

    def test_toggle_twice_preserves_other_options_in_both_keymaps(self):
        result = self.run_command("toggle")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("disabled", result.stdout)
        self.assertEqual(self.gnome_options(), ["compose:ralt"])
        self.assertEqual(self.x11_options(), ["compose:ralt"])
        self.assertEqual(self.layout.read_text(encoding="utf-8").strip(), "es")

        result = self.run_command("toggle")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("enabled", result.stdout)
        self.assertEqual(self.gnome_options(), ["compose:ralt", "caps:swapescape"])
        self.assertEqual(self.x11_options(), ["compose:ralt", "caps:swapescape"])

    def test_toggle_handles_an_otherwise_empty_option_list(self):
        self.gnome.write_text("['caps:swapescape']\n", encoding="utf-8")
        self.x11.write_text("caps:swapescape\n", encoding="utf-8")

        result = self.run_command("toggle")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.gnome.read_text(encoding="utf-8").strip(), "@as []")
        self.assertEqual(self.x11_options(), [])

        result = self.run_command("toggle")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.gnome_options(), ["caps:swapescape"])
        self.assertEqual(self.x11_options(), ["caps:swapescape"])

    def test_toggle_preserves_x11_options_if_gnome_reloads_the_keymap(self):
        env = {**self.env, "FAKE_GNOME_RESETS_X11": "1"}
        result = self.run_command("off", env=env)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.x11_options(), ["compose:ralt"])
        result = self.run_command("on", env=env)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(set(self.x11_options()), {"compose:ralt", "caps:swapescape"})

    def test_explicit_on_and_off_are_idempotent_and_keep_option_order(self):
        self.gnome.write_text("['caps:swapescape', 'compose:ralt']\n", encoding="utf-8")
        self.x11.write_text("caps:swapescape,compose:ralt\n", encoding="utf-8")
        result = self.run_command("on")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.gnome_options(), ["caps:swapescape", "compose:ralt"])
        self.assertNotIn("gsettings set", self.log.read_text(encoding="utf-8"))
        self.assertNotIn("setxkbmap <-layout>", self.log.read_text(encoding="utf-8"))

        for _ in range(2):
            result = self.run_command("off")
            self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.gnome_options(), ["compose:ralt"])
        self.assertEqual(self.x11_options(), ["compose:ralt"])

    def test_saved_setting_synchronizes_x11_after_login(self):
        self.gnome.write_text("['compose:ralt']\n", encoding="utf-8")
        self.layout.write_text("us\n", encoding="utf-8")
        result = self.run_command("--sync-x11")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.gnome_options(), ["compose:ralt"])
        self.assertEqual(self.x11_options(), ["compose:ralt"])
        self.assertEqual(self.layout.read_text(encoding="utf-8").strip(), "es")

        result = self.run_command("--sync-x11")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.log.read_text(encoding="utf-8").count("setxkbmap <-layout>"), 1)

        self.gnome.write_text("['compose:ralt', 'caps:swapescape']\n", encoding="utf-8")
        result = self.run_command("--sync-x11")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.x11_options(), ["compose:ralt", "caps:swapescape"])

    def test_graphical_startup_files_apply_the_saved_setting(self):
        env = {**self.env, "DOTFILES": str(REPO_ROOT), "DISTRO": "ubuntu"}
        for startup in (REPO_ROOT / ".xprofile", REPO_ROOT / ".config/zsh/.zprofile"):
            with self.subTest(startup=startup):
                self.gnome.write_text("['compose:ralt']\n", encoding="utf-8")
                self.x11.write_text("compose:ralt,caps:swapescape\n", encoding="utf-8")
                result = subprocess.run(
                    ["bash", str(startup)], env=env, capture_output=True, text=True, check=False
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(self.x11_options(), ["compose:ralt"])
                self.assertEqual(self.gnome_options(), ["compose:ralt"])

    def test_fails_without_changing_x11_when_gnome_does_not_persist(self):
        result = self.run_command("off", env={**self.env, "FAKE_DROP_GNOME_WRITE": "1"})
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("did not persist", result.stderr)
        self.assertEqual(self.gnome_options(), ["compose:ralt", "caps:swapescape"])
        self.assertEqual(self.x11_options(), ["compose:ralt", "caps:swapescape"])
        self.assertNotIn("setxkbmap <-layout>", self.log.read_text(encoding="utf-8"))

    def test_invalid_or_unreadable_state_does_not_change_the_setting(self):
        self.gnome.write_text("not-an-array\n", encoding="utf-8")
        result = self.run_command("off")
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("invalid xkb-options", result.stderr)
        self.assertNotIn("gsettings set", self.log.read_text(encoding="utf-8"))

        self.gnome.write_text("['caps:swapescape']\n", encoding="utf-8")
        result = self.run_command("off", env={**self.env, "FAKE_X11_QUERY_FAIL": "1"})
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(self.gnome_options(), ["caps:swapescape"])
        self.assertNotIn("gsettings set", self.log.read_text(encoding="utf-8"))

    def test_failed_x11_write_reports_the_partially_saved_change(self):
        result = self.run_command("off", env={**self.env, "FAKE_DROP_X11_WRITE": "1"})
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("GNOME setting was saved", result.stderr)
        self.assertEqual(self.gnome_options(), ["compose:ralt"])
        self.assertEqual(self.x11_options(), ["compose:ralt", "caps:swapescape"])

    def test_wayland_without_x11_and_non_gnome_fallback(self):
        result = self.run_command("off", env={**self.env, "DISPLAY": ""})
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.gnome_options(), ["compose:ralt"])
        self.assertNotIn("setxkbmap", self.log.read_text(encoding="utf-8"))

        env = {**self.env, "XDG_CURRENT_DESKTOP": "i3"}
        result = self.run_command("on", env=env)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(self.gnome_options(), ["compose:ralt"])
        result = self.run_command("--sync-x11", env=env)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("setxkbmap <-layout> <es> <-option> <caps:swapescape>", self.log.read_text(encoding="utf-8"))

    def test_audit_shows_working_commands_without_changing_the_machine(self):
        root = self.root / "repo with spaces"
        root.symlink_to(REPO_ROOT, target_is_directory=True)
        reporter, output = self.audit(root)
        self.assertEqual((reporter.issues, reporter.warnings), (0, 0))
        self.assertIn("swap is enabled", output)
        self.assertIn("Disable:", output)
        self.assertIn("Enable:", output)
        self.assertIn("Run it again to switch back", output)
        self.assertNotIn("gsettings set", self.log.read_text(encoding="utf-8"))
        self.assertNotIn("setxkbmap <-layout>", self.log.read_text(encoding="utf-8"))

        command = next(line.split("Toggle:", 1)[1].strip() for line in output.splitlines() if "Toggle:" in line)
        for enabled in (False, True):
            result = subprocess.run(shlex.split(command), env=self.env, capture_output=True, text=True, check=False)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual("caps:swapescape" in self.gnome_options(), enabled)
            self.assertEqual("caps:swapescape" in self.x11_options(), enabled)

    def test_audit_reports_mismatched_x11_with_a_working_fix(self):
        self.x11.write_text("compose:ralt\n", encoding="utf-8")
        reporter, output = self.audit()
        self.assertEqual(reporter.issues, 1)
        self.assertIn("[DRIFT]", output)
        command = next(line.split("Fix:", 1)[1].strip() for line in output.splitlines() if "Fix:" in line)
        result = subprocess.run(shlex.split(command), env=self.env, capture_output=True, text=True, check=False)
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.x11_options(), ["compose:ralt", "caps:swapescape"])
        reporter, output = self.audit()
        self.assertEqual(reporter.issues, 0, output)

    def test_main_audit_includes_the_keyboard_commands(self):
        audit = load_audit()
        other_audits = (
            "audit_git", "audit_stow", "audit_shell_configuration", "audit_environment",
            "audit_git_configuration", "audit_editors_and_fonts", "audit_packages",
            "audit_ide_update_channels", "audit_extra_packages", "audit_self_installed_apps",
            "audit_automations",
        )
        output = io.StringIO()
        with contextlib.ExitStack() as stack:
            stack.enter_context(mock.patch.dict(os.environ, self.env, clear=True))
            stack.enter_context(mock.patch.object(sys, "argv", ["audit_installation.py", "--profile", "work"]))
            stack.enter_context(mock.patch.object(audit, "expected_packages", return_value=set()))
            for name in other_audits:
                stack.enter_context(mock.patch.object(audit, name))
            stack.enter_context(contextlib.redirect_stdout(output))
            result = audit.main()
        self.assertEqual(result, 0)
        self.assertIn("swap is enabled", output.getvalue())
        self.assertIn("Toggle:", output.getvalue())

    def test_audit_accepts_a_disabled_swap_and_skips_other_desktops(self):
        self.gnome.write_text("['compose:ralt']\n", encoding="utf-8")
        self.x11.write_text("compose:ralt\n", encoding="utf-8")
        reporter, output = self.audit()
        self.assertEqual(reporter.issues, 0)
        self.assertIn("swap is disabled", output)

        reporter, output = self.audit(env={**self.env, "XDG_CURRENT_DESKTOP": "KDE"})
        self.assertEqual((reporter.issues, reporter.warnings, output), (0, 0, ""))


if __name__ == "__main__":
    unittest.main()
