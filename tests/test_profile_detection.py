"""Profile detection: the audit and `make repair` must agree on which bootstrap profile a machine runs.

bootstrap/work writes DISTRO=ubuntu on purpose: the shell config branches on it (apt aliases,
update/updateall, cleanup), so DISTRO cannot name the work profile. The bootstrap records the
profile in DOTFILES_PROFILE instead, and both detectors read that before DISTRO. The detectors
are separate implementations (Python in audit_installation.py, bash in repair-installation), so
every case below runs through both.
"""

import importlib.util
import os
import pathlib
import subprocess
import sys
import tempfile
import unittest
from unittest import mock


REPO_ROOT = pathlib.Path(__file__).resolve().parents[1]
AUDIT_PATH = REPO_ROOT / ".local" / "scripts" / "audit_installation.py"
REPAIR_PATH = REPO_ROOT / ".local" / "scripts" / "repair-installation"

# (label, text of ~/.config/distro, profile both detectors must report)
DISTRO_FILES = (
    ("arch bootstrap", "#! /bin/sh\nexport DISTRO=arch\n", "arch"),
    ("ubuntu bootstrap", "#! /bin/sh\nexport DISTRO=ubuntu\n", "ubuntu"),
    (
        "work bootstrap",
        "#! /bin/sh\nexport DISTRO=ubuntu\nexport DOTFILES_PROFILE=work\n",
        "work",
    ),
    (
        "a recorded profile beats DISTRO",
        "#! /bin/sh\nexport DISTRO=arch\nexport DOTFILES_PROFILE=ubuntu\n",
        "ubuntu",
    ),
    (
        "a commented-out record is ignored",
        "#! /bin/sh\nexport DISTRO=ubuntu\n# export DOTFILES_PROFILE=work\n",
        "ubuntu",
    ),
    (
        "an empty record is ignored",
        "#! /bin/sh\nexport DISTRO=ubuntu\nexport DOTFILES_PROFILE=\n",
        "ubuntu",
    ),
)
# A typo must not silently fall back to DISTRO: that is the wrong-profile audit this prevents.
NOT_A_PROFILE = "#! /bin/sh\nexport DISTRO=ubuntu\nexport DOTFILES_PROFILE=wrok\n"


def load_audit_module():
    spec = importlib.util.spec_from_file_location("audit_installation", AUDIT_PATH)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def host_is_plain_linux() -> bool:
    """The bash detector reads the real OSTYPE and /proc/version, so it can only be run on plain Linux."""
    if not sys.platform.startswith("linux"):
        return False
    try:
        return "microsoft" not in pathlib.Path("/proc/version").read_text(errors="ignore").lower()
    except OSError:
        return True


class ProfileDetectionTests(unittest.TestCase):
    def audit_detects(self, distro_file: str) -> str:
        """What audit_installation.detect_profile() reports on a fake Linux machine."""
        audit = load_audit_module()
        with tempfile.TemporaryDirectory() as root:
            base = pathlib.Path(root)
            home = base / "home"
            (home / ".config").mkdir(parents=True)
            (home / ".config" / "distro").write_text(distro_file, encoding="utf-8")
            proc_version = base / "proc_version"
            proc_version.write_text("Linux version 6.8.0 (gcc)\n", encoding="utf-8")
            os_release = base / "os-release"
            os_release.write_text("ID=ubuntu\n", encoding="utf-8")
            with (
                mock.patch.object(pathlib.Path, "home", return_value=home),
                mock.patch.object(audit.sys, "platform", "linux"),
                mock.patch.object(audit, "PROC_VERSION", proc_version),
                mock.patch.object(audit, "OS_RELEASE", os_release),
            ):
                return audit.detect_profile()

    def repair_vim_under(self, distro_file: str) -> subprocess.CompletedProcess:
        """Run `repair-installation vim auto` in a fake repo whose profiles' vim_install name themselves.

        The script's own detection decides which profile's functions file it sources, so the
        output shows which profile it detected.
        """
        with tempfile.TemporaryDirectory() as root:
            base = pathlib.Path(root)
            scripts = base / "repo" / ".local" / "scripts"
            bootstrap = scripts / "bootstrap"
            bootstrap.mkdir(parents=True)
            (scripts / "repair-installation").write_text(
                REPAIR_PATH.read_text(encoding="utf-8"), encoding="utf-8"
            )
            (bootstrap / "base_functions").write_text("", encoding="utf-8")
            for name in ("arch_functions", "ubuntu_functions", "mac_functions", "work_functions"):
                (bootstrap / name).write_text(f"vim_install() {{ echo {name}; }}\n", encoding="utf-8")
            home = base / "home"
            (home / ".config").mkdir(parents=True)
            (home / ".config" / "distro").write_text(distro_file, encoding="utf-8")
            return subprocess.run(
                ["bash", str(scripts / "repair-installation"), "vim", "auto"],
                env={**os.environ, "HOME": str(home)},
                text=True,
                capture_output=True,
                check=False,
            )

    def test_audit_reports_the_profile_the_bootstrap_recorded(self):
        for label, distro_file, expected in DISTRO_FILES:
            with self.subTest(label):
                self.assertEqual(self.audit_detects(distro_file), expected)

    @unittest.skipUnless(host_is_plain_linux(), "the bash detector reads the host's OSTYPE and /proc/version")
    def test_repair_reports_the_same_profile_as_the_audit(self):
        for label, distro_file, expected in DISTRO_FILES:
            with self.subTest(label):
                result = self.repair_vim_under(distro_file)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout.strip(), f"{expected}_functions")

    def test_audit_rejects_a_recorded_profile_that_is_not_a_profile(self):
        with self.assertRaisesRegex(RuntimeError, "DOTFILES_PROFILE=wrok"):
            self.audit_detects(NOT_A_PROFILE)

    @unittest.skipUnless(host_is_plain_linux(), "the bash detector reads the host's OSTYPE and /proc/version")
    def test_repair_rejects_a_recorded_profile_that_is_not_a_profile(self):
        result = self.repair_vim_under(NOT_A_PROFILE)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("DOTFILES_PROFILE=wrok", result.stderr)
        self.assertEqual(result.stdout, "", "nothing may run under a guessed profile")


if __name__ == "__main__":
    unittest.main()
