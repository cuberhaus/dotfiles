"""Packages installed outside the bootstrap: what the audit reports, suggests and prints.

`make audit-installation` compares the machine with the active bootstrap in both directions. This file
covers the reverse one: a package that is installed but that no bootstrap function declares. It is a
notice (it never changes the exit code), it comes with a removal command only for packages a person
chose, and it must stay readable in a terminal of any width.

Every host command goes through a fake that answers from a table and fails the test for any command
it does not know, so a test can neither touch the real package managers nor run a mutating command.
"""

import contextlib
import gzip
import importlib.util
import io
import os
import pathlib
import re
import subprocess
import sys
import tempfile
import unittest
from unittest import mock


REPO_ROOT = pathlib.Path(__file__).resolve().parents[1]
AUDIT_PATH = REPO_ROOT / ".local" / "scripts" / "audit_installation.py"

DPKG_QUERY = ("dpkg-query", "-W", "-f=${binary:Package}\t${Essential}\t${Priority}\n")

# What /var/log/apt/history.log holds: the installer's transactions have no Requested-By line, a
# person's (through sudo) has one, and dependencies of a request are marked automatic.
APT_HISTORY_LOG = """\
Start-Date: 2026-02-10  10:00:00
Commandline: apt-get --yes install ubuntu-desktop-minimal
Install: ubuntu-desktop-minimal:amd64 (1.5), libpinyin15:amd64 (2.10, automatic)
End-Date: 2026-02-10  10:05:00

Start-Date: 2026-09-04  09:00:00
Commandline: apt-get --quiet --assume-yes install efibootmgr grub-efi-amd64
Install: efibootmgr:amd64 (18-1), grub-efi-amd64:amd64 (2.12)
End-Date: 2026-09-04  09:01:00

Start-Date: 2026-09-05  08:00:00
Commandline: apt install ffmpeg git-lfs
Requested-By: pol (1000)
Install: ffmpeg:amd64 (7.1), libavcodec61:amd64 (7.1, automatic), git-lfs:amd64 (3.6)
End-Date: 2026-09-05  08:00:30

Start-Date: 2026-09-06  08:00:00
Commandline: apt-get full-upgrade
Requested-By: pol (1000)
Upgrade: libc6:amd64 (2.40, 2.41)
Install: linux-image-7.0.0-38-generic:amd64 (7.0.0-38.38, automatic)
End-Date: 2026-09-06  08:10:00
"""

SNAP_LIST = """\
Name                       Version                         Rev    Tracking         Publisher      Notes
bare                       1.0                             5      latest/stable    canonical**    base
core22                     20260824                        2955   latest/stable    canonical**    base
desktop-security-center    0+git.4adf2b9                   206    1/stable/…       canonical**    -
discord                    1.0.160                         311    latest/stable    snapcrafters*  -
firefox                    147.0.3-1                       7766   latest/stable/…  mozilla**      -
gnome-42-2204              0+git.4982e7b-sdk0+git.69b626a  263    latest/stable/…  canonical**    -
mesa-2404                  25.2.8-snap288                  1839   latest/stable    canonical**    -
obsidian                   1.13.7                          68     latest/stable    obsidianmd     classic
prompting-client           0+git.dd8c750                   228    1/stable/…       canonical**    -
snap-store                 0+git.d402afd5                  1427   2/stable/…       canonical**    -
snapd                      2.77.1                          28254  latest/stable    canonical**    snapd
walc                       0.2.1                           19     latest/stable    cstayyab       -
"""

SNAP_SEED_YAML = """\
snaps:
  -
    name: snapd
    channel: latest/stable
  -
    name: firefox
    channel: latest/stable/ubuntu-26.04
  - name: snap-store
    channel: 2/stable
"""

# discord takes its theme and its graphics stack from content snaps.
DISCORD_SNAP_YAML = """\
name: discord
base: core22
plugs:
  gnome-42-2204:
    interface: content
    default-provider: gnome-42-2204
  graphics-core22:
    interface: content
    default-provider: mesa-2404
"""

READ_ONLY_COMMANDS = {
    ("apt-mark", "showmanual"),
    ("dpkg-query", "-W"),
    ("snap", "list"),
    ("pacman", "-Qqe"),
    ("pacman", "-Qgq"),
    ("brew", "leaves"),
    ("brew", "list"),
}


def load_audit_module():
    spec = importlib.util.spec_from_file_location("audit_installation", AUDIT_PATH)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def write_gz(path: pathlib.Path, text: str) -> None:
    path.write_bytes(gzip.compress(text.encode("utf-8")))


class AuditCase(unittest.TestCase):
    """A loaded audit module whose files and package managers are fakes."""

    def setUp(self) -> None:
        self.audit = load_audit_module()
        scratch = tempfile.TemporaryDirectory()
        self.addCleanup(scratch.cleanup)
        self.scratch = pathlib.Path(scratch.name)
        self.apt_history = self.scratch / "apt"
        self.apt_history.mkdir()
        self.snap_root = self.scratch / "snap"
        self.snap_root.mkdir()
        self.initial_status = self.scratch / "initial-status.gz"
        self.seed = self.scratch / "seed.yaml"
        self.config_home = self.scratch / "config"
        self.patch(self.audit, "APT_HISTORY", self.apt_history)
        self.patch(self.audit, "INSTALLER_INITIAL_STATUS", self.initial_status)
        self.patch(self.audit, "SNAP_SEED", self.seed)
        self.patch(self.audit, "SNAP_ROOT", self.snap_root)
        environment = mock.patch.dict(os.environ, {"XDG_CONFIG_HOME": str(self.config_home)})
        environment.start()
        self.addCleanup(environment.stop)
        self.commands: list[tuple[str, ...]] = []

    def patch(self, target, name, value):
        patcher = mock.patch.object(target, name, value)
        patcher.start()
        self.addCleanup(patcher.stop)

    def host(self, tools, outputs=None):
        """Only `tools` exist, and each command answers from `outputs` (stdout, or (stdout, status))."""
        outputs = outputs or {}

        def which(name):
            return f"/usr/bin/{name}" if name in tools else None

        def run(command, cwd=None):
            self.commands.append(tuple(command))
            try:
                answer = outputs[tuple(command)]
            except KeyError:
                raise AssertionError(f"unexpected command: {command}") from None
            stdout, status = (answer, 0) if isinstance(answer, str) else answer
            return subprocess.CompletedProcess(command, status, stdout=stdout, stderr="")

        self.patch(self.audit.shutil, "which", which)
        self.patch(self.audit, "run", run)

    def keep_file(self, text: str) -> pathlib.Path:
        path = self.audit.known_extra_packages_path()
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")
        return path


class ReadingCommandOutputTests(AuditCase):
    def test_package_lines_keeps_names_only(self) -> None:
        text = "ffmpeg\nlibfoo:i386\nWARNING: apt does not have a stable CLI interface.\n\nE: boom\n  spaced  \n"
        self.assertEqual(self.audit.package_lines(text), {"ffmpeg", "libfoo", "spaced"})

    def test_package_lines_keeps_homebrew_taps_and_versioned_formulae(self) -> None:
        self.assertEqual(
            self.audit.package_lines("user/tap/tool\npython@3.12\n"), {"user/tap/tool", "python@3.12"}
        )


class AptHistoryTests(AuditCase):
    def test_a_sudo_request_counts_and_its_dependencies_and_the_installer_do_not(self) -> None:
        (self.apt_history / "history.log").write_text(APT_HISTORY_LOG, encoding="utf-8")

        self.assertEqual(self.audit.apt_requested_packages(), {"ffmpeg", "git-lfs"})

    def test_rotated_logs_are_read_too(self) -> None:
        (self.apt_history / "history.log").write_text(APT_HISTORY_LOG, encoding="utf-8")
        write_gz(
            self.apt_history / "history.log.1.gz",
            "Start-Date: 2026-01-01  10:00:00\nRequested-By: pol (1000)\nInstall: qrencode:amd64 (4.1.1)\n",
        )

        self.assertEqual(self.audit.apt_requested_packages(), {"ffmpeg", "git-lfs", "qrencode"})

    def test_a_damaged_log_is_skipped_without_losing_the_others(self) -> None:
        (self.apt_history / "history.log").write_text(APT_HISTORY_LOG, encoding="utf-8")
        archive = gzip.compress(b"Start-Date: 2025-01-01\n" * 500)
        (self.apt_history / "history.log.2.gz").write_bytes(archive[: len(archive) // 2])  # cut short
        (self.apt_history / "history.log.3.gz").write_bytes(b"this is not gzip")

        self.assertEqual(self.audit.apt_requested_packages(), {"ffmpeg", "git-lfs"})

    def test_no_log_directory_means_no_request_is_known(self) -> None:
        self.patch(self.audit, "APT_HISTORY", self.scratch / "missing")

        self.assertEqual(self.audit.apt_requested_packages(), set())

    def test_the_installer_record_lists_every_package_it_put_on_the_machine(self) -> None:
        write_gz(
            self.initial_status,
            "Package: bash\nStatus: install ok installed\n\nPackage: grub-efi-amd64\nStatus: install ok installed\n",
        )

        self.assertEqual(self.audit.installer_initial_packages(), {"bash", "grub-efi-amd64"})

    def test_a_missing_or_damaged_installer_record_is_empty(self) -> None:
        self.assertEqual(self.audit.installer_initial_packages(), set())  # Ubuntu's own installer keeps none
        self.initial_status.write_bytes(b"not gzip")
        self.assertEqual(self.audit.installer_initial_packages(), set())


class AptInstalledByHandTests(AuditCase):
    MANUAL = "bash\nlogin\nubuntu-desktop-minimal\nefibootmgr\nffmpeg\ngit-lfs\nlibfoo:i386\nWARNING: noise\n"
    STATUS = (
        "bash\tyes\trequired\nlogin\tno\trequired\nubuntu-desktop-minimal\tno\toptional\n"
        "efibootmgr\tno\toptional\nffmpeg\tno\toptional\ngit-lfs\tno\toptional\nlibfoo:i386\tno\toptional\n"
    )

    def test_drops_what_the_os_owns_and_flags_what_nobody_requested(self) -> None:
        (self.apt_history / "history.log").write_text(APT_HISTORY_LOG, encoding="utf-8")
        write_gz(self.initial_status, "Package: ubuntu-desktop-minimal\n")
        self.host(
            {"apt-mark", "dpkg-query"},
            {("apt-mark", "showmanual"): self.MANUAL, DPKG_QUERY: self.STATUS},
        )

        installed = self.audit.apt_installed_by_hand()

        # bash is Essential, login has required priority, ubuntu-desktop-minimal came with the installer.
        self.assertEqual(installed.names, {"efibootmgr", "ffmpeg", "git-lfs", "libfoo"})
        self.assertEqual(installed.not_chosen, {"efibootmgr", "libfoo"})

    def test_without_any_log_nothing_counts_as_chosen(self) -> None:
        self.host(
            {"apt-mark", "dpkg-query"},
            {("apt-mark", "showmanual"): "ffmpeg\n", DPKG_QUERY: "ffmpeg\tno\toptional\n"},
        )

        installed = self.audit.apt_installed_by_hand()

        self.assertEqual(installed.names, {"ffmpeg"})
        self.assertEqual(installed.not_chosen, {"ffmpeg"})

    def test_none_when_apt_cannot_answer(self) -> None:
        self.host({"dpkg-query"})
        self.assertIsNone(self.audit.apt_installed_by_hand())

        self.host(
            {"apt-mark", "dpkg-query"},
            {("apt-mark", "showmanual"): ("E: broken\n", 100), DPKG_QUERY: ""},
        )
        self.assertIsNone(self.audit.apt_installed_by_hand())


class SnapInstalledByHandTests(AuditCase):
    def setUp(self) -> None:
        super().setUp()
        self.seed.write_text(SNAP_SEED_YAML, encoding="utf-8")
        manifest = self.snap_root / "discord" / "current" / "meta"
        manifest.mkdir(parents=True)
        (manifest / "snap.yaml").write_text(DISCORD_SNAP_YAML, encoding="utf-8")

    def test_drops_bases_snapd_the_image_and_dependencies_and_flags_canonical_snaps(self) -> None:
        self.host({"snap"}, {("snap", "list"): SNAP_LIST})

        installed = self.audit.snap_installed_by_hand()

        # bare and core22 are bases, snapd is snapd, firefox and snap-store came with the image,
        # gnome-42-2204 and mesa-2404 are what discord takes content from.
        self.assertEqual(
            installed.names, {"desktop-security-center", "discord", "obsidian", "prompting-client", "walc"}
        )
        self.assertEqual(installed.not_chosen, {"desktop-security-center", "prompting-client"})

    def test_an_empty_snap_list_is_not_an_error(self) -> None:
        self.host({"snap"}, {("snap", "list"): "No snaps are installed yet. Try 'snap install hello-world'.\n"})

        self.assertEqual(self.audit.snap_installed_by_hand(), self.audit.Installed(set()))

    def test_none_when_snap_cannot_answer(self) -> None:
        self.host(set())
        self.assertIsNone(self.audit.snap_installed_by_hand())

        self.host({"snap"}, {("snap", "list"): ("error: cannot communicate with server\n", 1)})
        self.assertIsNone(self.audit.snap_installed_by_hand())


class PacmanAndBrewTests(AuditCase):
    def test_pacman_drops_the_members_of_declared_groups_even_when_pacman_exits_nonzero(self) -> None:
        self.host(
            {"pacman"},
            {
                ("pacman", "-Qqe"): "base\nbase-devel\ngcc\nmake\nvim\nparu-bin\n",
                # -Qg exits 1 when one of the names is not a group, but it still prints the members.
                ("pacman", "-Qgq", "base-devel", "vim"): ("gcc\nmake\nerror: group 'vim' was not found\n", 1),
            },
        )

        installed = self.audit.pacman_installed_by_hand(["vim", "base-devel"])

        self.assertEqual(installed.names, {"base", "base-devel", "vim", "paru-bin"})

    def test_pacman_asks_about_groups_only_when_something_is_declared(self) -> None:
        self.host({"pacman"}, {("pacman", "-Qqe"): "base\nvim\n"})

        self.assertEqual(self.audit.pacman_installed_by_hand([]).names, {"base", "vim"})
        self.assertEqual(self.commands, [("pacman", "-Qqe")])

    def test_pacman_is_none_when_missing_or_failing(self) -> None:
        self.host(set())
        self.assertIsNone(self.audit.pacman_installed_by_hand([]))
        self.host({"pacman"}, {("pacman", "-Qqe"): ("", 1)})
        self.assertIsNone(self.audit.pacman_installed_by_hand([]))

    def test_brew_reports_formulae_and_casks_under_their_own_labels(self) -> None:
        self.host(
            {"brew"},
            {("brew", "leaves"): "git\nwget\n", ("brew", "list", "--cask"): "firefox\n"},
        )

        installed = self.audit.brew_installed_by_hand()

        self.assertEqual(installed["brew"].names, {"git", "wget"})
        self.assertEqual(installed["brew-cask"].names, {"firefox"})

    def test_brew_is_none_when_missing_or_failing(self) -> None:
        self.host(set())
        self.assertIsNone(self.audit.brew_installed_by_hand())
        self.host({"brew"}, {("brew", "leaves"): ("", 1), ("brew", "list", "--cask"): ""})
        self.assertIsNone(self.audit.brew_installed_by_hand())


class KeepFileTests(AuditCase):
    def test_lives_in_the_xdg_config_home(self) -> None:
        self.assertEqual(
            self.audit.known_extra_packages_path(), self.config_home / "dotfiles" / "known-extra-packages"
        )

    def test_falls_back_to_dot_config(self) -> None:
        with mock.patch.dict(os.environ):
            os.environ.pop("XDG_CONFIG_HOME")
            self.assertEqual(
                self.audit.known_extra_packages_path(),
                pathlib.Path.home() / ".config" / "dotfiles" / "known-extra-packages",
            )

    def test_reads_manager_name_pairs_bare_names_comments_and_blank_lines(self) -> None:
        path = self.keep_file(
            "# kept on purpose\n\napt:ffmpeg   # video\nyay:paru-bin\nbrew-cask:firefox\nqrencode\nnot-a-manager:name\n"
        )

        self.assertEqual(
            self.audit.known_extra_entries(path),
            {
                ("apt", "ffmpeg"),
                ("pacman", "paru-bin"),
                ("brew", "firefox"),
                (None, "qrencode"),
                (None, "not-a-manager:name"),
            },
        )

    def test_a_missing_file_lists_nothing(self) -> None:
        self.assertEqual(self.audit.known_extra_entries(self.scratch / "missing"), set())

    def test_display_path_writes_home_as_a_tilde(self) -> None:
        self.assertEqual(self.audit.display_path(pathlib.Path.home() / ".config" / "x"), "~/.config/x")
        self.assertEqual(self.audit.display_path(pathlib.Path("/etc/x")), "/etc/x")


class DeclaredSpellingTests(AuditCase):
    def test_apt_names_match_in_lowercase_and_after_the_t64_rename(self) -> None:
        spellings = self.audit.comparable_names("apt", {"LXAppearance", "libfuse2"})

        self.assertLessEqual({"LXAppearance", "lxappearance", "libfuse2", "libfuse2t64"}, spellings)

    def test_brew_names_match_without_their_tap(self) -> None:
        self.assertIn("tool", self.audit.comparable_names("brew", {"user/tap/tool"}))

    def test_other_managers_match_exactly(self) -> None:
        self.assertEqual(self.audit.comparable_names("snap", {"Discord"}), {"Discord"})


class BootstrapInstalledPackagesTests(AuditCase):
    """Packages the profile installs although expected_packages cannot read their names."""

    ENTRY = "#!/usr/bin/env bash\ninstall_browser\nasusctl_install || true\n"
    FUNCTIONS = """\
install_browser() {
    local chrome_deb="/tmp/chrome.deb"
    # local warp_deb="/tmp/warp.deb"
    wget -O "$chrome_deb" https://example.invalid/chrome.deb
    sudo apt-get install -y "$chrome_deb"
    # sudo apt-get install -y "$warp_deb"
}

install_unused() {
    local rstudio_deb="/tmp/rstudio.deb"
    sudo dpkg -i "$rstudio_deb"
}
"""
    INSTALLER = """\
#!/usr/bin/env bash
readonly BUILD_PACKAGES=(
    ca-certificates git make   # tools
    build-essential
    "pkg-config"
)
OTHER=(not-this)
"""

    def repo(self, entry: str = ENTRY, installer: str | None = INSTALLER) -> pathlib.Path:
        scripts = self.scratch / "repo" / ".local" / "scripts"
        (scripts / "bootstrap").mkdir(parents=True, exist_ok=True)
        (scripts / "bootstrap" / "work").write_text(entry, encoding="utf-8")
        (scripts / "bootstrap" / "work_functions").write_text(self.FUNCTIONS, encoding="utf-8")
        if installer is not None:
            (scripts / "asusctl_install.sh").write_text(installer, encoding="utf-8")
        return self.scratch / "repo"

    def test_a_deb_installed_through_a_variable_is_named_by_its_package(self) -> None:
        packages = self.audit.bootstrap_installed_packages(self.repo(), "work")

        self.assertIn(self.audit.Package("apt", "google-chrome-stable"), packages)

    def test_commented_out_and_uncalled_installs_name_nothing(self) -> None:
        names = {package.name for package in self.audit.bootstrap_installed_packages(self.repo(), "work")}

        self.assertNotIn("warp-terminal", names)  # its install line is a comment
        self.assertNotIn("rstudio", names)  # install_unused is never called

    def test_a_standalone_installer_the_profile_calls_contributes_its_array(self) -> None:
        names = {package.name for package in self.audit.bootstrap_installed_packages(self.repo(), "work")}

        self.assertLessEqual(
            {"ca-certificates", "git", "make", "build-essential", "pkg-config"}, names
        )
        self.assertNotIn("not-this", names)

    def test_a_standalone_installer_the_profile_does_not_call_contributes_nothing(self) -> None:
        entry = "#!/usr/bin/env bash\ninstall_browser\n# asusctl_install\n"

        names = {package.name for package in self.audit.bootstrap_installed_packages(self.repo(entry), "work")}

        self.assertNotIn("build-essential", names)

    def test_script_array_reads_words_across_lines_and_ignores_the_rest(self) -> None:
        path = self.scratch / "installer.sh"
        path.write_text(self.INSTALLER, encoding="utf-8")

        self.assertEqual(
            self.audit.script_array(path, "BUILD_PACKAGES"),
            ["ca-certificates", "git", "make", "build-essential", "pkg-config"],
        )
        self.assertEqual(self.audit.script_array(path, "MISSING"), [])
        self.assertEqual(self.audit.script_array(self.scratch / "nope.sh", "BUILD_PACKAGES"), [])


class RepositoryContractTests(unittest.TestCase):
    """The tables in the audit must keep up with the files they describe."""

    def setUp(self) -> None:
        self.audit = load_audit_module()

    def test_every_deb_variable_in_the_bootstrap_has_a_package_name(self) -> None:
        # A .deb installed through "$name_deb" is invisible to expected_packages. Without an entry
        # in DEB_FILE_PACKAGES its package would be reported as an extra on every run.
        variables = set()
        for path in (REPO_ROOT / ".local" / "scripts" / "bootstrap").glob("*_functions"):
            variables.update(re.findall(r"^\s*#?\s*local\s+(\w*_deb)=", path.read_text(encoding="utf-8"), re.M))

        self.assertTrue(variables, "no *_deb variable found: the pattern no longer matches the bootstrap")
        self.assertEqual(variables - set(self.audit.DEB_FILE_PACKAGES), set())

    def test_standalone_installers_exist_and_their_arrays_are_readable(self) -> None:
        base_functions = self.audit.function_bodies(
            (REPO_ROOT / ".local" / "scripts" / "bootstrap" / "base_functions").read_text(encoding="utf-8")
        )
        for function, script, array in self.audit.STANDALONE_INSTALLERS:
            with self.subTest(function=function):
                self.assertIn(function, base_functions)
                words = self.audit.script_array(REPO_ROOT / ".local" / "scripts" / script, array)
                self.assertTrue(words)
                self.assertTrue(all(self.audit.PACKAGE_LINE.fullmatch(word) for word in words), words)

    def test_every_label_a_manager_can_report_has_a_group_and_a_removal_command(self) -> None:
        for label in ("apt", "snap", "pacman", "brew", "brew-cask"):
            with self.subTest(label=label):
                self.assertIn(label, self.audit.MANAGER_GROUPS)
                self.assertIn(label, self.audit.REMOVE_COMMANDS)

    def test_no_profile_declares_an_apt_name_with_capitals(self) -> None:
        # Debian package names are lowercase, and apt-get aborts the whole transaction on a name it
        # cannot find. "LXAppearance" in ubuntu_install therefore kept every other package of that
        # one call from being installed.
        for profile in self.audit.PROFILE_SOURCES:
            with self.subTest(profile=profile):
                names = {
                    package.name
                    for package in self.audit.expected_packages(REPO_ROOT, profile)
                    if package.manager == "apt"
                }
                self.assertEqual(sorted(name for name in names if name != name.lower()), [])

    def test_removal_commands_ask_before_acting_and_never_go_through_yay(self) -> None:
        self.assertEqual(
            self.audit.REMOVE_COMMANDS,
            {
                "apt": "sudo apt-get remove",
                "snap": "sudo snap remove",
                "pacman": "sudo pacman -Rs",
                "brew": "brew uninstall",
                "brew-cask": "brew uninstall --cask",
            },
        )
        forbidden = {"-y", "--yes", "--assume-yes", "--noconfirm", "--purge", "-f", "--force", "autoremove", "yay"}
        for label, command in self.audit.REMOVE_COMMANDS.items():
            with self.subTest(label=label):
                self.assertTrue(forbidden.isdisjoint(command.split()), command)

    def test_the_real_bootstrap_declares_nothing_it_would_then_report_as_extra(self) -> None:
        # Round trip with the repository's own work profile: a machine on which exactly the
        # declared packages are installed by hand has no extras, and one more package is one.
        audit = self.audit
        profile = "work"
        packages = audit.expected_packages(REPO_ROOT, profile)
        declared = packages | audit.bootstrap_installed_packages(REPO_ROOT, profile)
        apt = sorted({package.name for package in declared if package.manager == "apt"})
        snaps = sorted({package.name for package in packages if package.manager == "snap"})
        rows = "".join(f"{name} 1.0 1 latest/stable someone -\n" for name in snaps)
        scratch = tempfile.TemporaryDirectory()
        self.addCleanup(scratch.cleanup)
        root = pathlib.Path(scratch.name)

        def run(command, cwd=None):
            outputs = {
                ("apt-mark", "showmanual"): "\n".join([*apt, "zzz-extra-tool"]) + "\n",
                DPKG_QUERY: "",
                ("snap", "list"): "Name Version Rev Tracking Publisher Notes\n" + rows,
            }
            return subprocess.CompletedProcess(command, 0, stdout=outputs[tuple(command)], stderr="")

        with (
            mock.patch.object(audit, "run", run),
            mock.patch.object(audit.shutil, "which", lambda name: f"/usr/bin/{name}"),
            mock.patch.object(audit, "APT_HISTORY", root / "apt"),
            mock.patch.object(audit, "INSTALLER_INITIAL_STATUS", root / "none.gz"),
            mock.patch.object(audit, "SNAP_SEED", root / "seed.yaml"),
            mock.patch.object(audit, "SNAP_ROOT", root / "snap"),
            mock.patch.dict(os.environ, {"XDG_CONFIG_HOME": str(root / "config")}),
        ):
            extras = audit.find_extra_packages(REPO_ROOT, profile, packages)

        self.assertEqual(extras.found["apt"], ["zzz-extra-tool"])
        self.assertEqual(extras.found["snap"], [])


class FindExtraPackagesTests(AuditCase):
    def setUp(self) -> None:
        super().setUp()
        self.patch(self.audit, "bootstrap_installed_packages", lambda repo_root, profile: set())
        self.asked: list[tuple[str, frozenset[str]]] = []
        self.installed: dict = {}

    def fake_installed_by_hand(self, group, declared):
        self.asked.append((group, frozenset(declared)))
        installed = self.installed.get(group)
        # The real function answers {label: Installed}; one label per manager except brew.
        return {group: installed} if isinstance(installed, self.audit.Installed) else installed

    def find(self, *packages):
        self.patch(self.audit, "installed_by_hand", self.fake_installed_by_hand)
        return self.audit.find_extra_packages(REPO_ROOT, "work", set(packages))

    def test_reports_what_is_installed_and_not_declared(self) -> None:
        self.installed["apt"] = self.audit.Installed({"git", "ffmpeg", "xdotool"}, not_chosen={"xdotool"})

        extras = self.find(self.audit.Package("apt", "git"))

        self.assertEqual(extras.found, {"apt": ["ffmpeg", "xdotool"]})
        self.assertEqual(extras.not_chosen, {"apt": {"xdotool"}})

    def test_a_declared_name_matches_under_the_spellings_apt_uses(self) -> None:
        # A declaration spelled with capitals (ubuntu_functions once had LXAppearance) must still
        # match the lowercase name dpkg reports.
        self.installed["apt"] = self.audit.Installed({"lxappearance", "libfuse2t64", "ffmpeg"})

        extras = self.find(self.audit.Package("apt", "LXAppearance"), self.audit.Package("apt", "libfuse2"))

        self.assertEqual(extras.found, {"apt": ["ffmpeg"]})

    def test_asks_only_the_managers_the_profile_declares_packages_for(self) -> None:
        self.installed["apt"] = self.audit.Installed(set())
        self.installed["snap"] = self.audit.Installed(set())

        self.find(self.audit.Package("apt", "git"), self.audit.Package("snap", "discord"))

        self.assertEqual({group for group, _ in self.asked}, {"apt", "snap"})

    def test_pacman_and_yay_share_one_query_and_one_set_of_declared_names(self) -> None:
        self.installed["pacman"] = self.audit.Installed({"git", "paru-bin", "extra"})

        extras = self.find(self.audit.Package("pacman", "git"), self.audit.Package("yay", "paru-bin"))

        self.assertEqual(self.asked, [("pacman", frozenset({"git", "paru-bin"}))])
        self.assertEqual(extras.found, {"pacman": ["extra"]})

    def test_brew_reports_formulae_and_casks_separately_and_matches_without_the_tap(self) -> None:
        self.installed["brew"] = {
            "brew": self.audit.Installed({"tool", "wget"}),
            "brew-cask": self.audit.Installed({"firefox", "slack"}),
        }

        extras = self.find(
            self.audit.Package("brew", "user/tap/tool"), self.audit.Package("brew-cask", "firefox")
        )

        self.assertEqual(extras.found, {"brew": ["wget"], "brew-cask": ["slack"]})

    def test_a_manager_that_cannot_be_read_is_left_out_of_the_report(self) -> None:
        self.installed["snap"] = self.audit.Installed({"discord"})

        extras = self.find(self.audit.Package("apt", "git"), self.audit.Package("snap", "obsidian"))

        self.assertEqual(extras.found, {"snap": ["discord"]})

    def test_the_keep_file_accepts_a_package_under_its_manager_or_any_manager(self) -> None:
        self.keep_file("apt:ffmpeg\nqrencode\nsnap:xdotool\n")
        self.installed["apt"] = self.audit.Installed({"ffmpeg", "qrencode", "xdotool", "git-lfs"})

        extras = self.find(self.audit.Package("apt", "git"))

        # snap:xdotool does not accept the apt package of the same name.
        self.assertEqual(extras.found, {"apt": ["git-lfs", "xdotool"]})
        self.assertEqual(extras.acknowledged, 2)

    def test_the_keep_file_counts_each_accepted_package_once_per_manager(self) -> None:
        self.keep_file("apt:ffmpeg\nbrew:wget\n")
        self.installed["apt"] = self.audit.Installed({"ffmpeg"})
        self.installed["brew"] = {"brew": self.audit.Installed({"wget", "jq"}), "brew-cask": self.audit.Installed(set())}

        extras = self.find(self.audit.Package("apt", "git"), self.audit.Package("brew", "git"))

        self.assertEqual(extras.found, {"apt": [], "brew": ["jq"], "brew-cask": []})
        self.assertEqual(extras.acknowledged, 2)

    def test_the_queries_are_read_only(self) -> None:
        (self.apt_history / "history.log").write_text(APT_HISTORY_LOG, encoding="utf-8")
        self.seed.write_text(SNAP_SEED_YAML, encoding="utf-8")
        self.host(
            {"apt-mark", "dpkg-query", "snap", "pacman", "brew"},
            {
                ("apt-mark", "showmanual"): "ffmpeg\n",
                DPKG_QUERY: "ffmpeg\tno\toptional\n",
                ("snap", "list"): SNAP_LIST,
                ("pacman", "-Qqe"): "vim\n",
                ("pacman", "-Qgq", "git"): "",
                ("brew", "leaves"): "wget\n",
                ("brew", "list", "--cask"): "firefox\n",
            },
        )

        self.audit.find_extra_packages(
            REPO_ROOT,
            "work",
            {
                self.audit.Package("apt", "git"),
                self.audit.Package("snap", "discord"),
                self.audit.Package("pacman", "git"),
                self.audit.Package("brew", "git"),
            },
        )

        self.assertTrue(self.commands)
        for command in self.commands:
            with self.subTest(command=command):
                self.assertIn(command[:2], READ_ONLY_COMMANDS)
                self.assertNotIn("sudo", command)


class SplitRemovableTests(AuditCase):
    def split(self, label, names, not_chosen=()):
        return self.audit.split_removable(label, names, set(not_chosen))

    def test_apt_keeps_kernel_boot_firmware_driver_and_os_packages_out_of_the_command(self) -> None:
        names = [
            "ffmpeg",
            "linux-generic",
            "grub-efi-amd64-signed",
            "shim-signed",
            "efibootmgr",
            "ubuntu-restricted-addons",
            "intel-microcode",
            "linux-firmware",
            "nvidia-driver-595-open",
            "xserver-xorg-video-nouveau",
            "git-lfs",
        ]

        removable, left_out = self.split("apt", names)

        self.assertEqual(removable, ["ffmpeg", "git-lfs"])
        self.assertEqual(left_out, [name for name in names if name not in removable])

    def test_user_tools_with_a_driver_or_xorg_sounding_name_stay_removable(self) -> None:
        names = ["nvidia-container-toolkit", "xserver-xephyr", "xdotool"]

        self.assertEqual(self.split("apt", names), (names, []))

    def test_a_package_with_no_sign_of_being_chosen_stays_out_whatever_its_name(self) -> None:
        removable, left_out = self.split("apt", ["language-pack-en", "qrencode"], not_chosen={"language-pack-en"})

        self.assertEqual((removable, left_out), (["qrencode"], ["language-pack-en"]))

    def test_snap_follows_not_chosen_and_has_no_name_patterns(self) -> None:
        removable, left_out = self.split(
            "snap", ["desktop-security-center", "linux-tool", "sublime-merge"], not_chosen={"desktop-security-center"}
        )

        self.assertEqual((removable, left_out), (["linux-tool", "sublime-merge"], ["desktop-security-center"]))

    def test_pacman_keeps_the_base_system_out(self) -> None:
        removable, left_out = self.split(
            "pacman", ["base", "linux", "linux-lts", "intel-ucode", "mkinitcpio", "nvidia-utils", "vim", "paru-bin"]
        )

        self.assertEqual(removable, ["vim", "paru-bin"])
        self.assertEqual(len(left_out), 6)

    def test_brew_and_casks_are_never_left_out(self) -> None:
        self.assertEqual(self.split("brew", ["wget"]), (["wget"], []))
        self.assertEqual(self.split("brew-cask", ["firefox"]), (["firefox"], []))

    def test_order_is_kept(self) -> None:
        removable, left_out = self.split("apt", ["z-tool", "linux-generic", "a-tool", "grub-pc"])

        self.assertEqual((removable, left_out), (["z-tool", "a-tool"], ["linux-generic", "grub-pc"]))


class WrappingTests(AuditCase):
    def test_output_width_follows_the_terminal_within_sane_bounds(self) -> None:
        for columns, expected in ((200, 100), (80, 80), (40, 60)):
            with self.subTest(columns=columns):
                with mock.patch.object(
                    self.audit.shutil, "get_terminal_size", return_value=os.terminal_size((columns, 24))
                ):
                    self.assertEqual(self.audit.output_width(), expected)

    def test_wrap_never_splits_a_hyphenated_name(self) -> None:
        names = ["nvidia-container-toolkit", "language-pack-gnome-en-base", "qrencode", "ibus-table-cangjie-big"]

        lines = self.audit.wrap(", ".join(names), 30, "  ")

        self.assertGreater(len(lines), 1)
        self.assertTrue(all(line.startswith("  ") for line in lines))
        for name in names:
            self.assertEqual(sum(name in line for line in lines), 1, name)

    def test_command_lines_continue_with_backslashes_and_run_as_one_command(self) -> None:
        words = ["sudo", "apt-get", "remove", *(f"package-number-{number}" for number in range(12))]

        lines = self.audit.command_lines(words, 40)

        self.assertGreater(len(lines), 1)
        self.assertTrue(all(len(line) <= 40 for line in lines), lines)
        self.assertTrue(all(line.endswith(" \\") for line in lines[:-1]))
        self.assertFalse(lines[-1].endswith("\\"))
        # What a shell makes of the lines is the original command.
        self.assertEqual(re.sub(r" \\\n\s*", " ", "\n".join(lines)).split(), words)

    def test_command_lines_quote_what_a_shell_would_split(self) -> None:
        self.assertEqual(self.audit.command_lines(["sudo", "snap", "remove", "odd name"], 80), ["sudo snap remove 'odd name'"])

    def test_a_short_command_stays_on_one_line(self) -> None:
        self.assertEqual(self.audit.command_lines(["sudo", "snap", "remove", "x"], 80), ["sudo snap remove x"])

    def test_a_message_can_span_lines_and_the_later_ones_hang_under_the_text(self) -> None:
        reporter = self.audit.Reporter()
        output = io.StringIO()

        with contextlib.redirect_stdout(output):
            reporter.result("NOTICE", "first line\nsecond line", ["cmd one \\", "  cmd two"])

        self.assertEqual(
            output.getvalue().splitlines(),
            [
                "  [NOTICE] first line",
                "           second line",
                "         Fix: cmd one \\",
                "                cmd two",
            ],
        )
        self.assertEqual((reporter.notices, reporter.warnings, reporter.issues), (1, 0, 0))


class ReportTests(AuditCase):
    """What audit_extra_packages prints, with the findings handed to it."""

    WORK_FUNCTIONS = "work_functions"

    def setUp(self) -> None:
        super().setUp()
        self.patch(
            self.audit,
            "known_extra_packages_path",
            lambda: pathlib.Path.home() / ".config" / "dotfiles" / "known-extra-packages",
        )
        self.width = 80
        self.patch(self.audit, "output_width", lambda: self.width)

    def report(self, extras, profile: str = "work"):
        self.patch(self.audit, "find_extra_packages", lambda repo_root, profile, packages: extras)
        reporter = self.audit.Reporter()
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            self.audit.audit_extra_packages(REPO_ROOT, profile, set(), reporter)
        return reporter, output.getvalue()

    def extras(self, found, not_chosen=None, acknowledged=0):
        return self.audit.ExtraPackages(found=found, not_chosen=not_chosen or {}, acknowledged=acknowledged)

    @staticmethod
    def flat(output: str) -> str:
        """The text on one line, for assertions about prose that wraps at the terminal width."""
        return " ".join(output.split())

    def test_the_layout_at_80_columns(self) -> None:
        reporter, output = self.report(
            self.extras(
                {"apt": ["efibootmgr", "ffmpeg", "git-lfs", "language-pack-en"]},
                {"apt": {"efibootmgr", "language-pack-en"}},
            )
        )

        self.assertEqual(
            output,
            """
Packages installed outside the bootstrap
  [NOTICE] 2 apt package(s) are not declared by the work bootstrap:
             ffmpeg, git-lfs
         Fix: sudo apt-get remove ffmpeg git-lfs
  [NOTICE] 2 more apt package(s) are not declared either. No removal command is
           suggested, because they are kernel, boot loader, firmware or driver
           packages, or the apt log shows no request from you to install them:
             efibootmgr, language-pack-en
  Nothing was changed. For each package you can:
    - remove it, with the Fix command above where one is given;
    - declare it in .local/scripts/bootstrap/work_functions, to make it part of
      the bootstrap;
    - or accept it by listing manager:name in
      ~/.config/dotfiles/known-extra-packages (`--list-extra` prints that
      format).
""",
        )
        self.assertEqual((reporter.notices, reporter.warnings, reporter.issues), (2, 0, 0))

    def test_no_line_is_longer_than_the_width_and_no_name_is_printed_twice_by_one_notice(self) -> None:
        # Zero-padded so that no name is a prefix of another and a count of one name is exact.
        chosen = [f"chosen-package-with-a-long-name-{number:02d}" for number in range(30)]
        os_names = [f"linux-image-{number:02d}" for number in range(30)]
        extras = self.extras({"apt": sorted(chosen + os_names)}, {"apt": set(os_names)})
        for width in (60, 80, 100):
            self.width = width
            with self.subTest(width=width):
                _, output = self.report(extras)

                for line in output.splitlines():
                    self.assertLessEqual(len(line), width, line)
                # A chosen name is listed once and once more in the command; an OS name only once.
                for name in chosen:
                    self.assertEqual(output.count(name), 2, name)
                for name in os_names:
                    self.assertEqual(output.count(name), 1, name)

    def test_headline_and_names_hang_under_the_tag(self) -> None:
        self.width = 60
        _, output = self.report(self.extras({"apt": [f"package-{number:02d}" for number in range(12)]}))
        lines = output.splitlines()
        headline = next(index for index, line in enumerate(lines) if "[NOTICE]" in line)
        fix = next(index for index, line in enumerate(lines) if line.startswith(self.audit.FIX_PREFIX))
        # At 60 columns the headline itself wraps; everything between it and the command is
        # either the rest of the headline or the names.
        shown = lines[headline + 1 : fix]
        names = [line for line in shown if "package-" in line]
        headline_rest = [line for line in shown if "package-" not in line]

        self.assertTrue(lines[headline].startswith("  [NOTICE] "))
        self.assertTrue(headline_rest, "the headline should wrap at 60 columns")
        self.assertTrue(all(re.match(r" {11}\S", line) for line in headline_rest), headline_rest)
        self.assertTrue(names)
        self.assertTrue(all(line.startswith(" " * 13 + "package-") for line in names), names)

    def test_the_fix_command_is_one_pasteable_command(self) -> None:
        self.width = 60
        names = [f"package-{number:02d}" for number in range(12)]
        _, output = self.report(self.extras({"apt": names}))
        lines = output.splitlines()
        start = next(index for index, line in enumerate(lines) if line.startswith(self.audit.FIX_PREFIX))
        block = [lines[start]]
        while block[-1].endswith("\\"):
            block.append(lines[start + len(block)])

        self.assertGreater(len(block), 1)
        self.assertTrue(block[0].startswith(self.audit.FIX_PREFIX + "sudo apt-get remove "))
        self.assertTrue(all(line.startswith(" " * len(self.audit.FIX_PREFIX)) for line in block[1:]))
        # What a shell makes of the block (without the "Fix:" label) is the single command.
        text = "\n".join([block[0][len(self.audit.FIX_PREFIX) :], *block[1:]])
        self.assertEqual(re.sub(r" \\\n\s*", " ", text).split(), ["sudo", "apt-get", "remove", *names])

    def test_each_manager_gets_its_own_removal_command(self) -> None:
        _, output = self.report(
            self.extras(
                {
                    "apt": ["a"],
                    "snap": ["b"],
                    "pacman": ["c"],
                    "brew": ["d"],
                    "brew-cask": ["e"],
                }
            )
        )

        for command in (
            "sudo apt-get remove a",
            "sudo snap remove b",
            "sudo pacman -Rs c",
            "brew uninstall d",
            "brew uninstall --cask e",
        ):
            self.assertIn(f"Fix: {command}\n", output)

    def test_when_nothing_looks_chosen_no_command_is_suggested(self) -> None:
        reporter, output = self.report(
            self.extras({"snap": ["desktop-security-center", "prompting-client"]}, {"snap": {"desktop-security-center", "prompting-client"}})
        )

        self.assertNotIn("Fix:", output)
        self.assertNotIn("sudo", output)
        self.assertIn(
            "2 snap package(s) are not declared by the work bootstrap. No removal command is suggested",
            self.flat(output),
        )
        self.assertIn("Canonical publishes them", self.flat(output))
        self.assertEqual(reporter.notices, 1)

    def test_a_manager_without_extras_is_ok(self) -> None:
        reporter, output = self.report(self.extras({"apt": [], "snap": ["x"]}))

        self.assertIn("[OK] Every apt package installed by hand is declared by the work bootstrap or accepted.", output)
        self.assertEqual((reporter.notices, reporter.issues, reporter.warnings), (1, 0, 0))

    def test_nothing_is_printed_when_no_package_manager_could_be_read(self) -> None:
        reporter, output = self.report(self.extras({}))

        self.assertEqual(output, "")
        self.assertEqual(reporter.notices, 0)

    def test_the_footer_appears_only_when_there_is_something_to_decide(self) -> None:
        _, output = self.report(self.extras({"apt": []}))

        self.assertNotIn("Nothing was changed", output)

    def test_accepted_packages_are_counted_even_when_nothing_else_is_left(self) -> None:
        _, output = self.report(self.extras({"apt": []}, acknowledged=3))

        self.assertIn(
            "3 package(s) accepted in ~/.config/dotfiles/known-extra-packages are not reported.",
            self.flat(output),
        )
        self.assertNotIn("Nothing was changed", output)

    def test_the_footer_names_the_functions_file_of_the_profile(self) -> None:
        _, output = self.report(self.extras({"apt": ["x"]}), profile="ubuntu")

        self.assertIn(".local/scripts/bootstrap/ubuntu_functions", output)

    def test_notices_are_not_issues_or_warnings(self) -> None:
        # apt gives two notices (a removable package, and an OS package left out of the command),
        # snap one (its only package has no sign of being chosen).
        reporter, _ = self.report(self.extras({"apt": ["a", "linux-generic"], "snap": ["b"]}, {"snap": {"b"}}))

        self.assertEqual((reporter.notices, reporter.issues, reporter.warnings), (3, 0, 0))


@unittest.skipUnless(sys.platform.startswith(("linux", "darwin")), "main() audits Linux and macOS hosts only")
class MainTests(AuditCase):
    AUDITS = (
        "audit_git",
        "audit_stow",
        "audit_shell_configuration",
        "audit_environment",
        "audit_git_configuration",
        "audit_editors_and_fonts",
        "audit_packages",
        "audit_ide_update_channels",
        "audit_automations",
    )

    def run_main(self, *arguments: str, extras=None, before=None):
        """Run main() with every other audit stubbed out; return (exit code, stdout)."""
        for name in self.AUDITS:
            self.patch(self.audit, name, mock.MagicMock(name=name))
        if before:
            self.patch(self.audit, "audit_git", before)
        self.patch(self.audit, "audit_extra_packages", extras or mock.MagicMock(name="audit_extra_packages"))
        output = io.StringIO()
        with (
            mock.patch.object(sys, "argv", ["audit_installation.py", "--profile", "work", *arguments]),
            contextlib.redirect_stdout(output),
        ):
            code = self.audit.main()
        return code, output.getvalue()

    @staticmethod
    def notices(count: int):
        def audit(repo_root, profile, packages, reporter):
            for number in range(count):
                reporter.result("NOTICE", f"extra {number}")

        return audit

    def test_notices_do_not_change_the_exit_code(self) -> None:
        code, output = self.run_main(extras=self.notices(2))

        self.assertEqual(code, 0)
        self.assertIn("Installation is aligned (0 warning(s), 2 notice(s)).", output)

    def test_the_summary_is_unchanged_when_there_are_no_notices(self) -> None:
        code, output = self.run_main()

        self.assertEqual(code, 0)
        self.assertIn("Installation is aligned (0 warning(s)).", output)

    def test_an_issue_still_fails_the_audit_and_notices_are_still_counted(self) -> None:
        code, output = self.run_main(
            extras=self.notices(1), before=lambda repo_root, reporter: reporter.result("MISSING", "broken")
        )

        self.assertEqual(code, 1)
        self.assertIn("Installation needs attention: 1 issue(s), 0 warning(s), 1 notice(s).", output)

    def test_the_extras_audit_runs_after_the_package_checks(self) -> None:
        calls: list[str] = []
        for name in ("audit_packages", "audit_ide_update_channels", "audit_automations"):
            self.patch(self.audit, name, lambda *args, _name=name: calls.append(_name))
        for name in set(self.AUDITS) - {"audit_packages", "audit_ide_update_channels", "audit_automations"}:
            self.patch(self.audit, name, mock.MagicMock(name=name))
        self.patch(self.audit, "audit_extra_packages", lambda *args: calls.append("audit_extra_packages"))
        with (
            mock.patch.object(sys, "argv", ["audit_installation.py", "--profile", "work"]),
            contextlib.redirect_stdout(io.StringIO()),
        ):
            self.audit.main()

        self.assertEqual(calls, ["audit_packages", "audit_ide_update_channels", "audit_extra_packages", "audit_automations"])

    def test_list_extra_prints_manager_name_pairs_and_audits_nothing_else(self) -> None:
        self.patch(
            self.audit,
            "find_extra_packages",
            lambda repo_root, profile, packages: self.audit.ExtraPackages(
                found={"apt": ["ffmpeg", "xdotool"], "snap": [], "brew-cask": ["slack"]}
            ),
        )
        for name in self.AUDITS:
            self.patch(self.audit, name, mock.MagicMock(side_effect=AssertionError(f"{name} must not run")))
        output = io.StringIO()
        with (
            mock.patch.object(sys, "argv", ["audit_installation.py", "--profile", "work", "--list-extra"]),
            contextlib.redirect_stdout(output),
        ):
            code = self.audit.main()

        self.assertEqual(code, 0)
        self.assertEqual(output.getvalue(), "apt:ffmpeg\napt:xdotool\nbrew-cask:slack\n")

    def test_list_extra_output_is_what_the_keep_file_reads_back(self) -> None:
        listing = "apt:ffmpeg\napt:xdotool\nbrew-cask:slack\n"
        path = self.keep_file(listing)

        self.assertEqual(
            self.audit.known_extra_entries(path), {("apt", "ffmpeg"), ("apt", "xdotool"), ("brew", "slack")}
        )


if __name__ == "__main__":
    unittest.main()
