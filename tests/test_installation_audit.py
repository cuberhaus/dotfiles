import contextlib
import importlib.util
import io
import pathlib
import subprocess
import sys
import tempfile
import unittest
from unittest import mock


REPO_ROOT = pathlib.Path(__file__).resolve().parents[1]
AUDIT_PATH = REPO_ROOT / ".local" / "scripts" / "audit_installation.py"
CONFIG_LIFECYCLE_PATH = REPO_ROOT / ".local" / "scripts" / "config_lifecycle.py"


def load_audit_module():
    spec = importlib.util.spec_from_file_location("audit_installation", AUDIT_PATH)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


def load_config_lifecycle_module():
    spec = importlib.util.spec_from_file_location("config_lifecycle", CONFIG_LIFECYCLE_PATH)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class InstallationAuditContractTests(unittest.TestCase):
    def test_config_lifecycle_diffs_only_managed_existing_targets(self):
        lifecycle = load_config_lifecycle_module()
        import tempfile

        with tempfile.TemporaryDirectory() as root:
            root_path = pathlib.Path(root)
            repo = root_path / "repo"
            home = root_path / "home"
            repo.mkdir()
            home.mkdir()
            (repo / ".stow-local-ignore").write_text("^/README.*\n", encoding="utf-8")
            (repo / ".zshenv").write_text("repo\n", encoding="utf-8")
            (home / ".zshenv").write_text("home\n", encoding="utf-8")
            (repo / "README.md").write_text("repo docs\n", encoding="utf-8")
            (home / "README.md").write_text("home docs\n", encoding="utf-8")

            differences = lifecycle.managed_differences(
                repo, home, [pathlib.Path(".zshenv"), pathlib.Path("README.md")]
            )

        self.assertEqual(len(differences), 1)
        self.assertIn("home/.zshenv", differences[0])

    def test_make_exposes_config_and_maintenance_observability(self):
        makefile = (REPO_ROOT / "Makefile").read_text(encoding="utf-8")
        for target in (
            "config-status:",
            "config-diff:",
            "config-import:",
            "maintenance-status:",
            "maintenance-logs:",
            "maintenance-digest:",
        ):
            self.assertIn(target, makefile)
        self.assertIn("gitleaks.sh", makefile)

    def test_deep_audit_covers_environment_git_shell_editors_and_fonts(self):
        audit = load_audit_module()
        expected_paths = audit.expected_shell_paths(pathlib.Path("/mock/user"))

        self.assertEqual(
            expected_paths,
            (
                pathlib.Path("/mock/user/.local/bin"),
                pathlib.Path("/mock/user/.local/scripts/bin"),
            ),
        )
        source = AUDIT_PATH.read_text(encoding="utf-8")
        for function in (
            "audit_shell_configuration",
            "audit_environment",
            "audit_git_configuration",
            "audit_editors_and_fonts",
        ):
            self.assertIn(f"{function}(", source)

        makefile = (REPO_ROOT / "Makefile").read_text(encoding="utf-8")
        self.assertIn('doctor.sh "$(PROFILE)"', makefile)

    def test_guarded_app_restore_is_preview_first_and_bootstrap_integrated(self):
        makefile = (REPO_ROOT / "Makefile").read_text(encoding="utf-8")
        restore = (REPO_ROOT / ".local" / "scripts" / "restore-app-data").read_text(
            encoding="utf-8"
        )

        self.assertIn("restore-app:", makefile)
        self.assertIn("restore-apps:", makefile)
        self.assertEqual(makefile.count("restore-apps\n"), 6)
        for app in ("thunderbird", "calibre", "anki"):
            self.assertIn(f"{app})", restore)
        for guard in ("rclone listremotes", "command -v pgrep", "rclone lsf", "--dry-run"):
            self.assertIn(guard, restore)

    def test_make_exposes_allowlisted_repair_target(self):
        makefile = (REPO_ROOT / "Makefile").read_text(encoding="utf-8")
        repair = (REPO_ROOT / ".local" / "scripts" / "repair-installation").read_text(
            encoding="utf-8"
        )

        self.assertIn("repair:", makefile)
        self.assertIn('repair-installation "$(REPAIR)" "$(PROFILE)"', makefile)
        for step in (
            "config",
            "aliases",
            "environment",
            "vim",
            "automations",
            "keyboard",
            "ide-repos",
        ):
            self.assertIn(step, repair)

    def test_unattended_bootstrap_choices_are_deterministic(self):
        makefile = (REPO_ROOT / "Makefile").read_text(encoding="utf-8")
        source = (
            REPO_ROOT / ".local" / "scripts" / "bootstrap" / "base_functions"
        ).read_text(encoding="utf-8")

        self.assertIn("BOOTSTRAP_ARGS ?= --unattended", makefile.splitlines())
        for option in ("--unattended)", "--high-dpi=yes|--high-dpi=no)"):
            self.assertIn(option, source)
        self.assertIn("HIGH_DPI=false", source)

    def test_bootstrap_derives_one_time_setup_from_machine_state(self):
        bootstrap_dir = REPO_ROOT / ".local" / "scripts" / "bootstrap"
        makefile = (REPO_ROOT / "Makefile").read_text(encoding="utf-8")
        sources = {
            path.name: path.read_text(encoding="utf-8")
            for path in bootstrap_dir.iterdir()
            if path.is_file()
        }

        self.assertNotIn("FIRST_RUN", makefile)
        for name, source in sources.items():
            self.assertNotIn("FirstRun", source, name)
            self.assertNotIn("FIRST_RUN", source, name)
            self.assertNotIn("--first-run", source, name)

        arch_functions = sources["arch_functions"]
        ubuntu_functions = sources["ubuntu_functions"]
        self.assertIn("laptop-detect", arch_functions)
        self.assertIn("systemctl enable --now", arch_functions)
        self.assertIn('id -nG "$user"', ubuntu_functions)

    def test_every_bootstrap_entrypoint_parses_shared_arguments(self):
        bootstrap_dir = REPO_ROOT / ".local" / "scripts" / "bootstrap"
        for name in ("arch", "manjaro", "ubuntu", "ubuntu_windows", "mac", "work"):
            source = (bootstrap_dir / name).read_text(encoding="utf-8")
            self.assertIn('parse_bootstrap_args "$@"', source, name)

    def test_make_exposes_read_only_audit_target(self):
        makefile = (REPO_ROOT / "Makefile").read_text(encoding="utf-8")
        self.assertIn("audit-installation:", makefile)
        self.assertIn("audit_installation.py", makefile)

    def test_make_exposes_one_ordered_workspace_target(self):
        makefile = (REPO_ROOT / "Makefile").read_text(encoding="utf-8")
        self.assertIn("workspace:", makefile)
        for old_target in (
            "sync-workspace:",
            "sync-workspace-dry-run:",
            "update-repos:",
            "audit-policies:",
        ):
            self.assertNotIn(old_target, makefile)

        bootstrap_workspace = makefile.split("\nbootstrap-workspace:", 1)[1].split("\n\n", 1)[0]
        workspace = makefile.split("\nworkspace:", 1)[1].split("\n\n", 1)[0]
        self.assertIn("sync.sh", bootstrap_workspace)
        self.assertIn("workspace: bootstrap-workspace", makefile)
        self.assertIn("build-repos.py", workspace)
        self.assertNotIn("audit-policies.py", workspace)

    def test_profiles_derive_packages_from_active_bootstrap_functions(self):
        audit = load_audit_module()

        ubuntu = audit.expected_packages(REPO_ROOT, "ubuntu")
        mac = audit.expected_packages(REPO_ROOT, "mac")
        work = audit.expected_packages(REPO_ROOT, "work")

        self.assertIn(audit.Package("apt", "age"), ubuntu)
        self.assertIn(audit.Package("apt", "smartmontools"), ubuntu)
        self.assertIn(audit.Package("apt", "stow"), ubuntu)
        self.assertIn(audit.Package("snap", "code"), ubuntu)
        self.assertIn(audit.Package("apt", "antigravity"), ubuntu)
        self.assertIn(audit.Package("snap", "obsidian"), ubuntu)
        self.assertNotIn(audit.Package("snap", "android-studio"), ubuntu)
        self.assertIn(audit.Package("brew", "age"), mac)
        self.assertIn(audit.Package("brew", "opencode"), mac)
        self.assertIn(audit.Package("brew", "pi-coding-agent"), mac)
        self.assertIn(audit.Package("brew-cask", "copilot-cli"), mac)
        self.assertIn(audit.Package("brew", "aider"), mac)
        self.assertIn(audit.Package("brew", "sops"), mac)
        self.assertIn(audit.Package("brew", "stow"), mac)
        self.assertIn(audit.Package("brew", "google-chrome"), mac)
        self.assertIn(audit.Package("brew-cask", "obsidian"), mac)
        self.assertIn(audit.Package("apt", "age"), work)
        self.assertIn(audit.Package("apt", "docker-ce"), work)
        # The IDEs install from vendor apt repositories so the weekly upgrade updates them.
        for package in ("code", "cursor", "antigravity"):
            self.assertIn(audit.Package("apt", package), work)
        self.assertIn(audit.Package("apt", "cursor"), ubuntu)

    def test_ubuntu_based_profiles_install_verified_sops_binary(self):
        base_functions = (
            REPO_ROOT / ".local" / "scripts" / "bootstrap" / "base_functions"
        ).read_text(encoding="utf-8")

        self.assertIn("sops_install()", base_functions)
        self.assertIn("sops-v${version}.checksums.txt", base_functions)
        self.assertIn("sha256sum -c", base_functions)
        for bootstrap_name in ("ubuntu", "ubuntu_windows", "work"):
            bootstrap = (
                REPO_ROOT / ".local" / "scripts" / "bootstrap" / bootstrap_name
            ).read_text(encoding="utf-8")
            self.assertRegex(bootstrap, r"(?m)^\s*sops_install$")
            if bootstrap_name != "work":
                self.assertNotRegex(bootstrap, r"(?m)^brew_install$")

    def test_arch_and_manjaro_profiles_remain_distinct(self):
        audit = load_audit_module()

        arch = audit.expected_packages(REPO_ROOT, "arch")
        manjaro = audit.expected_packages(REPO_ROOT, "manjaro")

        self.assertIn(audit.Package("pacman", "age"), arch)
        self.assertIn(audit.Package("pacman", "opencode"), arch)
        self.assertIn(audit.Package("pacman", "sops"), arch)
        self.assertIn(audit.Package("pacman", "stow"), arch)
        self.assertIn(audit.Package("yay", "warp-terminal-bin"), arch)
        self.assertIn(audit.Package("yay", "pi-coding-agent-bin"), arch)
        self.assertIn(audit.Package("yay", "github-copilot-cli"), arch)
        self.assertIn(audit.Package("yay", "aider-chat"), arch)
        self.assertNotIn(audit.Package("pacman", "opencode"), manjaro)
        self.assertNotIn(audit.Package("yay", "pi-coding-agent-bin"), manjaro)
        self.assertNotIn(audit.Package("yay", "github-copilot-cli"), manjaro)
        self.assertNotIn(audit.Package("yay", "aider-chat"), manjaro)
        self.assertNotIn(audit.Package("snap", "whatsie"), arch)
        self.assertIn(audit.Package("snap", "whatsie"), manjaro)

    def test_automation_contract_matches_installer_assets(self):
        audit = load_audit_module()

        self.assertEqual(
            audit.LINUX_USER_TIMERS,
            (
                "cuberhaus-user-package-maintenance.timer",
                "cuberhaus-workspace-pull.timer",
            ),
        )
        self.assertEqual(
            audit.MACOS_AGENTS,
            (
                "com.cuberhaus.user-package-maintenance",
                "com.cuberhaus.workspace-pull",
            ),
        )

    def test_apt_policy_parser_reads_versions_and_repository_presence(self):
        audit = load_audit_module()
        from_repository = """\
cursor:
  Installed: 3.19.19-1788887598
  Candidate: 3.19.19-1788887598
  Version table:
 *** 3.19.19-1788887598 500
        500 https://downloads.cursor.com/aptrepo stable/main amd64 Packages
        100 /var/lib/dpkg/status
"""
        upgradable = """\
cursor:
  Installed: 3.19.19-1788887598
  Candidate: 3.22.12-1790000000
  Version table:
     3.22.12-1790000000 500
        500 https://downloads.cursor.com/aptrepo stable/main amd64 Packages
 *** 3.19.19-1788887598 100
        100 /var/lib/dpkg/status
"""
        # What apt prints once a release upgrade has disabled the vendor source.
        orphaned = """\
antigravity:
  Installed: 1.23.2-1776332190
  Candidate: 1.23.2-1776332190
  Version table:
 *** 1.23.2-1776332190 100
        100 /var/lib/dpkg/status
"""

        self.assertEqual(
            audit.parse_apt_policy(from_repository),
            ("3.19.19-1788887598", "3.19.19-1788887598", True),
        )
        self.assertEqual(
            audit.parse_apt_policy(upgradable),
            ("3.19.19-1788887598", "3.22.12-1790000000", True),
        )
        self.assertEqual(
            audit.parse_apt_policy(orphaned),
            ("1.23.2-1776332190", "1.23.2-1776332190", False),
        )
        self.assertEqual(audit.parse_apt_policy(""), ("(none)", "(none)", False))

    def run_ide_update_channel_audit(self, audit, installed, policies):
        """Run the IDE update-channel audit against fake dpkg and apt state."""

        def fake_run(command, cwd=None):
            return subprocess.CompletedProcess(command, 0, stdout=policies[command[-1]], stderr="")

        reporter = audit.Reporter()
        output = io.StringIO()
        with (
            mock.patch.object(audit.shutil, "which", return_value="/usr/bin/tool"),
            mock.patch.object(audit, "installed_package_names", return_value=installed),
            mock.patch.object(audit, "run", side_effect=fake_run),
            contextlib.redirect_stdout(output),
        ):
            audit.audit_ide_update_channels(reporter)
        return reporter, output.getvalue()

    def test_ide_update_channel_audit_reports_each_state(self):
        audit = load_audit_module()
        policies = {
            "code": "code:\n  Installed: 1.140.0-1\n  Candidate: 1.140.0-1\n  Version table:\n"
            " *** 1.140.0-1 500\n        500 https://packages.microsoft.com/repos/code stable/main amd64 Packages\n",
            "cursor": "cursor:\n  Installed: 3.19.19-1\n  Candidate: 3.22.12-1\n  Version table:\n"
            "     3.22.12-1 500\n        500 https://downloads.cursor.com/aptrepo stable/main amd64 Packages\n"
            " *** 3.19.19-1 100\n        100 /var/lib/dpkg/status\n",
            "antigravity": "antigravity:\n  Installed: 1.23.2-1\n  Candidate: 1.23.2-1\n  Version table:\n"
            " *** 1.23.2-1 100\n        100 /var/lib/dpkg/status\n",
        }

        reporter, output = self.run_ide_update_channel_audit(
            audit, {"code", "cursor", "antigravity", "unrelated"}, policies
        )

        self.assertIn("IDE update channels", output)
        self.assertIn("[OK] code 1.140.0-1 is the newest version its apt source offers.", output)
        self.assertIn("[WARN] cursor 3.19.19-1 can be upgraded to 3.22.12-1.", output)
        self.assertIn("[DRIFT] antigravity 1.23.2-1 has no enabled apt source, so it never updates.", output)
        self.assertIn("Fix: make repair REPAIR=ide-repos", output)
        self.assertEqual((reporter.issues, reporter.warnings), (1, 1))

    def test_ide_update_channel_audit_only_covers_installed_ide_packages(self):
        audit = load_audit_module()

        # No IDE .deb installed: stay silent instead of printing an empty section.
        reporter, output = self.run_ide_update_channel_audit(audit, {"bash", "coreutils"}, {})
        self.assertEqual(output, "")
        self.assertEqual((reporter.issues, reporter.warnings), (0, 0))

        # Only the installed one is inspected, so a missing policy entry would raise KeyError.
        policies = {
            "code": "code:\n  Installed: 1.140.0-1\n  Candidate: 1.140.0-1\n  Version table:\n"
            " *** 1.140.0-1 500\n        500 https://packages.microsoft.com/repos/code stable/main amd64 Packages\n"
        }
        reporter, output = self.run_ide_update_channel_audit(audit, {"code"}, policies)
        self.assertIn("[OK] code", output)
        self.assertNotIn("cursor", output)

        # Machines without dpkg or apt (Arch, macOS) skip the section silently.
        quiet = io.StringIO()
        reporter = audit.Reporter()
        with (
            mock.patch.object(audit.shutil, "which", return_value=None),
            contextlib.redirect_stdout(quiet),
        ):
            audit.audit_ide_update_channels(reporter)
        self.assertEqual(quiet.getvalue(), "")
        self.assertEqual((reporter.issues, reporter.warnings), (0, 0))

    def test_main_audits_ide_update_channels_after_package_declarations(self):
        source = AUDIT_PATH.read_text(encoding="utf-8")

        self.assertRegex(
            source,
            r"audit_packages\(packages, reporter\)\n\s+audit_ide_update_channels\(reporter\)",
        )

    def test_cursor_installed_outside_apt_mirrors_the_bootstrap_check(self):
        audit = load_audit_module()
        bootstrap = (REPO_ROOT / ".local" / "scripts" / "bootstrap" / "base_functions").read_text(encoding="utf-8")

        # The audit and cursor_is_installed must agree on what a complete AppImage is.
        self.assertIn(f"-ge {audit.MIN_APPIMAGE_BYTES}", bootstrap)

        with tempfile.TemporaryDirectory() as home:
            app_image = pathlib.Path(home) / "Applications" / "cursor.AppImage"
            with (
                mock.patch.object(pathlib.Path, "home", return_value=pathlib.Path(home)),
                mock.patch.object(audit.shutil, "which", return_value=None),
            ):
                self.assertFalse(audit.cursor_installed_outside_apt(), "nothing installed")

                app_image.parent.mkdir()
                app_image.write_bytes(b"interrupted download")
                self.assertFalse(audit.cursor_installed_outside_apt(), "a truncated AppImage must not count")

                app_image.write_bytes(bytes(audit.MIN_APPIMAGE_BYTES))
                self.assertTrue(audit.cursor_installed_outside_apt(), "a complete AppImage counts")

                app_image.unlink()
                with mock.patch.object(audit.shutil, "which", return_value="/usr/bin/cursor"):
                    self.assertTrue(audit.cursor_installed_outside_apt(), "a cursor command counts")

    def audit_declared_packages(self, audit, declared, installed, manager="apt"):
        """Run the package-declaration audit against a fake set of installed names."""
        reporter = audit.Reporter()
        output = io.StringIO()
        with (
            mock.patch.object(audit.shutil, "which", return_value="/usr/bin/tool"),
            mock.patch.object(audit, "installed_package_names", return_value=installed),
            contextlib.redirect_stdout(output),
        ):
            audit.audit_packages({audit.Package(manager, name) for name in declared}, reporter)
        return reporter, output.getvalue()

    def test_alternative_install_satisfies_a_declared_package(self):
        audit = load_audit_module()
        cursor = audit.Package("apt", "cursor")

        with mock.patch.dict(audit.ALTERNATIVE_INSTALLS, {cursor: lambda: True}):
            reporter, output = self.audit_declared_packages(audit, ["cursor"], set())
        self.assertEqual(reporter.issues, 0)
        self.assertIn("[OK] All 1 expected apt package(s) are installed.", output)

        with mock.patch.dict(audit.ALTERNATIVE_INSTALLS, {cursor: lambda: False}):
            reporter, output = self.audit_declared_packages(audit, ["cursor"], set())
        self.assertEqual(reporter.issues, 1)
        self.assertIn("[MISSING] 1 expected apt package(s): cursor", output)

    def test_t64_renamed_library_satisfies_its_old_apt_name(self):
        audit = load_audit_module()

        # Ubuntu 24.04+ ships libfuse2 as libfuse2t64, which still provides the old name.
        reporter, _ = self.audit_declared_packages(audit, ["libfuse2"], {"libfuse2t64"})
        self.assertEqual(reporter.issues, 0)

        # Only the exact rename counts, and only for apt.
        reporter, output = self.audit_declared_packages(audit, ["libfuse2"], {"libfuse2x", "libfuse2-dev"})
        self.assertEqual(reporter.issues, 1)
        self.assertIn("libfuse2", output)
        reporter, _ = self.audit_declared_packages(audit, ["libfuse2"], {"libfuse2t64"}, manager="pacman")
        self.assertEqual(reporter.issues, 1)

    def run_git_credential_helper_audit(self, audit, stdout, returncode=0):
        """Run the credential-helper audit against a fake `git config --get-regexp` answer."""
        commands = []

        def fake_run(command, cwd=None):
            commands.append(command)
            return subprocess.CompletedProcess(command, returncode, stdout=stdout, stderr="")

        reporter = audit.Reporter()
        output = io.StringIO()
        with mock.patch.object(audit, "run", side_effect=fake_run), contextlib.redirect_stdout(output):
            audit.audit_git_credential_helpers(reporter)
        return reporter, output.getvalue(), commands

    def test_git_audit_flags_every_helper_that_saves_passwords_in_plain_text(self):
        audit = load_audit_module()

        for helper in (
            "store",
            "store --file=/tmp/credentials",
            "store --file /tmp/credentials",
            "!git credential-store",
            "!/usr/lib/git-core/git-credential-store --file x",
            "/usr/lib/git-core/git-credential-store",
        ):
            with self.subTest(helper=helper):
                reporter, output, commands = self.run_git_credential_helper_audit(
                    audit, f"credential.helper {helper}\n"
                )
                self.assertEqual((reporter.issues, reporter.warnings), (1, 0))
                self.assertIn(f"[DRIFT] git credential.helper is {helper!r}", output)
                self.assertIn("saves passwords in plain text", output)
                self.assertIn("Fix: git config --global credential.helper 'cache --timeout=28800'", output)
                # Global scope only, and every credential.*.helper key, not just the generic one.
                self.assertEqual(commands[0][:4], ["git", "config", "--global", "--get-regexp"])

    def test_git_audit_flags_a_host_specific_helper_that_saves_passwords(self):
        audit = load_audit_module()

        reporter, output, _ = self.run_git_credential_helper_audit(
            audit,
            "credential.helper cache --timeout=28800\ncredential.https://gitlab.com.helper store\n",
        )

        self.assertEqual(reporter.issues, 1)
        self.assertIn("[DRIFT] git credential.https://gitlab.com.helper is 'store'", output)
        self.assertIn(
            "Fix: git config --global credential.https://gitlab.com.helper 'cache --timeout=28800'",
            output,
        )

    def test_git_audit_accepts_helpers_that_do_not_save_passwords_in_plain_text(self):
        audit = load_audit_module()
        gh = (
            "credential.https://github.com.helper \n"
            "credential.https://github.com.helper !/usr/bin/gh auth git-credential\n"
        )

        for config in (
            "credential.helper cache --timeout=28800\n",
            "credential.helper cache\n",
            "credential.helper libsecret\n",
            "credential.helper osxkeychain\n",
            "credential.helper manager\n",
            # git credential-store-more is another helper, not the plain-text one.
            "credential.helper store-more\n",
            # The gh helper for one host, after the empty value that resets the list.
            "credential.helper cache --timeout=28800\n" + gh,
        ):
            with self.subTest(config=config):
                reporter, output, _ = self.run_git_credential_helper_audit(audit, config)
                self.assertEqual((reporter.issues, reporter.warnings), (0, 0))
                self.assertIn("[OK] git credential helpers do not save passwords in plain text.", output)

    def test_git_audit_treats_no_helper_as_fine_and_an_unreadable_config_as_unknown(self):
        audit = load_audit_module()

        # No helper at all: git asks every time and saves nothing.
        reporter, output, _ = self.run_git_credential_helper_audit(audit, "", returncode=1)
        self.assertEqual((reporter.issues, reporter.warnings), (0, 0))
        self.assertIn("[OK] git has no global credential helper", output)

        # Only the empty value that resets the list is still no helper.
        reporter, output, _ = self.run_git_credential_helper_audit(
            audit, "credential.https://github.com.helper \n"
        )
        self.assertEqual((reporter.issues, reporter.warnings), (0, 0))
        self.assertIn("[OK] git has no global credential helper", output)

        # A config git cannot read says nothing about its helpers: never report that as fine.
        reporter, output, _ = self.run_git_credential_helper_audit(
            audit, "fatal: bad config line 1 in file /home/user/.gitconfig\n", returncode=128
        )
        self.assertEqual((reporter.issues, reporter.warnings), (0, 1))
        self.assertIn("[WARN] The global git config could not be read", output)
        self.assertNotIn("[OK]", output)

    def test_git_configuration_audit_includes_the_credential_helpers(self):
        audit = load_audit_module()

        def fake_run(command, cwd=None):
            if "--get-regexp" in command:
                return subprocess.CompletedProcess(command, 0, stdout="credential.helper store\n", stderr="")
            values = {"user.name": "cuberhaus", "user.email": "polcg10@gmail.com"}
            return subprocess.CompletedProcess(command, 0, stdout=values[command[-1]] + "\n", stderr="")

        reporter = audit.Reporter()
        output = io.StringIO()
        with mock.patch.object(audit, "run", side_effect=fake_run), contextlib.redirect_stdout(output):
            audit.audit_git_configuration(reporter)

        self.assertIn("[OK] git user.name is cuberhaus.", output.getvalue())
        self.assertIn("[DRIFT] git credential.helper is 'store'", output.getvalue())
        self.assertEqual(reporter.issues, 1)

    def test_secure_credential_helper_is_the_same_everywhere(self):
        audit = load_audit_module()
        bootstrap = (REPO_ROOT / ".local" / "scripts" / "bootstrap" / "base_functions").read_text(
            encoding="utf-8"
        )
        mini = (REPO_ROOT / ".local" / "Mini" / ".gitconfig").read_text(encoding="utf-8")
        source = AUDIT_PATH.read_text(encoding="utf-8")

        self.assertEqual(audit.SECURE_CREDENTIAL_HELPER, "cache --timeout=28800")
        self.assertIn(f"local secure_helper='{audit.SECURE_CREDENTIAL_HELPER}'", bootstrap)
        self.assertIn(f"helper = {audit.SECURE_CREDENTIAL_HELPER}\n", mini)
        # Nothing may demand or configure the plain-text helper again.
        self.assertNotIn('"credential.helper": "store"', source)
        self.assertNotRegex(mini, r"(?m)^\s*helper\s*=\s*store\b")


if __name__ == "__main__":
    unittest.main()