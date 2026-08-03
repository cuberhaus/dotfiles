import importlib.util
import pathlib
import sys
import unittest


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
        expected_paths = audit.expected_shell_paths(pathlib.Path("/home/example"))

        self.assertEqual(
            expected_paths,
            (
                pathlib.Path("/home/example/.local/bin"),
                pathlib.Path("/home/example/.local/scripts/bin"),
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
        for step in ("config", "aliases", "environment", "vim", "automations", "keyboard"):
            self.assertIn(step, repair)

    def test_unattended_bootstrap_choices_are_deterministic(self):
        source = (
            REPO_ROOT / ".local" / "scripts" / "bootstrap" / "base_functions"
        ).read_text(encoding="utf-8")

        for option in ("--unattended)", "--first-run=yes|--first-run=no)", "--high-dpi=yes|--high-dpi=no)"):
            self.assertIn(option, source)
        self.assertIn("FirstRun=n", source)
        self.assertIn("HIGH_DPI=false", source)

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

        workspace = makefile.split("workspace:", 1)[1].split("\n\n", 1)[0]
        self.assertLess(workspace.index("sync.sh"), workspace.index("build-repos.py"))
        self.assertLess(workspace.index("build-repos.py"), workspace.index("audit-policies.py"))

    def test_profiles_derive_packages_from_active_bootstrap_functions(self):
        audit = load_audit_module()

        ubuntu = audit.expected_packages(REPO_ROOT, "ubuntu")
        mac = audit.expected_packages(REPO_ROOT, "mac")
        work = audit.expected_packages(REPO_ROOT, "work")

        self.assertIn(audit.Package("apt", "age"), ubuntu)
        self.assertIn(audit.Package("apt", "stow"), ubuntu)
        self.assertIn(audit.Package("snap", "code"), ubuntu)
        self.assertIn(audit.Package("apt", "antigravity"), ubuntu)
        self.assertNotIn(audit.Package("snap", "android-studio"), ubuntu)
        self.assertIn(audit.Package("brew", "age"), mac)
        self.assertIn(audit.Package("brew", "sops"), mac)
        self.assertIn(audit.Package("brew", "stow"), mac)
        self.assertIn(audit.Package("brew", "google-chrome"), mac)
        self.assertIn(audit.Package("brew-cask", "obsidian"), mac)
        self.assertIn(audit.Package("apt", "age"), work)
        self.assertIn(audit.Package("apt", "docker-ce"), work)

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
            self.assertRegex(bootstrap, r"(?m)^sops_install$")
            if bootstrap_name != "work":
                self.assertNotRegex(bootstrap, r"(?m)^brew_install$")

    def test_arch_and_manjaro_profiles_remain_distinct(self):
        audit = load_audit_module()

        arch = audit.expected_packages(REPO_ROOT, "arch")
        manjaro = audit.expected_packages(REPO_ROOT, "manjaro")

        self.assertIn(audit.Package("pacman", "age"), arch)
        self.assertIn(audit.Package("pacman", "sops"), arch)
        self.assertIn(audit.Package("pacman", "stow"), arch)
        self.assertIn(audit.Package("yay", "warp-terminal-bin"), arch)
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


if __name__ == "__main__":
    unittest.main()