import importlib.util
import pathlib
import sys
import unittest


REPO_ROOT = pathlib.Path(__file__).resolve().parents[1]
AUDIT_PATH = REPO_ROOT / ".local" / "scripts" / "audit_installation.py"


def load_audit_module():
    spec = importlib.util.spec_from_file_location("audit_installation", AUDIT_PATH)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class InstallationAuditContractTests(unittest.TestCase):
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

        self.assertIn(audit.Package("apt", "stow"), ubuntu)
        self.assertIn(audit.Package("snap", "code"), ubuntu)
        self.assertIn(audit.Package("apt", "antigravity"), ubuntu)
        self.assertNotIn(audit.Package("snap", "android-studio"), ubuntu)
        self.assertIn(audit.Package("brew", "stow"), mac)
        self.assertIn(audit.Package("brew", "google-chrome"), mac)
        self.assertIn(audit.Package("brew-cask", "obsidian"), mac)
        self.assertIn(audit.Package("apt", "docker-ce"), work)

    def test_arch_and_manjaro_profiles_remain_distinct(self):
        audit = load_audit_module()

        arch = audit.expected_packages(REPO_ROOT, "arch")
        manjaro = audit.expected_packages(REPO_ROOT, "manjaro")

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