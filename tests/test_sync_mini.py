import importlib.util
import io
import shutil
import tempfile
import unittest
from contextlib import redirect_stderr, redirect_stdout
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]
SCRIPT_PATH = REPO_ROOT / ".local" / "scripts" / "sync-mini.py"

SPEC = importlib.util.spec_from_file_location("sync_mini", SCRIPT_PATH)
assert SPEC and SPEC.loader
sync_mini = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(sync_mini)


class SyncMiniTests(unittest.TestCase):
    def test_repo_mini_bashrc_is_in_sync_with_canonical_scripts(self) -> None:
        stderr = io.StringIO()
        with redirect_stderr(stderr):
            rc = sync_mini.sync_mini_bashrc(REPO_ROOT, check_only=True)
        self.assertEqual(rc, 0, stderr.getvalue())

    def test_detects_drift_and_regenerates_from_canonical_sources(self) -> None:
        rel_files = [
            ".config/zsh/aliases",
            ".config/zsh/functions",
            ".local/scripts/lib/git-repo-defaults.sh",
            ".local/scripts/bin/clone-all",
            ".local/scripts/bin/clone-team",
            ".local/scripts/bin/git-ahead",
            ".local/scripts/bin/git-clean-branches",
            ".local/scripts/bin/git-recurse",
            ".local/Mini/.bashrc",
        ]
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            for rel in rel_files:
                src = REPO_ROOT / rel
                dst = root / rel
                dst.parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(src, dst)

            git_ahead = root / ".local/scripts/bin/git-ahead"
            git_ahead.write_text(
                git_ahead.read_text(encoding="utf-8").replace("depth=4", "depth=5", 1),
                encoding="utf-8",
            )

            stderr = io.StringIO()
            with redirect_stderr(stderr):
                rc = sync_mini.sync_mini_bashrc(root, check_only=True)
            self.assertEqual(rc, 1)
            self.assertIn("Fix: make sync-mini", stderr.getvalue())

            stdout = io.StringIO()
            with redirect_stdout(stdout):
                rc = sync_mini.sync_mini_bashrc(root, check_only=False)
            self.assertEqual(rc, 0)
            self.assertEqual(sync_mini.sync_mini_bashrc(root, check_only=True), 0)

    def test_rejects_bash4_constructs_in_generated_output(self) -> None:
        for snippet in (
            "mapfile -t items < <(ls)",
            "wait -n",
            "declare -A table=()",
            'echo "${name^^}"',
        ):
            with self.assertRaises(ValueError):
                sync_mini.validate_generated(snippet)


if __name__ == "__main__":
    unittest.main()
