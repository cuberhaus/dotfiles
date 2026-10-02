"""Apps installed by their own installer: what the audit finds, declares, accepts and prints.

Zed, GPT4All and AppImages put a program in the home folder and leave a launcher in
~/.local/share/applications. No package manager records them, so the package report cannot see them,
and the audit reads the launcher instead. Like the package report it is a notice that never changes
the exit code, and it suggests no removal command, because nothing owns these apps.

Every test runs against a throwaway home folder: HOME, XDG_DATA_HOME and XDG_CONFIG_HOME all point
into a scratch directory, so a test can neither read the real launchers nor touch them.
"""

import contextlib
import importlib.util
import io
import os
import pathlib
import sys
import tempfile
import unittest
from unittest import mock


REPO_ROOT = pathlib.Path(__file__).resolve().parents[1]
AUDIT_PATH = REPO_ROOT / ".local" / "scripts" / "audit_installation.py"


def load_audit_module():
    spec = importlib.util.spec_from_file_location("audit_installation", AUDIT_PATH)
    module = importlib.util.module_from_spec(spec)
    sys.modules[spec.name] = module
    spec.loader.exec_module(module)
    return module


class AppsCase(unittest.TestCase):
    """A loaded audit module, a throwaway home folder and a small fake bootstrap for the `work` profile."""

    ENTRYPOINT = """\
#!/usr/bin/env bash
install_apps
"""

    # `declared` is written by a function the entrypoint reaches through another one. `gpt4all` only
    # appears in a comment, and `orphan` only in a function that nothing calls.
    FUNCTIONS = """\
install_apps() {
    write_launchers
    # GPT4All (local LLM chat) - not needed right now
    # cat > "$HOME/.local/share/applications/gpt4all.desktop" <<EOF
}

write_launchers() {
    cat > "$HOME/.local/share/applications/declared.desktop" <<EOF
[Desktop Entry]
Type=Application
Exec=$HOME/Applications/declared.AppImage
EOF
}

never_called() {
    cat > "$HOME/.local/share/applications/orphan.desktop" <<EOF
EOF
}
"""

    def setUp(self) -> None:
        self.audit = load_audit_module()
        scratch = tempfile.TemporaryDirectory()
        self.addCleanup(scratch.cleanup)
        # Resolved, so that the paths the audit prints match the ones the tests build.
        self.scratch = pathlib.Path(scratch.name).resolve()
        self.home = self.scratch / "home"
        self.outside = self.scratch / "opt"  # a place that is not in the home folder
        self.applications = self.home / ".local" / "share" / "applications"
        self.home.mkdir()
        self.outside.mkdir()
        environment = mock.patch.dict(
            os.environ,
            {
                "HOME": str(self.home),
                "XDG_DATA_HOME": str(self.home / ".local" / "share"),
                "XDG_CONFIG_HOME": str(self.home / ".config"),
            },
        )
        environment.start()
        self.addCleanup(environment.stop)
        self.repo = self.scratch / "repo"
        self.write_bootstrap(self.FUNCTIONS)

    def write_bootstrap(self, functions: str, entrypoint: str | None = None) -> None:
        directory = self.repo / ".local" / "scripts" / "bootstrap"
        directory.mkdir(parents=True, exist_ok=True)
        (directory / "work").write_text(self.ENTRYPOINT if entrypoint is None else entrypoint, encoding="utf-8")
        (directory / "work_functions").write_text(functions, encoding="utf-8")

    def program(self, relative: str, outside: bool = False) -> pathlib.Path:
        """An executable file under the home folder (or beside it, with outside=True)."""
        path = (self.outside if outside else self.home) / relative
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("#!/bin/sh\n", encoding="utf-8")
        path.chmod(0o755)
        return path.resolve()

    def launcher(self, file_name: str, *lines: str) -> pathlib.Path:
        """A launcher whose [Desktop Entry] group holds exactly these lines."""
        self.applications.mkdir(parents=True, exist_ok=True)
        path = self.applications / file_name
        path.write_text("\n".join(["[Desktop Entry]", *lines, ""]), encoding="utf-8")
        return path

    def app(self, stem: str, command: object, name: str | None = None) -> pathlib.Path:
        """The launcher of an application that starts `command`."""
        lines = ["Type=Application", f"Exec={command}"]
        if name:
            lines.append(f"Name={name}")
        return self.launcher(f"{stem}.desktop", *lines)

    def find(self, profile: str = "work", repo: pathlib.Path | None = None):
        return self.audit.find_self_installed_apps(repo or self.repo, profile)

    def keys(self, profile: str = "work") -> list[str]:
        return [app.key for app in self.find(profile).found]

    def keep_file(self, text: str) -> pathlib.Path:
        path = self.audit.known_extra_packages_path()
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text, encoding="utf-8")
        return path


class FindingTests(AppsCase):
    def test_a_launcher_that_starts_a_program_in_the_home_folder_is_found(self) -> None:
        program = self.program("gpt4all/bin/chat")
        self.app("gpt4all", program, name="GPT4All")

        apps = self.find()

        self.assertEqual(apps.found, [self.audit.SelfInstalledApp("gpt4all", "GPT4All", program)])
        self.assertEqual(apps.acknowledged, 0)

    def test_a_program_outside_the_home_folder_does_not_count(self) -> None:
        self.app("tool", self.program("tool/bin/run", outside=True))

        self.assertEqual(self.keys(), [])

    def test_a_link_in_the_home_folder_to_a_system_program_does_not_count(self) -> None:
        # ~/.local/bin/code -> /usr/share/code/bin/code: the package owns the file, not the user.
        target = self.program("code/bin/code", outside=True)
        link = self.home / ".local" / "bin" / "code"
        link.parent.mkdir(parents=True)
        link.symlink_to(target)
        self.app("code", link)

        self.assertEqual(self.keys(), [])

    def test_a_link_outside_the_home_folder_to_a_program_in_it_counts(self) -> None:
        target = self.program("tools/bin/run")
        link = self.outside / "bin" / "run"
        link.parent.mkdir(parents=True)
        link.symlink_to(target)
        self.app("run", link)

        self.assertEqual([(app.key, app.program) for app in self.find().found], [("run", target)])

    def test_a_bare_command_name_is_looked_up_on_the_path(self) -> None:
        program = self.program(".local/bin/zed")
        self.app("zed", "zed")

        with mock.patch.object(self.audit.shutil, "which", lambda name: str(program) if name == "zed" else None):
            self.assertEqual(self.keys(), ["zed"])

    def test_a_bare_command_name_that_is_not_on_the_path_does_not_count(self) -> None:
        self.program(".local/bin/zed")
        self.app("zed", "zed")

        with mock.patch.object(self.audit.shutil, "which", lambda name: None):
            self.assertEqual(self.keys(), [])

    def test_a_tilde_in_the_program_path_means_the_home_folder(self) -> None:
        self.program("gpt4all/bin/chat")
        self.app("gpt4all", "~/gpt4all/bin/chat")

        self.assertEqual(self.keys(), ["gpt4all"])

    def test_a_relative_program_path_cannot_be_told_and_is_skipped(self) -> None:
        self.program("gpt4all/bin/chat")
        self.app("gpt4all", "gpt4all/bin/chat")
        # From the home folder the relative path would reach a real file, so skipping it is a decision
        # of the audit and not a side effect of the directory the test happens to run in.
        previous = os.getcwd()
        self.addCleanup(os.chdir, previous)
        os.chdir(self.home)

        self.assertEqual(self.keys(), [])

    def test_an_env_prefix_is_skipped(self) -> None:
        program = self.program("tools/bin/run")
        self.app("run", f"env GDK_BACKEND=x11 FOO=bar {program} %U")

        self.assertEqual([app.program for app in self.find().found], [program])

    def test_a_quoted_program_path_may_contain_spaces(self) -> None:
        program = self.program("My Tools/bin/run")
        self.app("run", f'"{program}" --flag %U')

        self.assertEqual([app.program for app in self.find().found], [program])

    def test_a_program_started_through_a_shell_is_not_seen(self) -> None:
        # The program that Exec names is the shell, which is not in the home folder.
        program = self.program("tools/bin/run")
        self.app("run", f'sh -c "exec {program}"')

        self.assertEqual(self.keys(), [])

    def test_a_launcher_left_behind_for_a_program_that_is_gone_does_not_count(self) -> None:
        self.app("isaacsim", self.home / "isaac-sim" / "isaac-sim.sh", name="Isaac Sim")

        self.assertEqual(self.keys(), [])

    def test_hidden_entries_and_entries_that_are_not_applications_are_ignored(self) -> None:
        program = self.program("tools/bin/run")
        self.launcher("hidden.desktop", "Type=Application", f"Exec={program}", "Hidden=true")
        self.launcher("hidden-caps.desktop", "Type=Application", f"Exec={program}", "Hidden=True")  # not valid, but seen
        self.launcher("link.desktop", "Type=Link", f"Exec={program}")
        self.launcher("untyped.desktop", f"Exec={program}")
        self.launcher("shown.desktop", "Type=Application", f"Exec={program}", "Hidden=false")

        self.assertEqual(self.keys(), ["shown"])

    def test_an_exec_in_a_desktop_action_does_not_make_an_app(self) -> None:
        # A desktop action (`[Desktop Action new]`) has an Exec of its own; the entry itself has none.
        inside = self.program("tools/bin/run")
        path = self.launcher("run.desktop", "Type=Application", "Name=Run")
        path.write_text(path.read_text(encoding="utf-8") + f"\n[Desktop Action new]\nExec={inside}\n", encoding="utf-8")

        self.assertEqual(self.keys(), [])

    def test_a_name_in_a_desktop_action_is_not_the_name_of_the_app(self) -> None:
        program = self.program("tools/bin/run")
        path = self.launcher("run.desktop", "Type=Application", f"Exec={program}")
        path.write_text(path.read_text(encoding="utf-8") + "\n[Desktop Action new]\nName=New window\n", encoding="utf-8")

        (app,) = self.find().found

        self.assertEqual(app.name, "run")

    def test_the_entry_may_follow_another_group(self) -> None:
        program = self.program("tools/bin/run")
        self.applications.mkdir(parents=True)
        (self.applications / "run.desktop").write_text(
            f"[Other Group]\nExec=/nowhere\n\n[Desktop Entry]\nType=Application\nName=Run\nExec={program}\n",
            encoding="utf-8",
        )

        (app,) = self.find().found

        self.assertEqual((app.key, app.name, app.program), ("run", "Run", program))

    def test_the_name_comes_from_the_unlocalized_key(self) -> None:
        program = self.program("tools/bin/run")
        self.launcher("run.desktop", "Type=Application", f"Exec={program}", "Name[es]=Ejecutar", "Name=Run")
        self.launcher("only-es.desktop", "Type=Application", f"Exec={program}", "Name[es]=Solo")

        names = {app.key: app.name for app in self.find().found}

        self.assertEqual(names, {"run": "Run", "only-es": "only-es"})

    def test_a_launcher_without_a_name_is_listed_by_its_file_name(self) -> None:
        self.app("dev.zed.Zed", self.program(".local/zed.app/bin/zed"))

        (app,) = self.find().found

        self.assertEqual((app.key, app.name), ("dev.zed.Zed", "dev.zed.Zed"))

    def test_launchers_that_cannot_be_read_are_skipped_and_the_rest_are_still_found(self) -> None:
        self.applications.mkdir(parents=True)
        (self.applications / "garbage.desktop").write_bytes(b"\xff\xfe\x00\x01 not a launcher \x80")
        (self.applications / "empty.desktop").write_text("", encoding="utf-8")
        (self.applications / "directory.desktop").mkdir()
        self.launcher("quote.desktop", "Type=Application", 'Exec="/home/nobody/unclosed')
        self.app("good", self.program("good/bin/run"))

        self.assertEqual(self.keys(), ["good"])

    def test_only_launchers_directly_in_the_folder_are_scanned(self) -> None:
        program = self.program("tools/bin/run")
        nested = self.applications / "wine" / "Programs"
        nested.mkdir(parents=True)
        (nested / "run.desktop").write_text(f"[Desktop Entry]\nType=Application\nExec={program}\n", encoding="utf-8")
        (self.applications / "notes.txt").write_text(f"[Desktop Entry]\nType=Application\nExec={program}\n", encoding="utf-8")

        self.assertEqual(self.keys(), [])

    def test_the_apps_are_listed_in_the_order_of_their_keys(self) -> None:
        program = self.program("tools/bin/run")
        # Written in an order that is neither sorted nor reversed (a directory may list its files in
        # either), and long enough that a lucky listing is not a realistic pass.
        for stem in ("delta", "alpha", "golf", "Charlie", "hotel", "bravo", "foxtrot", "echo"):
            self.app(stem, program)

        self.assertEqual(
            self.keys(), ["Charlie", "alpha", "bravo", "delta", "echo", "foxtrot", "golf", "hotel"]
        )

    def test_a_missing_folder_of_launchers_means_there_is_nothing_to_check(self) -> None:
        self.assertIsNone(self.find())

    def test_the_folder_defaults_to_local_share_and_follows_xdg_data_home(self) -> None:
        del os.environ["XDG_DATA_HOME"]
        self.assertEqual(self.audit.user_applications_dir(), self.home / ".local" / "share" / "applications")

        os.environ["XDG_DATA_HOME"] = ""
        self.assertEqual(self.audit.user_applications_dir(), self.home / ".local" / "share" / "applications")

        os.environ["XDG_DATA_HOME"] = str(self.scratch / "elsewhere")
        self.assertEqual(self.audit.user_applications_dir(), self.scratch / "elsewhere" / "applications")


class DeclaredTests(AppsCase):
    def test_a_launcher_that_an_active_bootstrap_function_writes_is_declared(self) -> None:
        # Reached through another function (install_apps -> write_launchers).
        self.app("declared", self.program("Applications/declared.AppImage"))

        apps = self.find()

        self.assertEqual(apps.found, [])
        # Declared is not accepted: the keep file is not involved.
        self.assertEqual(apps.acknowledged, 0)

    def test_a_launcher_that_only_a_comment_names_is_not_declared(self) -> None:
        self.app("gpt4all", self.program("gpt4all/bin/chat"))

        self.assertEqual(self.keys(), ["gpt4all"])

    def test_a_launcher_that_only_a_function_nothing_calls_names_is_not_declared(self) -> None:
        self.app("orphan", self.program("orphan/bin/run"))

        self.assertEqual(self.keys(), ["orphan"])

    def test_the_entrypoint_itself_can_declare_a_launcher(self) -> None:
        self.write_bootstrap(
            self.FUNCTIONS,
            entrypoint='#!/usr/bin/env bash\ninstall_apps\ncp zed.desktop "$HOME/.local/share/applications/dev.zed.Zed.desktop"\n',
        )
        self.app("dev.zed.Zed", self.program(".local/zed.app/bin/zed"))

        self.assertEqual(self.keys(), [])

    def test_a_longer_file_name_does_not_declare_a_shorter_one(self) -> None:
        # The bootstrap writes x-declared.desktop; a launcher called declared.desktop is another app.
        self.write_bootstrap(self.FUNCTIONS.replace("applications/declared.desktop", "applications/x-declared.desktop"))
        self.app("declared", self.program("Applications/declared.AppImage"))
        self.app("x-declared", self.program("Applications/x-declared.AppImage"))

        self.assertEqual(self.keys(), ["declared"])

    def test_a_file_name_that_ends_like_another_one_is_not_declared_by_it(self) -> None:
        self.app("eclared", self.program("Applications/eclared.AppImage"))

        self.assertEqual(self.keys(), ["eclared"])

    def test_each_profile_reads_its_own_bootstrap(self) -> None:
        # In the fake repo only `work` exists; `ubuntu` has no files, so the audit cannot read it.
        self.app("declared", self.program("Applications/declared.AppImage"))
        directory = self.repo / ".local" / "scripts" / "bootstrap"
        (directory / "ubuntu").write_text("#!/usr/bin/env bash\nother\n", encoding="utf-8")
        (directory / "ubuntu_functions").write_text("other() {\n    :\n}\n", encoding="utf-8")

        self.assertEqual(self.keys("work"), [])
        self.assertEqual(self.keys("ubuntu"), ["declared"])

    def test_the_arch_bootstrap_declares_the_appimages_it_installs(self) -> None:
        # Real bootstrap files: ai_tools_install writes cursor.desktop and antigravity.desktop itself,
        # in a heredoc, and the arch entrypoint calls it. Reporting them as installed outside the
        # bootstrap would be wrong there. (The manjaro entrypoint does not call it, so Manjaro
        # rightly does not declare them, and this test says nothing about it.)
        self.app("cursor", self.program("Applications/cursor.AppImage"))
        self.app("antigravity", self.program("Applications/antigravity.AppImage"))
        self.app("zzz-declared-nowhere", self.program("Applications/zzz.AppImage"))

        found = self.find("arch", REPO_ROOT).found

        self.assertEqual([app.key for app in found], ["zzz-declared-nowhere"])


class AcceptedTests(AppsCase):
    def test_an_app_entry_in_the_keep_file_accepts_the_app_and_counts_it(self) -> None:
        self.app("dev.zed.Zed", self.program(".local/zed.app/bin/zed"))
        self.app("gpt4all", self.program("gpt4all/bin/chat"))
        self.keep_file("# kept on purpose\napp:dev.zed.Zed\n")

        apps = self.find()

        self.assertEqual([app.key for app in apps.found], ["gpt4all"])
        self.assertEqual(apps.acknowledged, 1)

    def test_a_bare_name_in_the_keep_file_accepts_an_app_as_well(self) -> None:
        self.app("gpt4all", self.program("gpt4all/bin/chat"))
        self.keep_file("gpt4all\n")

        apps = self.find()

        self.assertEqual((apps.found, apps.acknowledged), ([], 1))

    def test_an_entry_of_another_kind_does_not_accept_an_app(self) -> None:
        self.app("gpt4all", self.program("gpt4all/bin/chat"))
        self.keep_file("apt:gpt4all\nsnap:gpt4all\nbrew:gpt4all\n")

        apps = self.find()

        self.assertEqual(([app.key for app in apps.found], apps.acknowledged), (["gpt4all"], 0))

    def test_an_app_entry_for_another_name_accepts_nothing(self) -> None:
        self.app("gpt4all", self.program("gpt4all/bin/chat"))
        self.keep_file("app:gpt4\napp:gpt4all-extra\n")

        self.assertEqual(self.keys(), ["gpt4all"])

    def test_the_keep_file_reader_knows_the_app_prefix(self) -> None:
        path = self.keep_file("app:dev.zed.Zed  # editor\napt:ffmpeg\nplain\napp:\n")

        self.assertEqual(
            self.audit.known_extra_entries(path),
            {("app", "dev.zed.Zed"), ("apt", "ffmpeg"), (None, "plain"), (None, "app:")},
        )

    def test_an_accepted_app_that_is_also_declared_is_not_counted_as_accepted(self) -> None:
        self.app("declared", self.program("Applications/declared.AppImage"))
        self.keep_file("app:declared\n")

        apps = self.find()

        self.assertEqual((apps.found, apps.acknowledged), ([], 0))


class ReportTests(AppsCase):
    """What audit_self_installed_apps prints."""

    def setUp(self) -> None:
        super().setUp()
        self.width = 80
        patcher = mock.patch.object(self.audit, "output_width", lambda: self.width)
        patcher.start()
        self.addCleanup(patcher.stop)

    def report(self, profile: str = "work"):
        reporter = self.audit.Reporter()
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            self.audit.audit_self_installed_apps(self.repo, profile, reporter)
        return reporter, output.getvalue()

    def three_apps(self) -> None:
        self.app("dev.zed.Zed", self.program(".local/zed.app/bin/zed"), name="Zed")
        self.app("gpt4all", self.program("gpt4all/bin/chat"), name="GPT4All")
        self.app("cursor", self.program("Applications/cursor.AppImage"), name="Cursor")

    def test_the_layout_at_80_columns(self) -> None:
        self.three_apps()

        reporter, output = self.report()

        self.assertEqual(
            output,
            """
Apps installed outside the bootstrap
  [NOTICE] 3 app(s) start from your home folder and are not declared by the work
           bootstrap:
             Cursor (app:cursor): ~/Applications/cursor.AppImage
             Zed (app:dev.zed.Zed): ~/.local/zed.app/bin/zed
             GPT4All (app:gpt4all): ~/gpt4all/bin/chat
  Nothing was changed. No package manager owns these apps, so no removal command
  is suggested. For each app you can:
    - remove it with its own uninstaller, or delete its folder and its launcher
      yourself;
    - or accept it by listing app:NAME in
      ~/.config/dotfiles/known-extra-packages (`--list-extra` prints that
      format).
""",
        )
        self.assertEqual((reporter.notices, reporter.warnings, reporter.issues), (1, 0, 0))

    def test_a_long_path_is_never_split_at_the_narrowest_width(self) -> None:
        self.width = 60
        long_path = "isaac-sim/lib/python3.12/site-packages/isaacsim/exts/isaacsim.app.setup/bin/isaac-sim.sh"
        self.app("isaacsim", self.program(long_path), name="Isaac Sim")

        _, output = self.report()

        self.assertIn(f"~/{long_path}", output)
        for line in output.splitlines():
            self.assertTrue(len(line) <= 60 or f"~/{long_path}" in line, line)

    def test_nothing_is_printed_without_a_folder_of_launchers(self) -> None:
        reporter, output = self.report()

        self.assertEqual(output, "")
        self.assertEqual((reporter.notices, reporter.warnings, reporter.issues), (0, 0, 0))

    def test_it_says_so_when_every_app_is_declared(self) -> None:
        self.app("declared", self.program("Applications/declared.AppImage"))

        reporter, output = self.report()

        self.assertEqual(
            output,
            """
Apps installed outside the bootstrap
  [OK] Every launcher in ~/.local/share/applications that starts a program from
       your home folder is declared by the work bootstrap or accepted.
""",
        )
        self.assertEqual(reporter.notices, 0)

    def test_accepted_apps_are_counted_and_not_listed(self) -> None:
        self.three_apps()
        self.keep_file("app:dev.zed.Zed\napp:gpt4all\n")

        reporter, output = self.report()

        self.assertIn("1 app(s) start from your home folder", output)
        self.assertIn("Cursor (app:cursor)", output)
        self.assertNotIn("Zed", output)
        self.assertNotIn("GPT4All", output)
        self.assertIn("2 app(s) accepted in ~/.config/dotfiles/known-extra-packages are not reported.", " ".join(output.split()))
        self.assertEqual(reporter.notices, 1)

    def test_everything_accepted_reads_as_ok_and_still_counts_what_was_accepted(self) -> None:
        self.three_apps()
        self.keep_file("app:cursor\napp:dev.zed.Zed\napp:gpt4all\n")

        reporter, output = self.report()

        self.assertIn("[OK] Every launcher", output)
        self.assertIn("3 app(s) accepted in ~/.config/dotfiles/known-extra-packages are not reported.", " ".join(output.split()))
        self.assertEqual((reporter.notices, reporter.issues), (0, 0))

    def test_the_findings_are_one_notice_that_never_fails_the_audit(self) -> None:
        self.three_apps()

        reporter, _ = self.report()

        self.assertEqual((reporter.notices, reporter.warnings, reporter.issues), (1, 0, 0))

    def test_no_removal_command_is_suggested(self) -> None:
        self.three_apps()

        _, output = self.report()

        self.assertNotIn("Fix:", output)
        self.assertNotRegex(output, r"\b(rm|sudo)\b")

    def test_the_declaring_profile_is_named_in_the_report(self) -> None:
        self.three_apps()
        directory = self.repo / ".local" / "scripts" / "bootstrap"
        (directory / "ubuntu").write_text("#!/usr/bin/env bash\n:\n", encoding="utf-8")
        (directory / "ubuntu_functions").write_text("other() {\n    :\n}\n", encoding="utf-8")

        _, output = self.report("ubuntu")

        self.assertIn("not declared by the ubuntu bootstrap", " ".join(output.split()))


@unittest.skipUnless(sys.platform.startswith(("linux", "darwin")), "main() audits Linux and macOS hosts only")
class MainTests(AppsCase):
    AUDITS = (
        "audit_git",
        "audit_stow",
        "audit_shell_configuration",
        "audit_environment",
        "audit_git_configuration",
        "audit_editors_and_fonts",
        "audit_packages",
        "audit_ide_update_channels",
        "audit_extra_packages",
        "audit_self_installed_apps",
        "audit_automations",
    )

    def run_main(self, *arguments: str):
        output = io.StringIO()
        with (
            mock.patch.object(sys, "argv", ["audit_installation.py", "--profile", "work", *arguments]),
            contextlib.redirect_stdout(output),
        ):
            code = self.audit.main()
        return code, output.getvalue()

    def stub(self, *names: str, side_effect=None) -> None:
        for name in names:
            patcher = mock.patch.object(self.audit, name, mock.MagicMock(name=name, side_effect=side_effect))
            patcher.start()
            self.addCleanup(patcher.stop)

    def no_extra_packages(self) -> None:
        patcher = mock.patch.object(
            self.audit, "find_extra_packages", lambda repo_root, profile, packages: self.audit.ExtraPackages(found={})
        )
        patcher.start()
        self.addCleanup(patcher.stop)

    def test_the_apps_audit_runs_right_after_the_extras_audit(self) -> None:
        calls: list[str] = []
        for name in self.AUDITS:
            patcher = mock.patch.object(self.audit, name, lambda *args, _name=name: calls.append(_name))
            patcher.start()
            self.addCleanup(patcher.stop)

        self.run_main()

        self.assertEqual(calls[-3:], ["audit_extra_packages", "audit_self_installed_apps", "audit_automations"])

    def test_an_apps_notice_is_counted_and_does_not_fail_the_audit(self) -> None:
        self.stub(*[name for name in self.AUDITS if name != "audit_self_installed_apps"])
        self.app("zzz-not-in-any-bootstrap", self.program("tools/bin/run"))

        code, output = self.run_main()

        self.assertEqual(code, 0)
        self.assertIn("[NOTICE] 1 app(s) start from your home folder", output)
        self.assertIn("Installation is aligned (0 warning(s), 1 notice(s)).", output)

    def test_list_extra_prints_the_apps_after_the_packages_and_audits_nothing_else(self) -> None:
        self.stub(*self.AUDITS, side_effect=AssertionError("no audit may run for --list-extra"))
        patcher = mock.patch.object(
            self.audit,
            "find_extra_packages",
            lambda repo_root, profile, packages: self.audit.ExtraPackages(found={"apt": ["ffmpeg"], "snap": []}),
        )
        patcher.start()
        self.addCleanup(patcher.stop)
        program = self.program("tools/bin/run")
        self.app("zzz-b", program)
        self.app("zzz-a", program)

        code, output = self.run_main("--list-extra")

        self.assertEqual(code, 0)
        self.assertEqual(output, "apt:ffmpeg\napp:zzz-a\napp:zzz-b\n")

    def test_list_extra_prints_no_app_line_when_there_is_no_folder_of_launchers(self) -> None:
        self.stub(*self.AUDITS, side_effect=AssertionError("no audit may run for --list-extra"))
        self.no_extra_packages()

        code, output = self.run_main("--list-extra")

        self.assertEqual((code, output), (0, ""))

    def test_the_list_extra_output_is_what_the_keep_file_reads_back(self) -> None:
        # `--list-extra >> known-extra-packages` accepts everything listed at once, apps included.
        self.stub(*self.AUDITS)
        self.no_extra_packages()
        program = self.program("tools/bin/run")
        self.app("zzz-one", program)
        self.app("zzz.two", program)
        _, listing = self.run_main("--list-extra")
        self.assertEqual(listing, "app:zzz-one\napp:zzz.two\n")

        self.keep_file(listing)
        apps = self.find()

        self.assertEqual((apps.found, apps.acknowledged), ([], 2))


if __name__ == "__main__":
    unittest.main()
