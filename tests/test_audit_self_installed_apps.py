"""Apps installed by their own installer: what the audit finds, declares, accepts and prints.

Zed, GPT4All and AppImages put a program in the home folder and leave a launcher in
~/.local/share/applications. No package manager records them, so the package report cannot see them,
and the audit reads the launcher instead. Like the package report it is a notice that never changes
the exit code. It prints a `Fix:` command that removes an app only where the place of its program
proves what belongs to it (an AppImage, an app folder in an install prefix, the folder of a Qt
installer); for any other app it prints no command.

Every test runs against a throwaway home folder: HOME, XDG_DATA_HOME and XDG_CONFIG_HOME all point
into a scratch directory, so a test can neither read the real launchers nor touch them. The tests of
the removal command run it, with `bash`, against that folder.
"""

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


class RemovalCase(AppsCase):
    """Helpers for the tests of the files that make up an app and of the command that removes them."""

    def installed(self, relative: str, stem: str = "app") -> tuple[pathlib.Path, pathlib.Path]:
        """An app whose program lies at `relative` in the home folder: (program, launcher)."""
        program = self.program(relative)
        return program, self.app(stem, program)

    def shortcut(self, file_name: str, program: pathlib.Path, folder: pathlib.Path | None = None) -> pathlib.Path:
        """A desktop shortcut for the program, written the way the Qt installer does (quoted Exec)."""
        folder = folder or self.home / "Desktop"
        folder.mkdir(parents=True, exist_ok=True)
        path = folder / file_name
        path.write_text(f'[Desktop Entry]\nType=Application\nExec="{program}"\nName=Shortcut\n', encoding="utf-8")
        return path

    def uninstaller(self, folder: str, name: str = "maintenancetool") -> None:
        (self.home / folder / name).write_text("", encoding="utf-8")

    def files(self, stem: str = "app") -> tuple[pathlib.Path, ...]:
        """What the audit would delete for the app with this key, once it is shown to be safe to name."""
        (app,) = [app for app in self.find().found if app.key == stem]
        never_whole = {self.home, self.home.parent, self.scratch}
        never_whole |= {
            self.home / name
            for name in (
                ".local", ".local/bin", ".local/share", ".local/share/applications", ".local/opt", ".opt", "opt",
                "Applications", "bin", "Desktop", ".config", ".cache",
            )
        }
        for path in app.files:
            self.assertIn(self.home, path.parents, f"{path} is not inside the home folder")
            self.assertNotIn(path, never_whole, f"{path} must never be removed whole")
        return app.files


class RemovalTests(RemovalCase):
    """Which files make up an app: SelfInstalledApp.files, empty where nothing proves what belongs to it."""

    def test_an_appimage_goes_with_its_launcher(self) -> None:
        program, launcher = self.installed("Applications/cursor.AppImage", "cursor")

        self.assertEqual(self.files("cursor"), (program, launcher))

    def test_an_appimage_is_recognised_in_any_folder_and_in_any_case(self) -> None:
        for relative in ("Downloads/Foo-1.2.AppImage", "stuff/tool.appimage", "x/y/Z.APPIMAGE"):
            with self.subTest(relative):
                program, launcher = self.installed(relative, "foo")

                self.assertEqual(self.files("foo"), (program, launcher))

    def test_an_appimage_inside_an_app_folder_goes_without_the_folder(self) -> None:
        program, launcher = self.installed("Applications/Foo/foo.AppImage", "foo")

        self.assertEqual(self.files("foo"), (program, launcher))

    def test_a_link_to_an_appimage_goes_with_it(self) -> None:
        program, launcher = self.installed("Applications/cursor.AppImage", "cursor")
        link = self.home / ".local" / "bin" / "cursor"
        link.parent.mkdir(parents=True)
        link.symlink_to(program)

        self.assertEqual(self.files("cursor"), (program, link, launcher))

    def test_an_app_folder_in_an_install_prefix_goes_whole(self) -> None:
        folders = {
            ".local/zed.app/bin/zed": ".local/zed.app",
            ".local/zed-preview.app/libexec/zed-editor": ".local/zed-preview.app",
            ".local/opt/foo/bin/foo": ".local/opt/foo",
            ".opt/foo/bin/foo": ".opt/foo",
            "opt/foo/foo": "opt/foo",
            "Applications/Foo/bin/foo": "Applications/Foo",
        }
        for relative, folder in folders.items():
            with self.subTest(relative):
                _, launcher = self.installed(relative, "foo")

                self.assertEqual(self.files("foo"), (self.home / folder, launcher))

    def test_the_folder_of_a_qt_installer_goes_whole(self) -> None:
        _, launcher = self.installed("gpt4all/bin/chat", "gpt4all")
        self.uninstaller("gpt4all")

        self.assertEqual(self.files("gpt4all"), (self.home / "gpt4all", launcher))

    def test_the_uninstaller_may_be_spelled_either_way_and_the_program_may_be_deep(self) -> None:
        _, launcher = self.installed("Qt/Tools/QtCreator/bin/qtcreator", "qtcreator")
        self.uninstaller("Qt", "MaintenanceTool")

        self.assertEqual(self.files("qtcreator"), (self.home / "Qt", launcher))

    def test_the_nearest_folder_with_an_uninstaller_is_the_app(self) -> None:
        _, launcher = self.installed("suite/tools/app/bin/app", "app")
        self.uninstaller("suite")
        self.uninstaller("suite/tools/app")

        self.assertEqual(self.files("app"), (self.home / "suite" / "tools" / "app", launcher))

    def test_an_uninstaller_in_the_home_folder_itself_does_not_make_the_home_folder_an_app(self) -> None:
        self.installed("tools/bin/run", "run")
        self.uninstaller(".")

        self.assertEqual(self.files("run"), ())

    def test_a_place_that_does_not_say_what_belongs_to_the_app_gets_no_files(self) -> None:
        for relative in (
            "gpt4all/bin/chat",  # a folder of the home folder, but no uninstaller in it
            "projects/bin/tool",  # the owner's own folder
            "code/tool",
            ".local/bin/script",  # a folder that many programs share
            ".local/share/foo/bin/foo",
            ".local/zed.app.old/bin/zed",  # a name that merely contains .app
            ".local/pipx/venvs/foo/bin/foo",  # a folder that tools share
            ".local/cargo/bin/foo",
            ".local/lib/foo/bin/foo",
            ".local/opt/tool",  # a file straight in the prefix
            "opt/bin/tool",  # the prefix's own folder
            "bin/tool",
            "Applications/tool",  # a file, and not an AppImage
            "tool",  # straight in the home folder
        ):
            with self.subTest(relative):
                self.installed(relative, "foo")

                self.assertEqual(self.files("foo"), ())

    def test_a_link_onto_the_path_goes_with_the_app(self) -> None:
        program, launcher = self.installed(".local/zed.app/bin/zed", "zed")
        inner = self.program(".local/zed.app/libexec/zed-editor")
        first = self.home / ".local" / "bin" / "zed"
        second = self.home / "bin" / "zed-editor"
        for link, target in ((first, program), (second, inner)):
            link.parent.mkdir(parents=True, exist_ok=True)
            link.symlink_to(target)

        self.assertEqual(self.files("zed"), (self.home / ".local" / "zed.app", first, second, launcher))

    def test_what_is_not_a_link_into_the_app_stays(self) -> None:
        _, launcher = self.installed(".local/zed.app/bin/zed", "zed")
        folder = self.home / ".local" / "bin"
        folder.mkdir(parents=True)
        (folder / "other").symlink_to(self.program("elsewhere/bin/other"))
        (folder / "dangling").symlink_to(self.home / "missing")
        (folder / "zed-copy").write_text("#!/bin/sh\n", encoding="utf-8")  # a plain file, not a link

        self.assertEqual(self.files("zed"), (self.home / ".local" / "zed.app", launcher))

    def test_every_launcher_and_shortcut_that_starts_the_app_goes_with_it(self) -> None:
        program, launcher = self.installed("gpt4all/bin/chat", "gpt4all")
        self.uninstaller("gpt4all")
        second = self.app("gpt4all-chat", f'"{program}" --chat')
        shortcut = self.shortcut("GPT4All.desktop", program)

        self.assertEqual(self.files("gpt4all"), (self.home / "gpt4all", second, launcher, shortcut))

    def test_the_desktop_folder_is_the_one_user_dirs_dirs_names(self) -> None:
        program, _ = self.installed("gpt4all/bin/chat", "gpt4all")
        self.uninstaller("gpt4all")
        (self.home / ".config").mkdir()
        (self.home / ".config" / "user-dirs.dirs").write_text(
            '# comment\nXDG_DESKTOP_DIR="$HOME/Escritorio"\n', encoding="utf-8"
        )
        not_the_desktop = self.shortcut("GPT4All.desktop", program)
        shortcut = self.shortcut("GPT4All.desktop", program, self.home / "Escritorio")

        files = self.files("gpt4all")

        self.assertIn(shortcut, files)
        self.assertNotIn(not_the_desktop, files)

    def test_only_desktop_files_are_launchers(self) -> None:
        # Copies and notes that repeat a launcher's text are not launchers.
        _, launcher = self.installed(".local/zed.app/bin/zed", "zed")
        (self.home / "Desktop").mkdir()
        for path in (self.applications / "zed.desktop.bak", self.applications / "zed.txt", self.home / "Desktop" / "zed-notes"):
            path.write_text(launcher.read_text(encoding="utf-8"), encoding="utf-8")

        self.assertEqual(self.files("zed"), (self.home / ".local" / "zed.app", launcher))

    def test_a_launcher_of_another_program_stays(self) -> None:
        _, launcher = self.installed(".local/zed.app/bin/zed", "zed")
        other = self.program("elsewhere/bin/other")
        self.app("other", other)
        self.shortcut("Other.desktop", other)

        self.assertEqual(self.files("zed"), (self.home / ".local" / "zed.app", launcher))

    def test_a_desktop_file_that_cannot_be_read_is_ignored(self) -> None:
        _, launcher = self.installed(".local/zed.app/bin/zed", "zed")
        (self.applications / "folder.desktop").mkdir()
        (self.applications / "garbage.desktop").write_bytes(b"\xff\xfe\x00 not a launcher")
        (self.home / "Desktop").mkdir()
        (self.home / "Desktop" / "empty.desktop").write_text("", encoding="utf-8")

        self.assertEqual(self.files("zed"), (self.home / ".local" / "zed.app", launcher))

    def test_a_program_outside_the_home_folder_has_no_location(self) -> None:
        for relative in ("tool/bin/run", "tool/Foo.AppImage"):
            with self.subTest(relative):
                program = self.program(relative, outside=True)

                self.assertIsNone(self.audit.app_location(program, self.home))

    def test_a_folder_named_like_an_appimage_is_not_one(self) -> None:
        folder = self.home / "Applications" / "Foo.AppImage"
        folder.mkdir(parents=True)
        self.app("foo", folder)

        self.assertEqual(self.files("foo"), ())

    def test_an_uninstaller_in_a_folder_that_tools_share_does_not_make_it_an_app(self) -> None:
        for relative, folder in (
            ("Applications/tool", "Applications"),
            ("opt/tool", "opt"),
            (".local/opt/tool", ".local/opt"),
            (".local/bin/script", ".local"),
            ("bin/tool", "bin"),
            ("projects/lib/tool", "projects/lib"),  # named like the folder of a prefix
        ):
            with self.subTest(relative):
                self.installed(relative, "foo")
                self.uninstaller(folder)

                self.assertEqual(self.files("foo"), ())

    def test_a_folder_where_links_are_made_is_shared_whatever_it_is_called(self) -> None:
        self.installed("tools/run", "foo")
        self.uninstaller("tools")

        with mock.patch.object(self.audit, "LINK_FOLDERS", (".local/bin", "bin", "tools")):
            files = self.files("foo")

        self.assertEqual(files, ())

    def test_no_folder_named_like_a_part_of_a_prefix_is_an_app(self) -> None:
        for name in ("bin", "etc", "include", "lib", "lib64", "libexec", "opt", "sbin", "share", "src", "state", "var"):
            with self.subTest(name):
                self.installed(f"opt/{name}/tool", "foo")
                self.uninstaller(f"opt/{name}")

                self.assertEqual(self.files("foo"), ())

    def test_a_folder_named_like_the_uninstaller_is_not_one(self) -> None:
        self.installed("gpt4all/bin/chat", "gpt4all")
        (self.home / "gpt4all" / "maintenancetool").mkdir()

        self.assertEqual(self.files("gpt4all"), ())

    def test_a_file_seen_through_a_linked_folder_is_not_taken_for_a_link(self) -> None:
        # ~/bin leads into the app, so ~/bin/zed is the program itself, which goes with the folder.
        _, launcher = self.installed(".local/zed.app/bin/zed", "zed")
        (self.home / "bin").symlink_to(self.home / ".local" / "zed.app" / "bin")

        self.assertEqual(self.files("zed"), (self.home / ".local" / "zed.app", launcher))

    def test_a_link_that_cannot_be_followed_is_left_alone(self) -> None:
        _, launcher = self.installed(".local/zed.app/bin/zed", "zed")
        folder = self.home / ".local" / "bin"
        folder.mkdir(parents=True)
        for name in ("loop", "locked"):
            (folder / name).symlink_to(folder / name)
        failures = {"loop": RuntimeError("Symlink loop"), "locked": PermissionError("Permission denied")}
        resolve = pathlib.Path.resolve

        def refusing(path: pathlib.Path, *args: object, **kwargs: object) -> pathlib.Path:
            if path.name in failures:
                raise failures[path.name]
            return resolve(path, *args, **kwargs)

        with mock.patch.object(pathlib.Path, "resolve", refusing):
            files = self.files("zed")

        self.assertEqual(files, (self.home / ".local" / "zed.app", launcher))

    def test_a_file_is_named_once_even_when_two_folders_are_the_same(self) -> None:
        program, launcher = self.installed("Applications/cursor.AppImage", "cursor")
        (self.home / ".config").mkdir()
        (self.home / ".config" / "user-dirs.dirs").write_text(
            f'XDG_DESKTOP_DIR="{self.applications}"\n', encoding="utf-8"
        )

        self.assertEqual(self.files("cursor"), (program, launcher))

    def test_the_files_come_in_a_fixed_order_whatever_order_the_folders_list_them_in(self) -> None:
        program, launcher = self.installed("gpt4all/bin/chat", "gpt4all")
        self.uninstaller("gpt4all")
        second = self.app("gpt4all-chat", f'"{program}"')
        links = [self.home / ".local" / "bin" / name for name in ("a-chat", "b-chat")]
        links[0].parent.mkdir(parents=True)
        for link in links:
            link.symlink_to(program)
        listing = pathlib.Path.glob

        def backwards(folder: pathlib.Path, pattern: str, **kwargs: object):
            return iter(sorted(listing(folder, pattern, **kwargs), reverse=True))

        with mock.patch.object(pathlib.Path, "glob", backwards):
            files = self.files("gpt4all")

        self.assertEqual(files, (self.home / "gpt4all", *links, second, launcher))

    def test_a_folder_that_cannot_be_listed_gives_no_entries(self) -> None:
        with mock.patch.object(pathlib.Path, "glob", side_effect=PermissionError("denied")):
            self.assertEqual(self.audit.directory_entries(self.home), [])

    def test_an_accepted_app_needs_no_files(self) -> None:
        self.installed(".local/zed.app/bin/zed", "zed")
        self.keep_file("app:zed\n")

        self.assertEqual(self.find().found, [])


class DesktopFolderTests(AppsCase):
    """Where the desktop keeps its shortcuts."""

    def desktop(self, text: str | None = None) -> pathlib.Path:
        if text is not None:
            (self.home / ".config").mkdir(exist_ok=True)
            (self.home / ".config" / "user-dirs.dirs").write_text(text, encoding="utf-8")
        return self.audit.user_desktop_dir()

    def test_it_is_the_desktop_folder_of_the_home_folder_by_default(self) -> None:
        self.assertEqual(self.desktop(), self.home / "Desktop")

    def test_it_follows_xdg_desktop_dir(self) -> None:
        self.assertEqual(self.desktop('XDG_DESKTOP_DIR="$HOME/Escritorio"\n'), self.home / "Escritorio")

    def test_an_absolute_path_is_taken_as_it_is(self) -> None:
        self.assertEqual(self.desktop(f'XDG_DESKTOP_DIR="{self.outside}/desk"\n'), self.outside / "desk")

    def test_the_home_folder_itself_means_there_is_no_desktop_folder(self) -> None:
        # The user-dirs specification switches a folder off by pointing it at $HOME/.
        self.assertEqual(self.desktop('XDG_DESKTOP_DIR="$HOME/"\n'), self.home / "Desktop")

    def test_a_relative_path_is_no_folder_to_look_in(self) -> None:
        self.assertEqual(self.desktop('XDG_DESKTOP_DIR="Escritorio"\n'), self.home / "Desktop")

    def test_only_a_leading_home_variable_is_expanded(self) -> None:
        cases = {
            '"$HOME"': self.home / "Desktop",  # no folder below the home folder, so no desktop folder
            '"$HOMEFOO"': self.home / "Desktop",  # another variable, which is not a path
            f'"{self.outside}/$HOME/desk"': self.outside / "$HOME" / "desk",  # a name in an absolute path
            '"$HOME/a/$HOME"': self.home / "a" / "$HOME",  # the second one is part of a name
        }
        for value, expected in cases.items():
            with self.subTest(value):
                self.assertEqual(self.desktop(f"XDG_DESKTOP_DIR={value}\n"), expected)

    def test_a_file_without_the_setting_gives_the_default(self) -> None:
        self.assertEqual(self.desktop('# XDG_DESKTOP_DIR="$HOME/Nope"\nXDG_MUSIC_DIR="$HOME/Musica"\n'), self.home / "Desktop")

    def test_it_reads_the_file_from_xdg_config_home(self) -> None:
        config = self.outside / "config"
        config.mkdir()
        (config / "user-dirs.dirs").write_text('XDG_DESKTOP_DIR="$HOME/Escritorio"\n', encoding="utf-8")

        with mock.patch.dict(os.environ, {"XDG_CONFIG_HOME": str(config)}):
            self.assertEqual(self.audit.user_desktop_dir(), self.home / "Escritorio")


class CommandTests(RemovalCase):
    """The removal command: its words, and what a shell does when it runs them."""

    def words(self, stem: str = "app") -> list[str]:
        return self.audit.removal_words(self.files(stem))

    def run_command(self, words: list[str], answer: str) -> subprocess.CompletedProcess[str]:
        """Run the words as a shell reads them once pasted, with this answer to the question rm asks."""
        return subprocess.run(
            ["bash", "-c", " ".join(words)],
            input=answer,
            text=True,
            capture_output=True,
            check=False,
            env={"HOME": str(self.home), "PATH": os.environ.get("PATH", "/usr/bin:/bin")},
        )

    def zed_with_neighbours(self) -> tuple[tuple[pathlib.Path, ...], list[pathlib.Path]]:
        """Zed as its install script leaves it, and files beside it that must survive: (files, kept)."""
        program, _ = self.installed(".local/zed.app/bin/zed", "zed")
        self.program(".local/zed.app/libexec/zed-editor")
        link = self.home / ".local" / "bin" / "zed"
        link.parent.mkdir(parents=True)
        link.symlink_to(program)
        kept = []
        for relative in (
            ".config/zed/settings.json",  # its settings
            ".local/share/zed/db/data",  # its data
            ".cache/zed/x",
            ".local/bin/other",
            ".local/zed.app.notes",  # starts like the folder's name
            ".local/share/applications/other.desktop",
        ):
            path = self.home / relative
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text("keep\n", encoding="utf-8")
            kept.append(path)
        return self.files("zed"), kept

    def test_the_command_asks_once_and_never_forces_or_escalates(self) -> None:
        self.installed(".local/zed.app/bin/zed", "zed")

        self.assertEqual(
            self.words("zed"),
            ["rm", "-rI", "--", "~/.local/zed.app", "~/.local/share/applications/zed.desktop"],
        )

    def test_the_usual_characters_of_a_file_name_need_no_quoting(self) -> None:
        self.installed("Applications/Foo+Bar_1.0@x,y=z:w%/bin/foo", "foo")

        self.assertEqual(self.words("foo")[3], "~/Applications/Foo+Bar_1.0@x,y=z:w%")

    def test_a_path_that_needs_quoting_is_quoted_whole_and_written_in_full(self) -> None:
        # A tilde inside quotes is not expanded, so a quoted word has to carry the whole path.
        program = self.program("Applications/My App $x 'q' *.AppImage")
        self.app("odd", shlex.quote(str(program)))

        self.assertEqual(self.words("odd")[3:], [shlex.quote(str(program)), "~/.local/share/applications/odd.desktop"])

    def test_running_the_command_removes_the_app_and_nothing_else(self) -> None:
        files, kept = self.zed_with_neighbours()

        done = self.run_command(self.audit.removal_words(files), "y\n")

        self.assertEqual(done.returncode, 0, done.stderr)
        for path in files:
            self.assertFalse(path.exists() or path.is_symlink(), f"{path} is still there")
        for path in kept:
            self.assertTrue(path.exists(), f"{path} was removed")
        self.assertTrue((self.home / ".local").is_dir())

    def test_the_command_removes_nothing_unless_it_is_told_to(self) -> None:
        files, _ = self.zed_with_neighbours()

        for answer in ("n\n", "\n", ""):
            with self.subTest(answer=answer):
                self.run_command(self.audit.removal_words(files), answer)

                for path in files:
                    self.assertTrue(path.exists() or path.is_symlink(), f"{path} was removed on the answer {answer!r}")

    def test_a_path_with_spaces_and_shell_characters_survives_the_shell(self) -> None:
        program = self.program("Applications/My App $x 'q' *.AppImage")
        launcher = self.app("odd", shlex.quote(str(program)))
        # What a command that split the name, or let the shell expand the star, would remove as well.
        neighbours = [self.program("Applications/My.AppImage"), self.program("Applications/other.AppImage")]

        done = self.run_command(self.words("odd"), "y\n")

        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertFalse(program.exists() or launcher.exists())
        self.assertTrue(all(path.exists() for path in neighbours))

    def test_the_shell_reads_the_word_of_every_special_character_back_as_the_path(self) -> None:
        # One character per file name, so that a class that lets just one of them through is noticed.
        for character in " $'\"*?;&|<>()\\`!#~{}[]^\t%@+=:,-":
            with self.subTest(character=character):
                program = self.program(f"Applications/a{character}b.AppImage")
                self.app("odd", shlex.quote(str(program)))

                read = subprocess.run(
                    ["bash", "-c", f"printf %s {self.words('odd')[3]}"],
                    capture_output=True, text=True, check=False, env={"HOME": str(self.home), "PATH": os.environ["PATH"]},
                )

                self.assertEqual(read.stdout, str(program), read.stderr)

    def test_a_name_beyond_ascii_is_quoted_whole(self) -> None:
        program, _ = self.installed("Applications/Café/bin/cafe", "cafe")

        self.assertEqual(self.words("cafe")[3], shlex.quote(str(program.parent.parent)))

    def test_a_home_folder_reached_through_a_link_gets_a_command_that_works(self) -> None:
        # Where /home is a link to /var/home, the program resolves to the real path and Path.home() does not.
        real = self.scratch / "real-home"
        self.home.rename(real)
        self.home.symlink_to(real)
        program = self.program("GPT4All Chat/bin/chat")
        self.app("gpt4all", shlex.quote(str(program)))
        self.uninstaller("GPT4All Chat")
        settings = real / ".config" / "gpt4all" / "settings"
        settings.parent.mkdir(parents=True)
        settings.write_text("keep\n", encoding="utf-8")

        (app,) = self.find().found
        words = self.audit.removal_words(app.files)
        done = self.run_command(words, "y\n")

        self.assertEqual(app.files, (real / "GPT4All Chat", self.applications / "gpt4all.desktop"))
        # Outside the unresolved home folder, so written in full and quoted; the launcher still gets the ~.
        self.assertEqual(words[3:], [shlex.quote(str(real / "GPT4All Chat")), "~/.local/share/applications/gpt4all.desktop"])
        self.assertEqual(done.returncode, 0, done.stderr)
        self.assertFalse((real / "GPT4All Chat").exists() or (self.applications / "gpt4all.desktop").exists())
        self.assertTrue(settings.exists())

    def test_command_lines_can_leave_the_words_as_they_are(self) -> None:
        self.assertEqual(self.audit.command_lines(["rm", "~/a b"], 80), ["rm '~/a b'"])
        self.assertEqual(self.audit.command_lines(["rm", "~/a"], 80, quote=str), ["rm ~/a"])

    def test_a_continuation_is_indented_unless_that_would_overflow_the_line(self) -> None:
        word = "x" * 10

        self.assertEqual(self.audit.command_lines(["rm", word], 12, quote=str), ["rm \\", f"  {word}"])
        self.assertEqual(self.audit.command_lines(["rm", word], 11, quote=str), ["rm \\", word])
        self.assertEqual(self.audit.command_lines(["rm", "x" * 20], 11, quote=str), ["rm \\", "x" * 20])


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
        # Cursor (an AppImage) and Zed (an app folder in ~/.local) have a command. GPT4All has none here:
        # nothing in its folder says the folder is the app's.
        self.three_apps()

        reporter, output = self.report()

        self.assertEqual(
            output,
            """
Apps installed outside the bootstrap
  [NOTICE] 3 app(s) start from your home folder and are not declared by the work
           bootstrap:
             Cursor (app:cursor): ~/Applications/cursor.AppImage
               Fix: rm -rI -- ~/Applications/cursor.AppImage \\
                      ~/.local/share/applications/cursor.desktop
             Zed (app:dev.zed.Zed): ~/.local/zed.app/bin/zed
               Fix: rm -rI -- ~/.local/zed.app \\
                      ~/.local/share/applications/dev.zed.Zed.desktop
             GPT4All (app:gpt4all): ~/gpt4all/bin/chat
  Nothing was changed. For each app you can:
    - remove it, with the Fix command under it where one is given (it removes
      the app, its launchers and its links, asks once, and keeps the settings
      and data of the app);
    - or accept it by listing app:NAME in
      ~/.config/dotfiles/known-extra-packages (`--list-extra` prints that
      format).
""",
        )
        self.assertEqual((reporter.notices, reporter.warnings, reporter.issues), (1, 0, 0))

    def test_the_layout_of_an_app_with_a_shortcut_at_80_columns(self) -> None:
        program = self.program("gpt4all/bin/chat")
        self.uninstaller_of("gpt4all")
        self.app("gpt4all", program, name="GPT4All")
        desktop = self.home / "Desktop"
        desktop.mkdir()
        (desktop / "GPT4All.desktop").write_text(f'[Desktop Entry]\nType=Application\nExec="{program}"\n', encoding="utf-8")

        _, output = self.report()

        self.assertIn(
            """\
             GPT4All (app:gpt4all): ~/gpt4all/bin/chat
               Fix: rm -rI -- ~/gpt4all \\
                      ~/.local/share/applications/gpt4all.desktop \\
                      ~/Desktop/GPT4All.desktop
""",
            output,
        )

    def uninstaller_of(self, folder: str) -> None:
        (self.home / folder / "maintenancetool").write_text("", encoding="utf-8")

    def fix_commands(self, output: str) -> list[str]:
        """The commands printed after `Fix:`, each joined back into the one line a shell reads."""
        commands: list[str] = []
        pending: list[str] | None = None
        for line in output.splitlines():
            text = line.strip()
            if pending is None and text.startswith("Fix: "):
                pending = [text[len("Fix: "):]]
            elif pending is not None:
                pending.append(text)
            else:
                continue
            if pending[-1].endswith("\\"):
                pending[-1] = pending[-1][:-1].rstrip()
            else:
                commands.append(" ".join(pending))
                pending = None
        return commands

    def test_each_fix_line_is_the_command_that_removes_that_app(self) -> None:
        self.three_apps()
        self.uninstaller_of("gpt4all")

        _, output = self.report()

        apps = {app.key: app for app in self.find().found}
        self.assertEqual(
            self.fix_commands(output),
            [" ".join(self.audit.removal_words(apps[key].files)) for key in ("cursor", "dev.zed.Zed", "gpt4all")],
        )

    def test_an_app_without_a_safe_command_has_no_fix_line(self) -> None:
        self.three_apps()

        _, output = self.report()

        self.assertEqual(len(self.fix_commands(output)), 2)
        lines = output.splitlines()
        entry = next(index for index, line in enumerate(lines) if "GPT4All (app:gpt4all)" in line)
        self.assertNotIn("Fix:", lines[entry + 1])

    def test_a_fix_command_wraps_with_backslashes_and_never_splits_a_path_at_the_narrowest_width(self) -> None:
        self.width = 60
        name = "Some-Very-Long-Application-Name-1.2.3-x86_64.AppImage"
        program = self.program(f"Applications/{name}")
        launcher = self.app("long", program, name="Long")

        _, output = self.report()

        self.assertEqual(self.fix_commands(output), [" ".join(self.audit.removal_words((program, launcher)))])
        fix_lines = [line for line in output.splitlines() if "Fix: " in line or line.rstrip().endswith("\\") or name in line]
        self.assertGreater(len(fix_lines), 1)
        for line in output.splitlines():
            self.assertTrue(len(line) <= 60 or name in line, line)

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

    def test_a_command_never_forces_escalates_or_names_more_than_paths(self) -> None:
        self.three_apps()
        self.uninstaller_of("gpt4all")

        _, output = self.report()

        commands = self.fix_commands(output)
        self.assertEqual(len(commands), 3)
        for command in commands:
            words = shlex.split(command)
            self.assertEqual(words[:3], ["rm", "-rI", "--"], command)
            for word in words[3:]:
                expanded = pathlib.Path(os.path.expanduser(word))
                self.assertTrue(expanded.is_absolute(), command)
                self.assertIn(self.home, expanded.parents, command)
                self.assertFalse(set(word) & set("*?[]{}$`;&|<>()!"), command)
        self.assertNotRegex(output, r"\bsudo\b")

    def test_nothing_is_removed_by_the_audit_itself(self) -> None:
        self.three_apps()
        self.uninstaller_of("gpt4all")
        before = sorted(path for path in self.home.rglob("*"))

        self.report()

        self.assertEqual(sorted(path for path in self.home.rglob("*")), before)

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
