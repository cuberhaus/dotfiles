#!/usr/bin/env python3
"""Read-only audit of a machine against this dotfiles repository."""

from __future__ import annotations

import argparse
import dataclasses
import gzip
import os
import pathlib
import re
import shlex
import shutil
import subprocess
import sys
import textwrap
import zlib
from collections.abc import Callable, Iterable, Sequence


@dataclasses.dataclass(frozen=True, order=True)
class Package:
    manager: str
    name: str
    # How a snap declaration installs it. It is not part of the package's identity, so
    # it never changes equality, hashing, or sorting.
    classic: bool = dataclasses.field(default=False, compare=False)


PROFILE_SOURCES = {
    "arch": ("arch", "arch_functions"),
    "manjaro": ("manjaro", "arch_functions"),
    "ubuntu": ("ubuntu", "ubuntu_functions"),
    "ubuntu-windows": ("ubuntu_windows", "ubuntu_functions"),
    "mac": ("mac", "mac_functions"),
    "work": ("work", "work_functions"),
}
LINUX_USER_TIMERS = (
    "cuberhaus-user-package-maintenance.timer",
    "cuberhaus-workspace-pull.timer",
)
LINUX_SYSTEM_TIMER = "cuberhaus-system-maintenance.timer"
MACOS_AGENTS = (
    "com.cuberhaus.user-package-maintenance",
    "com.cuberhaus.workspace-pull",
)
MANAGER_VARIABLES = {"apt": "apt", "pac": "pacman", "yay": "yay"}
# Host files profile detection reads; module-level so tests can point them at fakes.
PROC_VERSION = pathlib.Path("/proc/version")
OS_RELEASE = pathlib.Path("/etc/os-release")
# IDEs the bootstraps install as .deb packages. They update only while an apt source offers them.
IDE_APT_PACKAGES = ("code", "cursor", "antigravity")
MIN_APPIMAGE_BYTES = 1_048_576
# What each profile installs system packages with, and the command line that does it.
PROFILE_MANAGERS = {
    "arch": "pacman",
    "manjaro": "pacman",
    "ubuntu": "apt",
    "ubuntu-windows": "apt",
    "work": "apt",
    "mac": "brew",
}
INSTALL_COMMANDS = {
    "apt": "sudo apt-get install -y",
    "pacman": "sudo pacman -S --needed",
    "yay": "yay -S --needed",
    "brew": "brew install",
    "brew-cask": "brew install --cask",
}
# Package that provides each editor command the audit expects.
EDITOR_PACKAGES = {"vim": "vim", "nvim": "neovim"}
# Package that satisfies the terminal-font check, by the profile's package manager.
FONT_PACKAGES = {
    "apt": Package("apt", "fonts-powerline"),
    "brew": Package("brew-cask", "font-meslo-lg-nerd-font"),
}
FIX_PREFIX = "         Fix: "


def uncomment(line: str) -> str:
    lexer = shlex.shlex(line, posix=True)
    lexer.whitespace_split = True
    lexer.commenters = "#"
    try:
        return " ".join(lexer)
    except ValueError:
        return line.split("#", 1)[0].strip()


def shell_words(value: str) -> list[str]:
    try:
        return shlex.split(value, comments=True, posix=True)
    except ValueError:
        return []


def function_bodies(source: str) -> dict[str, str]:
    lines = source.splitlines()
    functions: dict[str, str] = {}
    start_pattern = re.compile(
        r"^\s*(?:function\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*\(\s*\)\s*\{"
    )
    index = 0
    while index < len(lines):
        match = start_pattern.match(lines[index])
        if not match:
            index += 1
            continue
        name = match.group(1)
        body: list[str] = []
        index += 1
        while index < len(lines) and not re.match(r"^}\s*(?:#.*)?$", lines[index]):
            body.append(lines[index])
            index += 1
        if index == len(lines):
            raise ValueError(f"Unterminated shell function: {name}")
        functions[name] = "\n".join(body)
        index += 1
    return functions


def called_functions(source: str, available: set[str]) -> set[str]:
    called: set[str] = set()
    for line in source.splitlines():
        code = uncomment(line)
        if not code:
            continue
        match = re.match(r"^(?:command\s+)?([A-Za-z_][A-Za-z0-9_]*)\b", code)
        if match and match.group(1) in available:
            called.add(match.group(1))
    return called


def active_function_names(bootstrap: str, functions: dict[str, str]) -> set[str]:
    available = set(functions)
    active = called_functions(bootstrap, available)
    pending = list(active)
    while pending:
        name = pending.pop()
        for called in called_functions(functions[name], available) - active:
            active.add(called)
            pending.append(called)
    return active


def arrays_in(body: str) -> dict[str, list[str]]:
    arrays: dict[str, list[str]] = {}
    pattern = re.compile(
        r"(?:^|\n)\s*(?:local\s+)?([A-Za-z_][A-Za-z0-9_]*)=\(\s*\n(.*?)\n\s*\)",
        re.DOTALL,
    )
    for match in pattern.finditer(body):
        values: list[str] = []
        for line in match.group(2).splitlines():
            values.extend(shell_words(line))
        arrays[match.group(1)] = values
    return arrays


def logical_lines(body: str) -> Iterable[str]:
    current = ""
    for raw_line in body.splitlines():
        code = uncomment(raw_line).strip()
        if not code:
            continue
        if code.endswith("\\"):
            current += code[:-1] + " "
            continue
        yield current + code
        current = ""
    if current:
        yield current


def literal_arguments(value: str) -> list[str]:
    ignored = {"||", "&&", ";", "do", "done", "then", "fi"}
    return [
        word
        for word in shell_words(value)
        if word not in ignored
        and not word.startswith(("-", "$", "/", "."))
        and re.fullmatch(r"[A-Za-z0-9@][A-Za-z0-9+_.@:/-]*", word)
    ]


def packages_in_function(body: str) -> set[Package]:
    arrays = arrays_in(body)
    packages: set[Package] = set()

    for line in logical_lines(body):
        variable_call = re.match(r"^\$(apt|pac|yay)\s+(.+)$", line)
        if variable_call:
            manager = MANAGER_VARIABLES[variable_call.group(1)]
            arguments = variable_call.group(2)
            array_match = re.search(r"\$\{([A-Za-z_][A-Za-z0-9_]*)\[@\]\}", arguments)
            names = arrays.get(array_match.group(1), []) if array_match else literal_arguments(arguments)
            packages.update(Package(manager, name) for name in names)

        for apt_match in re.finditer(r"(?:sudo\s+)?apt(?:-get)?\s+install\s+(?:-\S+\s+)*([^;&|]+)", line):
            packages.update(Package("apt", name) for name in literal_arguments(apt_match.group(1)))

        brew_match = re.search(r"\bbrew\s+install\s+(--cask\s+)?(.+)$", line)
        if brew_match:
            manager = "brew-cask" if brew_match.group(1) else "brew"
            arguments = brew_match.group(2)
            array_match = re.search(r"\$\{([A-Za-z_][A-Za-z0-9_]*)\[@\]\}", arguments)
            names = arrays.get(array_match.group(1), []) if array_match else literal_arguments(arguments)
            packages.update(Package(manager, name) for name in names)

        snap_match = re.search(r"(?:sudo\s+)?snap\s+install\s+([^;&|]+)", line)
        if snap_match:
            names = literal_arguments(snap_match.group(1))
            if names:
                classic = "--classic" in shell_words(snap_match.group(1))
                packages.add(Package("snap", names[0], classic))

    return packages


def expected_packages(repo_root: pathlib.Path, profile: str) -> set[Package]:
    bootstrap_name, functions_name = PROFILE_SOURCES[profile]
    bootstrap_dir = repo_root / ".local" / "scripts" / "bootstrap"
    bootstrap = (bootstrap_dir / bootstrap_name).read_text(encoding="utf-8")
    functions = function_bodies(
        (bootstrap_dir / functions_name).read_text(encoding="utf-8")
    )
    active = active_function_names(bootstrap, functions)
    packages: set[Package] = set()
    for name in active:
        packages.update(packages_in_function(functions[name]))
    return packages


def run(command: list[str], cwd: pathlib.Path | None = None) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        command,
        cwd=cwd,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        check=False,
    )


def distro_file_setting(name: str) -> str | None:
    """The value of one `NAME=value` assignment in ~/.config/distro, or None when it is not there.

    Each bootstrap writes that file as a shell fragment that .zshenv sources, so only a real
    assignment counts: an optional `export`, at the start of a line (a comment does not).
    """
    path = pathlib.Path.home() / ".config" / "distro"
    try:
        text = path.read_text(encoding="utf-8", errors="ignore")
    except OSError:
        return None
    match = re.search(rf"^[ \t]*(?:export[ \t]+)?{re.escape(name)}=([A-Za-z0-9_-]+)", text, re.MULTILINE)
    return match.group(1) if match else None


def detect_profile() -> str:
    # A profile a bootstrap recorded beats everything inferred below. DISTRO cannot say "work": the
    # shell config branches on DISTRO=ubuntu, so bootstrap/work records the profile separately.
    # repair-installation has its own copy of this lookup; keep the two in step.
    recorded = distro_file_setting("DOTFILES_PROFILE")
    if recorded is not None:
        if recorded not in PROFILE_SOURCES:
            raise RuntimeError(
                f"DOTFILES_PROFILE={recorded} in ~/.config/distro is not a profile "
                f"({', '.join(PROFILE_SOURCES)}); fix the file or pass --profile explicitly"
            )
        return recorded
    if sys.platform == "darwin":
        return "mac"
    if not sys.platform.startswith("linux"):
        raise RuntimeError("automatic profile detection is supported on Linux and macOS only")
    if PROC_VERSION.exists() and "microsoft" in PROC_VERSION.read_text(
        encoding="utf-8", errors="ignore"
    ).lower():
        return "ubuntu-windows"
    distro = distro_file_setting("DISTRO")
    if distro is not None and distro in PROFILE_SOURCES:
        return distro
    if OS_RELEASE.exists():
        match = re.search(
            r"^ID=\"?([^\"\n]+)",
            OS_RELEASE.read_text(encoding="utf-8", errors="ignore"),
            re.MULTILINE,
        )
        if match and match.group(1) in {"arch", "manjaro", "ubuntu"}:
            return match.group(1)
    raise RuntimeError("could not detect a supported profile; pass --profile explicitly")


def bootstrap_command(profile: str | None) -> str:
    """The Makefile target that provisions a whole machine of this profile."""
    return f"make bootstrap-{profile or '<profile>'}"


def install_commands(packages: Iterable[Package]) -> list[str]:
    """Shell commands that install packages: one per manager, or one per snap (classic takes a flag)."""
    by_manager: dict[str, list[Package]] = {}
    for package in packages:
        by_manager.setdefault(package.manager, []).append(package)
    commands: list[str] = []
    for manager, group in by_manager.items():
        if manager == "snap":
            commands.extend(
                f"sudo snap install {shlex.quote(package.name)}{' --classic' if package.classic else ''}"
                for package in group
            )
        else:
            names = " ".join(shlex.quote(package.name) for package in group)
            commands.append(f"{INSTALL_COMMANDS[manager]} {names}")
    return commands


def install_hint(profile: str | None, *names: str) -> str:
    """Install command for system packages with the profile's package manager; the bootstrap when unknown."""
    manager = PROFILE_MANAGERS.get(profile or "")
    if manager is None:
        return bootstrap_command(profile)
    return install_commands(Package(manager, name) for name in names)[0]


def font_hint(profile: str | None) -> str:
    """The command that installs a terminal font this profile's audit accepts."""
    package = FONT_PACKAGES.get(PROFILE_MANAGERS.get(profile or "", ""))
    return install_commands([package])[0] if package else bootstrap_command(profile)


class Reporter:
    def __init__(self) -> None:
        self.issues = 0
        self.warnings = 0
        # Findings worth knowing about that are not a problem: they never change the exit code.
        self.notices = 0

    def result(self, status: str, message: str, remedy: str | Sequence[str] = "") -> None:
        """Print one finding. A remedy is a command, or several lines (commands, then # notes).

        A message can span several lines; the later ones are indented under the first one's text.
        """
        prefix = f"  [{status}] "
        print(prefix + message.replace("\n", "\n" + " " * len(prefix)))
        lines = [remedy] if isinstance(remedy, str) else list(remedy)
        for index, line in enumerate(line for line in lines if line):
            print(f"{FIX_PREFIX}{line}" if index == 0 else f"{' ' * len(FIX_PREFIX)}{line}")
        if status in {"DRIFT", "MISSING"}:
            self.issues += 1
        elif status == "WARN":
            self.warnings += 1
        elif status == "NOTICE":
            self.notices += 1


def audit_git(repo_root: pathlib.Path, reporter: Reporter) -> None:
    print("\nSource checkout")
    local = run(["git", "-C", str(repo_root), "rev-parse", "HEAD"])
    branch = run(["git", "-C", str(repo_root), "branch", "--show-current"])
    if local.returncode or not re.fullmatch(r"[0-9a-f]{40}\n?", local.stdout):
        reporter.result("MISSING", "The dotfiles checkout is not a usable Git repository.")
        return
    branch_name = branch.stdout.strip()
    if not branch_name:
        reporter.result("WARN", f"The checkout is detached at {local.stdout[:7]}.")
    else:
        remote = run(["git", "-C", str(repo_root), "ls-remote", "--exit-code", "origin", f"refs/heads/{branch_name}"])
        if remote.returncode or not remote.stdout:
            reporter.result("WARN", f"Could not query origin/{branch_name}; remote freshness is unknown.")
        elif remote.stdout.split()[0] == local.stdout.strip():
            reporter.result("OK", f"Checkout matches origin/{branch_name} ({local.stdout[:7]}).")
        else:
            reporter.result("DRIFT", f"Checkout does not match origin/{branch_name}.", f"git pull --ff-only origin {branch_name}")
    status = run(["git", "-C", str(repo_root), "status", "--short"])
    changes = [line for line in status.stdout.splitlines() if line]
    if changes:
        reporter.result("WARN", f"The checkout has {len(changes)} uncommitted path(s).")


def audit_stow(repo_root: pathlib.Path, reporter: Reporter, profile: str | None = None) -> None:
    print("\nStow-managed configs, aliases, functions, and scripts")
    if not shutil.which("stow"):
        reporter.result("MISSING", "GNU Stow is not installed.", install_hint(profile, "stow"))
        return
    result = run(
        [
            "stow",
            "-v",
            "-n",
            "-t",
            str(pathlib.Path.home()),
            "-d",
            str(repo_root.parent),
            repo_root.name,
        ],
        cwd=repo_root,
    )
    output = [
        line
        for line in result.stdout.splitlines()
        if line.strip() and "simulation mode" not in line.lower()
    ]
    if result.returncode or output:
        reporter.result("DRIFT", f"Stow reports {len(output)} pending action/conflict line(s).", "make dry-run, then make restow; restart open shells afterward")
        for line in output[:20]:
            print(f"         {line}")
    else:
        reporter.result("OK", "All Stow-managed files match the repository.")


def expected_shell_paths(home: pathlib.Path) -> tuple[pathlib.Path, ...]:
    return (home / ".local" / "bin", home / ".local" / "scripts" / "bin")


def audit_environment(repo_root: pathlib.Path, reporter: Reporter, profile: str | None = None) -> None:
    print("\nShell environment and PATH")
    home = pathlib.Path.home()
    path_entries = {pathlib.Path(entry).expanduser() for entry in os.environ.get("PATH", "").split(os.pathsep) if entry}
    for expected in expected_shell_paths(home):
        if not expected.is_dir():
            reporter.result("MISSING", f"Expected user executable directory: {expected}", "make restow")
        elif expected not in path_entries:
            reporter.result("DRIFT", f"PATH does not include {expected}.", "Start a new login shell or source ~/.zshenv")
        else:
            reporter.result("OK", f"PATH includes {expected}.")

    dotfiles = os.environ.get("DOTFILES")
    if dotfiles and pathlib.Path(dotfiles).expanduser().resolve() == repo_root.resolve():
        reporter.result("OK", "DOTFILES resolves to this checkout.")
    elif dotfiles:
        reporter.result("DRIFT", f"DOTFILES points to {dotfiles}, not this checkout.", "Start a new login shell after make restow")
    else:
        reporter.result("WARN", "DOTFILES is not set in this process; login-shell initialization could not be verified.")

    for variable in ("EDITOR", "VISUAL"):
        value = os.environ.get(variable)
        if not value:
            reporter.result("WARN", f"{variable} is not set in this process.")
        elif shutil.which(shlex.split(value)[0]):
            reporter.result("OK", f"{variable} resolves to {value}.")
        else:
            command = shlex.split(value)[0]
            package = EDITOR_PACKAGES.get(command)
            fix = (
                install_hint(profile, package)
                if package
                else f"install {command}, or point {variable} at an installed editor in ~/.zshenv"
            )
            reporter.result("MISSING", f"{variable} points to unavailable command: {value}", fix)


def same_file_content(first: pathlib.Path, second: pathlib.Path) -> bool:
    try:
        return first.read_bytes() == second.read_bytes()
    except OSError:
        return False


def audit_shell_configuration(repo_root: pathlib.Path, reporter: Reporter) -> None:
    print("\nAliases and shell functions")
    home = pathlib.Path.home()
    relative_paths = (
        pathlib.Path(".config/zsh/aliases"),
        pathlib.Path(".config/zsh/functions"),
        pathlib.Path(".zshenv"),
    )
    for relative_path in relative_paths:
        source = repo_root / relative_path
        target = home / relative_path
        if target.exists() and same_file_content(source, target):
            reporter.result("OK", f"{relative_path} is deployed from this checkout.")
        else:
            reporter.result("MISSING", f"Managed shell file is missing or differs: ~/{relative_path}", "make restow")


def global_git_value(key: str) -> str:
    result = run(["git", "config", "--global", "--get", key])
    return result.stdout.strip() if result.returncode == 0 else ""


# Keep in step with git_credential_helper_configure (bootstrap/base_functions) and with
# .local/Mini/.gitconfig.
SECURE_CREDENTIAL_HELPER = "cache --timeout=28800"
# The 'store' helper in every spelling Git accepts. It saves passwords unencrypted.
PLAINTEXT_CREDENTIAL_HELPER = re.compile(r"^store(\s|$)|credential[ -]store(\s|$)")


def global_git_credential_helpers() -> list[tuple[str, str]] | None:
    """Every global credential helper as (config key, helper), or None when git cannot say.

    This covers the generic credential.helper and the per-host credential.<url>.helper, because
    either can save a password in plain text.
    """
    result = run(["git", "config", "--global", "--get-regexp", r"^credential\.(.*\.)?helper$"])
    if result.returncode == 1:  # git's answer for "no such key"
        return []
    if result.returncode != 0:
        return None
    helpers = []
    for line in result.stdout.splitlines():
        key, _, helper = line.partition(" ")
        helpers.append((key, helper.strip()))
    return helpers


def audit_git_credential_helpers(reporter: Reporter) -> None:
    helpers = global_git_credential_helpers()
    if helpers is None:
        reporter.result("WARN", "The global git config could not be read; its credential helpers are unknown.")
        return
    plaintext = [(key, helper) for key, helper in helpers if PLAINTEXT_CREDENTIAL_HELPER.search(helper)]
    for key, helper in plaintext:
        reporter.result(
            "DRIFT",
            f"git {key} is {helper!r}, which saves passwords in plain text in ~/.git-credentials.",
            f"git config --global {key} {shlex.quote(SECURE_CREDENTIAL_HELPER)}",
        )
    if plaintext:
        return
    if any(helper for _, helper in helpers):
        reporter.result("OK", "git credential helpers do not save passwords in plain text.")
    else:
        reporter.result("OK", "git has no global credential helper, so it saves no passwords.")


def audit_git_configuration(reporter: Reporter) -> None:
    print("\nGlobal Git configuration")
    expected = {
        "user.name": "cuberhaus",
        "user.email": "polcg10@gmail.com",
    }
    for key, expected_value in expected.items():
        actual = global_git_value(key)
        if actual == expected_value:
            reporter.result("OK", f"git {key} is {expected_value}.")
        elif actual:
            reporter.result("DRIFT", f"git {key} is {actual!r}; expected {expected_value!r}.", f"git config --global {key} {shlex.quote(expected_value)}")
        else:
            reporter.result("MISSING", f"git {key} is not configured.", f"git config --global {key} {shlex.quote(expected_value)}")
    audit_git_credential_helpers(reporter)


def font_families() -> str:
    if shutil.which("fc-list"):
        return run(["fc-list", ":", "family"]).stdout
    if sys.platform == "darwin":
        font_dirs = (pathlib.Path.home() / "Library" / "Fonts", pathlib.Path("/Library/Fonts"))
        return "\n".join(path.name for directory in font_dirs if directory.is_dir() for path in directory.iterdir())
    return ""


def audit_editors_and_fonts(profile: str, reporter: Reporter) -> None:
    print("\nEditors and terminal fonts")
    expected_editor = "nvim" if profile == "mac" else "vim"
    if shutil.which(expected_editor):
        reporter.result("OK", f"Expected editor is available: {expected_editor}.")
    else:
        reporter.result(
            "MISSING",
            f"Expected editor is unavailable: {expected_editor}.",
            install_hint(profile, EDITOR_PACKAGES[expected_editor]),
        )

    families = font_families()
    if re.search(r"Nerd Font|Powerline|Meslo", families, re.IGNORECASE):
        reporter.result("OK", "A Nerd Font or Powerline-compatible terminal font is installed.")
    elif families:
        reporter.result("MISSING", "No Nerd Font or Powerline-compatible font was found.", font_hint(profile))
    else:
        reporter.result("WARN", "Installed terminal fonts could not be enumerated.")


def installed_package_names(manager: str) -> set[str]:
    if manager in {"pacman", "yay"}:
        result = run(["pacman", "-Qq"])
        return set(result.stdout.splitlines()) if not result.returncode else set()
    if manager == "apt":
        result = run(["dpkg-query", "-W", "-f=${binary:Package}\n"])
        return {name.split(":", 1)[0] for name in result.stdout.splitlines()} if not result.returncode else set()
    if manager == "snap":
        result = run(["snap", "list"])
        return {line.split()[0] for line in result.stdout.splitlines()[1:] if line.split()} if not result.returncode else set()
    if manager in {"brew", "brew-cask"}:
        formulae = run(["brew", "list", "--formula", "--full-name"])
        casks = run(["brew", "list", "--cask", "--full-name"])
        return set(formulae.stdout.splitlines()) | set(casks.stdout.splitlines())
    return set()


def cursor_installed_outside_apt() -> bool:
    """Mirror cursor_is_installed in the bootstrap: a `cursor` command or a complete AppImage."""
    if shutil.which("cursor"):
        return True
    app_image = pathlib.Path.home() / "Applications" / "cursor.AppImage"
    try:
        return app_image.stat().st_size >= MIN_APPIMAGE_BYTES
    except OSError:
        return False


# Packages a bootstrap also accepts from another install method, on purpose.
ALTERNATIVE_INSTALLS = {Package("apt", "cursor"): cursor_installed_outside_apt}


def package_is_installed(package: Package, installed: set[str]) -> bool:
    """Whether a declared package is present, by name or through an accepted alternative."""
    names = {package.name, package.name.split("/", 1)[-1]}
    if package.manager == "apt":
        # Ubuntu's 64-bit time_t transition renamed libraries (libfuse2 -> libfuse2t64).
        names |= {f"{name}t64" for name in names}
    if names & installed:
        return True
    return ALTERNATIVE_INSTALLS.get(package, lambda: False)()


def audit_packages(packages: set[Package], reporter: Reporter, profile: str | None = None) -> None:
    print("\nActive bootstrap package declarations")
    for manager in sorted({package.manager for package in packages}):
        declared = {package.name: package for package in packages if package.manager == manager}
        manager_packages = sorted(declared)
        command = {"apt": "dpkg-query", "pacman": "pacman", "yay": "pacman", "snap": "snap", "brew": "brew", "brew-cask": "brew"}[manager]
        if not shutil.which(command):
            reporter.result(
                "MISSING",
                f"{command} is unavailable; {len(manager_packages)} {manager} package(s) cannot be verified.",
                bootstrap_command(profile),
            )
            continue
        installed = installed_package_names(manager)
        missing = [name for name in manager_packages if not package_is_installed(declared[name], installed)]
        if missing:
            reporter.result(
                "MISSING",
                f"{len(missing)} expected {manager} package(s): {', '.join(missing)}",
                package_fix_lines([declared[name] for name in missing], profile),
            )
        else:
            reporter.result("OK", f"All {len(manager_packages)} expected {manager} package(s) are installed.")


def parse_apt_policy(output: str) -> tuple[str, str, bool]:
    """Return the installed and candidate versions, and whether any repository offers the package."""
    installed = re.search(r"^\s*Installed:\s*(\S+)", output, re.MULTILINE)
    candidate = re.search(r"^\s*Candidate:\s*(\S+)", output, re.MULTILINE)
    has_repository = bool(
        re.search(r"^\s+\d+\s+(?:https?|ftp|file|cdrom)://", output, re.MULTILINE)
    )
    return (
        installed.group(1) if installed else "(none)",
        candidate.group(1) if candidate else "(none)",
        has_repository,
    )


def apt_names_with_candidate(names: Iterable[str]) -> set[str] | None:
    """The names an enabled apt source can install, or None when apt-cache cannot tell.

    apt-cache prints nothing for a name it has never heard of, and "Candidate: (none)"
    for one that no enabled source offers. The t64 spelling counts too, because Ubuntu's
    64-bit time_t transition renamed libraries and the new package provides the old name.
    """
    wanted = sorted({variant for name in names for variant in (name, f"{name}t64")})
    if not wanted or not shutil.which("apt-cache"):
        return None
    policy = run(["env", "LC_ALL=C", "apt-cache", "policy", *wanted])
    if policy.returncode:
        return None
    offered: set[str] = set()
    for block in re.split(r"(?m)^(?=\S)", policy.stdout):
        header = re.match(r"(\S+):[ \t]*\n", block)
        if header and parse_apt_policy(block)[1] != "(none)":
            offered.add(header.group(1))
    return offered


def package_fix_lines(missing: Sequence[Package], profile: str | None) -> list[str]:
    """The commands that install missing packages of one manager, plus # notes for apt names no source offers."""
    if missing[0].manager != "apt":
        return install_commands(missing)
    offered = apt_names_with_candidate(package.name for package in missing)
    unavailable = (
        []
        if offered is None
        else [package for package in missing if not {package.name, f"{package.name}t64"} & offered]
    )
    lines = install_commands(package for package in missing if package not in unavailable)
    vendor = [package for package in unavailable if package.name in IDE_APT_PACKAGES]
    retired = [package for package in unavailable if package.name not in IDE_APT_PACKAGES]
    if vendor:
        lines += [
            bootstrap_command(profile),
            f"# {', '.join(package.name for package in vendor)}: no enabled apt source offers it yet;"
            " the bootstrap registers the vendor source before it installs.",
        ]
    if retired:
        lines += [
            f"sudo apt-get update && {install_commands(retired)[0]}",
            f"# {', '.join(package.name for package in retired)}: apt shows no install candidate."
            " If it is still unknown after the update,",
            f"# this release dropped it: replace it in the bootstrap (try: apt-cache search {retired[0].name})",
        ]
    return lines


def audit_ide_update_channels(reporter: Reporter) -> None:
    """Check that every installed IDE .deb can still be updated through apt.

    A release upgrade disables third-party apt sources, after which the weekly
    full-upgrade skips these packages without any error.
    """
    if not (shutil.which("dpkg-query") and shutil.which("apt-cache")):
        return
    installed = installed_package_names("apt")
    ide_packages = [name for name in IDE_APT_PACKAGES if name in installed]
    if not ide_packages:
        return
    print("\nIDE update channels")
    for name in ide_packages:
        policy = run(["env", "LC_ALL=C", "apt-cache", "policy", name])
        current, candidate, has_repository = parse_apt_policy(policy.stdout)
        if not has_repository:
            reporter.result(
                "DRIFT",
                f"{name} {current} has no enabled apt source, so it never updates.",
                "make repair REPAIR=ide-repos (DRY_RUN=true previews it)",
            )
        elif candidate != current:
            reporter.result(
                "WARN",
                f"{name} {current} can be upgraded to {candidate}.",
                "sudo apt-get update && sudo apt-get full-upgrade (the weekly maintenance timer also does this)",
            )
        else:
            reporter.result("OK", f"{name} {current} is the newest version its apt source offers.")


# --- Packages installed outside the bootstrap -------------------------------------------------
#
# The reverse of audit_packages: a package that is on the machine but that no active bootstrap
# function declares. It is a notice, not drift: the owner decides whether to remove it, keep it,
# or add it to the bootstrap.

# Package managers that share one namespace of installed names: pacman and yay read the same
# database, and brew installs a name as a formula or as a cask.
MANAGER_GROUPS = {
    "apt": "apt",
    "snap": "snap",
    "pacman": "pacman",
    "yay": "pacman",
    "brew": "brew",
    "brew-cask": "brew",
}
# What the known-extra-packages file writes in front of the name of an app that came with its own
# installer, where a package writes its manager (`app:dev.zed.Zed`, `apt:ffmpeg`).
APP_KEY = "app"
# Where the Debian and Ubuntu installers that keep one recorded the packages they installed.
INSTALLER_INITIAL_STATUS = pathlib.Path("/var/log/installer/initial-status.gz")
# The snaps the OS image shipped with, and where snapd mounts each installed snap.
SNAP_SEED = pathlib.Path("/var/lib/snapd/seed/seed.yaml")
SNAP_ROOT = pathlib.Path("/snap")
# apt priorities of packages every system has; nobody installs them by hand.
SYSTEM_APT_PRIORITIES = {"required", "important"}
# apt logs every transaction here (history.log, then history.log.1.gz and so on until logrotate
# drops them). A transaction that a person started through sudo carries a Requested-By line; the
# installer's and unattended-upgrades' do not.
APT_HISTORY = pathlib.Path("/var/log/apt")
# One package of an `Install:` line: name:arch (version[, automatic]).
APT_INSTALL_ENTRY = re.compile(r"([A-Za-z0-9][A-Za-z0-9+_.-]*)(?::[A-Za-z0-9-]+)? \(([^)]*)\)")
# What a person types to remove the packages of one report label. Each asks before it acts (no
# -y, --noconfirm or --purge). pacman stays pacman for AUR packages too: the interactive shell
# wraps `yay`, so every yay call there becomes an install.
REMOVE_COMMANDS = {
    "apt": "sudo apt-get remove",
    "snap": "sudo snap remove",
    "pacman": "sudo pacman -Rs",
    "brew": "brew uninstall",
    "brew-cask": "brew uninstall --cask",
}
# Why a package is reported but kept out of the removal command: it looks like part of the
# operating system. Each reason completes "No removal command is suggested, because ...". The
# first part covers what SYSTEM_PACKAGE_PATTERNS matches; each manager adds the sign of its own
# (see Installed.not_chosen).
LEFT_OUT_REASONS = {
    "apt": "they are kernel, boot loader, firmware or driver packages, or the apt log shows no request from you to install them",
    "snap": "Canonical publishes them, and its snaps are mostly OS components",
    "pacman": "they are kernel, boot loader, firmware or driver packages",
}
# Names that are part of the operating system itself. They are still reported, but kept out of
# the removal command: a kernel, boot loader, firmware or GPU driver is not something to remove
# by pasting a line. snap and brew cannot break the system, so they have no pattern.
SYSTEM_PACKAGE_PATTERNS = {
    "apt": re.compile(
        r"linux-.*|grub.*|shim-.*|efibootmgr|ubuntu-.*|.*-firmware|firmware-.*|.*-microcode"
        r"|initramfs-tools.*|nvidia-(?:driver|kernel|dkms|utils).*|libnvidia-.*|xserver-xorg.*"
    ),
    "pacman": re.compile(
        r"base|linux\d*(?:-.*)?|.*-ucode|mkinitcpio.*|grub|efibootmgr|nvidia.*|manjaro-.*|mhwd.*"
    ),
}
# A package the bootstrap installs from a downloaded .deb is named only by the variable that
# holds the file ("$chrome_deb"), so expected_packages cannot read it from the install line.
DEB_FILE_PACKAGES = {
    "chrome_deb": "google-chrome-stable",
    "openlogi_deb": "openlogi",
    "rstudio_deb": "rstudio",
    "warp_deb": "warp-terminal",
    "lms_deb": "lm-studio",
}
# Standalone installers that a profile calls and that install apt packages of their own. Each
# list stays inside its script on purpose (see AGENTS.md); this is the function the profile
# calls, its script under .local/scripts, and the bash array in it that lists the packages.
# A step that several profiles share keeps its array in bootstrap/base_functions; it is gated
# on hardware there, so a profile function body must not list its package (the audit would then
# expect it on every machine).
STANDALONE_INSTALLERS = (
    ("asusctl_install", "asusctl_install.sh", "BUILD_PACKAGES"),
    ("nvidia_container_toolkit_install", "bootstrap/base_functions", "NVIDIA_CONTAINER_TOOLKIT_PACKAGES"),
)
# One package per line. Any other line (a warning that run merged in) is skipped.
PACKAGE_LINE = re.compile(r"[A-Za-z0-9][A-Za-z0-9+_.@/-]*(?::[A-Za-z0-9-]+)?")


@dataclasses.dataclass
class Installed:
    """The packages one package manager reports as installed by hand."""

    names: set[str]
    # Names whose records give no sign that a person chose them (apt: no logged install request;
    # snap: published by Canonical). They stay in the report but never go into a removal command.
    not_chosen: set[str] = dataclasses.field(default_factory=set)


@dataclasses.dataclass
class ExtraPackages:
    # Report label (apt, snap, pacman, brew, brew-cask) -> sorted names that no declaration
    # covers. A label whose package manager could be read but has none maps to an empty list.
    found: dict[str, list[str]]
    # Report label -> the names of `found` that have no sign a person chose them.
    not_chosen: dict[str, set[str]] = dataclasses.field(default_factory=dict)
    # How many more the known-extra-packages file left out of `found`.
    acknowledged: int = 0


def known_extra_packages_path() -> pathlib.Path:
    """The per-machine file that lists the packages its owner keeps outside the bootstrap."""
    config_home = os.environ.get("XDG_CONFIG_HOME") or str(pathlib.Path.home() / ".config")
    return pathlib.Path(config_home) / "dotfiles" / "known-extra-packages"


def display_path(path: pathlib.Path) -> str:
    """The path with the home directory written as ~."""
    try:
        return f"~/{path.relative_to(pathlib.Path.home())}"
    except ValueError:
        return str(path)


def known_extra_entries(path: pathlib.Path) -> set[tuple[str | None, str]]:
    """The (manager group, name) pairs a known-extra-packages file lists.

    One `manager:name` per line, as `--list-extra` prints it, or a bare name that matches under any
    package manager. An app that came with its own installer is `app:name`, and its group is
    APP_KEY. Blank lines and `#` comments are ignored, as is a file that cannot be read.
    """
    try:
        text = path.read_text(encoding="utf-8", errors="ignore")
    except OSError:
        return set()
    entries: set[tuple[str | None, str]] = set()
    for line in text.splitlines():
        entry = line.split("#", 1)[0].strip()
        if not entry:
            continue
        manager, separator, name = entry.partition(":")
        if separator and name and manager in MANAGER_GROUPS:
            entries.add((MANAGER_GROUPS[manager], name))
        elif separator and name and manager == APP_KEY:
            entries.add((APP_KEY, name))
        else:
            entries.add((None, entry))
    return entries


def package_lines(text: str) -> set[str]:
    """Package names from command output that prints one per line, without any architecture suffix."""
    return {
        line.strip().split(":", 1)[0]
        for line in text.splitlines()
        if PACKAGE_LINE.fullmatch(line.strip())
    }


def installer_initial_packages() -> set[str]:
    """Every package the Debian or Ubuntu installer put on the machine, when it kept a record.

    Subiquity (the Ubuntu 23.04+ installer) keeps none, so on those machines this is empty.
    """
    try:
        with gzip.open(INSTALLER_INITIAL_STATUS, "rt", encoding="utf-8", errors="ignore") as status:
            return {line[len("Package: "):].strip() for line in status if line.startswith("Package: ")}
    except (OSError, EOFError, zlib.error):  # missing, unreadable, or cut short
        return set()


def apt_requested_packages() -> set[str]:
    """Packages that a person asked apt to install, according to the retained apt logs.

    Only a transaction with a Requested-By line counts (sudo records it; the installer and
    unattended-upgrades do not), and only the packages it did not mark automatic, which leaves
    out the dependencies it pulled in. The logs rotate, so an old install can be missing.
    """
    requested: set[str] = set()
    for log in sorted(APT_HISTORY.glob("history.log*")):
        opener = gzip.open if log.suffix == ".gz" else open
        try:
            with opener(log, "rt", encoding="utf-8", errors="ignore") as handle:
                text = handle.read()
        except (OSError, EOFError, zlib.error):  # unreadable, or a truncated archive
            continue
        for transaction in re.split(r"\n\s*\n", text):
            if not re.search(r"(?m)^Requested-By:", transaction):
                continue
            for line in re.findall(r"(?m)^Install: (.*)$", transaction):
                requested.update(
                    name for name, details in APT_INSTALL_ENTRY.findall(line) if "automatic" not in details
                )
    return requested


def apt_installed_by_hand() -> Installed | None:
    """apt packages marked as manually installed, minus those the OS ships; None when apt cannot say.

    A package nobody logged a request for is kept (the OS installer marks hundreds as manual) but
    flagged not_chosen.
    """
    if not (shutil.which("apt-mark") and shutil.which("dpkg-query")):
        return None
    manual = run(["apt-mark", "showmanual"])
    status = run(["dpkg-query", "-W", "-f=${binary:Package}\t${Essential}\t${Priority}\n"])
    if manual.returncode or status.returncode:
        return None
    system: set[str] = set()
    for line in status.stdout.splitlines():
        name, _, rest = line.partition("\t")
        essential, _, priority = rest.partition("\t")
        if essential == "yes" or priority in SYSTEM_APT_PRIORITIES:
            system.add(name.split(":", 1)[0])
    names = package_lines(manual.stdout) - system - installer_initial_packages()
    return Installed(names, not_chosen=names - apt_requested_packages())


def snap_seed_names() -> set[str]:
    """The snaps the OS image shipped with."""
    try:
        text = SNAP_SEED.read_text(encoding="utf-8", errors="ignore")
    except OSError:
        return set()
    return set(re.findall(r"(?m)^\s*(?:-\s*)?name:\s*([A-Za-z0-9][A-Za-z0-9-]*)\s*$", text))


def snap_dependencies(names: Iterable[str]) -> set[str]:
    """Snaps that other installed snaps are built on, or take content from (their default providers)."""
    dependencies: set[str] = set()
    for name in names:
        try:
            manifest = (SNAP_ROOT / name / "current" / "meta" / "snap.yaml").read_text(
                encoding="utf-8", errors="ignore"
            )
        except OSError:
            continue
        base = re.search(r"(?m)^base:\s*([A-Za-z0-9][A-Za-z0-9-]*)", manifest)
        if base:
            dependencies.add(base.group(1))
        dependencies.update(
            re.findall(r"(?m)^\s*default-provider:\s*([A-Za-z0-9][A-Za-z0-9-]*)", manifest)
        )
    return dependencies


def snap_installed_by_hand() -> Installed | None:
    """Installed snaps minus base snaps, snapd, the OS image's own snaps and snap dependencies.

    A snap that Canonical publishes is kept but flagged not_chosen: Ubuntu installs components such
    as prompting-client and desktop-security-center on its own, after the image was built.
    """
    if not shutil.which("snap"):
        return None
    result = run(["snap", "list"])
    if result.returncode:
        return None
    lines = result.stdout.splitlines()
    if not lines or not lines[0].startswith("Name"):
        return Installed(set())  # "No snaps are installed yet."
    # Columns: Name Version Rev Tracking Publisher Notes. The publisher of a verified account
    # ends in "**", and the last column says "base" for a base snap and "snapd" for snapd itself.
    rows = [row for row in (line.split() for line in lines[1:]) if len(row) >= 6]
    names = {row[0] for row in rows}
    infrastructure = {row[0] for row in rows if {"base", "snapd"} & set(row[-1].split(","))}
    by_hand = names - infrastructure - snap_seed_names() - snap_dependencies(names)
    canonical = {row[0] for row in rows if row[4].rstrip("*") == "canonical"}
    return Installed(by_hand, not_chosen=by_hand & canonical)


def pacman_installed_by_hand(declared: Iterable[str]) -> Installed | None:
    """Explicitly installed pacman packages (repository and AUR), minus members of declared groups."""
    if not shutil.which("pacman"):
        return None
    explicit = run(["pacman", "-Qqe"])
    if explicit.returncode:
        return None
    names = package_lines(explicit.stdout)
    declared = sorted(declared)
    if declared:
        # Installing a group (base-devel, xorg) marks every member explicit. -Qg exits 1 when some
        # of the names are not groups, so only what it prints counts.
        names -= package_lines(run(["pacman", "-Qgq", *declared]).stdout)
    return Installed(names)


def brew_installed_by_hand() -> dict[str, Installed] | None:
    """Homebrew formulae that no other formula or cask needs, and every installed cask."""
    if not shutil.which("brew"):
        return None
    leaves = run(["brew", "leaves"])
    casks = run(["brew", "list", "--cask"])
    if leaves.returncode or casks.returncode:
        return None
    return {"brew": Installed(package_lines(leaves.stdout)), "brew-cask": Installed(package_lines(casks.stdout))}


def installed_by_hand(group: str, declared: Iterable[str]) -> dict[str, Installed] | None:
    """What a person installed with one package manager, under the label the report uses.

    None when that manager cannot be queried here (not installed, or the query failed).
    """
    if group == "brew":
        return brew_installed_by_hand()
    installed = {
        "apt": apt_installed_by_hand,
        "snap": snap_installed_by_hand,
        "pacman": lambda: pacman_installed_by_hand(declared),
    }[group]()
    return None if installed is None else {group: installed}


def comparable_names(group: str, names: Iterable[str]) -> set[str]:
    """Every spelling under which an installed package can answer to one of these declared names."""
    spellings: set[str] = set()
    for name in names:
        spellings.add(name)
        if group == "apt":
            # dpkg names are lowercase, and Ubuntu's 64-bit time_t transition renamed libraries
            # (libfuse2 -> libfuse2t64).
            spellings.update({name.lower(), f"{name.lower()}t64"})
        elif group == "brew":
            spellings.add(name.rsplit("/", 1)[-1])
    return spellings


def script_array(path: pathlib.Path, name: str) -> list[str]:
    """The words of a multi-line bash array `name=( ... )` in a script; empty when it is not there."""
    try:
        source = path.read_text(encoding="utf-8")
    except OSError:
        return []
    match = re.search(rf"(?m)^\s*(?:readonly\s+)?{re.escape(name)}=\(\s*\n(.*?)\n\s*\)", source, re.DOTALL)
    if not match:
        return []
    words: list[str] = []
    for line in match.group(1).splitlines():
        words.extend(shell_words(line))
    return words


def bootstrap_installed_packages(repo_root: pathlib.Path, profile: str) -> set[Package]:
    """Packages the profile's bootstrap installs although expected_packages cannot read them.

    The .deb files held in variables (DEB_FILE_PACKAGES) that an active function installs, and the
    packages of each standalone installer (STANDALONE_INSTALLERS) the bootstrap calls. A line that
    is commented out installs nothing, so it names nothing.
    """
    bootstrap_name, functions_name = PROFILE_SOURCES[profile]
    bootstrap_dir = repo_root / ".local" / "scripts" / "bootstrap"
    bootstrap = (bootstrap_dir / bootstrap_name).read_text(encoding="utf-8")
    functions = function_bodies((bootstrap_dir / functions_name).read_text(encoding="utf-8"))
    packages: set[Package] = set()
    for function in active_function_names(bootstrap, functions):
        for line in logical_lines(functions[function]):
            if re.search(r"\b(?:apt(?:-get)?\s+install|dpkg\s+(?:-i|--install))\b", line):
                for variable in re.findall(r"\$\{?([A-Za-z_][A-Za-z0-9_]*)", line):
                    if variable in DEB_FILE_PACKAGES:
                        packages.add(Package("apt", DEB_FILE_PACKAGES[variable]))
    for function, script, array in STANDALONE_INSTALLERS:
        if called_functions(bootstrap, {function}):
            packages.update(
                Package("apt", name) for name in script_array(bootstrap_dir.parent / script, array)
            )
    return packages


def find_extra_packages(repo_root: pathlib.Path, profile: str, packages: set[Package]) -> ExtraPackages:
    """The packages installed by hand that the bootstrap does not declare, per package manager.

    Only managers the profile declares packages for are inspected. A name counts as declared when
    expected_packages or bootstrap_installed_packages names it, in any manager of the same group.
    """
    known = packages | bootstrap_installed_packages(repo_root, profile)
    entries = known_extra_entries(known_extra_packages_path())
    extras = ExtraPackages(found={})
    for group in sorted({MANAGER_GROUPS[package.manager] for package in packages}):
        declared_names = {package.name for package in known if MANAGER_GROUPS[package.manager] == group}
        installed = installed_by_hand(group, declared_names)
        if installed is None:
            continue
        spellings = comparable_names(group, declared_names)
        for label, by_hand in installed.items():
            undeclared = {
                name
                for name in by_hand.names
                if name not in spellings and name.rsplit("/", 1)[-1] not in spellings
            }
            kept = {name for name in undeclared if (group, name) in entries or (None, name) in entries}
            extras.acknowledged += len(kept)
            extras.found[label] = sorted(undeclared - kept)
            extras.not_chosen[label] = by_hand.not_chosen
    return extras


def split_removable(label: str, names: Sequence[str], not_chosen: set[str]) -> tuple[list[str], list[str]]:
    """(removable, left_out) names: the command may name only what looks chosen and is not the OS.

    Left out are the names that match SYSTEM_PACKAGE_PATTERNS and those the manager's records give
    no sign a person chose (not_chosen). They are reported either way; only the command skips them.
    """
    pattern = SYSTEM_PACKAGE_PATTERNS.get(MANAGER_GROUPS[label])
    left_out = [name for name in names if name in not_chosen or (pattern and pattern.fullmatch(name))]
    return [name for name in names if name not in left_out], left_out


def output_width() -> int:
    """Columns for wrapped text: the terminal's, kept between 60 and 100 so lines stay readable."""
    return min(max(shutil.get_terminal_size(fallback=(100, 24)).columns, 60), 100)


def wrap(text: str, width: int, first: str = "", rest: str | None = None) -> list[str]:
    """Fill text to `width` columns, `first` in front of the first line and `rest` of the others.

    A package name is never split: the hyphens in it are not break points.
    """
    return textwrap.wrap(
        text,
        width=width,
        initial_indent=first,
        subsequent_indent=first if rest is None else rest,
        break_long_words=False,
        break_on_hyphens=False,
    )


def command_lines(words: Sequence[str], width: int, quote: Callable[[str], str] = shlex.quote) -> list[str]:
    """One shell command as lines of at most `width` columns; every line but the last ends in a backslash.

    Pasted into a shell, the lines run as the single command that the words spell. Each word goes
    through `quote`; words that are already shell-ready (a path written with ~) pass `str` instead.
    """
    lines: list[str] = []
    line = ""
    for word in map(quote, words):
        # The 2 is the " \" that closes a line when another word follows.
        if line and len(line) + 1 + len(word) + 2 > width:
            lines.append(f"{line} \\")
            # A continuation is indented by two spaces, unless the word then no longer fits the line.
            line = f"  {word}" if len(word) + 2 <= width else word
        else:
            line = f"{line} {word}" if line else word
    lines.append(line)
    return lines


def notice_text(headline: str, names: Sequence[str]) -> str:
    """The text of one NOTICE: a wrapped headline, then the names as an indented, wrapped list."""
    width = output_width() - len("  [NOTICE] ")
    return "\n".join([*wrap(headline, width), *wrap(", ".join(names), width, "  ")])


def audit_extra_packages(
    repo_root: pathlib.Path, profile: str, packages: set[Package], reporter: Reporter
) -> None:
    """Notice packages that are installed by hand but declared nowhere in the bootstrap.

    Read-only, and silent when no package manager can be queried (Windows, a bare container).
    Nothing here counts as an issue or a warning: the owner chooses between removing a package,
    keeping it (the known-extra-packages file), or declaring it in the bootstrap.

    Each package manager gets up to two notices, and every name appears once per notice: the
    packages a removal command may name (with that command), then those it must not (OS packages).
    """
    extras = find_extra_packages(repo_root, profile, packages)
    if not extras.found:
        return
    print("\nPackages installed outside the bootstrap")
    for label, names in extras.found.items():
        if not names:
            reporter.result("OK", f"Every {label} package installed by hand is declared by the {profile} bootstrap or accepted.")
            continue
        removable, left_out = split_removable(label, names, extras.not_chosen.get(label, set()))
        if removable:
            headline = f"{len(removable)} {label} package(s) are not declared by the {profile} bootstrap:"
            command = [*REMOVE_COMMANDS[label].split(), *removable]
            reporter.result(
                "NOTICE", notice_text(headline, removable), command_lines(command, output_width() - len(FIX_PREFIX))
            )
        if left_out:
            subject = (
                f"{len(left_out)} more {label} package(s) are not declared either."
                if removable
                else f"{len(left_out)} {label} package(s) are not declared by the {profile} bootstrap."
            )
            reason = LEFT_OUT_REASONS.get(MANAGER_GROUPS[label], "they look like part of the OS")
            reporter.result(
                "NOTICE", notice_text(f"{subject} No removal command is suggested, because {reason}:", left_out)
            )
    keep_file = display_path(known_extra_packages_path())
    width = output_width()
    if any(extras.found.values()):
        print("  Nothing was changed. For each package you can:")
        for option in (
            "remove it, with the Fix command above where one is given;",
            f"declare it in .local/scripts/bootstrap/{PROFILE_SOURCES[profile][1]}, to make it part of the bootstrap;",
            f"or accept it by listing manager:name in {keep_file} (`--list-extra` prints that format).",
        ):
            print("\n".join(wrap(option, width, "    - ", "      ")))
    if extras.acknowledged:
        print("\n".join(wrap(f"{extras.acknowledged} package(s) accepted in {keep_file} are not reported.", width, "  ")))


# Apps that came with their own installer: Zed's install.sh, GPT4All's .run file, an AppImage. They
# put the program in the home folder, and no package manager records them, so the package report
# above cannot see them. The launcher they leave in ~/.local/share/applications is the one trace
# they all share, so that is what this reads. An app without a launcher stays invisible.
#
# A removal command is printed only where the place of the program proves what belongs to the app.
# A command that deleted a folder on a wrong guess would delete someone's work, so any other layout
# gets no command and the owner removes the app by hand.

# A folder in one of these (below the home folder) is the folder of one app, by convention.
APP_FOLDER_PARENTS = (".local/opt", ".opt", "opt", "Applications")
# What a prefix holds besides apps: a folder with one of these names is never the folder of one app.
PREFIX_FOLDERS = frozenset(
    {"bin", "etc", "include", "lib", "lib64", "libexec", "opt", "sbin", "share", "src", "state", "var"}
)
# ~/.local is a prefix that tools share (bin, share, pipx, cargo, ...), so only a bundle named like
# the one Zed's install script unpacks there (~/.local/zed.app) is the folder of one app.
BUNDLE_PARENT = ".local"
BUNDLE_SUFFIX = ".app"
# The Qt Installer Framework leaves its uninstaller in the folder it installed into (GPT4All, the Qt SDK).
INSTALLER_UNINSTALLERS = ("maintenancetool", "MaintenanceTool")
# Where an installer links a program onto the PATH, below the home folder.
LINK_FOLDERS = (".local/bin", "bin")
# In front of the command that removes an app, under the line that names it.
APP_FIX_PREFIX = "    Fix: "
# What shlex.quote leaves unquoted: the characters a path may have to be written with ~.
SHELL_SAFE_PATH = re.compile(r"[\w@%+=:,./-]+", re.ASCII)


@dataclasses.dataclass(frozen=True)
class SelfInstalledApp:
    # The launcher's file name without .desktop: what the known-extra-packages file lists as app:KEY.
    key: str
    # The launcher's Name=, or the key when it has none.
    name: str
    # The program the launcher starts, every symbolic link followed. It is inside the home folder.
    program: pathlib.Path
    # What removing the app deletes: its file or folder, the links onto the PATH, then its launchers
    # and desktop shortcuts. Empty when the place of the program does not say what belongs to the app.
    files: tuple[pathlib.Path, ...] = ()


@dataclasses.dataclass
class SelfInstalledApps:
    # The apps that no active bootstrap step declares and the known-extra-packages file does not accept.
    found: list[SelfInstalledApp]
    # How many more the known-extra-packages file left out of `found`.
    acknowledged: int = 0


def user_applications_dir() -> pathlib.Path:
    """Where the current user's launchers live: $XDG_DATA_HOME/applications, ~/.local/share by default."""
    data_home = os.environ.get("XDG_DATA_HOME") or str(pathlib.Path.home() / ".local" / "share")
    return pathlib.Path(data_home) / "applications"


def desktop_entry(path: pathlib.Path) -> dict[str, str]:
    """The keys of the [Desktop Entry] group of a launcher; empty when the file cannot be read.

    Localized keys (`Name[es]`) are separate keys, and the other groups (`[Desktop Action ...]`, which
    have an Exec of their own) are left out.
    """
    try:
        text = path.read_text(encoding="utf-8", errors="replace")
    except OSError:
        return {}
    entry: dict[str, str] = {}
    in_group = False
    for raw_line in text.splitlines():
        line = raw_line.strip()
        if line.startswith("["):
            if in_group:
                break
            in_group = line == "[Desktop Entry]"
        elif in_group and not line.startswith("#"):
            key, separator, value = line.partition("=")
            if separator:
                entry.setdefault(key.strip(), value.strip())
    return entry


def launcher_program(entry: dict[str, str]) -> pathlib.Path | None:
    """The file a launcher starts, with every symbolic link followed; None when it cannot be told.

    A bare command name is looked up on PATH. An `env VAR=value` prefix is skipped. A launcher that
    runs the program through a shell (`sh -c "..."`) names the shell, which is not in the home folder,
    so such an app is not seen.
    """
    try:
        words = shlex.split(entry.get("Exec", ""))
    except ValueError:
        return None
    if words and words[0] == "env":
        words = words[1:]
    while words and re.fullmatch(r"[A-Za-z_][A-Za-z0-9_]*=.*", words[0]):
        words = words[1:]
    if not words:
        return None
    if "/" in words[0]:
        program = pathlib.Path(words[0]).expanduser()
        if not program.is_absolute():
            return None
    else:
        found = shutil.which(words[0])
        if not found:
            return None
        program = pathlib.Path(found)
    return program.resolve()


def active_bootstrap_code(repo_root: pathlib.Path, profile: str) -> str:
    """The code the profile's bootstrap runs, without comments: its entrypoint and the functions it reaches.

    This is the code the package check reads. A function that nothing calls and a block that is
    commented out run nothing, so they declare nothing.
    """
    bootstrap_name, functions_name = PROFILE_SOURCES[profile]
    bootstrap_dir = repo_root / ".local" / "scripts" / "bootstrap"
    bootstrap = (bootstrap_dir / bootstrap_name).read_text(encoding="utf-8")
    functions = function_bodies((bootstrap_dir / functions_name).read_text(encoding="utf-8"))
    lines = list(logical_lines(bootstrap))
    for name in sorted(active_function_names(bootstrap, functions)):
        lines.extend(logical_lines(functions[name]))
    return "\n".join(lines)


def names_file(code: str, file_name: str) -> bool:
    """Whether the code names this file, and not a longer name that ends the same way."""
    return re.search(rf"(?<![\w.-]){re.escape(file_name)}(?![\w.-])", code) is not None


def user_desktop_dir() -> pathlib.Path:
    """The folder the desktop shows shortcuts from: XDG_DESKTOP_DIR of user-dirs.dirs, ~/Desktop by default."""
    home = pathlib.Path.home()
    config_home = os.environ.get("XDG_CONFIG_HOME") or str(home / ".config")
    try:
        text = (pathlib.Path(config_home) / "user-dirs.dirs").read_text(encoding="utf-8", errors="replace")
    except OSError:
        text = ""
    match = re.search(r'^XDG_DESKTOP_DIR="([^"]*)"', text, re.MULTILINE)
    if match:
        value = match.group(1)
        # Like GLib, which the desktop reads the file with, expand a leading $HOME/ and nothing else.
        if value.startswith("$HOME/"):
            value = str(home) + value[len("$HOME"):]
        folder = pathlib.Path(value)
        # The user-dirs specification switches a folder off by pointing it at the home folder.
        if folder.is_absolute() and folder != home:
            return folder
    return home / "Desktop"


def app_location(program: pathlib.Path, home: pathlib.Path) -> pathlib.Path | None:
    """The file or folder that is the app, when the place of its program says so; None otherwise.

    An AppImage is one file. Any other app is a folder: the one an installer made for it, directly
    in APP_FOLDER_PARENTS or as a `*.app` bundle in ~/.local, or the nearest one that holds the Qt
    installer's uninstaller. A script in ~/.local/bin, a program in a folder of the owner's own, and
    one in a folder that tools share say nothing about what else is in their folder, so they get None.
    The result is never the home folder, a folder that holds apps, or a folder named like a prefix's.
    """
    if home not in program.parents:
        return None
    if program.suffix.lower() == ".appimage" and program.is_file():
        return program
    parents = {home / relative for relative in APP_FOLDER_PARENTS}
    bundle_parent = home / BUNDLE_PARENT
    shared = {bundle_parent, *parents, *(home / relative for relative in LINK_FOLDERS)}
    for folder in program.parents:
        if folder == home:
            break
        if folder in shared or folder.name in PREFIX_FOLDERS:
            continue
        if any((folder / name).is_file() for name in INSTALLER_UNINSTALLERS):
            return folder
        if folder.parent in parents or (folder.parent == bundle_parent and folder.name.endswith(BUNDLE_SUFFIX)):
            return folder
    return None


def directory_entries(directory: pathlib.Path, pattern: str = "*") -> list[pathlib.Path]:
    """The entries of a folder that match the pattern, sorted; none when the folder cannot be read."""
    try:
        return sorted(directory.glob(pattern))
    except OSError:
        return []


def is_within(path: pathlib.Path | None, location: pathlib.Path) -> bool:
    """Whether the path is the file or folder, or lies inside the folder."""
    return path is not None and (path == location or location in path.parents)


def app_files(program: pathlib.Path, home: pathlib.Path) -> tuple[pathlib.Path, ...]:
    """What removing the app deletes: its file or folder, the links onto the PATH, then its launchers.

    Empty where app_location cannot tell what belongs to the app. A launcher is every desktop file
    that starts a program inside what is deleted, in the folder of launchers and on the desktop (an
    installer often leaves a shortcut there too); a link is a symbolic link in LINK_FOLDERS that
    leads there. A file that cannot be read, or leads nowhere near, is left alone.
    """
    location = app_location(program, home)
    if location is None:
        return ()
    files = [location]
    for relative in LINK_FOLDERS:
        for link in directory_entries(home / relative):
            try:
                if link.is_symlink() and is_within(link.resolve(), location):
                    files.append(link)
            except (OSError, RuntimeError):
                continue
    for directory in (user_applications_dir(), user_desktop_dir()):
        for launcher in directory_entries(directory, "*.desktop"):
            if is_within(launcher_program(desktop_entry(launcher)), location):
                files.append(launcher)
    return tuple(dict.fromkeys(files))


def shell_path(path: pathlib.Path) -> str:
    """The path as one shell word: written with ~ where it lies in the home folder and needs no quoting.

    A tilde inside quotes is not expanded, so a path that needs quoting is quoted whole and written in full.
    """
    try:
        relative = path.relative_to(pathlib.Path.home())
    except ValueError:
        return shlex.quote(str(path))
    return f"~/{relative}" if SHELL_SAFE_PATH.fullmatch(str(relative)) else shlex.quote(str(path))


def removal_words(files: Sequence[pathlib.Path]) -> list[str]:
    """The shell words of the command that removes these files: `rm -rI`, which asks once, never `-f`."""
    return ["rm", "-rI", "--", *map(shell_path, files)]


def find_self_installed_apps(repo_root: pathlib.Path, profile: str) -> SelfInstalledApps | None:
    """The launchers of apps that start from the home folder and that the bootstrap does not declare.

    A launcher counts when it is an application entry that is not hidden, and the program it starts
    exists and lies inside the home folder once symbolic links are followed (so `~/.local/bin/code`
    pointing at a system package does not count, and a launcher left behind for a program that is
    gone does not either). It is declared when an active line of the profile's bootstrap names the
    launcher's file, as the Arch bootstraps do for the Cursor and Antigravity AppImages they install.
    An app whose own installer writes the launcher is not recognized that way: the known-extra-packages
    file accepts it (`app:KEY`, or the bare key). Each app found carries the files that removing it
    deletes (app_files), which stay empty where nothing proves what belongs to it.

    None when the directory of launchers does not exist (macOS, a bare container).
    """
    directory = user_applications_dir()
    if not directory.is_dir():
        return None
    home = pathlib.Path.home().resolve()
    declared = active_bootstrap_code(repo_root, profile)
    entries = known_extra_entries(known_extra_packages_path())
    apps = SelfInstalledApps(found=[])
    for launcher in sorted(directory.glob("*.desktop")):
        entry = desktop_entry(launcher)
        if entry.get("Type") != "Application" or entry.get("Hidden", "").lower() == "true":
            continue
        program = launcher_program(entry)
        if program is None or not program.exists() or not program.is_relative_to(home):
            continue
        if names_file(declared, launcher.name):
            continue
        key = launcher.stem
        if (APP_KEY, key) in entries or (None, key) in entries:
            apps.acknowledged += 1
            continue
        apps.found.append(
            SelfInstalledApp(key=key, name=entry.get("Name") or key, program=program, files=app_files(program, home))
        )
    return apps


def audit_self_installed_apps(repo_root: pathlib.Path, profile: str, reporter: Reporter) -> None:
    """Notice apps that came with their own installer and that no active bootstrap step declares.

    Read-only, and silent where there is no directory of launchers. Like the package report, it is a
    notice that never changes the exit code. Under each app it prints the `Fix:` command that
    removes the app (`rm -rI`, so it asks once), but only where the place of the program proves what
    belongs to the app; for any other app there is no command and the owner removes it by hand. The
    owner can also keep an app (the known-extra-packages file). Settings and data are never named.
    """
    apps = find_self_installed_apps(repo_root, profile)
    if apps is None:
        return
    print("\nApps installed outside the bootstrap")
    width = output_width()
    keep_file = display_path(known_extra_packages_path())
    if not apps.found:
        directory = display_path(user_applications_dir())
        message = (
            f"Every launcher in {directory} that starts a program from your home folder "
            f"is declared by the {profile} bootstrap or accepted."
        )
        reporter.result("OK", "\n".join(wrap(message, width - len("  [OK] "))))
    else:
        text_width = width - len("  [NOTICE] ")
        headline = f"{len(apps.found)} app(s) start from your home folder and are not declared by the {profile} bootstrap:"
        lines = wrap(headline, text_width)
        for app in apps.found:
            lines.extend(wrap(f"{app.name} ({APP_KEY}:{app.key}): {display_path(app.program)}", text_width, "  ", "      "))
            if app.files:
                # The command comes under the app it removes; its words are already shell-ready.
                fix = command_lines(removal_words(app.files), text_width - len(APP_FIX_PREFIX), quote=str)
                lines.append(APP_FIX_PREFIX + fix[0])
                lines.extend(" " * len(APP_FIX_PREFIX) + line for line in fix[1:])
        reporter.result("NOTICE", "\n".join(lines))
        print("  Nothing was changed. For each app you can:")
        for option in (
            "remove it, with the Fix command under it where one is given (it removes the app, its "
            "launchers and its links, asks once, and keeps the settings and data of the app);",
            f"or accept it by listing app:NAME in {keep_file} (`--list-extra` prints that format).",
        ):
            print("\n".join(wrap(option, width, "    - ", "      ")))
    if apps.acknowledged:
        print("\n".join(wrap(f"{apps.acknowledged} app(s) accepted in {keep_file} are not reported.", width, "  ")))


def command_succeeds(command: list[str]) -> bool:
    return run(command).returncode == 0


def audit_automations(profile: str, reporter: Reporter) -> None:
    print("\nNative automations")
    if profile == "ubuntu-windows":
        reporter.result("WARN", "WSL automation is owned by Windows Task Scheduler and is not audited here.")
        return
    if profile == "mac":
        if not shutil.which("launchctl"):
            reporter.result("MISSING", "launchctl is unavailable.")
            return
        missing = []
        for label in MACOS_AGENTS:
            plist = pathlib.Path.home() / "Library" / "LaunchAgents" / f"{label}.plist"
            if not plist.is_file() or not command_succeeds(["launchctl", "print", f"gui/{getattr(__import__('os'), 'getuid')()}/{label}"]):
                missing.append(label)
        if missing:
            reporter.result("MISSING", f"LaunchAgent(s): {', '.join(missing)}", "make install-automations")
        else:
            reporter.result("OK", f"All {len(MACOS_AGENTS)} expected LaunchAgents are loaded.")
        return
    if not shutil.which("systemctl"):
        reporter.result("MISSING", "systemctl is unavailable.", "make install-automations")
        return
    missing = [
        timer
        for timer in LINUX_USER_TIMERS
        if not command_succeeds(["systemctl", "--user", "is-enabled", "--quiet", timer])
        or not command_succeeds(["systemctl", "--user", "is-active", "--quiet", timer])
    ]
    if not command_succeeds(["systemctl", "is-enabled", "--quiet", LINUX_SYSTEM_TIMER]) or not command_succeeds(["systemctl", "is-active", "--quiet", LINUX_SYSTEM_TIMER]):
        missing.append(LINUX_SYSTEM_TIMER)
    if missing:
        reporter.result("MISSING", f"Enabled and active timer(s): {', '.join(missing)}", "make install-automations")
    else:
        reporter.result("OK", "All expected systemd timers are enabled and active.")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--profile", choices=("auto", *PROFILE_SOURCES), default="auto")
    parser.add_argument("--list-expected", action="store_true", help="print expected packages without inspecting the host")
    parser.add_argument(
        "--list-extra",
        action="store_true",
        help="print the packages and apps installed outside the bootstrap as manager:name and app:name, the format of the known-extra-packages file, and audit nothing else",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    repo_root = pathlib.Path(__file__).resolve().parents[2]
    try:
        profile = detect_profile() if args.profile == "auto" else args.profile
        packages = expected_packages(repo_root, profile)
    except (OSError, RuntimeError, ValueError) as error:
        print(f"[ERROR] {error}", file=sys.stderr)
        return 2

    if args.list_expected:
        for package in sorted(packages):
            print(f"{package.manager}:{package.name}")
        return 0

    if not (sys.platform.startswith("linux") or sys.platform == "darwin"):
        print("[ERROR] Host auditing is supported on Linux and macOS only; use --list-expected for parser validation.", file=sys.stderr)
        return 2

    if args.list_extra:
        for label, names in find_extra_packages(repo_root, profile, packages).found.items():
            for name in names:
                print(f"{label}:{name}")
        apps = find_self_installed_apps(repo_root, profile)
        for app in apps.found if apps else []:
            print(f"{APP_KEY}:{app.key}")
        return 0

    reporter = Reporter()
    print(f"dotfiles installation audit ({profile})")
    print("Read-only: no files, packages, services, or timers will be changed.")
    audit_git(repo_root, reporter)
    audit_stow(repo_root, reporter, profile)
    audit_shell_configuration(repo_root, reporter)
    audit_environment(repo_root, reporter, profile)
    audit_git_configuration(reporter)
    audit_editors_and_fonts(profile, reporter)
    audit_packages(packages, reporter, profile)
    audit_ide_update_channels(reporter)
    audit_extra_packages(repo_root, profile, packages, reporter)
    audit_self_installed_apps(repo_root, profile, reporter)
    audit_automations(profile, reporter)
    print()
    # Notices are not problems, so they only appear in the summary when there are some.
    notices = f", {reporter.notices} notice(s)" if reporter.notices else ""
    if reporter.issues:
        print(f"Installation needs attention: {reporter.issues} issue(s), {reporter.warnings} warning(s){notices}.")
        return 1
    print(f"Installation is aligned ({reporter.warnings} warning(s){notices}).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())