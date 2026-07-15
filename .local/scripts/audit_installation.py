#!/usr/bin/env python3
"""Read-only audit of a machine against this dotfiles repository."""

from __future__ import annotations

import argparse
import dataclasses
import pathlib
import re
import shlex
import shutil
import subprocess
import sys
from collections.abc import Iterable


@dataclasses.dataclass(frozen=True, order=True)
class Package:
    manager: str
    name: str


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
                packages.add(Package("snap", names[0]))

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


def detect_profile() -> str:
    if sys.platform == "darwin":
        return "mac"
    if not sys.platform.startswith("linux"):
        raise RuntimeError("automatic profile detection is supported on Linux and macOS only")
    proc_version = pathlib.Path("/proc/version")
    if proc_version.exists() and "microsoft" in proc_version.read_text(
        encoding="utf-8", errors="ignore"
    ).lower():
        return "ubuntu-windows"
    distro_file = pathlib.Path.home() / ".config" / "distro"
    if distro_file.exists():
        match = re.search(r"DISTRO=([A-Za-z0-9_-]+)", distro_file.read_text(errors="ignore"))
        if match and match.group(1) in PROFILE_SOURCES:
            return match.group(1)
    os_release = pathlib.Path("/etc/os-release")
    if os_release.exists():
        match = re.search(
            r"^ID=\"?([^\"\n]+)",
            os_release.read_text(encoding="utf-8", errors="ignore"),
            re.MULTILINE,
        )
        if match and match.group(1) in {"arch", "manjaro", "ubuntu"}:
            return match.group(1)
    raise RuntimeError("could not detect a supported profile; pass --profile explicitly")


class Reporter:
    def __init__(self) -> None:
        self.issues = 0
        self.warnings = 0

    def result(self, status: str, message: str, remedy: str = "") -> None:
        print(f"  [{status}] {message}")
        if remedy:
            print(f"         Fix: {remedy}")
        if status in {"DRIFT", "MISSING"}:
            self.issues += 1
        elif status == "WARN":
            self.warnings += 1


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


def audit_stow(repo_root: pathlib.Path, reporter: Reporter) -> None:
    print("\nStow-managed configs, aliases, functions, and scripts")
    if not shutil.which("stow"):
        reporter.result("MISSING", "GNU Stow is not installed.", "Run the matching bootstrap target.")
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


def audit_packages(packages: set[Package], reporter: Reporter) -> None:
    print("\nActive bootstrap package declarations")
    for manager in sorted({package.manager for package in packages}):
        manager_packages = sorted(package.name for package in packages if package.manager == manager)
        command = {"apt": "dpkg-query", "pacman": "pacman", "yay": "pacman", "snap": "snap", "brew": "brew", "brew-cask": "brew"}[manager]
        if not shutil.which(command):
            reporter.result("MISSING", f"{command} is unavailable; {len(manager_packages)} {manager} package(s) cannot be verified.", "Run the matching bootstrap target.")
            continue
        installed = installed_package_names(manager)
        missing = [name for name in manager_packages if name not in installed and name.split("/", 1)[-1] not in installed]
        if missing:
            reporter.result("MISSING", f"{len(missing)} expected {manager} package(s): {', '.join(missing)}", "Run the matching bootstrap target.")
        else:
            reporter.result("OK", f"All {len(manager_packages)} expected {manager} package(s) are installed.")


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

    reporter = Reporter()
    print(f"dotfiles installation audit ({profile})")
    print("Read-only: no files, packages, services, or timers will be changed.")
    audit_git(repo_root, reporter)
    audit_stow(repo_root, reporter)
    audit_packages(packages, reporter)
    audit_automations(profile, reporter)
    print()
    if reporter.issues:
        print(f"Installation needs attention: {reporter.issues} issue(s), {reporter.warnings} warning(s).")
        return 1
    print(f"Installation is aligned ({reporter.warnings} warning(s)).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())