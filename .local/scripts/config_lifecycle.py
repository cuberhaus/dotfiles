#!/usr/bin/env python3
"""Inspect and preview synchronization of Stow-managed configuration."""

from __future__ import annotations

import argparse
import difflib
import os
import pathlib
import re
import shutil
import subprocess
import sys
from collections.abc import Iterable


def run(command: list[str], cwd: pathlib.Path) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
        command,
        cwd=cwd,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        check=False,
    )


def stow_command(repo_root: pathlib.Path, home: pathlib.Path) -> list[str]:
    return [
        "stow",
        "-v",
        "-t",
        str(home),
        "-d",
        str(repo_root.parent),
        repo_root.name,
    ]


def ignore_patterns(repo_root: pathlib.Path) -> list[re.Pattern[str]]:
    ignore_file = repo_root / ".stow-local-ignore"
    patterns: list[re.Pattern[str]] = []
    if not ignore_file.is_file():
        return patterns
    for raw_line in ignore_file.read_text(encoding="utf-8").splitlines():
        line = re.split(r"(?<!\\)#", raw_line, maxsplit=1)[0].strip()
        if line:
            patterns.append(re.compile(line))
    return patterns


def is_managed_path(relative_path: pathlib.Path, patterns: Iterable[re.Pattern[str]]) -> bool:
    value = "/" + relative_path.as_posix()
    return relative_path.as_posix() != ".stow-local-ignore" and not any(
        pattern.search(value) for pattern in patterns
    )


def tracked_paths(repo_root: pathlib.Path) -> list[pathlib.Path]:
    result = run(["git", "ls-files", "-z"], repo_root)
    if result.returncode:
        raise RuntimeError(result.stdout.strip() or "git ls-files failed")
    return [pathlib.Path(value) for value in result.stdout.split("\0") if value]


def file_difference(source: pathlib.Path, target: pathlib.Path, label: str) -> str:
    source_bytes = source.read_bytes()
    target_bytes = target.read_bytes()
    if source_bytes == target_bytes:
        return ""
    try:
        source_lines = source_bytes.decode("utf-8").splitlines(keepends=True)
        target_lines = target_bytes.decode("utf-8").splitlines(keepends=True)
    except UnicodeDecodeError:
        return f"Binary files differ: repo/{label} and home/{label}\n"
    return "".join(
        difflib.unified_diff(
            source_lines,
            target_lines,
            fromfile=f"repo/{label}",
            tofile=f"home/{label}",
        )
    )


def managed_differences(
    repo_root: pathlib.Path,
    home: pathlib.Path,
    paths: Iterable[pathlib.Path],
) -> list[str]:
    patterns = ignore_patterns(repo_root)
    differences: list[str] = []
    for relative_path in paths:
        if not is_managed_path(relative_path, patterns):
            continue
        source = repo_root / relative_path
        target = home / relative_path
        if not source.is_file() or not (target.exists() or target.is_symlink()):
            continue
        if target.is_symlink() and target.resolve(strict=False) == source.resolve():
            continue
        if target.is_file():
            difference = file_difference(source, target, relative_path.as_posix())
            if difference:
                differences.append(difference)
    return differences


def status(repo_root: pathlib.Path, home: pathlib.Path) -> int:
    stow = run([*stow_command(repo_root, home)[:1], "-n", *stow_command(repo_root, home)[1:]], repo_root)
    print("Stow deployment")
    print(stow.stdout.strip() or "  aligned")
    git = run(["git", "status", "--short"], repo_root)
    print("\nSource checkout")
    print(git.stdout.strip() or "  clean")
    return stow.returncode or git.returncode


def diff(repo_root: pathlib.Path, home: pathlib.Path) -> int:
    differences = managed_differences(repo_root, home, tracked_paths(repo_root))
    if not differences:
        print("Tracked managed files match their existing home targets.")
        return 0
    print("".join(differences), end="")
    return 1


def import_config(repo_root: pathlib.Path, home: pathlib.Path, apply: bool) -> int:
    command = [*stow_command(repo_root, home), "--adopt"]
    if not apply:
        command.insert(1, "-n")
        print("Preview only. Re-run with `make config-import APPLY=1` to adopt these files.\n")
    else:
        print("Adopting home files into the repository. Review `git diff` before committing.\n")
    result = run(command, repo_root)
    print(result.stdout, end="")
    if apply and result.returncode == 0:
        changed = run(["git", "status", "--short"], repo_root)
        print("\nSource checkout changes")
        print(changed.stdout.strip() or "  none")
    return result.returncode


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=("status", "diff", "import"))
    parser.add_argument("--apply", action="store_true")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    repo_root = pathlib.Path(__file__).resolve().parents[2]
    home = pathlib.Path.home()
    if args.command in {"status", "import"} and not shutil.which("stow"):
        print("GNU Stow is required.", file=sys.stderr)
        return 127
    if args.command == "status":
        return status(repo_root, home)
    if args.command == "diff":
        return diff(repo_root, home)
    return import_config(repo_root, home, args.apply)


if __name__ == "__main__":
    raise SystemExit(main())