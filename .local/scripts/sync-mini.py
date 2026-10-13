#!/usr/bin/env python3
"""Synchronize .local/Mini/.bashrc generated sections from canonical dotfiles scripts.

Usage:
  sync-mini.py           # regenerate .local/Mini/.bashrc in place
  sync-mini.py --check   # verify .local/Mini/.bashrc is up to date (exit 1 on drift)
"""

from __future__ import annotations

import argparse
import difflib
import re
import sys
from pathlib import Path

BEGIN_MARKER = "# BEGIN GENERATED MINI FUNCTIONS (do not edit by hand; run: make sync-mini)"
END_MARKER = "# END GENERATED MINI FUNCTIONS"

BASH4_PATTERNS: list[tuple[re.Pattern[str], str]] = [
    (re.compile(r"\b(mapfile|readarray)\b"), "mapfile/readarray (Bash 4+)"),
    (re.compile(r"\bwait\s+-n\b"), "wait -n (Bash 4.3+)"),
    (re.compile(r"\bdeclare\s+-A\b"), "declare -A associative arrays (Bash 4+)"),
    (re.compile(r"\$\{[A-Za-z_][A-Za-z0-9_]*(\^\^|,,)"), "case-modification expansion (Bash 4+)"),
]


def extract_function(text: str, name: str) -> str:
    """Extract a top-level shell function definition by name."""
    pattern = re.compile(
        rf"^(?:function\s+)?{re.escape(name)}(?:\(\))?\s*\{{\n.*?\n\}}",
        re.MULTILINE | re.DOTALL,
    )
    match = pattern.search(text)
    if not match:
        raise ValueError(f"Could not find function '{name}'")
    return match.group(0)


def strip_script_header(lines: list[str]) -> list[str]:
    """Strip leading shebang, header comments, and blank lines."""
    idx = 0
    while idx < len(lines) and (lines[idx].startswith("#") or not lines[idx].strip()):
        idx += 1
    return lines[idx:]


def expand_sed_self_help(raw_text: str, body: str) -> str:
    """Replace `sed -n 'A,Bp' "$0" | sed ...` with an inline `cat <<'EOF'` block."""
    raw_lines = raw_text.splitlines()

    def repl(match: re.Match[str]) -> str:
        indent = match.group(1)
        start = int(match.group(2))
        end = int(match.group(3))
        help_lines = []
        for line in raw_lines[start - 1 : end]:
            help_lines.append(re.sub(r"^# ?", "", line))
        joined = "\n".join(help_lines)
        return f"{indent}cat <<'EOF'\n{joined}\nEOF"

    return re.sub(
        r"^([ \t]*)sed -n '(\d+),(\d+)p' \"\$0\" \| sed '[^']*'$",
        repl,
        body,
        flags=re.MULTILINE,
    )


def strip_repo_defaults_source(body: str) -> str:
    """Remove the `_clone_script_dir` + `. .../git-repo-defaults.sh` sourcing block."""
    return re.sub(
        r"\n?_clone_script_dir=.*\n# shellcheck source=.*\n\. \"\$_clone_script_dir/\.\./lib/git-repo-defaults\.sh\"\n",
        "\n",
        body,
    )


def rename_repo_default_helpers(body: str) -> str:
    """Rename library helpers to Mini's underscore-prefixed internal names."""
    body = re.sub(r"\bresolve_repo_dir\b", "_clone_repo_dir", body)
    body = re.sub(r"\bconfigure_tracked_repo_git\b", "_clone_configure_repo", body)
    return body


def indent_block(text: str, spaces: int = 4) -> str:
    prefix = " " * spaces
    out: list[str] = []
    heredoc_delim: str | None = None
    heredoc_re = re.compile(r"<<-?\s*['\"]?([A-Za-z0-9_]+)['\"]?")
    for line in text.strip().splitlines():
        if heredoc_delim is not None:
            out.append(line)
            if line.strip() == heredoc_delim:
                heredoc_delim = None
            continue
        m = heredoc_re.search(line)
        if m and not line.lstrip().startswith("#"):
            heredoc_delim = m.group(1)
        out.append(f"{prefix}{line}" if line.strip() else "")
    return "\n".join(out)


def wrap_subshell_function(name: str, body: str) -> str:
    return f"{name}() (\n{indent_block(body, 4)}\n)"


def transform_git_recurse(raw_text: str) -> str:
    """Transform .local/scripts/bin/git-recurse into Mini's self-contained git-recurse()."""
    body = "\n".join(strip_script_header(raw_text.splitlines()))
    # Strip macOS-only `mac_key` helper and `-k` flag handling.
    body = re.sub(r"\nfunction mac_key\(\) \{\n.*?\n\}\n", "\n", body, flags=re.DOTALL)
    body = re.sub(r'^[ \t]*echo "  -k[ \t]+Add ssh key \(macOS\)"\n', "", body, flags=re.MULTILINE)
    body = re.sub(r"^flag_key=false\n", "", body, flags=re.MULTILINE)
    body = body.replace('getopts ":hpksj:r:d:t:S"', 'getopts ":hpsj:r:d:t:S"')
    body = re.sub(r"^[ \t]*k\)\n[ \t]*flag_key=true\n[ \t]*;;\n", "", body, flags=re.MULTILINE)
    body = re.sub(r"\nif \$flag_key; then\n[ \t]*mac_key\nfi\n", "\n", body)
    return wrap_subshell_function("git-recurse", body)


def build_generated_section(repo_root: Path) -> str:
    aliases_text = (repo_root / ".config/zsh/aliases").read_text(encoding="utf-8")
    functions_text = (repo_root / ".config/zsh/functions").read_text(encoding="utf-8")
    defaults_text = (repo_root / ".local/scripts/lib/git-repo-defaults.sh").read_text(encoding="utf-8")
    clone_all_raw = (repo_root / ".local/scripts/bin/clone-all").read_text(encoding="utf-8")
    clone_team_raw = (repo_root / ".local/scripts/bin/clone-team").read_text(encoding="utf-8")
    git_ahead_raw = (repo_root / ".local/scripts/bin/git-ahead").read_text(encoding="utf-8")
    git_clean_branches_raw = (repo_root / ".local/scripts/bin/git-clean-branches").read_text(encoding="utf-8")
    git_recurse_raw = (repo_root / ".local/scripts/bin/git-recurse").read_text(encoding="utf-8")

    pull_fn = extract_function(aliases_text, "pull")
    add_pat_fn = extract_function(functions_text, "add_pat")
    # Normalize `function add_pat()` header to `add_pat()` for consistency.
    add_pat_fn = re.sub(r"^function\s+add_pat\(\)", "add_pat()", add_pat_fn)

    defaults_body = "\n".join(strip_script_header(defaults_text.splitlines()))
    defaults_body = rename_repo_default_helpers(defaults_body).strip()

    clone_all_body = "\n".join(strip_script_header(clone_all_raw.splitlines()))
    clone_all_body = strip_repo_defaults_source(clone_all_body)
    clone_all_body = rename_repo_default_helpers(clone_all_body)
    clone_all_fn = wrap_subshell_function("clone-all", clone_all_body)

    clone_team_body = "\n".join(strip_script_header(clone_team_raw.splitlines()))
    clone_team_body = strip_repo_defaults_source(clone_team_body)
    clone_team_body = expand_sed_self_help(clone_team_raw, clone_team_body)
    clone_team_body = rename_repo_default_helpers(clone_team_body)
    clone_team_fn = wrap_subshell_function("clone-team", clone_team_body)

    git_ahead_body = "\n".join(strip_script_header(git_ahead_raw.splitlines()))
    git_ahead_body = expand_sed_self_help(git_ahead_raw, git_ahead_body)
    git_ahead_fn = wrap_subshell_function("git-ahead", git_ahead_body)

    git_clean_branches_body = "\n".join(strip_script_header(git_clean_branches_raw.splitlines()))
    git_clean_branches_body = expand_sed_self_help(git_clean_branches_raw, git_clean_branches_body)
    git_clean_branches_fn = wrap_subshell_function("git-clean-branches", git_clean_branches_body)

    git_recurse_fn = transform_git_recurse(git_recurse_raw)

    parts = [
        BEGIN_MARKER,
        "# Pull current repo or recursively pull child repos (parallel)",
        pull_fn,
        "",
        "# Recursively add a GitHub PAT token to all repositories",
        add_pat_fn,
        "",
        "###############################################################",
        "# => Cloning GitHub repositories (clone-all, clone-team)",
        "###############################################################",
        "",
        "# Self-contained versions of the clone-all and clone-team scripts in .local/scripts/bin, and of",
        "# the helpers they share (.local/scripts/lib/git-repo-defaults.sh), for a machine that has only",
        "# this file. Both need the GitHub CLI (gh) signed in; clone-team also needs the read:org scope.",
        "# They stay above the aliases on purpose: Bash builds the aliases that exist when a function is",
        "# defined into its body, so a later alias for grep, rm, ... cannot change what runs here.",
        "",
        defaults_body,
        "",
        "# Clone every repository of a GitHub owner, or fast-forward the ones already present, with one",
        "# status line per repository (in parallel). Run it from the directory that holds the checkouts.",
        "# The body is a subshell, so its variables, helper functions and options never reach the shell.",
        clone_all_fn,
        "",
        "# Clone every repository that a GitHub team you belong to can access, or fast-forward the ones",
        "# already present. With no team it lists yours and asks for a number.",
        clone_team_fn,
        "",
        "###############################################################",
        "# => Git across repositories (git-recurse, git-ahead, git-clean-branches)",
        "###############################################################",
        "",
        "# Self-contained versions of the git-recurse, git-ahead and git-clean-branches scripts in",
        "# .local/scripts/bin, for a machine that has only this file: gr, gah, gclean and status below call",
        "# them. Same options, environment variables, output and exit codes as the scripts, except that",
        "# git-recurse has no -k (it adds the ssh key of one Mac). They stay above the aliases like the",
        "# clone commands do, and put `command` in front of grep, mv and rm, which the aliases below change",
        "# (-i, -v, -I) and which Bash would otherwise build into a function that is defined again when this",
        "# file is sourced a second time.",
        "# Bash 3.2 (macOS) is enough. tests/test_mini_bashrc.sh runs tests/test_git_recurse.sh against",
        "# git-recurse here and compares git-ahead with its script, so the copies cannot drift unnoticed.",
        "",
        git_ahead_fn,
        "",
        git_clean_branches_fn,
        "",
        git_recurse_fn,
        END_MARKER,
    ]
    generated = "\n".join(parts)
    validate_generated(generated)
    return generated


def validate_generated(generated: str) -> None:
    for line_no, line in enumerate(generated.splitlines(), start=1):
        stripped = line.lstrip()
        if stripped.startswith("#"):
            continue
        for regex, description in BASH4_PATTERNS:
            if regex.search(line):
                raise ValueError(
                    f"Generated Mini section contains {description} on line {line_no}: {line.strip()}"
                )


def sync_mini_bashrc(repo_root: Path, check_only: bool = False) -> int:
    mini_path = repo_root / ".local/Mini/.bashrc"
    current = mini_path.read_text(encoding="utf-8")
    if BEGIN_MARKER not in current or END_MARKER not in current:
        print(
            f"error: {mini_path} is missing '{BEGIN_MARKER}' or '{END_MARKER}' markers",
            file=sys.stderr,
        )
        return 2

    generated = build_generated_section(repo_root)
    pattern = re.compile(
        rf"^{re.escape(BEGIN_MARKER)}$.*?^{re.escape(END_MARKER)}$",
        re.MULTILINE | re.DOTALL,
    )
    expected = pattern.sub(lambda _: generated, current, count=1)

    if current == expected:
        if not check_only:
            print("\033[32m✓ .local/Mini/.bashrc is already up to date\033[0m")
        return 0

    if check_only:
        diff = difflib.unified_diff(
            current.splitlines(keepends=True),
            expected.splitlines(keepends=True),
            fromfile=".local/Mini/.bashrc (current)",
            tofile=".local/Mini/.bashrc (expected from canonical scripts)",
        )
        sys.stderr.writelines(diff)
        print(
            "\n\033[33m[DRIFT] .local/Mini/.bashrc is out of sync with canonical scripts.\033[0m\n"
            "  Fix: make sync-mini",
            file=sys.stderr,
        )
        return 1

    mini_path.write_text(expected, encoding="utf-8")
    print("\033[32m✓ Updated .local/Mini/.bashrc from canonical scripts\033[0m")
    return 0


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(
        prog="sync-mini",
        description="Synchronize .local/Mini/.bashrc generated sections from canonical dotfiles scripts.",
    )
    parser.add_argument(
        "--check",
        action="store_true",
        help="Verify .local/Mini/.bashrc matches canonical scripts without modifying it.",
    )
    parser.add_argument(
        "--repo-root",
        type=Path,
        default=Path(__file__).resolve().parents[2],
        help="Path to the dotfiles repository root.",
    )
    args = parser.parse_args(argv)
    return sync_mini_bashrc(args.repo_root.resolve(), check_only=args.check)


if __name__ == "__main__":
    raise SystemExit(main())
