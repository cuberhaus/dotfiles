#!/usr/bin/env python3
"""Build cuberhaus workspace repos.json from GitHub metadata + local scans.

Replaces the previous one-liner that wrote only GitHub API fields. This script
adds two extra layers of context so agents can reason about the workspace
without parsing every AGENTS.md:

1. Local filesystem scan (per repo clone in the workspace root):
   - cloned, has_agents_md, has_makefile, has_pr_template
   - has_skills: "root" | "web" | false

2. Curated, in-script mappings (edit the STATE / CONSUMED_BY dicts below when
   the workspace evolves):
   - state: active | active-research | mixed | frozen | frozen-hackathon |
            sandbox | unknown
   - consumed_by: cross-repo wiring (e.g. subgrup-prop7.1 -> PersonalPortfolio)

The output is pretty-printed, sorted by name, UTF-8 without BOM. Run via
`make update-repos` from dotfiles/ or WinDotfiles/; needs `gh` authenticated.

Usage:
    python3 build-repos.py                                # writes $HOME/cuberhaus/repos.json
    python3 build-repos.py /tmp/foo.json                  # custom output path
    python3 build-repos.py /tmp/foo.json --preserve-from existing.json
                                                          # keep entries from existing.json
                                                          # for repos the API didn't return (e.g.
                                                          # private repos unavailable in CI)
"""

from __future__ import annotations

import json
import os
import subprocess
import sys
from pathlib import Path

# --- Curated mappings ---------------------------------------------------------
# Source of truth for the "state" field. Edit when a repo changes purpose.
STATE: dict[str, str] = {
    # active personal/work
    "AgentesProactivos": "active",
    "AgenticEraHackathon": "active",
    "AgenticPoC": "active",
    "PersonalFinance": "active",
    "PersonalPortfolio": "active",
    "WinDotfiles": "active",
    "WorkoutApp": "active",
    "collaborative-central-server": "active",
    "cuberhaus": "active",
    "cuberhaus.github.io": "active",
    "cv": "active",
    "dotfiles": "active",
    "informatica-PoC": "active",
    "obsidian_vault": "active",
    "openclaw-ai": "active",
    "tenda_online": "active",
    # research / active (thesis with active web wrapper)
    "TFG": "active-research",
    # mixed (frozen core + active web wrapper)
    "bitsXlaMarato": "mixed",
    "desastresIA": "mixed",
    "joc_eda": "mixed",
    "pracpro2": "mixed",
    "subgrup-prop7.1": "mixed",
    # frozen coursework
    "ANTLR4_JSBach": "frozen",
    "APA": "frozen",
    "APA_Practica": "frozen",
    "AS": "frozen",
    "CAIM": "frozen",
    "MD": "frozen",
    "MD2": "frozen",
    "PAR": "frozen",
    "Practica_de_Planificacion": "frozen",
    "ROB": "frozen",
    "SBC_IA": "frozen",
    "SpringUnitTesting": "frozen",
    "TFG_DOC": "frozen",
    "VC": "frozen",
    "fib": "frozen",
    "projectA": "frozen",
    "projectA2": "frozen",
    "waslab01": "frozen",
    "waslab02": "frozen",
    "waslab03": "frozen",
    "waslab04": "frozen",
    # frozen hackathon
    "Draculin-Backend": "frozen-hackathon",
    "Draculin-Front": "frozen-hackathon",
    "datathon2023": "frozen-hackathon",
    "hackupc2023": "frozen-hackathon",
    # sandbox / personal notes
    "dev": "sandbox",
    "docs": "sandbox",
}

# Cross-repo wiring. {producer: [consumers]}. Edit when wiring changes.
CONSUMED_BY: dict[str, list[str]] = {
    "subgrup-prop7.1": ["PersonalPortfolio"],
}

# --- Enrichment ---------------------------------------------------------------


def enrich(record: dict, workspace_root: Path) -> dict:
    name = record["name"]
    record["state"] = STATE.get(name, "unknown")
    if name in CONSUMED_BY:
        record["consumed_by"] = CONSUMED_BY[name]
    repo_path = workspace_root / name
    if not repo_path.is_dir():
        record["cloned"] = False
        return record
    record["cloned"] = True
    record["has_agents_md"] = (repo_path / "AGENTS.md").is_file()
    record["has_makefile"] = (repo_path / "Makefile").is_file()
    record["has_pr_template"] = (
        repo_path / ".github" / "pull_request_template.md"
    ).is_file()
    if (repo_path / "skills-lock.json").is_file():
        record["has_skills"] = "root"
    elif (repo_path / "web" / "skills-lock.json").is_file():
        record["has_skills"] = "web"
    else:
        record["has_skills"] = False
    return record


def default_workspace_root() -> Path:
    home = Path(os.environ.get("USERPROFILE") or os.environ.get("HOME") or ".")
    return home / "cuberhaus"


def main() -> int:
    args = [a for a in sys.argv[1:] if a != '--preserve-from' and not a.startswith('--preserve-from=')]
    preserve_from: Path | None = None
    for i, a in enumerate(sys.argv[1:], start=1):
        if a == '--preserve-from' and i + 1 < len(sys.argv):
            preserve_from = Path(sys.argv[i + 1])
            args = [x for x in args if x != sys.argv[i + 1]]
        elif a.startswith('--preserve-from='):
            preserve_from = Path(a.split('=', 1)[1])

    out_path = (
        Path(args[0]) if args else default_workspace_root() / "repos.json"
    )
    workspace_root = out_path.parent

    print(f"[build-repos] Fetching repo list from GitHub via gh CLI...", file=sys.stderr)
    result = subprocess.run(
        [
            "gh", "repo", "list", "cuberhaus",
            "--json", "name,description,isArchived,isFork,visibility,repositoryTopics",
            "--limit", "100",
        ],
        check=True, capture_output=True, text=True,
    )
    records = json.loads(result.stdout)
    print(
        f"[build-repos] Got {len(records)} repos. Enriching from {workspace_root}...",
        file=sys.stderr,
    )

    enriched = [enrich(r, workspace_root) for r in records]

    # Preserve entries from an existing file for repos the API didn't return
    # (e.g. private repos unavailable to CI's default GITHUB_TOKEN).
    if preserve_from and preserve_from.is_file():
        api_names = {r["name"] for r in records}
        try:
            existing = json.loads(preserve_from.read_text(encoding="utf-8"))
            preserved = [r for r in existing if r.get("name") not in api_names]
            if preserved:
                print(
                    f"[build-repos] Preserving {len(preserved)} entries from {preserve_from} "
                    f"not returned by API: {', '.join(r['name'] for r in preserved)}",
                    file=sys.stderr,
                )
                enriched.extend(preserved)
        except (OSError, json.JSONDecodeError) as e:
            print(f"[build-repos] WARN: could not read --preserve-from {preserve_from}: {e}", file=sys.stderr)

    enriched.sort(key=lambda r: r["name"].lower())

    # Warn about curated entries that no longer match a real repo.
    api_names = {r["name"] for r in records}
    stale_state = sorted(set(STATE) - api_names)
    stale_consumed = sorted(set(CONSUMED_BY) - api_names)
    if stale_state:
        print(
            f"[build-repos] WARN: STATE entries with no matching repo: {', '.join(stale_state)}",
            file=sys.stderr,
        )
    if stale_consumed:
        print(
            f"[build-repos] WARN: CONSUMED_BY entries with no matching repo: {', '.join(stale_consumed)}",
            file=sys.stderr,
        )
    missing_state = sorted(api_names - set(STATE))
    if missing_state:
        print(
            f"[build-repos] WARN: repos missing from STATE map: {', '.join(missing_state)}",
            file=sys.stderr,
        )

    out_path.write_text(
        json.dumps(enriched, indent=2, ensure_ascii=False) + "\n",
        encoding="utf-8",
    )
    print(f"[build-repos] Wrote {out_path} ({len(enriched)} repos)", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
