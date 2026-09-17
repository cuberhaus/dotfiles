#!/usr/bin/env python3
# Description: Report repositories with failing commit status checks on their default branch.
"""git-ci-status: Scan GitHub repositories for commit status check rollup states.

Queries GitHub's GraphQL API for all owned repositories and checks the commit
status check rollup (the exact icon shown in GitHub commit and repository views).
"""

from __future__ import annotations

import argparse
from dataclasses import asdict, dataclass
from datetime import datetime, timezone
import json
import os
import shutil
import subprocess
import sys
from typing import Any, Dict, List, Optional, Set

GRAPHQL_QUERY = """
query($endCursor: String) {
  viewer {
    repositories(first: 100, after: $endCursor, affiliations: [OWNER], orderBy: {field: NAME, direction: ASC}) {
      pageInfo {
        hasNextPage
        endCursor
      }
      totalCount
      nodes {
        name
        nameWithOwner
        isArchived
        defaultBranchRef {
          name
          target {
            ... on Commit {
              oid
              abbreviatedOid
              messageHeadline
              committedDate
              statusCheckRollup {
                state
                contexts(first: 100) {
                  nodes {
                    __typename
                    ... on CheckRun {
                      name
                      status
                      conclusion
                      detailsUrl
                      url
                    }
                    ... on StatusContext {
                      context
                      state
                      targetUrl
                      description
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
"""

FAIL_CONCLUSIONS: Set[str] = {
    "FAILURE",
    "TIMED_OUT",
    "STARTUP_FAILURE",
    "ACTION_REQUIRED",
    "ERROR",
}


@dataclass
class CheckInfo:
    name: str
    status: str
    conclusion: str
    url: str
    is_failure: bool
    is_pending: bool


@dataclass
class RepoStatus:
    name: str
    name_with_owner: str
    is_archived: bool
    branch: Optional[str]
    commit_sha: Optional[str]
    commit_headline: Optional[str]
    rollup_state: Optional[str]
    status_category: str  # "failing", "passing", "pending", "no_checks"
    failing_checks: List[CheckInfo]
    all_checks: List[CheckInfo]


class Colors:
    def __init__(self, enabled: bool) -> None:
        self.enabled = enabled
        self.red = "\033[31m" if enabled else ""
        self.green = "\033[32m" if enabled else ""
        self.yellow = "\033[33m" if enabled else ""
        self.blue = "\033[34m" if enabled else ""
        self.cyan = "\033[36m" if enabled else ""
        self.dim = "\033[2m" if enabled else ""
        self.bold = "\033[1m" if enabled else ""
        self.reset = "\033[0m" if enabled else ""


def parse_graphql_responses(raw: str) -> List[Dict[str, Any]]:
    decoder = json.JSONDecoder()
    pos = 0
    results: List[Dict[str, Any]] = []
    text = raw.strip()
    while pos < len(text):
        while pos < len(text) and text[pos].isspace():
            pos += 1
        if pos >= len(text):
            break
        obj, end = decoder.raw_decode(text, pos)
        results.append(obj)
        pos = end
    return results


def query_github_status() -> List[Dict[str, Any]]:
    if not shutil.which("gh"):
        sys.stderr.write(
            "Error: 'gh' CLI tool was not found on PATH.\n"
            "Please install the GitHub CLI: https://cli.github.com/\n"
        )
        sys.exit(2)

    command = [
        "gh",
        "api",
        "graphql",
        "--paginate",
        "-f",
        f"query={GRAPHQL_QUERY}",
    ]

    try:
        proc = subprocess.run(command, capture_output=True, text=True, check=False)
    except Exception as exc:
        sys.stderr.write(f"Failed to execute gh command: {exc}\n")
        sys.exit(2)

    if proc.returncode != 0:
        err = proc.stderr.strip() or proc.stdout.strip()
        sys.stderr.write(
            f"GitHub API query failed (exit code {proc.returncode}):\n{err}\n"
            "Ensure you are authenticated with 'gh auth login'.\n"
        )
        sys.exit(proc.returncode if proc.returncode != 0 else 1)

    raw_output = proc.stdout.strip()
    if not raw_output:
        return []

    try:
        pages = parse_graphql_responses(raw_output)
    except Exception as exc:
        sys.stderr.write(f"Failed to parse GitHub response: {exc}\n")
        sys.exit(1)

    repos: List[Dict[str, Any]] = []
    for page in pages:
        data = page.get("data", {}).get("viewer", {}).get("repositories", {})
        repos.extend(data.get("nodes", []))

    return repos


def parse_repository_nodes(nodes: List[Dict[str, Any]]) -> List[RepoStatus]:
    statuses: List[RepoStatus] = []

    for node in nodes:
        name = node.get("name", "")
        name_with_owner = node.get("nameWithOwner", name)
        is_archived = bool(node.get("isArchived", False))

        default_branch = node.get("defaultBranchRef")
        if not default_branch:
            statuses.append(
                RepoStatus(
                    name=name,
                    name_with_owner=name_with_owner,
                    is_archived=is_archived,
                    branch=None,
                    commit_sha=None,
                    commit_headline=None,
                    rollup_state=None,
                    status_category="no_checks",
                    failing_checks=[],
                    all_checks=[],
                )
            )
            continue

        branch_name = default_branch.get("name")
        target = default_branch.get("target") or {}
        commit_sha = target.get("abbreviatedOid") or (target.get("oid", "")[:7] if target.get("oid") else None)
        commit_headline = target.get("messageHeadline")

        rollup = target.get("statusCheckRollup")
        rollup_state = rollup.get("state") if rollup else None

        all_checks: List[CheckInfo] = []
        failing_checks: List[CheckInfo] = []

        if rollup:
            contexts = rollup.get("contexts", {}).get("nodes", [])
            for ctx in contexts:
                check_name = ctx.get("name") or ctx.get("context") or "unknown"
                raw_status = ctx.get("status")
                raw_conclusion = ctx.get("conclusion") or ctx.get("state")
                url = ctx.get("detailsUrl") or ctx.get("targetUrl") or ctx.get("url") or ""

                status_upper = (raw_status or "").upper()
                conclusion_upper = (raw_conclusion or "").upper()

                is_fail = conclusion_upper in FAIL_CONCLUSIONS
                is_pending = (
                    status_upper in ("QUEUED", "IN_PROGRESS", "WAITING", "PENDING", "REQUESTED")
                    or conclusion_upper in ("PENDING", "EXPECTED", "UNKNOWN", "")
                ) and not is_fail

                display_conclusion = conclusion_upper or status_upper or "UNKNOWN"

                item = CheckInfo(
                    name=check_name,
                    status=status_upper or "COMPLETED",
                    conclusion=display_conclusion,
                    url=url,
                    is_failure=is_fail,
                    is_pending=is_pending,
                )
                all_checks.append(item)
                if is_fail:
                    failing_checks.append(item)

        if rollup_state in ("FAILURE", "ERROR") or failing_checks:
            category = "failing"
        elif rollup_state == "SUCCESS":
            category = "passing"
        elif rollup_state in ("PENDING", "EXPECTED"):
            category = "pending"
        else:
            category = "no_checks"

        statuses.append(
            RepoStatus(
                name=name,
                name_with_owner=name_with_owner,
                is_archived=is_archived,
                branch=branch_name,
                commit_sha=commit_sha,
                commit_headline=commit_headline,
                rollup_state=rollup_state,
                status_category=category,
                failing_checks=failing_checks,
                all_checks=all_checks,
            )
        )

    return statuses


def format_category_icon(category: str, colors: Colors) -> str:
    if category == "failing":
        return f"{colors.red}✕{colors.reset}"
    if category == "passing":
        return f"{colors.green}✔{colors.reset}"
    if category == "pending":
        return f"{colors.yellow}●{colors.reset}"
    return f"{colors.dim}○{colors.reset}"


def print_text_report(
    statuses: List[RepoStatus],
    filter_mode: str,
    verbose: bool,
    colors: Colors,
) -> None:
    failing = [s for s in statuses if s.status_category == "failing"]
    passing = [s for s in statuses if s.status_category == "passing"]
    pending = [s for s in statuses if s.status_category == "pending"]
    no_checks = [s for s in statuses if s.status_category == "no_checks"]

    display_list: List[RepoStatus] = []
    if filter_mode == "all":
        display_list = statuses
    elif filter_mode == "passing":
        display_list = passing
    elif filter_mode == "pending":
        display_list = pending
    else:  # "failing" (default)
        display_list = failing

    if not display_list:
        if filter_mode == "failing":
            print(
                f"{colors.green}✔ All {len(statuses)} repositories have passing or clean commit status checks.{colors.reset}"
            )
        else:
            print(f"No repositories matched the filter '{filter_mode}'.")
    else:
        title = {
            "failing": f"Failing repositories ({len(display_list)})",
            "passing": f"Passing repositories ({len(display_list)})",
            "pending": f"Pending repositories ({len(display_list)})",
            "all": f"All repositories ({len(display_list)})",
        }.get(filter_mode, "Repository status")

        print(f"\n{colors.bold}{title}{colors.reset}\n")

        for repo in display_list:
            icon = format_category_icon(repo.status_category, colors)
            sha_part = f" @ {repo.commit_sha}" if repo.commit_sha else ""
            branch_part = f" ({repo.branch}{sha_part})" if repo.branch else ""
            archived_tag = f" {colors.dim}[archived]{colors.reset}" if repo.is_archived else ""

            print(f"{icon} {colors.bold}{repo.name}{colors.reset}{branch_part}{archived_tag}")

            if repo.commit_headline:
                print(f"  {colors.dim}{repo.commit_headline}{colors.reset}")

            checks_to_show = repo.all_checks if verbose else repo.failing_checks
            for check in checks_to_show:
                if check.is_failure:
                    c_icon = f"{colors.red}✕{colors.reset}"
                elif check.is_pending:
                    c_icon = f"{colors.yellow}●{colors.reset}"
                else:
                    c_icon = f"{colors.green}✔{colors.reset}"
                url_str = f" -> {check.url}" if check.url else ""
                print(f"    {c_icon} {check.name} ({check.conclusion}){url_str}")

            print()

    summary_parts = [
        f"{colors.red}{len(failing)} failing{colors.reset}",
        f"{colors.green}{len(passing)} passing{colors.reset}",
        f"{colors.yellow}{len(pending)} pending{colors.reset}",
        f"{colors.dim}{len(no_checks)} no checks{colors.reset}",
        f"{colors.bold}{len(statuses)} total{colors.reset}",
    ]
    print(f"Summary: {' | '.join(summary_parts)}")


def build_json_payload(statuses: List[RepoStatus]) -> Dict[str, Any]:
    failing = [s for s in statuses if s.status_category == "failing"]
    passing = [s for s in statuses if s.status_category == "passing"]
    pending = [s for s in statuses if s.status_category == "pending"]
    no_checks = [s for s in statuses if s.status_category == "no_checks"]

    return {
        "scanned_at": datetime.now(timezone.utc).isoformat(),
        "total_count": len(statuses),
        "failing_count": len(failing),
        "passing_count": len(passing),
        "pending_count": len(pending),
        "no_checks_count": len(no_checks),
        "failing": [asdict(s) for s in failing],
        "passing": [asdict(s) for s in passing],
        "pending": [asdict(s) for s in pending],
        "no_checks": [asdict(s) for s in no_checks],
    }


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Scan GitHub repositories for commit status check rollups on default branches.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  git-ci-status                   # List all repositories with failing checks
  git-ci-status -a                # List all repositories with their CI status
  git-ci-status -r WinDotfiles    # Check a specific repository
  git-ci-status -j                # Output machine-readable JSON
  git-ci-status -v                # Verbose mode (show all checks)
  git-ci-status --strict          # Exit with code 1 if any repository is failing
""",
    )
    parser.add_argument(
        "-a",
        "--all",
        action="store_true",
        help="Show status for all repositories (failing, passing, pending, no checks)",
    )
    parser.add_argument(
        "--passing",
        action="store_true",
        help="Show only passing repositories",
    )
    parser.add_argument(
        "--pending",
        action="store_true",
        help="Show only repositories with pending/in-progress checks",
    )
    parser.add_argument(
        "-r",
        "--repo",
        type=str,
        default=None,
        help="Filter by repository name (supports substring or comma-separated names)",
    )
    parser.add_argument(
        "-j",
        "--json",
        action="store_true",
        help="Output results as JSON",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Show full check details, including passing steps",
    )
    parser.add_argument(
        "--no-color",
        action="store_true",
        help="Disable ANSI colors in terminal output",
    )
    parser.add_argument(
        "--strict",
        action="store_true",
        help="Exit with return code 1 if any repository has failing checks",
    )

    args = parser.parse_args()

    use_color = (
        not args.no_color
        and "NO_COLOR" not in os.environ
        and sys.stdout.isatty()
    )
    colors = Colors(use_color)

    nodes = query_github_status()
    statuses = parse_repository_nodes(nodes)

    if args.repo:
        patterns = [p.strip().lower() for p in args.repo.split(",") if p.strip()]
        statuses = [
            s
            for s in statuses
            if any(p in s.name.lower() or p in s.name_with_owner.lower() for p in patterns)
        ]

    if args.json:
        payload = build_json_payload(statuses)
        print(json.dumps(payload, indent=2))
    else:
        filter_mode = "failing"
        if args.all:
            filter_mode = "all"
        elif args.passing:
            filter_mode = "passing"
        elif args.pending:
            filter_mode = "pending"

        print_text_report(statuses, filter_mode, args.verbose, colors)

    if args.strict and any(s.status_category == "failing" for s in statuses):
        sys.exit(1)


if __name__ == "__main__":
    main()
