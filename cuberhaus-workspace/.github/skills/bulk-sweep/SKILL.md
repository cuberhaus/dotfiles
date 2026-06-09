---
name: bulk-sweep
description: 'Apply the same small change across many cuberhaus workspace repos at once, scoped by `repos.json` state. Use when: fixing policy drift surfaced by `make audit-policies`; rolling out a config file (e.g. `.github/pull_request_template.md`, `dependabot.yml`, `skills-update.yml`) to every active repo; running a one-shot fix (`npm audit fix`) across every repo with a given stack; bulk-triaging Dependabot PRs. Encodes the workspace-specific safety rules: state filtering, FROZEN coursework protection, push-policy fallback (direct push to main, then branch+PR if lefthook blocks). Do NOT use for cross-repo refactors, for changes that need per-repo review, or for anything touching FROZEN coursework without explicit consent.'
argument-hint: 'Describe the change to apply (e.g. "add .github/pull_request_template.md", "run npm audit fix", "enable allow_auto_merge"). Optionally filter by state (default: active, active-research, mixed) or stack (npm, python, etc).'
---

# /bulk-sweep

Apply the same small change across many cuberhaus workspace repos at once, with the workspace's safety rules built in.

## When to use

- Closing policy drift listed by `make audit-policies` (the recurring case).
- Rolling out a workspace-wide file: `.github/pull_request_template.md`, `.github/dependabot.yml`, `.github/workflows/skills-update.yml`, `AGENTS.md` updates, lefthook config.
- Running a single shell-level fix across every repo with a stack (`npm audit fix`, `cargo update`, `pip-compile`, `pre-commit autoupdate`).
- Bulk-triaging open Dependabot PRs (close superseded, comment `@dependabot recreate`, merge after CI).
- Toggling GitHub repo settings via `gh api -X PATCH repos/cuberhaus/<r>` (e.g. `delete_branch_on_merge`, `has_wiki`, `has_projects`).

## When NOT to use

- **A single repo**. Just do it directly — no sweep needed.
- **Cross-repo refactors** where each repo needs its own diff. Use per-repo invocations instead.
- **Anything touching FROZEN coursework** (state ∉ {`active`, `active-research`, `mixed`}). The default filter protects these — never widen it without explicit user consent.
- **Force pushes, `--no-verify`, or rewriting published history**. Out of scope, period.
- **Bulk-merging PRs without checking CI**. Always confirm `mergeStateStatus = CLEAN` before merging.

## Procedure

### 1. Refresh the catalog

Run from `WinDotfiles/` or `dotfiles/`:

```
make update-repos
```

This rebuilds `~/cuberhaus/repos.json` from the GitHub API + local filesystem enrichment. Stale catalog → wrong sweep target. Always refresh first.

### 2. Compute the target set

Load `~/cuberhaus/repos.json` and filter by `state`. Default filter: `state in ['active', 'active-research', 'mixed']` (matches `policies.json`'s `audited_states`). Print the list of repo names and ask the user to confirm before any state-changing operation.

```powershell
$repos = (Get-Content "$env:USERPROFILE\cuberhaus\repos.json" -Raw | ConvertFrom-Json) |
    Where-Object { $_.state -in 'active','active-research','mixed' }
$repos | Select-Object name, state | Format-Table -AutoSize
```

```bash
jq -r '.[] | select(.state=="active" or .state=="active-research" or .state=="mixed") | .name' \
   "$HOME/cuberhaus/repos.json"
```

If the user adds a stack filter (e.g. "npm repos only"), narrow further by checking for `package.json` / `pyproject.toml` / `Cargo.toml` etc. on disk.

### 3. Dry-run

Before changing anything, print the planned action per repo (e.g. "would add `.github/pull_request_template.md`") and ask the user to confirm. Skip repos where the change is already a no-op (e.g. file already present with the same content).

### 4. Per-repo loop

For each target repo, in order:

1. **Clean tree check.** `git status --porcelain` → if non-empty, skip and report (never auto-stash).
2. **Sync main.** `git fetch origin && git checkout main && git pull --ff-only`. If the pull is not fast-forward, skip and report.
3. **Apply the change.** This is the per-sweep payload — could be a file copy, an `npm audit fix`, a `gh api` call, anything.
4. **Commit + push to main first.**
   ```
   git add -A
   git commit -m "<conventional commit>"
   git push origin main
   ```
5. **Lefthook fallback.** If the commit or push fails with the `no-commit-on-main` lefthook message ("Direct commits to 'main' are blocked"), fall back to branch + PR:
   ```
   git reset --soft HEAD~1   # only if a commit landed locally
   git checkout -b chore/<slug>
   git commit -m "<conventional commit>"
   git push -u origin chore/<slug>
   gh pr create --fill --base main
   ```
   If `allow_auto_merge` is enabled on the repo (per `audit-policies.py` output), follow with `gh pr merge <n> --auto --squash --delete-branch`. Otherwise, leave the PR for review.
6. **NEVER** add `--no-verify`, `--force`, or `--force-with-lease` to bypass hooks or rewrite history.

### 5. Report

Tabular summary at the end:

| Repo | Action | Result |
|------|--------|--------|
| `cv` | direct push to `main` | merged `abc1234` |
| `informatica-PoC` | branch+PR fallback | PR #11, auto-merge enabled |
| `joc_eda` | skipped — dirty tree | — |

Surface anything that needs follow-up: skipped repos, failing CI, PRs awaiting review.

## Workspace rules to encode

These are not optional — they are why this skill exists instead of a raw for-loop:

- **State filter is the floor, not the ceiling.** Default to `active`/`active-research`/`mixed`. Widening to FROZEN states requires explicit user consent for that specific sweep.
- **Push direct to `main` first.** Only fall back to branch+PR when a lefthook or branch-protection rule actually rejects the push. Don't pre-emptively branch.
- **Never bypass hooks.** No `--no-verify`. If a hook rejects, branch + PR is the answer.
- **Refresh `repos.json` before sweeping.** Run `make update-repos` at the start so the filter operates on current state.
- **Idempotency.** Re-running the sweep should be a no-op for repos already in the target state. Skip rather than re-apply.
- **One commit per repo per sweep.** Don't combine unrelated changes; conventional-commit message describes only the sweep payload.

## Examples

**Add a file to every active repo (PowerShell):**

```powershell
$src = "$env:USERPROFILE\cuberhaus\assets\pull_request_template.md"
$repos = (Get-Content "$env:USERPROFILE\cuberhaus\repos.json" -Raw | ConvertFrom-Json) |
    Where-Object { $_.state -in 'active','active-research','mixed' -and -not $_.has_pr_template }
foreach ($r in $repos) {
    Push-Location "$env:USERPROFILE\cuberhaus\$($r.name)"
    git fetch origin 2>$null; git checkout main 2>$null; git pull --ff-only 2>$null
    New-Item -ItemType Directory -Path .github -Force | Out-Null
    Copy-Item $src .github\pull_request_template.md -Force
    git add .github/pull_request_template.md
    git commit -m "chore(github): add generic PR template"
    $push = git push origin main 2>&1
    if ($push -match 'no-commit-on-main|rejected') {
        git reset --soft HEAD~1
        git checkout -b chore/add-pr-template
        git commit -m "chore(github): add generic PR template"
        git push -u origin chore/add-pr-template
        gh pr create --fill --base main
    }
    Pop-Location
}
```

**Toggle GitHub repo settings (cross-platform via `gh`):**

```powershell
$repos = (Get-Content "$env:USERPROFILE\cuberhaus\repos.json" -Raw | ConvertFrom-Json) |
    Where-Object { $_.state -in 'active','active-research','mixed' }
foreach ($r in $repos) {
    gh api -X PATCH "repos/cuberhaus/$($r.name)" `
        -F delete_branch_on_merge=true -F has_wiki=false -F has_projects=false | Out-Null
}
```

(Pure API-side, no git push — lefthook fallback not needed.)

## Anti-patterns

- **Don't sweep `cuberhaus` itself.** The workspace root is not a git repo; the catalog should be filtered to repos with `cloned: true` and a `.git/` directory.
- **Don't run a sweep without `make update-repos` first.** A stale catalog will skip newly-cloned repos or include deleted ones.
- **Don't auto-merge PRs you opened in the same sweep.** Always check `mergeStateStatus` first. CI may legitimately fail.
- **Don't widen the state filter silently.** If the user asks for a sweep across "all repos", confirm explicitly that FROZEN coursework is in scope.
- **Don't combine multiple unrelated changes** into one sweep. One sweep = one logical change = one conventional commit message.

## Related

- [`audit-policies.py`](../../../scripts/audit-policies.py) — surfaces drift; the input list for most sweeps.
- [`policies.json`](../../../policies.json) — the contract being enforced (`audited_states` matches the default filter).
- [`build-repos.py`](../../../scripts/build-repos.py) — regenerates `repos.json`; runs as `make update-repos`.
- [`/init-repo`](../init-repo/SKILL.md) — per-repo AGENTS.md scaffold; `/bulk-sweep` is the multi-repo counterpart.

## Reproducibility

This skill's source lives in `WinDotfiles/cuberhaus-workspace/.github/skills/bulk-sweep/` (Windows) and `dotfiles/cuberhaus-workspace/.github/skills/bulk-sweep/` (Linux). Both copies must stay byte-identical; edit one, mirror to the other, run `make check-parity` to verify, then `make sync-workspace` from either repo to deploy.
