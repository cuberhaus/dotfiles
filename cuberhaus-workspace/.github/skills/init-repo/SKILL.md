---
name: init-repo
description: 'Scaffold an AGENTS.md at the root of a newly cloned repo so AI coding agents are immediately productive. Use when: opening a repo with no AGENTS.md; migrating a legacy .cursorrules or 15-byte stub copilot-instructions.md; refreshing stale agent guidance for one repo. Distills the actual stack, build commands, conventions, and gotchas from README + manifest files, migrates any substantive .cursorrules content, deletes legacy 15-byte copilot-instructions.md stubs, and validates the result. Do NOT use for bulk-running across many repos (use the workspace AGENTS.md flow instead) or for non-AGENTS.md customization files.'
argument-hint: 'Optional path to repo root (defaults to current workspace folder)'
---

# /init-repo

Scaffold a focused, concrete `AGENTS.md` at the root of a single git repository. Designed to be re-runnable: idempotent on already-initialized repos (refines rather than overwrites without consent).

## When to use

- A repo was just cloned and has no `AGENTS.md`.
- A repo has a 15-byte stub `.github/copilot-instructions.md` containing only `../.cursorrules` — the legacy pattern this workspace migrated away from.
- A repo has a substantive `.cursorrules` but no `AGENTS.md` (Cursor-only customization).
- Existing `AGENTS.md` is stale and needs a refresh.

## When NOT to use

- Initializing many repos at once — use the per-repo invocation in a loop driven by the workspace, not this skill.
- Creating instructions, prompts, custom agents, hooks, or skills — use the `agent-customization` skill.
- Editing content in an `obsidian_vault`-style notes repo — flag it as "not a code repo, do not edit content" and stop.

## Procedure

### 1. Locate the target repo

- If user passes a path argument → use it.
- Otherwise → use the current workspace folder (`$PWD`). Confirm it's a git repo (`.git/` exists). If not, ask the user which repo they meant.

### 2. Inventory existing customization (in this order)

| Path                                  | What to do                                                                                                                |
| ------------------------------------- | ------------------------------------------------------------------------------------------------------------------------- |
| `AGENTS.md` (root)                    | If present, read it, ask whether to **refine** (edit in place) or **replace** (rewrite). Default: refine.                 |
| `.github/copilot-instructions.md`     | If ≤ 50 bytes (typical stub: `../.cursorrules`), plan to **delete** after creating AGENTS.md. If substantive, migrate it. |
| `.cursorrules`                        | If present, read it. If > 200 bytes, **migrate its rules** into AGENTS.md, then **delete it** — modern Cursor reads AGENTS.md natively.        |
| `CLAUDE.md`, `.windsurfrules`, etc.   | Read for content; mention in summary but don't delete.                                                                    |

### 3. Explore the repo (cap at ~10 file reads)

Read in parallel where possible:

- `README.md` — primary source for description, build commands, gotchas.
- Manifest files: `package.json`, `requirements.txt`, `pyproject.toml`, `Cargo.toml`, `pom.xml`, `build.gradle`, `Gemfile`, `Makefile`, `Dockerfile`, `docker-compose.yml`, `*.csproj`, `Package.swift`.
- Top-level directory listing (1 level deep). List `docs/`, `src/`, `backend/`, `frontend/` contents if present.
- If unclear, read 1–2 source files at the main entry point.

**Stop exploring** as soon as you have enough to fill the template concretely. Do not over-explore.

### 4. Distill content into the template

Use [references/template.md](references/template.md). Key principles:

1. **Concrete over generic.** "Run `mvn -pl notification test`" beats "run the test suite".
2. **OMIT sections with nothing to say.** A 3-section AGENTS.md is better than a 5-section one padded with platitudes.
3. **Link, don't duplicate.** Point at `README.md`, `docs/architecture/`, `docs/guides/`. Inline only agent-critical gotchas that won't be discovered by reading README.
4. **Surface non-obvious gotchas.** Files that must NOT be deleted, env vars that are required, services that must be running, stale `requirements.txt` files, hardcoded secrets that need rotating, frozen vs active code boundaries.
5. **Frozen-coursework rule.** If the repo is FIB-UPC coursework (folder name in `APA APA_Practica AS CAIM MD MD2 PAR ROB SBC_IA VC ANTLR4_JSBach Practica_de_Planificacion projectA projectA2 SpringUnitTesting waslab01 waslab02 waslab03 waslab04 desastresIA joc_eda pracpro2 subgrup-prop7.1 TFG_DOC fib`), include a "frozen — do not refactor or modernize" pitfall.

### 5. Migrate `.cursorrules` if present and substantive

- Extract the rules (not the prose).
- Re-phrase concisely for AGENTS.md.
- **Delete `.cursorrules` after migrating** — modern Cursor reads AGENTS.md natively, so maintaining both is duplicated work.

### 6. Delete the legacy stub if applicable

- If `.github/copilot-instructions.md` is ≤ 50 bytes (a `../.cursorrules` pointer): delete it.
- **Do not** delete `.github/` itself or anything else (`workflows/`, etc.).
- Confirm with the user before deleting if the file is > 50 bytes.

### 7. Verify

After writing:

- File exists at `<repo>/AGENTS.md`.
- Stub `.github/copilot-instructions.md` is gone (if applicable).
- `.cursorrules` is gone (if it existed and was migrated).
- Line count is reasonable: 10–50 lines for most repos; up to ~60 for information-dense active projects.

### 8. Report

One short paragraph back to the user:

- File path created/refined.
- What was migrated (from `.cursorrules`? from old `copilot-instructions.md`?).
- What was deleted.
- Anything notable discovered during exploration that should be flagged (e.g., committed secrets, stale dependencies).

Do NOT paste the full AGENTS.md back — the user can open the file.

## Template

See [references/template.md](references/template.md) for the structure to fill in.

## Anti-patterns

- **Don't pad.** Empty sections like "## Conventions\n{Standard practices.}" actively hurt.
- **Don't invent build commands.** If you can't find `Makefile` / `package.json` / etc., omit the Build and Test section rather than guessing.
- **Don't lecture.** AGENTS.md is for agents who already know how to code. Skip "use semantic HTML" unless the repo has a specific rule that contradicts the default.
- **Don't echo the README.** Point at it.
- **Don't generate code, run installs, commit, push, or modify unrelated files.** This skill only writes one file and optionally deletes one stub.

## Linux equivalent

This skill's source lives in `WinDotfiles/cuberhaus-workspace/.github/skills/init-repo/` (Windows) and `dotfiles/cuberhaus-workspace/.github/skills/init-repo/` (Linux). Both copies must stay byte-identical; edit one and mirror to the other before re-syncing.
