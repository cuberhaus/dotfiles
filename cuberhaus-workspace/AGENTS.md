# cuberhaus workspace

Multi-root VS Code workspace at `~/cuberhaus` containing **~50 independent git repositories** cloned side-by-side. This folder is **not a git repo itself** — it just collects clones in one place for the editor. The authoritative catalog is [repos.json](repos.json) (GitHub metadata + per-clone facts: `state`, `cloned`, `has_agents_md`, `has_makefile`, `has_pr_template`, `has_skills`, `consumed_by`).

## Working in this workspace

- **Before workspace-wide tasks, refresh the catalog.** Run `make update-repos` from `WinDotfiles/` or `dotfiles/` to update [repos.json](repos.json). The catalog is regenerated end-to-end (curated `STATE` / `CONSUMED_BY` maps live in [cuberhaus-workspace/scripts/build-repos.py](WinDotfiles/cuberhaus-workspace/scripts/build-repos.py)) — edit the script when a repo changes purpose, then re-run.
- **Scope work to one repo at a time.** Every git-tracked subfolder has its own `AGENTS.md`, its own conventions, and its own `.git/`. Do not refactor across repo boundaries.
- **Read the per-repo `AGENTS.md` first** when you enter a repo. All 50 have one.
- **Most repos are FROZEN graded coursework** (FIB-UPC). Do not modernize, refactor, or "improve" coursework code unless explicitly asked. Each course repo's `AGENTS.md` encodes this rule individually.
- **Prefer dedicated sibling repos over [fib/](fib/).** Many courses in `fib/` have a more current dedicated sibling: `APA`, `MD`, `MD2`, `ROB`, `PAR`, `AS`, `CAIM`, `VC`, `SBC_IA`, `desastresIA`, `Practica_de_Planificacion`, `SpringUnitTesting`, `joc_eda`, `pracpro2`, `projectA`, `projectA2`, `subgrup-prop7.1`, `ANTLR4_JSBach`, `waslab01..04`. Use those when they exist.
- **`AGENTS.md` is the single source of agent guidance.** Modern Cursor reads it natively; the workspace no longer maintains parallel `.cursorrules` files.

## Active vs frozen — quick map

- **Active personal/work** — `PersonalPortfolio`, `WinDotfiles`, `dotfiles`, `cuberhaus`, `obsidian_vault`, `cv`, `cuberhaus.github.io`, `AgenticPoC`, `AgenticEraHackathon`, `collaborative-central-server`, `openclaw-ai`, `informatica-PoC`, `PersonalFinance`, `WorkoutApp`, `AgentesProactivos` (placeholder).
- **Mixed (frozen core + active web wrapper)** — `bitsXlaMarato` (frozen MaskRCNN core + active `web/`), `subgrup-prop7.1` (frozen Java + active `web/` Spring Boot — consumed by PersonalPortfolio), `pracpro2` (frozen C++ + active Rust `web/`), `joc_eda` (frozen C++ + active Mithril+Go playback), `desastresIA` (frozen Java + active FastAPI+Solid web), and several others where a small `web/` folder wraps frozen course code.
- **Frozen coursework** — `APA`, `APA_Practica`, `AS`, `CAIM`, `MD`, `MD2`, `PAR`, `ROB`, `SBC_IA`, `VC`, `ANTLR4_JSBach`, `Practica_de_Planificacion`, `projectA`, `projectA2`, `SpringUnitTesting`, `waslab01..04`, `TFG_DOC`, `fib`.
- **Frozen hackathon** — `Draculin-Backend`, `Draculin-Front`, `datathon2023`, `hackupc2023`.
- **Research/active** — `TFG` (bachelor's thesis, now wrapped in FastAPI + React frontend).
- **Sandbox / personal notes** — `dev`, `docs`, `test-clone3`.

## Skills

- **`/init-repo`** — scaffold an `AGENTS.md` for a newly cloned repo. Migrates substantive `.cursorrules` content (then deletes it, since Cursor reads AGENTS.md natively), removes the legacy 15-byte `copilot-instructions.md → ../.cursorrules` stub pattern, and writes a concrete AGENTS.md using the project-tested template. See [.github/skills/init-repo/SKILL.md](.github/skills/init-repo/SKILL.md).
- **`/bulk-sweep`** — apply the same small change across many active workspace repos at once, filtered by `repos.json` state. Encodes the workspace safety rules (FROZEN coursework protection, direct-push-to-main with branch+PR fallback when lefthook blocks, no `--no-verify`). Use for policy-drift cleanup, rolling out config files, or one-shot `npm audit fix` / `gh api` sweeps. See [.github/skills/bulk-sweep/SKILL.md](.github/skills/bulk-sweep/SKILL.md).

## VS Code user prompts

User-scoped prompt files live in `cuberhaus-workspace/prompts/` (byte-identical in both repos). `make sync-workspace` deploys them to the platform's VS Code user prompts dir (`%APPDATA%\Code\User\prompts\` on Windows, `~/.config/Code/User/prompts/` on Linux), so they're available in every workspace as slash commands.

- **`/skills-install`** — walk through installing an agent skill end-to-end (lockfile, `.gitignore`, Makefile targets, AGENTS.md `## Agent skills` section, commit, push or PR). Advisory: pauses before any commit or push.

## Workspace policies

Workspace-wide defaults agents should treat as the baseline for any active repo. Per-repo `AGENTS.md` can override but should call out the exception.

- **Single source of agent guidance**: `AGENTS.md` only. No `.cursorrules`, no `copilot-instructions.md` stubs. Modern Cursor reads `AGENTS.md` natively.
- **Skills as dependencies**: skills are installed via `npx skills add owner/repo@skill-name`, pinned in `skills-lock.json` (committed), and unpacked into `.agents/skills/` (gitignored). Restore reproducibly with `make skills-restore`. Each repo with skills exposes `skills-list` / `skills-update` / `skills-restore` Makefile targets.
- **Skills documentation**: each repo with installed skills has a `## Agent skills` section in `AGENTS.md` listing each skill and **when to invoke it** (not just what it does). Agents should consult these before doing the matching kind of work.
- **Monthly skills refresh**: every repo with skills has a `.github/workflows/skills-update.yml` cron job that opens a PR on the 1st of each month if `skills-lock.json` changed.
- **PR template**: every active repo has `.github/pull_request_template.md` with a checklist covering AGENTS.md updates, tests, and secrets.
- **Push policy**: prefer direct push to `main` when branches are unprotected and no client hook blocks it. Some repos enforce branch+PR via a `no-commit-on-main` lefthook — detect via the commit-time error, switch to branch + PR. **Never `--no-verify`** to bypass hooks.
- **Coursework freeze**: FIB-UPC course repos are frozen. Do not refactor, modernize, or restructure their code unless explicitly asked. Each course `AGENTS.md` encodes this rule individually.
- **Workspace catalog**: [repos.json](repos.json) is the authoritative cross-repo index. Refresh before workspace-wide tasks (`make update-repos`). Curated `STATE` / `CONSUMED_BY` maps live in [cuberhaus-workspace/scripts/build-repos.py](WinDotfiles/cuberhaus-workspace/scripts/build-repos.py).

## Reproducibility

This file and the `/init-repo` and `/bulk-sweep` skills are git-tracked in [WinDotfiles/cuberhaus-workspace/](WinDotfiles/cuberhaus-workspace/) (Windows) and [dotfiles/cuberhaus-workspace/](dotfiles/cuberhaus-workspace/) (Linux), and copied here by `make sync-workspace` from either repo. Treat the copies under `~/cuberhaus/` as **generated** — edit the source files in WinDotfiles or dotfiles, then re-sync. Keep the two source folders byte-identical for `AGENTS.md`, `SKILL.md` (init-repo and bulk-sweep), `template.md`, `scripts/build-repos.py`, `prompts/*.prompt.md`, and `assets/pre-push`. Run `make check-parity` from either repo to verify (excludes `sync.sh`/`sync.ps1`/`README.md` which are intentionally platform-specific).

---

**Workspace root:** `c:\Users\lara\cuberhaus` (Windows) — managed via [WinDotfiles](WinDotfiles/). Linux peer lives under [dotfiles](dotfiles/).
