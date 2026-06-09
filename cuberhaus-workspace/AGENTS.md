# cuberhaus workspace

Multi-root VS Code workspace at `~/cuberhaus` containing **~50 independent git repositories** cloned side-by-side. This folder is **not a git repo itself** — it just collects clones in one place for the editor. The authoritative catalog is [repos.json](repos.json) (name, description, topics, archive/fork status).

## Working in this workspace

- **Before workspace-wide tasks, refresh the catalog.** Run `make update-repos` from `WinDotfiles/` or `dotfiles/` to update [repos.json](repos.json).
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

## Reproducibility

This file and the `/init-repo` skill are git-tracked in [WinDotfiles/cuberhaus-workspace/](WinDotfiles/cuberhaus-workspace/) (Windows) and [dotfiles/cuberhaus-workspace/](dotfiles/cuberhaus-workspace/) (Linux), and copied here by `make sync-workspace` from either repo. Treat the copies under `~/cuberhaus/` as **generated** — edit the source files in WinDotfiles or dotfiles, then re-sync. Keep the two source folders byte-identical for `AGENTS.md`, `SKILL.md`, `template.md`, and `assets/pre-push`.

---

**Workspace root:** `c:\Users\lara\cuberhaus` (Windows) — managed via [WinDotfiles](WinDotfiles/). Linux peer lives under [dotfiles](dotfiles/).
