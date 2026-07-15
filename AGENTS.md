# dotfiles

Personal Linux/Unix dotfiles for Arch, Manjaro, Ubuntu, and macOS — shell, editor, window-manager, and tooling configs. Managed via **GNU Stow**; primary shell is **Zsh** (antigen + p10k) with bash fallbacks.

## Architecture

- Repo lives at `~/dotfiles/dotfiles/` (nested so stow treats the inner dir as the package). Stow symlinks `.config/`, `.local/`, `.vim/`, `.xmonad/`, `.zshenv`, etc. into `$HOME`.
- `$DOTFILES` (exported by `.zshenv`) resolves the symlink back to the repo root — scripts and configs should reference paths via `$DOTFILES`, not hardcoded `~/dotfiles/dotfiles`.
- OS-specific setup is segregated under `.local/scripts/bootstrap/` (one entrypoint per OS, plus shared `base_functions` and per-OS `*_functions` files).
- Volatile, app-rewritten files (Warp prefs, LibreOffice settings) are tracked but masked with `git update-index --skip-worktree`.

## Build and Test

`make help` lists everything. Common targets:

- `make install` / `make uninstall` / `make restow` — stow lifecycle (install backs up conflicts first via `.local/scripts/stow-backup-conflicts`).
- `make dry-run` — simulate stow, report conflicts, no changes.
- `make lint` (shellcheck), `make test` (unit tests), and `make check` (tests + shellcheck + markdownlint + vint).
- `make audit-installation` — read-only comparison of the checkout, Stow-managed files, active bootstrap package declarations, and native automations. Set `PROFILE=arch|manjaro|ubuntu|ubuntu-windows|mac|work` to override auto-detection.
- `make bootstrap-{arch,manjaro,ubuntu,mac,work}` — full OS provisioning; **read the script first**, it installs hundreds of packages.
- `make skip-worktree` — run once after cloning to silence volatile files.

## Conventions

- **POSIX-compliant bash** by default; explicitly note when a feature requires Zsh.
- Use robust patterns: `find … -print0 | xargs -0`, `while IFS= read -r -d ''`, prefer `awk`/`sed`/`grep`/`find` over ad-hoc parsing.
- New aliases/functions in `.config/zsh/aliases` and `.config/zsh/functions` must not shadow standard Unix commands.
- User-facing output uses ANSI colors: success `\033[32m`, warning `\033[33m`, info `\033[34m`, always reset with `\033[0m`.
- Parallelize repo/file iteration with `xargs -P`, backgrounded `&` jobs + `wait`, especially for multi-repo helpers like `add_pat`.

## Agent skills

Installable skills live under `.agents/skills/` (gitignored; restore with `make skills-restore`). Pinned versions are in [skills-lock.json](skills-lock.json).

- **bash-defensive-patterns** — consult when writing or refactoring bash scripts under `.local/scripts/` (bootstrap, helpers, hooks).
- **shellcheck-configuration** — consult when configuring `.shellcheckrc` or addressing findings from `make lint` / `make check`.

## Pitfalls

- **Never overwrite `$HOME` files blindly** — symlink via stow or back up first; `make install` already handles conflict backups.
- **Do not run `sudo apt install`, `pacman -S`, `brew install`, or edit `/etc/`** without asking the user. Bootstrap scripts are opt-in.
- Keep platform-specific config separate (WSL vs native Linux vs macOS); don't merge Arch and Ubuntu package lists.
- Destructive helpers must support a dry-run mode and print clear usage.

## Workspace integration (cuberhaus multi-root)

The sibling [cuberhaus-workspace/](../cuberhaus-workspace/) repo holds the files that live at the workspace root (`~/cuberhaus`) but couldn't otherwise be versioned, because that root is a plain folder collecting ~50 sibling repos. `make sync-workspace` from here delegates to `../cuberhaus-workspace/sync.sh` to copy `AGENTS.md`, the `/init-repo` skill, hooks, and prompts into the root and remove legacy stubs.

The `cuberhaus-workspace/` repo is the single source of truth on both Windows (via WinDotfiles) and Linux (via this repo); only the sync driver invocation differs (`sync.sh` here, `sync.ps1` on Windows). See [../cuberhaus-workspace/README.md](../cuberhaus-workspace/README.md).

See [README.md](README.md) for full setup and [.local/README.md](.local/README.md) for the scripts layout.
