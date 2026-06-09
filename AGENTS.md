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
- `make lint` (shellcheck) and `make check` (shellcheck + markdownlint + vint).
- `make bootstrap-{arch,manjaro,ubuntu,mac,work}` — full OS provisioning; **read the script first**, it installs hundreds of packages.
- `make skip-worktree` — run once after cloning to silence volatile files.

## Conventions

- **POSIX-compliant bash** by default; explicitly note when a feature requires Zsh.
- Use robust patterns: `find … -print0 | xargs -0`, `while IFS= read -r -d ''`, prefer `awk`/`sed`/`grep`/`find` over ad-hoc parsing.
- New aliases/functions in `.config/zsh/aliases` and `.config/zsh/functions` must not shadow standard Unix commands.
- User-facing output uses ANSI colors: success `\033[32m`, warning `\033[33m`, info `\033[34m`, always reset with `\033[0m`.
- Parallelize repo/file iteration with `xargs -P`, backgrounded `&` jobs + `wait`, especially for multi-repo helpers like `add_pat`.

## Pitfalls

- **Never overwrite `$HOME` files blindly** — symlink via stow or back up first; `make install` already handles conflict backups.
- **Do not run `sudo apt install`, `pacman -S`, `brew install`, or edit `/etc/`** without asking the user. Bootstrap scripts are opt-in.
- Keep platform-specific config separate (WSL vs native Linux vs macOS); don't merge Arch and Ubuntu package lists.
- Destructive helpers must support a dry-run mode and print clear usage.

## Workspace integration (cuberhaus multi-root)

[cuberhaus-workspace/](cuberhaus-workspace/) is the git-tracked source for files that need to live at the workspace root (`~/cuberhaus`) but couldn't otherwise be versioned, because that root is a plain folder collecting ~50 sibling repos. `make sync-workspace` copies `AGENTS.md`, the `/init-repo` skill, and its template into the root and removes legacy stubs.

This is the Linux peer of `WinDotfiles/cuberhaus-workspace/`. Content files (`AGENTS.md`, `SKILL.md`, `template.md`, `assets/pre-push`) must stay byte-identical across the two; only the sync driver differs (`sync.sh` here, `sync.ps1` there). See [cuberhaus-workspace/README.md](cuberhaus-workspace/README.md) for the parity-check command.

See [README.md](README.md) for full setup and [.local/README.md](.local/README.md) for the scripts layout.
