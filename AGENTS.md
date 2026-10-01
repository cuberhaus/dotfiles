# dotfiles

Personal Linux/Unix dotfiles for Arch, Manjaro, Ubuntu, and macOS — shell, editor, window-manager, and tooling configs. Managed via **GNU Stow**; primary shell is **Zsh** (antigen + p10k) with bash fallbacks.

## Architecture

- Repo lives at `~/cuberhaus/dotfiles/`; `~/cuberhaus` is the stow directory and `dotfiles` is the package. Stow symlinks `.config/`, `.local/`, `.vim/`, `.xmonad/`, `.zshenv`, etc. into `$HOME`.
- `$DOTFILES` (exported by `.zshenv`) resolves the symlink back to the repo root — scripts and configs should reference paths via `$DOTFILES`, not a hardcoded checkout path.
- OS-specific setup is segregated under `.local/scripts/bootstrap/` (one entrypoint per OS, plus shared `base_functions` and per-OS `*_functions` files).
- Volatile, app-rewritten files (Warp prefs, LibreOffice settings, VLC interface config) are tracked but automatically masked with `git update-index --skip-worktree` via `clone-all`, `make install`, and bootstrap.

## Build and Test

`make help` lists everything. Common targets:

- `make install` / `make uninstall` / `make restow` — stow lifecycle (install backs up conflicts first via `.local/scripts/stow-backup-conflicts`).
- `make dry-run` — simulate stow, report conflicts, no changes.
- `make lint` (shellcheck), `make test` (unit tests), and `make check` (tests + shellcheck + markdownlint + vint).
- `make audit-installation` — read-only comparison of the checkout, Stow-managed files, active bootstrap package declarations, and native automations. Set `PROFILE=arch|manjaro|ubuntu|ubuntu-windows|mac|work` to override auto-detection.
- `make bootstrap-{arch,manjaro,ubuntu,mac,work}` — full OS provisioning; **read the script first**, it installs hundreds of packages.
- `make bootstrap-gentoo-dry-run` / `make bootstrap-gentoo` — experimental Gentoo profile that has never run on a real machine. Always dry-run first; it is excluded from `make audit-installation` and profile detection on purpose. See `docs/GENTOO-BOOTSTRAP.md`.

## Conventions

- **POSIX-compliant bash** by default; explicitly note when a feature requires Zsh.
- Use robust patterns: `find … -print0 | xargs -0`, `while IFS= read -r -d ''`, prefer `awk`/`sed`/`grep`/`find` over ad-hoc parsing.
- New aliases/functions in `.config/zsh/aliases` and `.config/zsh/functions` must not shadow standard Unix commands unless they intentionally add defaults to that same command.
- User-facing output uses ANSI colors: success `\033[32m`, warning `\033[33m`, info `\033[34m`, always reset with `\033[0m`.
- Parallelize repo/file iteration with `xargs -P`, backgrounded `&` jobs + `wait`, especially for multi-repo helpers like `add_pat`.
- **Vendor apt repositories**: register them only through `apt_vendor_source_ensure` / `ide_apt_sources_ensure` in `base_functions` (deb822 `.sources`, written after the signing key is verified); never hand-write a legacy `.list`. An Ubuntu release upgrade disables third-party sources and the weekly `apt-get full-upgrade` then skips their packages silently, which is why `make audit-installation` checks the IDE packages and `make repair REPAIR=ide-repos` fixes them. Agents preview that repair with `DRY_RUN=true` and leave the real run (it uses `sudo`) to the user.

### Shell command catalog

- The interactive `commands` function in `.config/zsh/functions` discovers entries dynamically; do not maintain a second hardcoded catalog.
- Put aliases and small shell wrappers in `.config/zsh/aliases`, reusable functions in `.config/zsh/functions`, and standalone executables in `.local/scripts/bin`.
- An alias whose expansion starts with its own name (for example, `alias grep='grep -i --color=auto'`) is shown under **Command defaults** with only the added arguments. Renamed commands and composed workflows are shown under **Aliases and workflows**.
- Add a concise behavioral description immediately above every function using a `##` documentation comment. The catalog parses this comment and supports `name()`, `name ()`, and `function name()` declarations:

  ```bash
  ## Show status for Git repositories recursively.
  status() { git-recurse "$@" git status; }
  ```

- Start each executable in `.local/scripts/bin` with a one-line `# Description: ...` comment after the shebang; the catalog uses it as the managed-command description.
- Keep descriptions imperative, behavior-focused, and short enough to scan. Do not describe only the implementation location or repeat the command name.
- Validate catalog changes with `shellcheck .config/zsh/aliases .config/zsh/functions`, then source both files and run `commands` under Bash and Zsh.

## Agent skills

Installable skills live under `.agents/skills/` (gitignored; restore with `make skills-restore`). Pinned versions are in [skills-lock.json](skills-lock.json).

- **bash-defensive-patterns** — consult when writing or refactoring bash scripts under `.local/scripts/` (bootstrap, helpers, hooks).
- **shellcheck-configuration** — consult when configuring `.shellcheckrc` or addressing findings from `make lint` / `make check`.

### Issue tracker

GitHub Issues for `cuberhaus/dotfiles`; use the `gh` CLI. See `docs/agents/issue-tracker.md`.

### Triage labels

Use the canonical labels `needs-triage`, `needs-info`, `ready-for-agent`, `ready-for-human`, and `wontfix`. See `docs/agents/triage-labels.md`.

### Domain docs

This is a single-context repository. Read the root `CONTEXT.md` when present and relevant ADRs under `docs/adr/`. See `docs/agents/domain.md`.

## Pitfalls

- **Never overwrite `$HOME` files blindly** — symlink via stow or back up first; `make install` already handles conflict backups.
- **Do not run `sudo apt install`, `pacman -S`, `brew install`, `emerge`, or edit `/etc/`** without asking the user. Bootstrap scripts are opt-in.
- Keep platform-specific config separate (WSL vs native Linux vs macOS); don't merge Arch and Ubuntu package lists.
- Destructive helpers must support a dry-run mode and print clear usage.
- **Bash prompt**: the colored prompt in `.bashrc` assigns `PS1` once and only refreshes the variables it references from `PROMPT_COMMAND`, because VS Code/Cursor shell integration wraps `PS1` and re-wraps it whenever it changes. Do not re-add prompt plugins that rebuild `PS1` on every prompt (the removed `bash-git-prompt` did). It honors `NO_COLOR`, skips `TERM=dumb`, and `PROMPT_GIT=0` hides the git segment. Chat-tool terminals run `bash --noprofile --norc` on purpose and stay plain. `tests/test_bashrc_prompt.sh` covers it.
- **ROG brightness**: on the G635LX in dGPU/MUX mode `nvidia_wmi_ec_backlight` is a ghost device (values are accepted, the panel ignores them), so brightness tools writing to it appear to do nothing. The fix is `acpi_backlight=native` from `.local/scripts/brightness_fix.sh`; keep it after `shutdown_fix` in the `work` bootstrap and read `docs/ROG-BRIGHTNESS-DIAGNOSIS.md` before changing it.
- **`updateall` steps**: run every step in the foreground. `nvim +PlugUpdate` draws a full-screen UI, so the shell stops it as a background job (`suspended (tty output)`) before it updates anything. Never call `pip install --user` unconditionally: Debian/Ubuntu, Arch and Homebrew mark Python `EXTERNALLY-MANAGED` (PEP 668) and apt owns `python3-pynvim`, so `pip_user_upgrade` skips those interpreters (and virtualenvs); do not force it in `updateall` with `--break-system-packages`. `tests/test_updateall.sh` covers it.
- **Distro-specific antigen bundles**: the oh-my-zsh `archlinux` bundle loads only when `DISTRO` is `arch` or `manjaro` (guard in `.config/zsh/.zshrc`). Unguarded, it adds pacman/AUR aliases and helpers like `upgrade` and `paclist` that cannot work without `pacman`, and zsh-syntax-highlighting paints them red. Guard on `DISTRO`, not `command -v pacman`: Debian and Ubuntu ship a `pacman` arcade game in `/usr/games`. While `~/.config/antigen/init.zsh` exists, antigen ignores every `antigen bundle` call and replays that cache; it rebuilds only when `.zshrc` is newer than its `.zwc`, so a faked `DISTRO` in a real shell can poison the cache. Test guards with a stub antigen. `tests/test_zshrc_bundles.sh` covers it.
- **`cleanup` steps**: `cleanup` is a function, not an alias, because an alias appends typed arguments to its last command and so cannot offer `--dry-run`/`--help`; run new steps through `_cleanup_step`, which honors `--dry-run` (only read-only probes such as `docker info` and `snap list` still run) and tallies failures. Call external tools through `command`: this file aliases `df` to `df -h` and aliases are expanded inside function bodies, and the Arch/Manjaro `yay()` wrapper (`yay -S --noconfirm --needed`) would otherwise run instead of `yay -Sc`, so detect yay with the wrapper unset rather than `command -v yay`. Prune Docker containers before images (an image used by a stopped container survives the image prune), probe the daemon once with `docker info`, and never prune Docker volumes (they hold data). `tests/test_cleanup.sh` covers it.

## Workspace integration (cuberhaus multi-root)

The sibling [cuberhaus-workspace/](../cuberhaus-workspace/) repo holds the files that live at the workspace root (`~/cuberhaus`) but couldn't otherwise be versioned, because that root is a plain folder collecting ~50 sibling repos. `make workspace` is the single maintenance command: it syncs central files, then rebuilds `repos.json`.

The `cuberhaus-workspace/` repo is the single source of truth on both Windows (via WinDotfiles) and Linux (via this repo); only the sync driver invocation differs (`sync.sh` here, `sync.ps1` on Windows). See [../cuberhaus-workspace/README.md](../cuberhaus-workspace/README.md).

See [README.md](README.md) for full setup and [.local/README.md](.local/README.md) for the scripts layout.
