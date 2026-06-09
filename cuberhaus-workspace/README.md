# cuberhaus-workspace (Linux mirror)

Source of truth for agent-customization files that live at the **workspace root** (`~/cuberhaus` on Linux) rather than inside any single repo.

`~/cuberhaus` is a plain folder that collects ~50 sibling git repos for the VS Code multi-root workspace — it is not itself a git repo, so files placed there directly would be untracked and unreproducible. This folder solves that: source files live here (tracked by `dotfiles`), and [sync.sh](sync.sh) copies them into place.

This is the **Linux peer** of [WinDotfiles/cuberhaus-workspace/](https://github.com/cuberhaus/WinDotfiles/tree/main/cuberhaus-workspace). Both folders ship byte-identical content files; only the sync driver differs (`sync.ps1` on Windows, `sync.sh` here). See [Keeping the two in sync](#keeping-the-two-in-sync) below.

## Files synced

| Source (this folder)                                          | Destination (workspace root)                       |
| ------------------------------------------------------------- | -------------------------------------------------- |
| [AGENTS.md](AGENTS.md)                                        | `$HOME/cuberhaus/AGENTS.md`                        |
| [.github/skills/init-repo/SKILL.md](.github/skills/init-repo/SKILL.md) | `$HOME/cuberhaus/.github/skills/init-repo/SKILL.md` |
| [.github/skills/init-repo/references/template.md](.github/skills/init-repo/references/template.md) | `$HOME/cuberhaus/.github/skills/init-repo/references/template.md` |

Sync also **deletes** the legacy 15-byte stub at `$HOME/cuberhaus/.github/copilot-instructions.md` if present (it pointed to a non-existent `../.cursorrules` and is superseded by `AGENTS.md`).

[assets/pre-push](assets/pre-push) is the canonical pre-push hook used by the protected-branch repos (`cv`, `cuberhaus.github.io`, `informatica-PoC`, `PersonalPortfolio`, `VC`). It is not synced into the workspace root — each repo copies it under its own `.githooks/` and enables it with `git config core.hooksPath .githooks` (or `make hooks` where a target exists).

## Usage

From the dotfiles repo root:

```bash
make sync-workspace            # copy source -> workspace root (idempotent)
make sync-workspace-dry-run    # show what would change without writing
```

Or run the script directly with a custom target path:

```bash
./cuberhaus-workspace/sync.sh -w /opt/cuberhaus -n
```

## When to edit

- Editing workspace-wide agent guidance → edit [AGENTS.md](AGENTS.md) here, then `make sync-workspace`.
- Improving the per-repo init flow → edit [.github/skills/init-repo/SKILL.md](.github/skills/init-repo/SKILL.md) here, then `make sync-workspace`.
- Adding a new file to sync → drop it under this folder, add the pair to the `pairs` array in [sync.sh](sync.sh), commit.

## Keeping the two in sync

The content files in this folder must stay byte-identical to the WinDotfiles peer. When you edit one, mirror it to the other:

```bash
# From the WinDotfiles side, into dotfiles:
cp -f WinDotfiles/cuberhaus-workspace/AGENTS.md dotfiles/cuberhaus-workspace/AGENTS.md
cp -f WinDotfiles/cuberhaus-workspace/.github/skills/init-repo/SKILL.md \
      dotfiles/cuberhaus-workspace/.github/skills/init-repo/SKILL.md
cp -f WinDotfiles/cuberhaus-workspace/.github/skills/init-repo/references/template.md \
      dotfiles/cuberhaus-workspace/.github/skills/init-repo/references/template.md
cp -f WinDotfiles/cuberhaus-workspace/assets/pre-push \
      dotfiles/cuberhaus-workspace/assets/pre-push
```

Verify parity with:

```bash
diff -qr WinDotfiles/cuberhaus-workspace dotfiles/cuberhaus-workspace \
    --exclude=README.md --exclude='sync.*'
```

Only `README.md` and `sync.*` legitimately differ between the two folders.
