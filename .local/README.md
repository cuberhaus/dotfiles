# .local directory

Everything under `.local/` follows the
[XDG Base Directory](https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html)
convention and is symlinked into `$HOME/.local/` by GNU Stow.

## Directory layout

```
.local/
├── etc/                        # Miscellaneous config snippets
│   ├── launch.json             # VSCode-style debug launch config
│   ├── tasks.json              # VSCode-style task definitions
│   └── .ycm_extra_conf.py     # YouCompleteMe C/C++ flags
│
├── scripts/                    # Shell scripts and automation
│   ├── bin/                    # User scripts added to $PATH
│   │   ├── changeBrightness   # Brightness control (used by i3/xmonad)
│   │   ├── changeVolume       # Volume control with notification
│   │   ├── clone-all          # Clone all repos from a GitHub user
│   │   ├── git-recurse        # Run git commands across multiple repos
│   │   ├── pfetch             # Minimal system info display
│   │   ├── program            # Launch-or-focus helper for scratchpads
│   │   ├── prompt             # Custom prompt helper
│   │   └── yolo               # Alias for quick git push
│   │
│   ├── bootstrap/              # OS-specific bootstrap scripts
│   │   ├── arch               # Arch Linux bootstrap entrypoint
│   │   ├── manjaro            # Manjaro bootstrap entrypoint
│   │   ├── ubuntu             # Ubuntu bootstrap entrypoint
│   │   ├── mac                # macOS bootstrap entrypoint
│   │   ├── base_functions     # Shared helpers (logging, $DOTFILES, prep)
│   │   ├── arch_functions     # Arch/Manjaro package lists & installers
│   │   ├── ubuntu_functions   # Ubuntu package lists & installers
│   │   ├── mac_functions      # macOS (Homebrew) package lists & installers
│   │   └── xterm-256color-italic.terminfo
│   │
│   ├── cinnamon_path/          # Scripts added to $PATH on Cinnamon DE
│   │   ├── cinnamon_load_config
│   │   ├── cinnamon_dump_config
│   │   ├── light-theme
│   │   └── dark-theme
│   │
│   ├── gnome_path/             # Scripts added to $PATH on GNOME DE
│   │   ├── gnome_load_config
│   │   └── gnome_dump_config
│   │
│   ├── hooks/                  # Git hooks
│   │   └── pre-commit         # Runs shellcheck on staged shell scripts
│   │
│   ├── lint.sh                 # Lint all tracked shell scripts with shellcheck
│   ├── toggle_theme            # Switch between light/dark themes
│   └── ...                     # Other utility scripts
│
├── share/                      # XDG data files
│   ├── fonts/                  # Nerd Fonts (SourceCodePro, UbuntuMono, etc.)
│   ├── icons/                  # Avatar and hicolor icon overrides
│   ├── nvim/                   # Neovim data
│   └── xfce4/                  # XFCE4 terminal color schemes
│
└── xdg/
    └── wallpapers/             # Bundled wallpapers
```

## How scripts are loaded

- **`bin/`** is added to `$PATH` by `.zshenv` so its contents are available
  as commands in any shell.
- **`cinnamon_path/`** and **`gnome_path/`** are conditionally added to
  `$PATH` based on the `$DESKTOP_SESSION` environment variable (see `.zshenv`).
- **`bootstrap/`** scripts are run via `make bootstrap-<os>` (see the root
  Makefile) and are **not** on `$PATH`.

## Bootstrap flow

Each bootstrap entrypoint (e.g. `bootstrap/arch`) follows this pattern:

1. Source `base_functions` (logging, `$DOTFILES` auto-detection, common prep).
2. Source the OS-specific `*_functions` file (package lists, installers).
3. Ask if this is a first-time install.
4. Run system update.
5. Call installer functions in dependency order.
6. Switch default shell to zsh.

See the root [README](../README.md) for quick-start instructions.
