#!/usr/bin/env bash
# Shared patterns for shell script linting (used by lint.sh and pre-commit).
# Single source of truth — edit here, both tools pick it up.

# Glob patterns passed to `git ls-files` to find shell scripts
LINT_PATTERNS=(
    '*.sh'
    '.zshenv'
    '.bashrc'
    '.bash_profile'
    '.xinitrc'
    '.xprofile'
    '.config/zsh/aliases'
    '.config/zsh/functions'
    '.config/i3-layout-manager/layouts/*.sh'
    '.config/i3/*.sh'
    '.config/xmobar/*.sh'
    '.local/scripts/bootstrap/*'
    '.local/scripts/bin/*'
    '.local/scripts/*.sh'
    '.local/scripts/applets'
    '.local/scripts/desklets'
    '.local/scripts/toggle_theme'
    '.local/scripts/spot'
    '.local/scripts/spoti'
    '.local/scripts/volume-change-output'
    '.local/scripts/cinnamon_path/*'
    '.local/Mini/.bashrc'
)

# Substrings to exclude (vendored / submodule paths)
LINT_EXCLUDES=(
    'texstudio/dictionaries'
    'BigBagKbdTrixXKB'
    'base16-shell'
    'i3blocks-contrib'
    'i3blocks-spotify'
    'i3-layout-manager/i3'
    '\.terminfo$'
)

# Print all tracked shell scripts that should be linted (one per line).
lint_get_scripts() {
    local grep_args=()
    for pattern in "${LINT_EXCLUDES[@]}"; do
        grep_args+=(-e "$pattern")
    done
    git ls-files -- "${LINT_PATTERNS[@]}" | grep -v "${grep_args[@]}"
}

# Pathspecs for files allowed to contain `/home/<user>/` or `/Users/<user>/`.
# Shared by lint.yml's hardcoded-paths job and the pre-commit hook.
HARDCODED_PATHS_EXCLUDES=(
    ':!*.md'
    ':!.gitignore'
    ':!SECURITY.md'
    ':!.config/warp-terminal/user_preferences.json'
    ':!.config/birdtray-config.json'
    ':!.config/texstudio/texstudio.ini'
    ':!.config/texstudio/cache/'
    ':!.config/texstudio/dictionaries/'
    ':!.cinnamon/backgrounds/user-folders.lst'
    ':!.config/cinnamon/dark/gnome.dconf'
    ':!.config/cinnamon/light/gnome.dconf'
    ':!.config/.p10k.zsh'
    ':!.config/zsh/.p10k.zsh'
    ':!.config/vlc/vlc-qt-interface.conf'
    ':!.vim/.netrwhist'
    ':!.local/scripts/bootstrap/arch_functions'
)
HARDCODED_PATHS_REGEX='(/home/[a-z][a-z0-9_-]*/|/Users/[a-z][a-z0-9_-]*/)'

# Markdown files that markdownlint should check.
LINT_MARKDOWN_FILES=(
    'README.md'
    '.local/README.md'
    '.local/xdg/wallpapers/README.md'
)
