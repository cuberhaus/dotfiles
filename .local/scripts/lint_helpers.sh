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
    '.config/.git-prompt-colors.sh'
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
    'bash-git-prompt'
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
