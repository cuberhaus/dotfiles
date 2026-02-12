#!/usr/bin/env bash
# Lint all shell scripts in the dotfiles repo with shellcheck.
# Usage: bash .local/scripts/lint.sh
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "$0")/../.." && pwd)"
cd "$REPO_ROOT"

if ! command -v shellcheck &>/dev/null; then
    echo "shellcheck is not installed. Install it first:"
    echo "  Arch/Manjaro: sudo pacman -S shellcheck"
    echo "  Ubuntu:       sudo apt install shellcheck"
    echo "  macOS:        brew install shellcheck"
    exit 1
fi

# Collect scripts: tracked files that are shell scripts, minus vendored/submodule dirs
scripts=()
while IFS= read -r f; do
    scripts+=("$f")
done < <(
    git ls-files -- \
        '*.sh' \
        '.zshenv' \
        '.bashrc' \
        '.bash_profile' \
        '.xinitrc' \
        '.xprofile' \
        '.config/zsh/aliases' \
        '.config/zsh/functions' \
        '.config/.git-prompt-colors.sh' \
        '.config/i3-layout-manager/layouts/*.sh' \
        '.config/i3/*.sh' \
        '.config/xmobar/*.sh' \
        '.local/scripts/bootstrap/*' \
        '.local/scripts/bin/*' \
        '.local/scripts/*.sh' \
        '.local/scripts/applets' \
        '.local/scripts/desklets' \
        '.local/scripts/toggle_theme' \
        '.local/scripts/spot' \
        '.local/scripts/spoti' \
        '.local/scripts/volume-change-output' \
        '.local/scripts/cinnamon_path/*' \
        '.local/Mini/.bashrc' \
    | grep -v \
        -e 'texstudio/dictionaries' \
        -e 'BigBagKbdTrixXKB' \
        -e 'base16-shell' \
        -e 'bash-git-prompt' \
        -e 'i3blocks-contrib' \
        -e 'i3blocks-spotify' \
        -e 'i3-layout-manager/i3' \
        -e '\.terminfo$'
)

if [[ ${#scripts[@]} -eq 0 ]]; then
    echo "No shell scripts found to lint."
    exit 0
fi

echo "Linting ${#scripts[@]} shell scripts..."
echo "---"

failures=0
for script in "${scripts[@]}"; do
    if ! shellcheck "$script"; then
        ((failures++)) || true
    fi
done

echo "---"
if [[ $failures -eq 0 ]]; then
    echo "All ${#scripts[@]} scripts passed."
else
    echo "$failures/${#scripts[@]} scripts had warnings."
    exit 1
fi
