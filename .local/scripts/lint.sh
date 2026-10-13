#!/usr/bin/env bash
# Lint all shell scripts in the dotfiles repo with shellcheck.
# Usage: bash .local/scripts/lint.sh
set -euo pipefail

REPO_ROOT="$(cd -P "$(dirname "$0")/../.." && pwd)"
cd "$REPO_ROOT"

if ! command -v shellcheck &>/dev/null; then
    echo "shellcheck is not installed. Install it first:"
    echo "  Arch/Manjaro: sudo pacman -S shellcheck"
    echo "  Ubuntu:       sudo apt install shellcheck"
    echo "  macOS:        brew install shellcheck"
    exit 1
fi

# shellcheck source=lint_helpers.sh
source "$(dirname "$0")/lint_helpers.sh"

scripts=()
while IFS= read -r f; do
    scripts+=("$f")
done < <(lint_get_scripts)

if [[ ${#scripts[@]} -eq 0 ]]; then
    echo "No shell scripts found to lint."
    exit 0
fi

mapfile -t unlisted < <(lint_get_unlisted_scripts)
if [[ ${#unlisted[@]} -gt 0 ]]; then
    echo "Shell scripts that LINT_PATTERNS in lint_helpers.sh does not cover:"
    printf '  %s\n' "${unlisted[@]}"
    echo "Add each to LINT_PATTERNS, or to LINT_EXCLUDES when it is vendored."
    exit 1
fi

echo "Linting ${#scripts[@]} shell scripts..."
echo "---"

if shellcheck "${scripts[@]}"; then
    echo "---"
    echo "All ${#scripts[@]} scripts passed."
else
    echo "---"
    echo "Some scripts had warnings."
    exit 1
fi

matches=$(git grep -InE "$HARDCODED_PATHS_REGEX" \
    -- "${HARDCODED_PATHS_EXCLUDES[@]}" | grep -v '/home/linuxbrew/' || true)
if [[ -n "$matches" ]]; then
    echo "$matches"
    echo ""
    echo "ERROR: hardcoded user paths found in tracked files (see above)."
    echo "Use \$HOME, ~/, or XDG variables instead."
    exit 1
fi
