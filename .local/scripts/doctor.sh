#!/usr/bin/env bash
# doctor: report missing tools and broken symlinks. Runs from `make doctor`.
set -uo pipefail

GREEN=$'\033[32m'
RED=$'\033[31m'
YELLOW=$'\033[33m'
RESET=$'\033[0m'

# command name -> install hint
declare -A TOOLS=(
    [stow]='pacman -S stow / apt install stow / brew install stow'
    [git]='pacman -S git / apt install git / brew install git'
    [shellcheck]='pacman -S shellcheck / apt install shellcheck / brew install shellcheck'
    [markdownlint-cli2]='npm install -g markdownlint-cli2'
    [vint]='pip install vim-vint'
)

echo "==> Checking required tools..."
missing=0
# Iterate in a stable order
for cmd in stow git shellcheck markdownlint-cli2 vint; do
    if path=$(command -v "$cmd" 2>/dev/null); then
        printf "  %s✓%s %-22s %s\n" "$GREEN" "$RESET" "$cmd" "$path"
    else
        printf "  %s✗%s %-22s install: %s\n" "$RED" "$RESET" "$cmd" "${TOOLS[$cmd]}"
        missing=$((missing + 1))
    fi
done

echo ""
echo "==> Scanning \$HOME for broken symlinks (max depth 4)..."
broken=$(find "$HOME" -maxdepth 4 -xtype l 2>/dev/null \
    | grep -v -e '/.cache/' -e '/.local/share/Trash/' || true)
if [ -n "$broken" ]; then
    printf "  %s⚠%s broken symlinks found:\n" "$YELLOW" "$RESET"
    echo "$broken" | sed 's/^/    /'
else
    printf "  %s✓%s no broken symlinks\n" "$GREEN" "$RESET"
fi

echo ""
if [ "$missing" -gt 0 ]; then
    echo "$missing tool(s) missing. Install them, then re-run 'make doctor'."
    exit 1
fi
