#!/usr/bin/env bash
# doctor: report missing tools and broken symlinks. Runs from `make doctor`.
set -uo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
profile="${1:-auto}"

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
    [gitleaks]='https://github.com/gitleaks/gitleaks#installing'
    [python3]='pacman -S python / apt install python3 / brew install python'
)

echo "==> Checking required tools..."
missing=0
# Iterate in a stable order
for cmd in stow git shellcheck markdownlint-cli2 vint gitleaks python3; do
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
    while IFS= read -r line; do printf '    %s\n' "$line"; done <<< "$broken"
else
    printf "  %s✓%s no broken symlinks\n" "$GREEN" "$RESET"
fi

echo ""
echo "==> Running installation alignment audit..."
audit_status=0
if command -v python3 >/dev/null 2>&1; then
    python3 "$repo_root/.local/scripts/audit_installation.py" --profile "$profile" || audit_status=$?
else
    echo "  Installation audit skipped because python3 is missing."
    audit_status=1
fi

echo ""
if [ "$missing" -gt 0 ] || [ "$audit_status" -ne 0 ]; then
    echo "Doctor found issues: $missing missing tool(s), audit exit code $audit_status."
    exit 1
fi

echo "Doctor found no actionable issues."
