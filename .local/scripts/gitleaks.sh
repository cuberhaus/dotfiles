#!/usr/bin/env bash
set -euo pipefail

gitleaks_runner=()
if gitleaks_command="$(command -v gitleaks 2>/dev/null)"; then
    gitleaks_runner=("$gitleaks_command")
elif command -v cmd.exe >/dev/null 2>&1 && command -v wslpath >/dev/null 2>&1; then
    for win_get_candidate in /mnt/c/Users/*/AppData/Local/Microsoft/WinGet/Links/gitleaks.exe; do
        if [[ -f "$win_get_candidate" ]]; then
            gitleaks_runner=(cmd.exe /d /c "$(wslpath -w "$win_get_candidate")")
            break
        fi
    done
elif [[ -n "${LOCALAPPDATA:-}" ]] && command -v cygpath >/dev/null 2>&1; then
    win_get_candidate="$(cygpath -u "$LOCALAPPDATA")/Microsoft/WinGet/Links/gitleaks.exe"
    [[ -f "$win_get_candidate" ]] && gitleaks_runner=("$win_get_candidate")
fi
if (( ${#gitleaks_runner[@]} == 0 )); then
    printf 'gitleaks not found (install from https://github.com/gitleaks/gitleaks).\n' >&2
    exit 127
fi

if [[ ${1:-} == "--staged" ]]; then
    if "${gitleaks_runner[@]}" git --help 2>&1 | grep -q -- '--staged'; then
        "${gitleaks_runner[@]}" git --staged --redact --no-banner
        exit $?
    fi
    "${gitleaks_runner[@]}" protect --staged --redact --no-banner
    exit $?
fi
if (($# > 0)); then
    printf 'Usage: %s [--staged]\n' "$0" >&2
    exit 2
fi
"${gitleaks_runner[@]}" git --redact --no-banner