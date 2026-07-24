#!/usr/bin/env bash
set -euo pipefail

command -v gitleaks >/dev/null 2>&1 || {
    printf 'gitleaks not found (install from https://github.com/gitleaks/gitleaks).\n' >&2
    exit 127
}

if [[ ${1:-} == "--staged" ]]; then
    if gitleaks git --help 2>&1 | grep -q -- '--staged'; then
        exec gitleaks git --staged --redact --no-banner
    fi
    exec gitleaks protect --staged --redact --no-banner
fi
if (($# > 0)); then
    printf 'Usage: %s [--staged]\n' "$0" >&2
    exit 2
fi
exec gitleaks git --redact --no-banner