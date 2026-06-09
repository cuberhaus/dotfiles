#!/usr/bin/env bash
# Copy dotfiles/cuberhaus-workspace sources into the workspace root.
#
# Source of truth for workspace-root agent-customization files lives in this
# folder (git-tracked under dotfiles). This script copies them into
# $WORKSPACE_ROOT so the VS Code multi-root workspace at ~/cuberhaus picks
# them up. Idempotent: only copies when source and destination SHA256 differ.
#
# Also deletes the legacy 15-byte stub at
# $WORKSPACE_ROOT/.github/copilot-instructions.md if it points at
# ../.cursorrules (superseded by AGENTS.md).
#
# Usage:
#   ./sync.sh                       # sync into $HOME/cuberhaus
#   ./sync.sh -n                    # dry run
#   ./sync.sh -w /opt/cuberhaus     # custom workspace root
#   ./sync.sh -h                    # help

set -euo pipefail

WORKSPACE_ROOT="${HOME}/cuberhaus"
DRY_RUN=0

usage() {
    cat <<EOF
Usage: $(basename "$0") [-w WORKSPACE_ROOT] [-n] [-h]

  -w PATH   Workspace root (default: \$HOME/cuberhaus)
  -n        Dry run (report what would change without writing)
  -h        Show this help and exit
EOF
}

while getopts ":w:nh" opt; do
    case "$opt" in
        w) WORKSPACE_ROOT="$OPTARG" ;;
        n) DRY_RUN=1 ;;
        h) usage; exit 0 ;;
        \?) echo "Unknown option: -$OPTARG" >&2; usage >&2; exit 2 ;;
        :)  echo "Option -$OPTARG requires an argument." >&2; exit 2 ;;
    esac
done

source_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"

if [[ ! -d "$WORKSPACE_ROOT" ]]; then
    printf '\033[31mWorkspace root not found:\033[0m %s\n' "$WORKSPACE_ROOT" >&2
    printf '\033[33mPass -w <path> or create the directory first.\033[0m\n' >&2
    exit 1
fi

# Top-level pairs of "source-relative|dest-relative". AGENTS.md needs
# special header handling, so it stays explicit. Files under
# .github/skills/ are auto-discovered below — new skills get picked up
# with no edits here.
pairs=(
    "AGENTS.md|AGENTS.md"
)

# Auto-discover everything under .github/skills/ (SKILL.md, references, etc).
if [[ -d "${source_dir}/.github/skills" ]]; then
    while IFS= read -r -d '' f; do
        rel="${f#${source_dir}/}"
        pairs+=("${rel}|${rel}")
    done < <(find "${source_dir}/.github/skills" -type f -print0 | sort -z)
fi

changed=0
same=0
missing=0

C_RESET=$'\033[0m'
C_GREEN=$'\033[32m'
C_CYAN=$'\033[36m'
C_YELLOW=$'\033[33m'
C_DIM=$'\033[90m'

printf '%sSource:%s      %s\n' "$C_DIM" "$C_RESET" "$source_dir"
printf '%sDestination:%s %s\n' "$C_DIM" "$C_RESET" "$WORKSPACE_ROOT"
if (( DRY_RUN )); then
    printf '%sMode:%s         DRY RUN\n' "$C_CYAN" "$C_RESET"
fi
echo

sha256_of() {
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum -- "$1" | awk '{print $1}'
    else
        shasum -a 256 -- "$1" | awk '{print $1}'
    fi
}

sha256_stdin() {
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum | awk '{print $1}'
    else
        shasum -a 256 | awk '{print $1}'
    fi
}

for pair in "${pairs[@]}"; do
    src_rel="${pair%%|*}"
    dst_rel="${pair##*|}"
    src_path="${source_dir}/${src_rel}"
    dst_path="${WORKSPACE_ROOT}/${dst_rel}"

    if [[ ! -f "$src_path" ]]; then
        printf '%s[skip]    missing source: %s%s\n' "$C_YELLOW" "$src_rel" "$C_RESET"
        ((missing++)) || true
        continue
    fi

    needs_copy=1
    if [[ -f "$dst_path" ]]; then
        if [[ "$dst_rel" == "AGENTS.md" ]]; then
            # Destination has a prepended GENERATED header. Compare body
            # (after the first blank line) to source to stay idempotent.
            if head -n 1 -- "$dst_path" | grep -q '^<!-- GENERATED FILE'; then
                dst_body_hash=$(sed '1,/^$/d' -- "$dst_path" | sha256_stdin)
                src_hash=$(sha256_of "$src_path")
                [[ "$dst_body_hash" == "$src_hash" ]] && needs_copy=0
            fi
        elif [[ "$(sha256_of "$src_path")" == "$(sha256_of "$dst_path")" ]]; then
            needs_copy=0
        fi
    fi

    if (( needs_copy == 0 )); then
        printf '%s[same]    %s%s\n' "$C_DIM" "$dst_rel" "$C_RESET"
        ((same++)) || true
        continue
    fi

    dst_dir="$(dirname -- "$dst_path")"
    if (( DRY_RUN )); then
        if [[ ! -d "$dst_dir" ]]; then
            printf '%s[mkdir]   %s (dry run)%s\n' "$C_CYAN" "$dst_dir" "$C_RESET"
        fi
        printf '%s[copy]    %s (dry run)%s\n' "$C_CYAN" "$dst_rel" "$C_RESET"
    else
        mkdir -p -- "$dst_dir"
        # For AGENTS.md, prepend a GENERATED warning so direct edits at the
        # workspace root are obviously wrong. Source files stay clean.
        if [[ "$dst_rel" == "AGENTS.md" ]]; then
            {
                printf '<!-- GENERATED FILE - DO NOT EDIT.\n'
                printf '     Source: dotfiles/cuberhaus-workspace/AGENTS.md (or WinDotfiles peer).\n'
                printf '     Re-sync with `make sync-workspace` from either repo. -->\n\n'
                cat -- "$src_path"
            } > "$dst_path"
        else
            cp -f -- "$src_path" "$dst_path"
        fi
        printf '%s[copied]  %s%s\n' "$C_GREEN" "$dst_rel" "$C_RESET"
    fi
    ((changed++)) || true
done

# Remove the legacy 15-byte stub copilot-instructions.md if it's the known
# ../.cursorrules pointer. Anything substantive is left alone.
stub="${WORKSPACE_ROOT}/.github/copilot-instructions.md"
if [[ -f "$stub" ]]; then
    size=$(wc -c < "$stub" | tr -d '[:space:]')
    content=$(tr -d '[:space:]' < "$stub")
    is_stub=0
    if (( size <= 50 )) && [[ "$content" == "../.cursorrules" || "$content" == '..\.cursorrules' ]]; then
        is_stub=1
    fi
    if (( is_stub )); then
        if (( DRY_RUN )); then
            printf '%s[rm]      .github/copilot-instructions.md (legacy stub, dry run)%s\n' "$C_CYAN" "$C_RESET"
        else
            rm -f -- "$stub"
            printf '%s[deleted] .github/copilot-instructions.md (legacy stub)%s\n' "$C_GREEN" "$C_RESET"
        fi
        ((changed++)) || true
    else
        printf '%s[keep]    .github/copilot-instructions.md (not a stub, size=%s)%s\n' "$C_DIM" "$size" "$C_RESET"
    fi
fi

echo
if (( DRY_RUN )); then
    printf '%sDry run: %d change(s) pending, %d already in sync, %d missing source(s).%s\n' \
        "$C_CYAN" "$changed" "$same" "$missing" "$C_RESET"
else
    printf '%sSync complete: %d change(s), %d already in sync, %d missing source(s).%s\n' \
        "$C_GREEN" "$changed" "$same" "$missing" "$C_RESET"
fi

# Deploy VS Code user prompts (separate destination root).
# Source: cuberhaus-workspace/prompts/*.prompt.md
# Dest:   $HOME/.config/Code/User/prompts/
prompts_source="${source_dir}/prompts"
prompts_dest="${HOME}/.config/Code/User/prompts"

if [[ -d "$prompts_source" ]]; then
    prompt_changed=0
    prompt_same=0
    echo
    printf '%sVS Code prompts: %s -> %s%s\n' "$C_DIM" "$prompts_source" "$prompts_dest" "$C_RESET"
    if [[ ! -d "$prompts_dest" ]]; then
        if (( DRY_RUN )); then
            printf '%s[mkdir]   %s (dry run)%s\n' "$C_CYAN" "$prompts_dest" "$C_RESET"
        else
            mkdir -p -- "$prompts_dest"
        fi
    fi
    shopt -s nullglob
    for prompt in "$prompts_source"/*.prompt.md; do
        name="$(basename -- "$prompt")"
        dst="${prompts_dest}/${name}"
        needs_copy=1
        if [[ -f "$dst" ]] && [[ "$(sha256_of "$prompt")" == "$(sha256_of "$dst")" ]]; then
            needs_copy=0
        fi
        if (( needs_copy == 0 )); then
            printf '%s[same]    prompts/%s%s\n' "$C_DIM" "$name" "$C_RESET"
            ((prompt_same++)) || true
        elif (( DRY_RUN )); then
            printf '%s[copy]    prompts/%s (dry run)%s\n' "$C_CYAN" "$name" "$C_RESET"
            ((prompt_changed++)) || true
        else
            cp -f -- "$prompt" "$dst"
            printf '%s[copied]  prompts/%s%s\n' "$C_GREEN" "$name" "$C_RESET"
            ((prompt_changed++)) || true
        fi
    done
    shopt -u nullglob
    printf '%sPrompts: %d change(s), %d already in sync.%s\n' \
        "$C_GREEN" "$prompt_changed" "$prompt_same" "$C_RESET"
fi
