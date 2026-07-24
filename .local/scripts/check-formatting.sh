#!/usr/bin/env bash
set -euo pipefail

markdown_files=(README.md .local/README.md .local/xdg/wallpapers/README.md)

if [[ -d "$HOME/.local/bin" ]]; then
    export PATH="$HOME/.local/bin:$PATH"
fi

if ! command -v markdownlint-cli2 >/dev/null 2>&1 &&
    ! command -v markdownlint >/dev/null 2>&1 &&
    [[ -s "$HOME/.nvm/nvm.sh" ]]; then
    # shellcheck source=/dev/null
    source "$HOME/.nvm/nvm.sh"
fi

run_markdownlint() {
    local fix_option=()
    [[ ${1:-} == "--fix" ]] && fix_option=(--fix)

    if command -v markdownlint-cli2 >/dev/null 2>&1; then
        markdownlint-cli2 "${fix_option[@]}" "${markdown_files[@]}"
    elif command -v markdownlint >/dev/null 2>&1; then
        markdownlint "${fix_option[@]}" "${markdown_files[@]}"
    else
        printf 'markdownlint not found (npm install -g markdownlint-cli2 or run make doctor).\n' >&2
        return 127
    fi
}

case "${1:-}" in
    --fix)
        run_markdownlint --fix
        ;;
    "")
        printf '\n==> Running markdownlint...\n'
        run_markdownlint
        printf '\n==> Running vint (vimrc)...\n'
        command -v vint >/dev/null 2>&1 || {
            printf 'vint not found (pip install vim-vint or run make doctor).\n' >&2
            exit 127
        }
        vint --style-problem .vim/vimrc || true
        ;;
    *)
        printf 'Usage: %s [--fix]\n' "$0" >&2
        exit 2
        ;;
esac