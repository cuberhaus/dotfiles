#!/usr/bin/env bash

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
case_dir="$(mktemp -d)"
trap 'rm -rf "$case_dir"' EXIT

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

if grep -Fq "Created by \`pipx\`" "$repo_root/.bashrc" "$repo_root/.bash_profile"; then
    fail 'shell startup files contain pipx-generated PATH entries'
fi

mkdir -p "$case_dir/home/.local/bin"
# Variables in the command string are expanded by the child Bash process.
# shellcheck disable=SC2016
path_result="$(
    REPO_ROOT="$repo_root" env HOME="$case_dir/home" PATH="/usr/bin:/bin" \
        bash --noprofile --norc -c '
            source "$REPO_ROOT/.bashrc"
            source "$REPO_ROOT/.bashrc"
            printf "%s\n" "$PATH"
        ' 2>/dev/null
)"

if [[ "$path_result" != "$case_dir/home/.local/bin:/usr/bin:/bin" ]]; then
    fail "expected one portable local-bin entry, got: $path_result"
fi

mkdir -p "$case_dir/bin"
cat > "$case_dir/bin/pipx" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$PIPX_LOG"
EOF
chmod +x "$case_dir/bin/pipx"
: > "$case_dir/pipx.log"

(
    export PATH="$case_dir/bin:/usr/bin:/bin"
    export PIPX_LOG="$case_dir/pipx.log"
    source "$repo_root/.local/scripts/bootstrap/work_functions"
    apt=true
    info() { :; }
    python_install
)

grep -Fxq 'install vim-vint' "$case_dir/pipx.log" ||
    fail 'python bootstrap did not install vim-vint through pipx'
grep -Fxq 'inject --force vim-vint setuptools<81' "$case_dir/pipx.log" ||
    fail 'python bootstrap did not provide vim-vint with pkg_resources'
if grep -Fq 'ensurepath' "$case_dir/pipx.log"; then
    fail 'python bootstrap invoked mutating pipx ensurepath'
fi

printf 'Shell PATH tests passed.\n'