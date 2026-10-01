#!/usr/bin/env bash
# Hermetic check that .config/zsh/.zshrc loads the oh-my-zsh archlinux bundle only on Arch
# and Manjaro. Elsewhere its pacman and AUR aliases and helpers (pacin, pacupg, upgrade,
# paclist, ...) are dead names that clutter completion and highlight red.
#
# The real .zshrc runs under zsh in a sandbox HOME. A stub antigen only records its
# arguments, so nothing is cloned or sourced and the user's antigen cache is never touched.
# ZSHRC=<file> points the test at another copy, to prove it catches a regression.

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
zshrc="${ZSHRC:-$repo_root/.config/zsh/.zshrc}"
case_dir="$(mktemp -d)"
trap 'rm -rf "$case_dir"' EXIT

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

if ! command -v zsh > /dev/null 2>&1; then
    printf 'SKIP: zsh is not installed; cannot exercise .zshrc.\n'
    exit 0
fi

home="$case_dir/home"
mkdir -p "$home/.config/antigen"
: > "$home/.config/distro" # keeps the "distro guard" warning in .zshrc quiet
cat > "$home/.config/antigen/antigen.zsh" <<'EOF'
antigen() { print -r -- "$*" >> "$ANTIGEN_LOG"; }
EOF

# Run the real .zshrc as DISTRO=$1 (empty: no distro, as on macOS) and print the antigen calls
# it made. .zshrc ends with an error status here (widgets from the real plugins are missing),
# which is irrelevant: only the antigen section matters.
antigen_calls() {
    local distro=$1 log="$case_dir/antigen-${1:-none}.log"
    : > "$log"
    # The single quotes keep $1 for the child zsh, which receives the file as its argument.
    # shellcheck disable=SC2016
    env -i HOME="$home" PATH="/usr/bin:/bin" ANTIGEN_LOG="$log" DISTRO="$distro" \
        zsh -f -c 'source "$1"' zsh "$zshrc" > /dev/null 2>&1 || true
    cat "$log"
}

# Every bundle except archlinux, in order.
other_bundles() {
    grep -vx 'bundle archlinux' || true
}

arch_calls="$(antigen_calls arch)"
[[ -n "$arch_calls" ]] ||
    fail 'the stub antigen recorded nothing: .zshrc did not reach its antigen section'
grep -Fxq 'bundle systemd' <<< "$arch_calls" ||
    fail 'the antigen section did not load the systemd bundle: the stub is not seeing .zshrc'
[[ "$(tail -n 1 <<< "$arch_calls")" == 'apply' ]] ||
    fail '.zshrc must finish its antigen section with "antigen apply"'

for distro in arch manjaro; do
    calls="$(antigen_calls "$distro")"
    count="$(grep -Fxc 'bundle archlinux' <<< "$calls" || true)"
    [[ "$count" == 1 ]] ||
        fail "expected the archlinux bundle once for DISTRO=$distro, got $count"
    [[ "$calls" == "$arch_calls" ]] ||
        fail "DISTRO=$distro must load the same bundles in the same order as arch"
done

others_expected="$(other_bundles <<< "$arch_calls")"
for distro in ubuntu ubuntu_windows gentoo ''; do
    calls="$(antigen_calls "$distro")"
    if grep -Fxq 'bundle archlinux' <<< "$calls"; then
        fail "the archlinux bundle is loaded for DISTRO='$distro': its pacman aliases leak into a shell without pacman"
    fi
    [[ "$calls" == "$others_expected" ]] ||
        fail "DISTRO='$distro' must still load every other bundle, in order"
done

printf 'Zsh bundle tests passed.\n'
