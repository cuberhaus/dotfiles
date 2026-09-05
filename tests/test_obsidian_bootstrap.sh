#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
CASE_DIR="$(mktemp -d)"
EVENT_LOG="$CASE_DIR/events.log"
FAKE_BIN="$CASE_DIR/bin"
trap 'rm -rf "$CASE_DIR"' EXIT

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

export HOME="$CASE_DIR/home"
export DOTFILES_ROOT="$REPO_ROOT"
export OBSIDIAN_VAULT_ROOT="$CASE_DIR/vault"
mkdir -p "$HOME" "$FAKE_BIN"

source "$REPO_ROOT/.local/scripts/bootstrap/base_functions"

git() {
    printf 'git:%s\n' "$*" >> "$EVENT_LOG"
    if [ "$1" = clone ]; then
        mkdir -p "$3/.git"
    fi
}

make() {
    printf 'make:%s\n' "$*" >> "$EVENT_LOG"
}

obsidian_vault_install

grep -Fxq "git:clone https://github.com/cuberhaus/obsidian_vault.git $OBSIDIAN_VAULT_ROOT" "$EVENT_LOG" ||
    fail 'bootstrap must clone the Obsidian vault into the canonical checkout path'
grep -Fxq "make:-C $OBSIDIAN_VAULT_ROOT plugin-install" "$EVENT_LOG" ||
    fail 'bootstrap must install the vault community plugins through its Makefile'

: > "$EVENT_LOG"
obsidian_vault_install

if grep -Fq 'git:clone' "$EVENT_LOG"; then
    fail 'bootstrap must not reclone an existing Obsidian vault'
fi
grep -Fxq "make:-C $OBSIDIAN_VAULT_ROOT plugin-install" "$EVENT_LOG" ||
    fail 'bootstrap must re-run plugin installation for an existing vault'

for bootstrap in arch manjaro ubuntu mac work; do
    grep -Eq '^[[:space:]]*obsidian_vault_install$' \
        "$REPO_ROOT/.local/scripts/bootstrap/$bootstrap" ||
        fail "$bootstrap bootstrap must install the Obsidian vault"
done

if grep -Eq '^[[:space:]]*obsidian_vault_install$' \
    "$REPO_ROOT/.local/scripts/bootstrap/ubuntu_windows"; then
    fail 'WSL bootstrap must not install GUI-specific Obsidian configuration'
fi

printf 'Obsidian bootstrap tests passed.\n'