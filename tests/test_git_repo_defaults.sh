#!/usr/bin/env bash
# Unit tests for .local/scripts/lib/git-repo-defaults.sh

set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
# shellcheck source=../.local/scripts/lib/git-repo-defaults.sh
. "$ROOT/.local/scripts/lib/git-repo-defaults.sh"

assert_eq() {
    if [ "$1" != "$2" ]; then
        printf 'FAIL: expected %q got %q\n' "$2" "$1" >&2
        exit 1
    fi
}

tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT

mkdir -p "$tmpdir/ws/org/repo/.git" "$tmpdir/ws/cv/.git"
touch "$tmpdir/ws/org/repo/.git/HEAD" "$tmpdir/ws/cv/.git/HEAD"

cd "$tmpdir/ws"
assert_eq "$(resolve_repo_dir "org/repo")" "org/repo"
assert_eq "$(resolve_repo_dir "cuberhaus/cv")" "cv"

git init -q "$tmpdir/work"
mkdir -p "$tmpdir/work/.githooks"
touch "$tmpdir/work/.githooks/pre-commit"

configure_tracked_repo_git "ExampleOrg/work" "$tmpdir/work" team
assert_eq "$(git -C "$tmpdir/work" config core.hooksPath)" ".githooks"
assert_eq "$(git -C "$tmpdir/work" config user.email)" "pcasacubertagil@deloitte.es"
assert_eq "$(git -C "$tmpdir/work" config user.name)" "Pol Casacuberta Gil"

CLONE_TEAM_WORK_GIT_NAME='Custom Name' CLONE_TEAM_WORK_GIT_EMAIL='custom@example.com' \
    configure_tracked_repo_git "ExampleOrg/work" "$tmpdir/work" team
assert_eq "$(git -C "$tmpdir/work" config user.name)" "Custom Name"
assert_eq "$(git -C "$tmpdir/work" config user.email)" "custom@example.com"

# apply-skip-worktree auto-invocation
mkdir -p "$tmpdir/work/.local/scripts"
cat <<'EOF' > "$tmpdir/work/.local/scripts/apply-skip-worktree"
#!/usr/bin/env bash
touch "$1/skip-worktree-executed"
EOF
chmod +x "$tmpdir/work/.local/scripts/apply-skip-worktree"
configure_tracked_repo_git "ExampleOrg/work" "$tmpdir/work" team
if [ ! -f "$tmpdir/work/skip-worktree-executed" ]; then
    printf 'FAIL: apply-skip-worktree was not executed by configure_tracked_repo_git\n' >&2
    exit 1
fi

# stale core.hooksPath unsetting when .githooks is deleted
rm -rf "$tmpdir/work/.githooks"
configure_tracked_repo_git "ExampleOrg/work" "$tmpdir/work" team
assert_eq "$(git -C "$tmpdir/work" config --get core.hooksPath || echo "unset")" "unset"

# lefthook installation when lefthook.yml is present
git init -q "$tmpdir/lefthook-repo"
touch "$tmpdir/lefthook-repo/lefthook.yml"
mkdir -p "$tmpdir/lefthook-repo/node_modules/.bin"
cat <<'EOF' > "$tmpdir/lefthook-repo/node_modules/.bin/lefthook"
#!/usr/bin/env bash
touch "$PWD/lefthook-installed"
EOF
chmod +x "$tmpdir/lefthook-repo/node_modules/.bin/lefthook"
configure_tracked_repo_git "cuberhaus/lefthook-repo" "$tmpdir/lefthook-repo" personal
if [ ! -f "$tmpdir/lefthook-repo/lefthook-installed" ]; then
    printf 'FAIL: lefthook was not installed by configure_tracked_repo_git\n' >&2
    exit 1
fi

# lefthook is found and run when the checkout is given by a relative path too, which is how
# clone-all and clone-team always pass it, and it says nothing: clone-all runs many of these at
# once, and a line from lefthook would land between the status lines. The three ways to find it
# (the repository's own node_modules, lefthook on PATH, npx) are tried one at a time with a PATH
# that holds only git, tr and stand-ins; a real npx would run the node_modules copy by itself and
# hide a broken first branch.
mkdir -p "$tmpdir/rel" "$tmpdir/path-base"
for tool in git tr; do
    ln -s "$(command -v "$tool")" "$tmpdir/path-base/$tool"
done

# stand_in FILE: a program that is noisy on both streams and leaves "NAME-ran" holding the way it
# was started (its own path and arguments) in the folder it ran in.
stand_in() {
    mkdir -p "$(dirname "$1")"
    cat <<'EOF' >"$1"
#!/bin/sh
echo "noise on stdout"
echo "noise on stderr" >&2
printf '%s\n' "$0 $*" >"$PWD/${0##*/}-ran"
EOF
    chmod +x "$1"
}

# lefthook_repo NAME: a repository with a lefthook.yml below $tmpdir/rel.
lefthook_repo() {
    git init -q "$tmpdir/rel/$1"
    touch "$tmpdir/rel/$1/lefthook.yml"
}

# configure_relative NAME PATH: configure that repository by its relative path with PATH as PATH.
# Sets $rel_output to everything it printed.
configure_relative() {
    rel_output=$(cd "$tmpdir/rel" && PATH="$2" configure_tracked_repo_git "cuberhaus/$1" "$1" personal 2>&1)
}

# 1. The repository's own program wins over lefthook and npx on PATH.
stand_in "$tmpdir/bin-all/lefthook"
stand_in "$tmpdir/bin-all/npx"
lefthook_repo own
stand_in "$tmpdir/rel/own/node_modules/.bin/lefthook"
configure_relative own "$tmpdir/bin-all:$tmpdir/path-base"
assert_eq "$(cat "$tmpdir/rel/own/lefthook-ran" 2>/dev/null || echo missing)" "./node_modules/.bin/lefthook install"
[ ! -e "$tmpdir/rel/own/npx-ran" ] || { printf 'FAIL: npx ran although the repository has its own lefthook\n' >&2; exit 1; }
assert_eq "$rel_output" ""

# 2. Without one, lefthook on PATH runs before npx.
lefthook_repo onpath
configure_relative onpath "$tmpdir/bin-all:$tmpdir/path-base"
assert_eq "$(cat "$tmpdir/rel/onpath/lefthook-ran" 2>/dev/null || echo missing)" "$tmpdir/bin-all/lefthook install"
[ ! -e "$tmpdir/rel/onpath/npx-ran" ] || { printf 'FAIL: npx ran although lefthook is on PATH\n' >&2; exit 1; }
assert_eq "$rel_output" ""

# 3. With neither, npx fetches and runs it.
stand_in "$tmpdir/bin-npx/npx"
lefthook_repo viapx
configure_relative viapx "$tmpdir/bin-npx:$tmpdir/path-base"
assert_eq "$(cat "$tmpdir/rel/viapx/npx-ran" 2>/dev/null || echo missing)" "$tmpdir/bin-npx/npx --yes lefthook install"
assert_eq "$rel_output" ""

# 4. With none of them nothing runs and nothing is said.
lefthook_repo nothing
configure_relative nothing "$tmpdir/path-base"
assert_eq "$(find "$tmpdir/rel/nothing" -maxdepth 1 -name '*-ran' | wc -l | tr -d ' ')" "0"
assert_eq "$rel_output" ""

printf 'OK: git-repo-defaults\n'
