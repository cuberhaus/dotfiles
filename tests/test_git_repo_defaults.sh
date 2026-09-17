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

printf 'OK: git-repo-defaults\n'
