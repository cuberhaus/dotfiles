#!/usr/bin/env bash
# Unit tests for .local/scripts/bin/git-recurse summary behavior

set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
GIT_RECURSE="$ROOT/.local/scripts/bin/git-recurse"

tmpdir=$(mktemp -d)
trap 'rm -rf "$tmpdir"' EXIT

mkdir -p "$tmpdir/remotes" "$tmpdir/workspace"

# 1. Setup origin repositories
git init --bare -b main "$tmpdir/remotes/r1" >/dev/null 2>&1
git clone "$tmpdir/remotes/r1" "$tmpdir/workspace/r1" >/dev/null 2>&1
git -C "$tmpdir/workspace/r1" config user.email "test@example.com"
git -C "$tmpdir/workspace/r1" config user.name "test"
echo "init r1" > "$tmpdir/workspace/r1/file.txt"
git -C "$tmpdir/workspace/r1" add .
git -C "$tmpdir/workspace/r1" commit -m "init r1" >/dev/null 2>&1
git -C "$tmpdir/workspace/r1" push -u origin main >/dev/null 2>&1

git init --bare -b main "$tmpdir/remotes/r2" >/dev/null 2>&1
git clone "$tmpdir/remotes/r2" "$tmpdir/workspace/r2" >/dev/null 2>&1
git -C "$tmpdir/workspace/r2" config user.email "test@example.com"
git -C "$tmpdir/workspace/r2" config user.name "test"
echo "init r2" > "$tmpdir/workspace/r2/file.txt"
git -C "$tmpdir/workspace/r2" add .
git -C "$tmpdir/workspace/r2" commit -m "init r2" >/dev/null 2>&1
git -C "$tmpdir/workspace/r2" push -u origin main >/dev/null 2>&1

# 2. Push an update to r2
pusher=$(mktemp -d)
git clone "$tmpdir/remotes/r2" "$pusher" >/dev/null 2>&1
git -C "$pusher" config user.email "test@example.com"
git -C "$pusher" config user.name "test"
echo "new line in r2" >> "$pusher/file.txt"
git -C "$pusher" commit -am "r2 update" >/dev/null 2>&1
git -C "$pusher" push origin main >/dev/null 2>&1
rm -rf "$pusher"

cd "$tmpdir/workspace"

# Test 1: Parallel pull with changes in r2
out=$("$GIT_RECURSE" git pull)
if ! grep -q "Updated repositories (1):" <<< "$out"; then
    printf 'FAIL: Expected "Updated repositories (1):" in output\n%s\n' "$out" >&2
    exit 1
fi
if ! grep -q "./r2/: 1 file changed, 1 insertion(+)" <<< "$out"; then
    printf 'FAIL: Expected diffstat for ./r2/\n%s\n' "$out" >&2
    exit 1
fi
if ! grep -q "Done: 2 ok, 0 failed (of 2)" <<< "$out"; then
    printf 'FAIL: Expected Done line\n%s\n' "$out" >&2
    exit 1
fi

# Test 2: Parallel pull when all repos are up to date
out_up_to_date=$("$GIT_RECURSE" git pull)
if ! grep -q "All repositories up to date." <<< "$out_up_to_date"; then
    printf 'FAIL: Expected "All repositories up to date."\n%s\n' "$out_up_to_date" >&2
    exit 1
fi

# Test 3: Sequential pull with changes
pusher=$(mktemp -d)
git clone "$tmpdir/remotes/r1" "$pusher" >/dev/null 2>&1
git -C "$pusher" config user.email "test@example.com"
git -C "$pusher" config user.name "test"
echo "new line in r1" >> "$pusher/file.txt"
git -C "$pusher" commit -am "r1 update" >/dev/null 2>&1
git -C "$pusher" push origin main >/dev/null 2>&1
rm -rf "$pusher"

out_seq=$("$GIT_RECURSE" -s git pull)
if ! grep -q "Updated repositories (1):" <<< "$out_seq"; then
    printf 'FAIL: Expected "Updated repositories (1):" in sequential mode\n%s\n' "$out_seq" >&2
    exit 1
fi
if ! grep -q "./r1/: 1 file changed, 1 insertion(+)" <<< "$out_seq"; then
    printf 'FAIL: Expected diffstat for ./r1/ in sequential mode\n%s\n' "$out_seq" >&2
    exit 1
fi

# Test 4: Non-pull command should not show summary by default
out_status=$("$GIT_RECURSE" git status --short)
if grep -q "Updated repositories" <<< "$out_status" || grep -q "repositories up to date" <<< "$out_status"; then
    printf 'FAIL: git status should not display pull summary\n%s\n' "$out_status" >&2
    exit 1
fi

# Test 5: GIT_RECURSE_SUMMARY=0 disables summary
out_disabled=$(GIT_RECURSE_SUMMARY=0 "$GIT_RECURSE" git pull)
if grep -q "All repositories up to date" <<< "$out_disabled"; then
    printf 'FAIL: GIT_RECURSE_SUMMARY=0 should disable summary\n%s\n' "$out_disabled" >&2
    exit 1
fi

# Test 6: Failed repository handling
mkdir -p "$tmpdir/workspace/r_fail"
git -C "$tmpdir/workspace/r_fail" init -q -b main

set +e
out_fail=$("$GIT_RECURSE" git pull 2>&1)
exit_code=$?
set -e

if [[ $exit_code -eq 0 ]]; then
    printf 'FAIL: git pull with unconfigured upstream should return non-zero\n' >&2
    exit 1
fi
if ! grep -q "Failed repositories (1):" <<< "$out_fail"; then
    printf 'FAIL: Expected "Failed repositories (1):"\n%s\n' "$out_fail" >&2
    exit 1
fi
if ! grep -q "./r_fail/ (exit 1)" <<< "$out_fail"; then
    printf 'FAIL: Expected "./r_fail/ (exit 1)" in failure list\n%s\n' "$out_fail" >&2
    exit 1
fi

printf 'OK: git-recurse pull summary tests passed\n'
