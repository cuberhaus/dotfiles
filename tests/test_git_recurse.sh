#!/usr/bin/env bash
# Unit tests for .local/scripts/bin/git-recurse summary behavior

set -euo pipefail

# git-recurse colours its output with tput even when that output is captured,
# and the assertions below match plain text. Pin a terminal without colour
# support instead of inheriting whichever one runs the tests (a real xterm puts
# escape codes between a repository name and the ": " after it).
export TERM=dumb

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

# --- git status summary ------------------------------------------------------

fail() {
    printf 'FAIL: %s\n' "$1" >&2
    if [[ $# -gt 1 ]]; then
        printf '%s\n' "$2" >&2
    fi
    exit 1
}

# new_repo WORKSPACE NAME: a clone of a fresh bare remote with one pushed commit.
new_repo() {
    local ws="$1" name="$2" remote
    remote="$tmpdir/remotes/$(basename "$ws")-$name"
    git init --bare -b main "$remote" >/dev/null 2>&1
    git clone "$remote" "$ws/$name" >/dev/null 2>&1
    git -C "$ws/$name" config user.email "test@example.com"
    git -C "$ws/$name" config user.name "test"
    git -C "$ws/$name" config commit.gpgsign false
    echo "init $name" > "$ws/$name/file.txt"
    echo "init $name" > "$ws/$name/other.txt"
    git -C "$ws/$name" add .
    git -C "$ws/$name" commit -m "init $name" >/dev/null 2>&1
    git -C "$ws/$name" push -u origin main >/dev/null 2>&1
}

# push_remote_commit WORKSPACE NAME [COUNT]: COUNT (default 1) new commits on the
# remote, fetched (not merged) by the clone.
push_remote_commit() {
    local ws="$1" name="$2" count="${3:-1}" other i
    other=$(mktemp -d)
    git clone "$tmpdir/remotes/$(basename "$ws")-$name" "$other" >/dev/null 2>&1
    git -C "$other" config user.email "test@example.com"
    git -C "$other" config user.name "test"
    git -C "$other" config commit.gpgsign false
    for (( i = 1; i <= count; i++ )); do
        echo "remote change $i" >> "$other/file.txt"
        git -C "$other" commit -qam "remote change $i"
    done
    git -C "$other" push -q origin main
    rm -rf "$other"
    git -C "$ws/$name" fetch -q
}

# changes_listed OUTPUT: the lines under the "Repositories with changes" heading,
# sorted (the order the repositories are found in is the file system's).
changes_listed() {
    sed -n '/^Repositories with changes/,$p' <<< "$1" | sed '1d;/^$/,$d' | LC_ALL=C sort
}

# strip_ansi: remove the escape codes tput writes (colour, dim, reset).
strip_ansi() {
    sed -e $'s/\033(B//g' -e $'s/\033\\[[0-9;]*m//g'
}

status_ws="$tmpdir/status-ws"
mkdir -p "$status_ws"
for name in clean modified staged untracked conflict ahead behind mixed; do
    new_repo "$status_ws" "$name"
done

# modified: two tracked files edited in the working tree.
echo more >> "$status_ws/modified/file.txt"
echo more >> "$status_ws/modified/other.txt"

# staged: a new file added, and a tracked file staged and then edited again (MM),
# which counts as staged and as modified.
echo new > "$status_ws/staged/new.txt"
git -C "$status_ws/staged" add new.txt
echo more >> "$status_ws/staged/file.txt"
git -C "$status_ws/staged" add file.txt
echo again >> "$status_ws/staged/file.txt"

# untracked: two files and a directory (git lists a new directory once).
echo a > "$status_ws/untracked/a.txt"
echo b > "$status_ws/untracked/b.txt"
mkdir "$status_ws/untracked/dir"
echo c > "$status_ws/untracked/dir/c.txt"

# conflict: both sides changed file.txt, so the merge stops on an unmerged path.
git -C "$status_ws/conflict" checkout -q -b side
echo side > "$status_ws/conflict/file.txt"
git -C "$status_ws/conflict" commit -qam side
git -C "$status_ws/conflict" checkout -q main
echo main > "$status_ws/conflict/file.txt"
git -C "$status_ws/conflict" commit -qam main
git -C "$status_ws/conflict" push -q origin main
GIT_MERGE_AUTOEDIT=no git -C "$status_ws/conflict" merge side >/dev/null 2>&1 || true

# ahead: eleven local commits that were never pushed (a two-digit count).
for i in 1 2 3 4 5 6 7 8 9 10 11; do
    echo "$i" >> "$status_ws/ahead/file.txt"
    git -C "$status_ws/ahead" commit -qam "local $i"
done

# behind: the remote has twelve commits the clone fetched but did not merge
# (a two-digit count).
push_remote_commit "$status_ws" behind 12

# mixed: a modified file, an untracked file, one unpushed commit and one new
# remote commit (ahead and behind at once).
echo local >> "$status_ws/mixed/other.txt"
git -C "$status_ws/mixed" commit -qam local
push_remote_commit "$status_ws" mixed
echo more >> "$status_ws/mixed/file.txt"
echo u > "$status_ws/mixed/u.txt"

# Clean in every sense, so they must never be listed: a repository without a
# remote, and one without a single commit.
git init -q -b main "$status_ws/local-only"
git -C "$status_ws/local-only" -c user.email=test@example.com -c user.name=test \
    -c commit.gpgsign=false commit -q --allow-empty -m init
git init -q -b main "$status_ws/unborn"

expected_changes=$(cat <<'EOF'
  ./ahead/: ahead 11
  ./behind/: behind 12
  ./conflict/: 1 conflicted
  ./mixed/: 1 modified, 1 untracked, ahead 1, behind 1
  ./modified/: 2 modified
  ./staged/: 2 staged, 1 modified
  ./untracked/: 3 untracked
EOF
)

cd "$status_ws"

# Test 7: git status lists exactly the repositories that are not clean, with what differs
out_changes=$("$GIT_RECURSE" git status)
if ! grep -q "^Repositories with changes (7):" <<< "$out_changes"; then
    fail 'Expected "Repositories with changes (7):"' "$out_changes"
fi
actual_changes=$(changes_listed "$out_changes")
if [[ "$actual_changes" != "$expected_changes" ]]; then
    fail "Unexpected repositories with changes. Expected:
$expected_changes
Got:
$actual_changes" "$out_changes"
fi
if ! grep -q "Done: 10 ok, 0 failed (of 10)" <<< "$out_changes"; then
    fail 'Expected Done line for 10 repositories' "$out_changes"
fi
if grep -q "Updated repositories" <<< "$out_changes"; then
    fail 'git status must not print the pull summary' "$out_changes"
fi

# Test 8: sequential mode lists the same repositories
out_changes_seq=$("$GIT_RECURSE" -s git status)
if [[ "$(changes_listed "$out_changes_seq")" != "$expected_changes" ]]; then
    fail 'Sequential mode: unexpected repositories with changes' "$out_changes_seq"
fi

# Test 9: the summary does not depend on the format the command prints
out_short=$("$GIT_RECURSE" git status --short)
if ! grep -q '?? a.txt' <<< "$out_short"; then
    fail 'Expected short-format status output to be shown' "$out_short"
fi
if [[ "$(changes_listed "$out_short")" != "$expected_changes" ]]; then
    fail 'git status --short: unexpected repositories with changes' "$out_short"
fi

# Test 10: a terminal with colour support adds escape codes and nothing else
out_color=$(TERM=xterm-256color "$GIT_RECURSE" git status)
if [[ "$(changes_listed "$(strip_ansi <<< "$out_color")")" != "$expected_changes" ]]; then
    fail 'Colour terminal: unexpected repositories with changes' "$out_color"
fi

# Test 11: GIT_RECURSE_SUMMARY=0 turns the status summary off; -S turns it back on
out_off=$(GIT_RECURSE_SUMMARY=0 "$GIT_RECURSE" git status)
if grep -qE "Repositories with changes|All repositories clean" <<< "$out_off"; then
    fail 'GIT_RECURSE_SUMMARY=0 should disable the status summary' "$out_off"
fi
out_forced=$(GIT_RECURSE_SUMMARY=0 "$GIT_RECURSE" -S git status)
if [[ "$(changes_listed "$out_forced")" != "$expected_changes" ]]; then
    fail '-S should show the status summary despite GIT_RECURSE_SUMMARY=0' "$out_forced"
fi

# Test 12: other commands get neither summary (a command that also works in the
# repository without commits)
out_other=$("$GIT_RECURSE" git rev-parse --git-dir)
if grep -qE "Repositories with changes|All repositories clean|Updated repositories" <<< "$out_other"; then
    fail 'git rev-parse should not display a summary' "$out_other"
fi

# Test 13: when nothing differs, say so
clean_ws="$tmpdir/clean-ws"
mkdir -p "$clean_ws"
new_repo "$clean_ws" ok1
new_repo "$clean_ws" ok2
git init -q -b main "$clean_ws/unborn"
cd "$clean_ws"
out_clean=$("$GIT_RECURSE" git status)
if ! grep -q "All repositories clean\." <<< "$out_clean"; then
    fail 'Expected "All repositories clean."' "$out_clean"
fi
if grep -q "Repositories with changes" <<< "$out_clean"; then
    fail 'A clean workspace must not list repositories with changes' "$out_clean"
fi

# Test 14: a repository that fails is reported as failed, next to the ones with changes
fail_ws="$tmpdir/fail-ws"
mkdir -p "$fail_ws"
new_repo "$fail_ws" dirty
echo more >> "$fail_ws/dirty/file.txt"
mkdir -p "$fail_ws/broken/.git"
cd "$fail_ws"
set +e
out_broken=$("$GIT_RECURSE" git status 2>&1)
broken_exit=$?
set -e
if [[ $broken_exit -eq 0 ]]; then
    fail 'A repository whose git status fails should make git-recurse fail' "$out_broken"
fi
if [[ "$(changes_listed "$out_broken")" != "  ./dirty/: 1 modified" ]]; then
    fail 'Expected only ./dirty/ under the repositories with changes' "$out_broken"
fi
if ! grep -q "./broken/ (exit 128)" <<< "$out_broken"; then
    fail 'Expected ./broken/ in the failed repositories' "$out_broken"
fi

# Test 15: if the summary cannot be read for a repository, that repository fails
# instead of being silently left out (and so reported as clean)
mkdir -p "$tmpdir/stub-bin"
real_git=$(command -v git)
cat > "$tmpdir/stub-bin/git" <<EOF
#!/usr/bin/env bash
# Refuse only the summary probe; every other call goes to the real git.
for arg in "\$@"; do
    if [[ "\$arg" == --porcelain* ]]; then
        echo "stub git: probe refused" >&2
        exit 7
    fi
done
exec "$real_git" "\$@"
EOF
chmod +x "$tmpdir/stub-bin/git"
cd "$clean_ws"
set +e
out_probe=$(PATH="$tmpdir/stub-bin:$PATH" "$GIT_RECURSE" git status 2>&1)
probe_exit=$?
set -e
if [[ $probe_exit -eq 0 ]]; then
    fail 'A failing summary probe should make git-recurse fail' "$out_probe"
fi
if ! grep -q "could not read the repository state for the summary (exit 7)" <<< "$out_probe"; then
    fail 'Expected the reason in the output of the repository' "$out_probe"
fi
if ! grep -q "Failed repositories (3):" <<< "$out_probe"; then
    fail 'Expected every repository in the failed list' "$out_probe"
fi
if grep -q "All repositories clean" <<< "$out_probe"; then
    fail 'Repositories without a readable state must not be reported as clean' "$out_probe"
fi

# --- the parser on its own ---------------------------------------------------
# describe_status and summarize_repo_status are loaded from the script itself, so
# the porcelain cases that are hard to build with a real repository (renames,
# ignored files, an unterminated last line) are covered as well.
eval "$(sed -n '/^function describe_status() {$/,/^}$/p;/^function summarize_repo_status() {$/,/^}$/p' "$GIT_RECURSE")"
if ! declare -F describe_status >/dev/null || ! declare -F summarize_repo_status >/dev/null; then
    fail 'Could not load describe_status and summarize_repo_status from git-recurse'
fi

# expect_describe NAME PORCELAIN_TEXT EXPECTED: EXPECTED is "<exit status>|<output>".
expect_describe() {
    local probe="$tmpdir/probe.txt" got out rc=0
    printf '%b' "$2" > "$probe"
    out=$(describe_status "$probe") || rc=$?
    got="$rc|$out"
    if [[ "$got" != "$3" ]]; then
        fail "describe_status ($1): expected [$3], got [$got]"
    fi
}

# Test 16: what the parser says for each state of a repository
expect_describe 'in sync'              '## main...origin/main\n'                    '1|'
expect_describe 'no upstream'          '## main\n'                                  '1|'
expect_describe 'no commit yet'        '## No commits yet on main\n'                '1|'
expect_describe 'upstream gone'        '## main...origin/main [gone]\n'             '1|'
expect_describe 'branch named ahead'   '## ahead...origin/ahead\n'                  '1|'
expect_describe 'ignored files'        '## main\n!! build/\n'                       '1|'
expect_describe 'ahead'                '## main...origin/main [ahead 3]\n'          '0|ahead 3'
expect_describe 'behind'               '## main...origin/main [behind 14]\n'        '0|behind 14'
expect_describe 'diverged'             '## main...o/main [ahead 2, behind 15]\n'    '0|ahead 2, behind 15'
expect_describe 'modified, untracked'  '## main\n M a\n M b\n?? c\n'                '0|2 modified, 1 untracked'
expect_describe 'staged and modified'  '## main\nMM a\nA  b\n'                       '0|2 staged, 1 modified'
expect_describe 'rename'               '## main\nR  old -> new\n'                    '0|1 staged'
expect_describe 'deletions'            '## main\n D a\nD  b\n'                       '0|1 staged, 1 modified'
expect_describe 'file names with spaces' '## main\n M my file.txt\n?? a b c\n'      '0|1 modified, 1 untracked'
expect_describe 'unterminated last line' '## main\n M a'                            '0|1 modified'
expect_describe 'every conflict code'  '## main\nDD a\nAU b\nUD c\nUA d\nDU e\nAA f\nUU g\n' '0|7 conflicted'
expect_describe 'order of the parts'   '## main...o/main [ahead 1, behind 2]\nUU a\nM  b\n M c\n?? d\n' \
    '0|1 conflicted, 1 staged, 1 modified, 1 untracked, ahead 1, behind 2'

# Test 17: a probe file that cannot be read is a failure (2), not a clean repository
rc=0
describe_status "$tmpdir/does-not-exist" >/dev/null || rc=$?
if [[ $rc -ne 2 ]]; then
    fail "describe_status on a missing file should return 2, got $rc"
fi

# Test 18: summarize_repo_status keeps "clean" (parser answer 1) apart from every
# real failure, with the probe replaced by a stub
unit_out="$tmpdir/unit.out"

capture_status_probe() { printf '%b' '## main...origin/main\n' > "$2"; }
rc=0; summarize_repo_status . "$unit_out" 0 || rc=$?
if [[ $rc -ne 0 || -s "$unit_out.summary" ]]; then
    fail "A clean repository should succeed with an empty summary (exit $rc)"
fi

capture_status_probe() { printf '%b' '## main\n M a\n' > "$2"; }
rc=0; summarize_repo_status . "$unit_out" 0 || rc=$?
if [[ $rc -ne 0 || "$(cat "$unit_out.summary")" != "1 modified" ]]; then
    fail "A repository with changes should succeed with its description (exit $rc)"
fi

capture_status_probe() { return 7; }
rc=0; summarize_repo_status . "$unit_out" 0 || rc=$?
if [[ $rc -ne 7 ]]; then
    fail "A failing probe should fail with its own status, got $rc"
fi

capture_status_probe() { return 0; }
rm -f "$unit_out.porcelain"
rc=0; summarize_repo_status . "$unit_out" 0 || rc=$?
if [[ $rc -ne 2 ]]; then
    fail "A probe that left nothing to read should fail, not look clean (got $rc)"
fi

# Test 19: the real probe, with and without a timeout command (macOS has none by
# default, and -t 0 turns the timeout off)
eval "$(sed -n '/^function capture_status_probe() {$/,/^}$/p' "$GIT_RECURSE")"
if ! declare -F capture_status_probe >/dev/null; then
    fail 'Could not load capture_status_probe from git-recurse'
fi
timeout_bin=$(command -v timeout || command -v gtimeout || true)
probe_file="$tmpdir/probe-real.txt"
for variant in "none 12" "none 0" "${timeout_bin:-none} 12" "${timeout_bin:-none} 0"; do
    read -r TIMEOUT_BIN timeout_secs <<< "$variant"
    if [[ "$TIMEOUT_BIN" == none ]]; then
        TIMEOUT_BIN=""
    fi
    rm -f "$probe_file"
    rc=0; capture_status_probe "$status_ws/modified" "$probe_file" "$timeout_secs" || rc=$?
    if [[ $rc -ne 0 ]] || ! grep -qx ' M file.txt' "$probe_file" || ! grep -qx '## main...origin/main' "$probe_file"; then
        fail "capture_status_probe (timeout command: ${TIMEOUT_BIN:-none}, limit: $timeout_secs) exit $rc" "$(cat "$probe_file" 2>/dev/null)"
    fi
done

# Test 20: a probe that hangs is stopped by the timeout instead of hanging the job
if [[ -n "$timeout_bin" ]]; then
    TIMEOUT_BIN="$timeout_bin"
    mkdir -p "$tmpdir/hang-bin"
    printf '#!/bin/sh\nexec sleep 30\n' > "$tmpdir/hang-bin/git"
    chmod +x "$tmpdir/hang-bin/git"
    started=$SECONDS
    rc=0; PATH="$tmpdir/hang-bin:$PATH" capture_status_probe "$status_ws/modified" "$probe_file" 1 || rc=$?
    if [[ $rc -ne 124 || $((SECONDS - started)) -gt 10 ]]; then
        fail "A hanging probe should be stopped by the timeout (exit $rc after $((SECONDS - started))s)"
    fi
fi

printf 'OK: git-recurse pull and status summary tests passed\n'
