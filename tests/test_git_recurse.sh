#!/usr/bin/env bash
# Unit tests for .local/scripts/bin/git-recurse summary behavior

set -euo pipefail

# git-recurse colours its output with tput even when that output is captured,
# and the assertions below match plain text. Pin a terminal without colour
# support instead of inheriting whichever one runs the tests (a real xterm puts
# escape codes between a repository name and the ": " after it).
export TERM=dumb

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
# The implementation under test is the script. tests/test_mini_bashrc.sh sets
# GIT_RECURSE_UNDER_TEST to a wrapper around the git-recurse function of
# .local/Mini/.bashrc, which must pass every test that goes through the command alone;
# the tests of the script's own functions (the last ones) run only on the script.
GIT_RECURSE="${GIT_RECURSE_UNDER_TEST:-$ROOT/.local/scripts/bin/git-recurse}"

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

# --- more tests through the command alone --------------------------------------
# These hold for any implementation of git-recurse, so the copy in Mini's .bashrc
# is held to them too.

assert_has() { # NAME OUTPUT TEXT
    grep -qF -- "$3" <<< "$2" || fail "$1: expected \"$3\"" "$2"
}

assert_lacks() { # NAME OUTPUT TEXT
    if grep -qF -- "$3" <<< "$2"; then
        fail "$1: did not expect \"$3\"" "$2"
    fi
}

assert_same() { # NAME ACTUAL EXPECTED
    [[ "$2" == "$3" ]] || fail "$1: expected [$3], got [$2]"
}

# run_git_recurse ARGS...: sets cap_out (stdout and stderr) and cap_rc (exit status).
run_git_recurse() {
    cap_rc=0
    cap_out=$("$GIT_RECURSE" "$@" 2>&1) || cap_rc=$?
}

# Test 16: what the status summary says for each state of a repository, through
# the whole command. A stub git answers the porcelain probe of each repository
# from a file named after it and hands every other call to the real git, so the
# cases that are hard to build with real repositories (renames, ignored files,
# an unterminated last line) are covered for any implementation.
porcelain_ws="$tmpdir/porcelain-ws"
porcelain_dir="$tmpdir/porcelain"
mkdir -p "$porcelain_ws" "$porcelain_dir" "$tmpdir/porcelain-bin"
cat > "$tmpdir/porcelain-bin/git" <<'EOF'
#!/usr/bin/env bash
# Answer `git -C DIR status --porcelain --branch` from $PORCELAIN_DIR/<name of DIR>;
# every other call is the real git.
dir=""
probe=false
args=("$@")
for (( i = 0; i < ${#args[@]}; i++ )); do
    [[ "${args[i]}" == -C ]] && dir="${args[i+1]}"
    [[ "${args[i]}" == --porcelain* ]] && probe=true
done
if [[ $probe == true ]]; then
    cat "$PORCELAIN_DIR/$(basename "$dir")"
    exit 0
fi
exec "$REAL_GIT" "$@"
EOF
chmod +x "$tmpdir/porcelain-bin/git"

# name|porcelain output|what the summary says (nothing: the repository is clean)
porcelain_cases=(
    'in-sync|## main...origin/main\n|'
    'no-upstream|## main\n|'
    'no-commit-yet|## No commits yet on main\n|'
    'upstream-gone|## main...origin/main [gone]\n|'
    'detached|## HEAD (no branch)\n|'
    'branch-named-ahead|## ahead...origin/ahead\n|'
    'ignored-only|## main\n!! build/\n|'
    'empty-probe||'
    'ahead|## main...origin/main [ahead 3]\n|ahead 3'
    'behind|## main...origin/main [behind 14]\n|behind 14'
    'diverged|## main...o/main [ahead 2, behind 15]\n|ahead 2, behind 15'
    'modified-untracked|## main\n M a\n M b\n?? c\n|2 modified, 1 untracked'
    'staged-and-modified|## main\nMM a\nA  b\n|2 staged, 1 modified'
    'rename|## main\nR  old -> new\n|1 staged'
    'type-change|## main\n T a\nT  b\n|1 staged, 1 modified'
    'deletions|## main\n D a\nD  b\n|1 staged, 1 modified'
    'intent-to-add|## main\n A a\n|1 modified'
    'spaces-in-names|## main\n M my file.txt\n?? a b c\n|1 modified, 1 untracked'
    'unterminated-last-line|## main\n M a|1 modified'
    'every-conflict-code|## main\nDD a\nAU b\nUD c\nUA d\nDU e\nAA f\nUU g\n|7 conflicted'
    'order-of-the-parts|## main...o/main [ahead 1, behind 2]\nUU a\nM  b\n M c\n?? d\n|1 conflicted, 1 staged, 1 modified, 1 untracked, ahead 1, behind 2'
)
expected_porcelain=""
for entry in "${porcelain_cases[@]}"; do
    IFS='|' read -r case_name case_porcelain case_summary <<< "$entry"
    git init -q -b main "$porcelain_ws/$case_name"
    printf '%b' "$case_porcelain" > "$porcelain_dir/$case_name"
    if [[ -n "$case_summary" ]]; then
        expected_porcelain+="  ./$case_name/: $case_summary"$'\n'
    fi
done
expected_porcelain=$(LC_ALL=C sort <<< "${expected_porcelain%$'\n'}")
cd "$porcelain_ws"
out_porcelain=$(REAL_GIT="$real_git" PORCELAIN_DIR="$porcelain_dir" PATH="$tmpdir/porcelain-bin:$PATH" "$GIT_RECURSE" git status)
assert_same 'Test 16: every state is described as expected' "$(changes_listed "$out_porcelain")" "$expected_porcelain"
assert_has 'Test 16: every repository is processed' "$out_porcelain" "Done: ${#porcelain_cases[@]} ok, 0 failed (of ${#porcelain_cases[@]})"

# Test 17: help, and what a wrong option or value is answered with. All of these
# finish before the command looks for a repository.
cd "$tmpdir"
for flag in --help -h; do
    run_git_recurse "$flag"
    assert_same "Test 17: $flag exits 0" "$cap_rc" 0
    assert_has "Test 17: $flag shows the usage" "$cap_out" 'Usage: git-recurse [options] <command> [args...]'
    assert_has "Test 17: $flag shows the job limit" "$cap_out" 'default: 0, env: GIT_RECURSE_JOBS'
    assert_has "Test 17: $flag shows the retries" "$cap_out" 'default: 2, env: GIT_RECURSE_RETRIES'
    assert_has "Test 17: $flag shows the timeout" "$cap_out" 'default: 12, env: GIT_RECURSE_TIMEOUT'
    assert_has "Test 17: $flag shows the depth" "$cap_out" '(default: 3)'
done

expect_rejected() { # NAME TEXT ARGS...: exit status 1 and TEXT in what it printed
    local name="$1" text="$2"
    shift 2
    run_git_recurse "$@"
    assert_same "Test 17: $name exits 1" "$cap_rc" 1
    assert_has "Test 17: $name says why" "$cap_out" "$text"
}
expect_rejected 'no command' 'Error: missing command' 
expect_rejected 'only options' 'Error: missing command' -s -p
expect_rejected 'a blank command' 'Error: missing command' ' '
expect_rejected 'jobs that are not a number' 'Error: -j requires a non-negative integer (0 for unlimited).' -j many git status
expect_rejected 'negative jobs' 'Error: -j requires a non-negative integer' -j -1 git status
expect_rejected 'retries that are not a number' 'Error: -r requires a non-negative integer.' -r x git status
expect_rejected 'a fractional timeout' 'Error: -t requires a non-negative integer (0 for disabled).' -t 1.5 git status
expect_rejected 'depth zero' 'Error: -d requires a positive integer.' -d 0 git status
expect_rejected 'depth that is not a number' 'Error: -d requires a positive integer.' -d deep git status
expect_rejected 'an unknown option' 'Usage: git-recurse' -Z git status
expect_rejected 'an option without its value' 'Usage: git-recurse' -j
for assignment in GIT_RECURSE_JOBS=lots GIT_RECURSE_RETRIES=-1 GIT_RECURSE_TIMEOUT=soon; do
    cap_rc=0
    cap_out=$(env "$assignment" "$GIT_RECURSE" git status 2>&1) || cap_rc=$?
    assert_same "Test 17: $assignment exits 1" "$cap_rc" 1
    assert_has "Test 17: $assignment is refused like its option" "$cap_out" 'requires a non-negative integer'
done

# Test 18: no repositories, one folder only (-d 1), and the depth limit
empty_ws="$tmpdir/empty-ws"
mkdir -p "$empty_ws"
cd "$empty_ws"
run_git_recurse git status
assert_same 'Test 18: an empty folder is not a failure' "$cap_rc" 0
assert_has 'Test 18: the depth is announced' "$cap_out" 'depth: 3'
assert_has 'Test 18: no repositories' "$cap_out" 'No git repositories found'

cd "$clean_ws/ok1"
run_git_recurse -d 1 git rev-parse --is-inside-work-tree
assert_same 'Test 18: -d 1 runs the command here' "$cap_rc" 0
assert_has 'Test 18: -d 1 announces the depth' "$cap_out" 'depth: 1'
assert_has 'Test 18: -d 1 names the folder' "$cap_out" "Executing \"git rev-parse --is-inside-work-tree\" in $PWD"
assert_has 'Test 18: -d 1 prints what the command printed' "$cap_out" 'true'
run_git_recurse -d 1 false
assert_same 'Test 18: -d 1 returns the status of the command' "$cap_rc" 1
run_git_recurse -d 1 true
assert_same 'Test 18: -d 1 returns the status of the command (success)' "$cap_rc" 0

deep_ws="$tmpdir/deep-ws"
mkdir -p "$deep_ws/x"
new_repo "$deep_ws" top
new_repo "$deep_ws/x" inner
cd "$deep_ws"
run_git_recurse git rev-parse --git-dir
assert_has 'Test 18: the default depth reaches ./x/inner' "$cap_out" 'Done: 2 ok, 0 failed (of 2)'
run_git_recurse -d 2 git rev-parse --git-dir
assert_has 'Test 18: -d 2 stops above ./x/inner' "$cap_out" 'Done: 1 ok, 0 failed (of 1)'
assert_lacks 'Test 18: -d 2 stops above ./x/inner' "$cap_out" './x/inner'

# Test 19: how the work is shared out. The announcement, the numbering of the
# lines, the output of a command that prints nothing, and how many copies of a
# command run at once.
cd "$status_ws"
run_git_recurse git rev-parse --git-dir
assert_has 'Test 19: default announcement' "$cap_out" 'Launching "git rev-parse --git-dir" in 10 repo(s) in parallel...'
assert_lacks 'Test 19: default announcement' "$cap_out" 'concurrency'
run_git_recurse -j 3 git rev-parse --git-dir
assert_has 'Test 19: -j 3 announces the limit' "$cap_out" 'in 10 repo(s) in parallel (concurrency: 3)...'
run_git_recurse -j 0 git rev-parse --git-dir
assert_lacks 'Test 19: -j 0 means all at once' "$cap_out" 'concurrency'
run_git_recurse -j 99 git rev-parse --git-dir
assert_lacks 'Test 19: more jobs than repositories means all at once' "$cap_out" 'concurrency'
run_git_recurse -s git rev-parse --git-dir
assert_lacks 'Test 19: -s announces no launch' "$cap_out" 'Launching'
cap_rc=0
cap_out=$(GIT_RECURSE_JOBS=4 "$GIT_RECURSE" git rev-parse --git-dir 2>&1) || cap_rc=$?
assert_has 'Test 19: GIT_RECURSE_JOBS sets the limit' "$cap_out" '(concurrency: 4)'

for mode in "" -s; do
    run_git_recurse ${mode:+"$mode"} git rev-parse --git-dir
    numbers=$(sed -n 's|^\[ *\([0-9]*\)/10\] .*|\1|p' <<< "$cap_out" | tr '\n' ' ')
    assert_same "Test 19: ${mode:-parallel} numbers its lines in order" "$numbers" '1 2 3 4 5 6 7 8 9 10 '
    assert_has "Test 19: ${mode:-parallel} pads the number to the width of the total" "$cap_out" '[ 1/10] '
done

run_git_recurse true
assert_same 'Test 19: a command without output says so for every repository' \
    "$(grep -c '^    (no output)$' <<< "$cap_out")" 10

conc_ws="$tmpdir/conc-ws"
mkdir -p "$conc_ws"
for n in 1 2 3 4 5 6; do
    git init -q -b main "$conc_ws/c$n"
done
occupy="$tmpdir/occupy.sh"
cat > "$occupy" <<'EOF'
#!/usr/bin/env bash
# Register, wait until OCCUPY_WAIT_FOR copies are registered (at most 5 s), write down how
# many there are, and leave. The largest number written is how many ran at the same time.
touch "$OCCUPY_DIR/$$"
for (( n = 0; n < 50; n++ )); do
    (( $(ls "$OCCUPY_DIR" | wc -l) >= OCCUPY_WAIT_FOR )) && break
    sleep 0.1
done
printf '%d\n' "$(ls "$OCCUPY_DIR" | wc -l)" >> "$OCCUPY_LOG"
sleep 0.2
rm -f "$OCCUPY_DIR/$$"
EOF
# max_concurrent WAIT_FOR ARGS...: the most copies of $occupy that ran at once.
max_concurrent() {
    local wait_for="$1"
    shift
    rm -rf "$tmpdir/occupy"
    mkdir -p "$tmpdir/occupy"
    : > "$tmpdir/occupy.log"
    OCCUPY_DIR="$tmpdir/occupy" OCCUPY_LOG="$tmpdir/occupy.log" OCCUPY_WAIT_FOR="$wait_for" \
        "$GIT_RECURSE" "$@" bash "$occupy" > "$tmpdir/occupy.out" 2>&1 \
        || fail "Test 19: the command failed" "$(cat "$tmpdir/occupy.out")"
    sort -n "$tmpdir/occupy.log" | tail -n 1
}
cd "$conc_ws"
assert_same 'Test 19: -j 2 runs two at once and no more' "$(max_concurrent 2 -j 2)" 2
assert_same 'Test 19: by default every repository runs at once' "$(max_concurrent 6)" 6
assert_same 'Test 19: -s never overlaps' "$(max_concurrent 1 -s)" 1

# Test 20: a failure that looks like the network is retried; any other is not
retry_ws="$tmpdir/retry-ws"
mkdir -p "$retry_ws" "$tmpdir/flaky-bin"
new_repo "$retry_ws" only
cat > "$tmpdir/flaky-bin/git" <<'EOF'
#!/usr/bin/env bash
# `git fetch` fails FLAKY_FAILURES times (counted in FLAKY_COUNT) with the message
# FLAKY_MESSAGE and the status FLAKY_EXIT, then works; everything else is the real git.
if [[ "$1" == fetch ]]; then
    n=$(cat "$FLAKY_COUNT" 2>/dev/null || echo 0)
    echo $((n + 1)) > "$FLAKY_COUNT"
    if (( n < FLAKY_FAILURES )); then
        echo "$FLAKY_MESSAGE" >&2
        exit "${FLAKY_EXIT:-128}"
    fi
    exit 0
fi
exec "$REAL_GIT" "$@"
EOF
chmod +x "$tmpdir/flaky-bin/git"

# run_flaky FAILURES MESSAGE EXIT ARGS...: git-recurse ARGS git fetch against the stub;
# sets cap_out, cap_rc and flaky_calls (how many times git fetch was started).
run_flaky() {
    local failures="$1" message="$2" exit_code="$3"
    shift 3
    rm -f "$tmpdir/flaky.count"
    cap_rc=0
    cap_out=$(cd "$retry_ws" && REAL_GIT="$real_git" FLAKY_COUNT="$tmpdir/flaky.count" \
        FLAKY_FAILURES="$failures" FLAKY_MESSAGE="$message" FLAKY_EXIT="$exit_code" \
        PATH="$tmpdir/flaky-bin:$PATH" "$GIT_RECURSE" "$@" git fetch 2>&1) || cap_rc=$?
    flaky_calls=$(cat "$tmpdir/flaky.count" 2>/dev/null || echo 0)
}

run_flaky 1 "fatal: unable to access 'https://example.invalid/r.git/': Could not resolve host: example.invalid" 128 -r 2
assert_same 'Test 20: a network failure that passes is a success' "$cap_rc" 0
assert_has 'Test 20: the retry is noted' "$cap_out" '[retry 1/2 succeeded]'
assert_has 'Test 20: the repository is listed as passed' "$cap_out" '✓ ./only/'
assert_same 'Test 20: it took two attempts' "$flaky_calls" 2

run_flaky 99 'error: RPC failed; HTTP 502 curl 22 The requested URL returned error: 502' 128 -r 1
assert_same 'Test 20: a network failure that stays is a failure' "$cap_rc" 1
assert_has 'Test 20: the retries are noted' "$cap_out" '[failed after 1 retries]'
assert_has 'Test 20: the status is reported' "$cap_out" '✗ ./only/ (exit 128)'
assert_has 'Test 20: the failure is listed' "$cap_out" 'Failed repositories (1):'
assert_same 'Test 20: it stopped after the retries it was given' "$flaky_calls" 2

run_flaky 99 'fatal: bad object HEAD' 1 -r 2
assert_same 'Test 20: another failure is a failure' "$cap_rc" 1
assert_same 'Test 20: another failure is not retried' "$flaky_calls" 1
assert_lacks 'Test 20: another failure is not retried' "$cap_out" 'retries'
assert_has 'Test 20: another failure keeps its status' "$cap_out" '✗ ./only/ (exit 1)'

run_flaky 99 'Could not resolve host: x' 128 -r 0
assert_same 'Test 20: -r 0 never retries' "$flaky_calls" 1
cap_rc=0
rm -f "$tmpdir/flaky.count"
cap_out=$(cd "$retry_ws" && REAL_GIT="$real_git" FLAKY_COUNT="$tmpdir/flaky.count" FLAKY_FAILURES=99 \
    FLAKY_MESSAGE='Could not resolve host: x' GIT_RECURSE_RETRIES=0 PATH="$tmpdir/flaky-bin:$PATH" \
    "$GIT_RECURSE" git fetch 2>&1) || cap_rc=$?
assert_same 'Test 20: GIT_RECURSE_RETRIES=0 never retries' "$(cat "$tmpdir/flaky.count")" 1

# Test 21: the time limit. A command that stalls is stopped at the limit, a status
# probe that hangs fails its repository, and without a timeout command everything
# still works (macOS has none by default).
time_limit_cmd=$(command -v timeout || command -v gtimeout || true)
if [[ -n "$time_limit_cmd" ]]; then
    mkdir -p "$tmpdir/stall-bin"
    cat > "$tmpdir/stall-bin/git" <<'EOF'
#!/usr/bin/env bash
# With STALL=fetch `git fetch` hangs; with STALL=probe the porcelain probe hangs;
# everything else is the real git.
case "$STALL:$*" in
    fetch:fetch*) exec sleep 30 ;;
    probe:*--porcelain*) exec sleep 30 ;;
esac
exec "$REAL_GIT" "$@"
EOF
    chmod +x "$tmpdir/stall-bin/git"
    cd "$retry_ws"

    started=$SECONDS
    cap_rc=0
    cap_out=$(STALL=fetch REAL_GIT="$real_git" PATH="$tmpdir/stall-bin:$PATH" "$GIT_RECURSE" -t 1 -r 0 git fetch 2>&1) || cap_rc=$?
    assert_same 'Test 21: a stalled command fails' "$cap_rc" 1
    assert_has 'Test 21: a stalled command is said to have timed out' "$cap_out" 'Operation timed out after 1s (stalled connection killed)'
    assert_has 'Test 21: a stalled command has the status of the timeout' "$cap_out" '✗ ./only/ (exit 124)'
    if (( SECONDS - started > 10 )); then
        fail "Test 21: a stalled command was not stopped at the limit ($((SECONDS - started))s)"
    fi

    cap_rc=0
    cap_out=$(STALL=fetch REAL_GIT="$real_git" GIT_RECURSE_TIMEOUT=1 PATH="$tmpdir/stall-bin:$PATH" "$GIT_RECURSE" -r 0 git fetch 2>&1) || cap_rc=$?
    assert_has 'Test 21: GIT_RECURSE_TIMEOUT sets the limit' "$cap_out" 'Operation timed out after 1s'

    started=$SECONDS
    cap_rc=0
    cap_out=$(STALL=probe REAL_GIT="$real_git" PATH="$tmpdir/stall-bin:$PATH" "$GIT_RECURSE" -t 1 git status 2>&1) || cap_rc=$?
    assert_same 'Test 21: a hanging status probe fails the repository' "$cap_rc" 1
    assert_has 'Test 21: a hanging status probe says why' "$cap_out" 'could not read the repository state for the summary (exit 124)'
    assert_lacks 'Test 21: a repository without a readable state is not clean' "$cap_out" 'All repositories clean'
    if (( SECONDS - started > 10 )); then
        fail "Test 21: a hanging status probe was not stopped at the limit ($((SECONDS - started))s)"
    fi
fi

mkdir -p "$tmpdir/notimeout-bin"
for tool in bash cat find git grep head mkfifo mktemp mv rm sed sleep sort tail tput; do
    tool_path=$(command -v "$tool" 2>/dev/null || true)
    if [[ "$tool_path" == /* ]]; then
        ln -sf "$tool_path" "$tmpdir/notimeout-bin/$tool"
    fi
done
cd "$status_ws"
for limit in 5 0; do
    cap_rc=0
    cap_out=$(PATH="$tmpdir/notimeout-bin" "$GIT_RECURSE" -t "$limit" git status 2>&1) || cap_rc=$?
    assert_same "Test 21: without timeout and with -t $limit the command works" "$cap_rc" 0
    assert_same "Test 21: without timeout and with -t $limit the summary is complete" "$(changes_listed "$cap_out")" "$expected_changes"
    assert_lacks "Test 21: without timeout and with -t $limit nothing is missing" "$cap_out" 'not found'
done

# Test 22: -S and GIT_RECURSE_SUMMARY=1 give any command the pull summary, and
# when something failed and nothing was updated it says so
cd "$clean_ws"
run_git_recurse -S git rev-parse --git-dir
assert_has 'Test 22: -S gives any command a summary' "$cap_out" 'All repositories up to date.'
cap_out=$(GIT_RECURSE_SUMMARY=1 "$GIT_RECURSE" git rev-parse --git-dir 2>&1)
assert_has 'Test 22: GIT_RECURSE_SUMMARY=1 gives any command a summary' "$cap_out" 'All repositories up to date.'
cd "$fail_ws"
run_git_recurse -S git rev-parse --git-dir
assert_same 'Test 22: a repository that fails fails the command' "$cap_rc" 1
assert_has 'Test 22: nothing updated is said when something failed' "$cap_out" 'No repositories updated.'
assert_lacks 'Test 22: nothing updated is not "up to date" when something failed' "$cap_out" 'All repositories up to date.'

if [[ -n "${GIT_RECURSE_UNDER_TEST:-}" ]]; then
    printf 'OK: git-recurse tests through the command passed (%s)\n' "$GIT_RECURSE_UNDER_TEST"
    exit 0
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
