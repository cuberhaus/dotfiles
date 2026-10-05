#!/usr/bin/env bash
# Tests for the commands that .local/Mini/.bashrc defines on its own: clone-all, clone-team,
# git-recurse and git-ahead (and the gr, gah and status shortcuts that call the last two).
#
# Mini is one portable file, so it carries its own copies of these scripts and of the helpers
# clone-all and clone-team share (.local/scripts/lib/git-repo-defaults.sh). These tests run the
# copies in a real interactive Bash against real git repositories (local bare repositories stand
# in for GitHub) and a stub `gh`, and compare them with the originals so the copies cannot drift
# apart unnoticed. git-recurse is also run through the whole black-box suite that the original
# passes (tests/test_git_recurse.sh), which is what keeps its many small behaviours identical.
#
# Every case runs in a clean `env -i` shell with a throwaway HOME and a PATH that holds nothing
# but symlinks to the few tools the commands need plus the stub, so the developer's git settings
# and a real `gh` can never leak in, and nothing outside one temporary folder is touched.

# The scripts handed to the child shells below are single-quoted on purpose: their variables must
# be expanded by the child Bash, not by this one.
# shellcheck disable=SC2016

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
mini_rc="$repo_root/.local/Mini/.bashrc"
lib="$repo_root/.local/scripts/lib/git-repo-defaults.sh"
case_dir="$(mktemp -d)"
trap 'rm -rf "$case_dir"' EXIT

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

assert_eq() { # actual expected message
    [[ $1 == "$2" ]] || fail "$3: expected $(printf '%q' "$2"), got $(printf '%q' "$1")"
}

assert_contains() { # haystack needle message
    [[ $1 == *"$2"* ]] || fail "$3: expected $(printf '%q' "$2") in $(printf '%q' "$1")"
}

assert_lacks() { # haystack needle message
    [[ $1 != *"$2"* ]] || fail "$3: did not expect $(printf '%q' "$2") in $(printf '%q' "$1")"
}

# assert_rc EXPECTED MESSAGE: check the exit status that run_mini stored in $rc.
assert_rc() {
    [[ $rc -eq $1 ]] || fail "$2: exit status $rc, expected $1 (stdout: $(printf '%q' "$out"); stderr: $(printf '%q' "$err"))"
}

# --- fixtures -----------------------------------------------------------------------------------

# A PATH of symlinks to the tools the commands and Mini's startup use, and nothing else.
tools="$case_dir/tools"
mkdir -p "$tools"
for tool in awk basename bash cat chmod cut dirname env find getconf git grep head id ls mkdir mkfifo mktemp mv nproc rm sed sleep sort tail tr uname wc xargs; do
    found="$(command -v "$tool" 2>/dev/null || true)"
    [[ $found == /* ]] && ln -s "$found" "$tools/$tool"
done

# env searches the restricted PATH below, so a time limit has to be given as an absolute path.
timeout_bin="$(command -v timeout 2>/dev/null || true)"

stub="$case_dir/stub"
stub_bin="$case_dir/stub-bin"
mkdir -p "$stub/remotes" "$stub/repos" "$stub/team-repos" "$stub/clone-fail" "$stub_bin"

# The stub gh answers from files under $STUB_DIR and never touches the network. It logs every call.
cat >"$stub_bin/gh" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >>"$STUB_DIR/gh.log"
case "$*" in
    "repo list "*)
        cat "$STUB_DIR/repos/$3"
        ;;
    "repo clone "*)
        key=${3//\//__}
        if [ -f "$STUB_DIR/clone-fail/$key" ]; then
            left=$(cat "$STUB_DIR/clone-fail/$key")
            if [ "$left" -gt 0 ]; then
                echo $((left - 1)) >"$STUB_DIR/clone-fail/$key"
                echo "fatal: simulated clone failure for $3" >&2
                exit 1
            fi
        fi
        git clone -q "$STUB_DIR/remotes/$3" "$4"
        ;;
    "api user "*)
        cat "$STUB_DIR/login"
        ;;
    "api --paginate /user/teams "*)
        cat "$STUB_DIR/teams"
        ;;
    "api --paginate /orgs/"*)
        path=${3#/orgs/}
        org=${path%%/*}
        rest=${path#*/teams/}
        cat "$STUB_DIR/team-repos/${org}__${rest%%/*}"
        ;;
    *)
        echo "stub gh: unexpected call: $*" >&2
        exit 9
        ;;
esac
EOF
chmod +x "$stub_bin/gh"

git_() {
    env HOME="$case_dir/home" XDG_CONFIG_HOME="$case_dir/home/.config" \
        GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_SYSTEM=/dev/null \
        GIT_AUTHOR_NAME=test GIT_AUTHOR_EMAIL=test@example.invalid \
        GIT_COMMITTER_NAME=test GIT_COMMITTER_EMAIL=test@example.invalid \
        git -c advice.ignoredHook=false "$@"
}

# make_remote OWNER/NAME [PATH ...]: a bare repository standing in for GitHub, with one commit that
# holds .githooks/pre-commit and each PATH. Files under node_modules/.bin and .local/scripts are
# fake programs that leave a marker file, so a test can see that the command ran them. The default
# branch is main; REMOTE_BRANCH=name makes it another one.
make_remote() {
    local nwo=$1 work file branch=${REMOTE_BRANCH:-main}
    shift
    git_ init -q --bare -b "$branch" "$stub/remotes/$nwo"
    work="$(mktemp -d "$case_dir/work.XXXXXX")"
    git_ clone -q "$stub/remotes/$nwo" "$work" 2>/dev/null
    mkdir -p "$work/.githooks"
    printf 'hook\n' >"$work/.githooks/pre-commit"
    printf 'one\n' >"$work/file"
    for file in "$@"; do
        mkdir -p "$work/$(dirname "$file")"
        case $file in
            node_modules/.bin/lefthook) printf '#!/bin/sh\n: >lefthook-ran\n' >"$work/$file" ;;
            .local/scripts/apply-skip-worktree) printf '#!/bin/sh\n: >"$1/skip-worktree-ran"\n' >"$work/$file" ;;
            *) printf 'one\n' >"$work/$file" ;;
        esac
        case $file in node_modules/.bin/* | .local/scripts/*) chmod +x "$work/$file" ;; esac
    done
    git_ -C "$work" add -A
    git_ -C "$work" commit -qm one
    git_ -C "$work" push -q origin "$branch" 2>/dev/null
    rm -rf "$work"
}

# push_branch OWNER/NAME BRANCH COMMITS: a branch with COMMITS commits on top of the remote's default
# branch (none: a branch that is already merged).
push_branch() {
    local nwo=$1 branch=$2 count=$3 work i
    work="$(mktemp -d "$case_dir/work.XXXXXX")"
    git_ clone -q "$stub/remotes/$nwo" "$work" 2>/dev/null
    git_ -C "$work" checkout -q -b "$branch"
    for ((i = 1; i <= count; i++)); do
        printf '%s %s\n' "$branch" "$i" >>"$work/branch-$branch"
        git_ -C "$work" add -A
        git_ -C "$work" commit -qm "$branch $i"
    done
    git_ -C "$work" push -q origin "$branch" 2>/dev/null
    rm -rf "$work"
}

# advance_remote OWNER/NAME: push one more commit, so a checkout of it falls behind.
advance_remote() {
    local work
    work="$(mktemp -d "$case_dir/work.XXXXXX")"
    git_ clone -q "$stub/remotes/$1" "$work" 2>/dev/null
    printf 'more\n' >>"$work/file"
    git_ -C "$work" commit -qam more
    git_ -C "$work" push -q origin main 2>/dev/null
    rm -rf "$work"
}

# new_ws NAME: an empty workspace folder to run a command in.
new_ws() {
    mkdir -p "$case_dir/ws/$1"
    printf '%s\n' "$case_dir/ws/$1"
}

# set_repos OWNER NWO...: what `gh repo list OWNER` answers.
set_repos() {
    local owner=$1
    shift
    : >"$stub/repos/$owner"
    (($#)) && printf '%s\n' "$@" >"$stub/repos/$owner"
    return 0
}

# set_team_repos ORG SLUG NWO...: what the team's repository listing answers.
set_team_repos() {
    local org=$1 slug=$2
    shift 2
    : >"$stub/team-repos/${org}__${slug}"
    (($#)) && printf '%s\n' "$@" >"$stub/team-repos/${org}__${slug}"
    return 0
}

reset_gh_log() { : >"$stub/gh.log"; }

# --- harness ------------------------------------------------------------------------------------

mini_path="$stub_bin:$tools"
stdin_text=""
out=""
err=""
rc=0

# run_mini DIR SCRIPT [VAR=value ...]
# Run SCRIPT in an interactive Bash inside DIR after sourcing Mini's .bashrc. stdin is $stdin_text
# (then reset), PATH is $mini_path. Sets $out (stdout), $err (the script's own stderr; the shell's
# startup noise is dropped) and $rc (its exit status).
run_mini() {
    local dir=$1 script=$2 outfile="$case_dir/out" errfile="$case_dir/err" guard=()
    shift 2
    : >"$errfile"
    [[ $timeout_bin == /* ]] && guard=("$timeout_bin" 120)
    rc=0
    printf '%s' "$stdin_text" | (
        cd "$dir" &&
            env -i PATH="$mini_path" HOME="$case_dir/home" TERM=dumb LC_ALL=C \
                GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_SYSTEM=/dev/null GIT_TERMINAL_PROMPT=0 \
                STUB_DIR="$stub" MINI_RC="$mini_rc" MINI_SCRIPT="$script" MINI_ERR="$errfile" \
                ${1+"$@"} \
                ${guard[@]+"${guard[@]}"} \
                bash --noprofile --norc -i -c 'source "$MINI_RC" 2>/dev/null; eval "$MINI_SCRIPT" 2>"$MINI_ERR"' \
                >"$outfile" 2>/dev/null
    ) || rc=$?
    out="$(<"$outfile")"
    err="$(<"$errfile")"
    stdin_text=""
}

# run_script DIR NAME [ARG ...]
# Run the original script .local/scripts/bin/NAME in DIR, in the same clean environment and with
# the same stdin handling as run_mini, so the two can be compared. Sets $out, $err and $rc.
run_script() {
    local dir=$1 name=$2 outfile="$case_dir/script.out" errfile="$case_dir/script.err" guard=()
    shift 2
    [[ $timeout_bin == /* ]] && guard=("$timeout_bin" 120)
    rc=0
    printf '%s' "$stdin_text" | (
        cd "$dir" &&
            env -i PATH="$mini_path" HOME="$case_dir/home" TERM=dumb LC_ALL=C \
                GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_SYSTEM=/dev/null GIT_TERMINAL_PROMPT=0 \
                STUB_DIR="$stub" \
                ${guard[@]+"${guard[@]}"} \
                bash "$repo_root/.local/scripts/bin/$name" "$@" \
                >"$outfile" 2>"$errfile"
    ) || rc=$?
    out="$(<"$outfile")"
    err="$(<"$errfile")"
    stdin_text=""
}

mkdir -p "$case_dir/home"

# git_config REPO KEY: the repository-local value, or <unset>.
git_config() { git -C "$1" config --local --get "$2" 2>/dev/null || echo '<unset>'; }

# status_line LABEL NWO: one clone-all status line without colors.
status_line() { printf '%-22s %s' "$1" "$2"; }

# sorted TEXT: the lines of TEXT in order (clone-all prints in the order its parallel jobs finish).
sorted() { printf '%s\n' "$1" | LC_ALL=C sort; }

# --- the commands exist, and nothing of them leaks into the shell -------------------------------
ws="$(new_ws basics)"
run_mini "$ws" 'declare -F clone-all clone-team'
assert_rc 0 'both commands are defined'
assert_contains "$out" 'clone-all' 'clone-all is defined by Mini'
assert_contains "$out" 'clone-team' 'clone-team is defined by Mini'

# Both bodies are subshells: their variables, helper functions, options and exported functions
# must not reach the interactive shell, and clone-team must not leave the caller in another folder.
set_repos cuberhaus cuberhaus/alpha
make_remote cuberhaus/alpha
set_team_repos acme backend cuberhaus/alpha
printf 'cuberhaus\n' >"$stub/login"
run_mini "$ws" '
    here=$PWD
    clone-all >/dev/null || echo "clone-all failed"
    clone-team acme/backend teamdest >/dev/null || echo "clone-team failed"
    [ "$PWD" = "$here" ] || echo "cwd changed"
    declare -F process_repo >/dev/null && echo "process_repo leaked"
    for v in C_RED C_RESET max_jobs clone_retries owner team dest org slug target repos teams full dir; do
        declare -p "$v" >/dev/null 2>&1 && echo "variable $v leaked"
    done
    shopt -oq pipefail && echo "pipefail leaked"
    case $- in *u*) echo "nounset leaked" ;; esac
    env | grep -E "BASH_FUNC_(_clone|process_repo|clone)" && echo "exported function leaked"
    echo done
'
assert_eq "$out" 'done' 'clone-all and clone-team leave the shell as they found it'

# --- help comes first, before any dependency check ----------------------------------------------
# No gh on the PATH, and the stub log must stay empty: help may not start anything.
for cmd in clone-all clone-team; do
    for flag in -h --help; do
        ws="$(new_ws "help-$cmd$flag")"
        reset_gh_log
        mini_path="$tools"
        run_mini "$ws" "$cmd $flag"
        mini_path="$stub_bin:$tools"
        assert_rc 0 "$cmd $flag"
        assert_contains "$out" "Usage: $cmd" "$cmd $flag names the command in its Usage line"
        assert_eq "$err" '' "$cmd $flag writes nothing to stderr"
        assert_eq "$(ls -A "$ws")" '' "$cmd $flag creates nothing"
        assert_eq "$(<"$stub/gh.log")" '' "$cmd $flag starts no gh"
    done
done

# clone-team also answers help when other words come first.
reset_gh_log
run_mini "$ws" 'clone-team acme/backend dest --help'
assert_rc 0 'clone-team help after other words'
assert_contains "$out" 'Usage: clone-team' 'clone-team help after other words'
assert_eq "$(<"$stub/gh.log")" '' 'help after other words starts no gh'

# --- a word that looks like an option is refused, not taken as data -----------------------------
for cmd in clone-all clone-team; do
    for word in --nope -x; do
        ws="$(new_ws "refuse-$cmd$word")"
        reset_gh_log
        run_mini "$ws" "$cmd $word"
        assert_rc 2 "$cmd $word"
        assert_eq "$out" '' "$cmd $word prints nothing on stdout"
        assert_contains "$err" "unknown option: $word" "$cmd $word names the option"
        assert_contains "$err" "$cmd --help" "$cmd $word points at --help"
        assert_eq "$(<"$stub/gh.log")" '' "$cmd $word starts no gh"
        assert_eq "$(ls -A "$ws")" '' "$cmd $word creates nothing"
    done
done

# --- gh is required -----------------------------------------------------------------------------
ws="$(new_ws no-gh)"
mini_path="$tools"
run_mini "$ws" 'clone-all'
assert_rc 1 'clone-all without gh'
assert_contains "$err" 'gh (GitHub CLI) is not installed' 'clone-all without gh'
run_mini "$ws" 'clone-team acme/backend'
assert_rc 1 'clone-team without gh'
assert_contains "$err" 'gh (GitHub CLI) is not installed' 'clone-team without gh'
mini_path="$stub_bin:$tools"

# ==================================================================================================
# clone-all
# ==================================================================================================

# --- a fresh run clones every repository and prepares each one ----------------------------------
make_remote cuberhaus/beta node_modules/.bin/lefthook lefthook.yml .local/scripts/apply-skip-worktree
set_repos cuberhaus cuberhaus/alpha cuberhaus/beta
ws="$(new_ws all-fresh)"
reset_gh_log
run_mini "$ws" 'clone-all'
assert_rc 0 'clone-all fresh'
assert_eq "$(sorted "$out")" "$(status_line cloned cuberhaus/alpha)
$(status_line cloned cuberhaus/beta)" 'clone-all fresh prints one cloned line per repository'
assert_eq "$err" '' 'clone-all fresh writes nothing to stderr'
assert_contains "$(<"$stub/gh.log")" 'repo list cuberhaus --limit 1000' 'owner comes from the signed-in gh account'
for repo in alpha beta; do
    assert_eq "$(git_config "$ws/$repo" core.hooksPath)" '.githooks' "$repo gets core.hooksPath"
    assert_eq "$(git_config "$ws/$repo" user.name)" 'cuberhaus' "$repo gets the cuberhaus name"
    assert_eq "$(git_config "$ws/$repo" user.email)" 'polcg10@gmail.com' "$repo gets the cuberhaus email"
done
# The checkout is found by a relative path, so this also guards the folder change of the lefthook step.
[[ -e $ws/beta/lefthook-ran ]] || fail 'the repository lefthook was not run in the new checkout'
[[ -e $ws/beta/skip-worktree-ran ]] || fail 'apply-skip-worktree was not run in the new checkout'
[[ ! -e $ws/alpha/lefthook-ran ]] || fail 'lefthook ran in a repository without a lefthook.yml'

# --- a second run finds everything current; a moved remote shows as updated --------------------
make_remote upd/one
make_remote upd/two
set_repos upd upd/one upd/two
ws="$(new_ws all-update)"
run_mini "$ws" 'clone-all upd'
assert_rc 0 'clone-all upd fresh'
run_mini "$ws" 'clone-all upd'
assert_rc 0 'clone-all upd again'
assert_eq "$(sorted "$out")" "$(status_line up-to-date upd/one)
$(status_line up-to-date upd/two)" 'a second run is up-to-date'
advance_remote upd/one
run_mini "$ws" 'clone-all upd'
assert_rc 0 'clone-all upd after a push'
assert_eq "$(sorted "$out")" "$(status_line up-to-date upd/two)
$(status_line updated upd/one)" 'a pulled change is reported as updated'
assert_eq "$(git -C "$ws/one" rev-parse HEAD)" "$(git -C "$stub/remotes/upd/one" rev-parse main)" 'the update really fast-forwarded'

# --- owner choice: argument, then CLONE_ALL_OWNER, then the signed-in account -------------------
make_remote acme/gamma
set_repos acme acme/gamma
ws="$(new_ws all-owner)"
reset_gh_log
run_mini "$ws" 'clone-all acme'
assert_rc 0 'clone-all with an owner'
assert_eq "$out" "$(status_line cloned acme/gamma)" 'clone-all acme'
assert_contains "$(<"$stub/gh.log")" 'repo list acme' 'the argument names the owner'
assert_lacks "$(<"$stub/gh.log")" 'api user' 'an explicit owner does not ask gh who is signed in'
# clone-all gives an identity to cuberhaus/* only.
assert_eq "$(git_config "$ws/gamma" user.name)" '<unset>' 'clone-all sets no identity outside cuberhaus'
assert_eq "$(git_config "$ws/gamma" core.hooksPath)" '.githooks' 'clone-all still sets core.hooksPath outside cuberhaus'

ws="$(new_ws all-owner-env)"
reset_gh_log
run_mini "$ws" 'clone-all' CLONE_ALL_OWNER=acme
assert_rc 0 'clone-all with CLONE_ALL_OWNER'
assert_eq "$out" "$(status_line cloned acme/gamma)" 'CLONE_ALL_OWNER picks the owner'
assert_lacks "$(<"$stub/gh.log")" 'api user' 'CLONE_ALL_OWNER does not ask gh who is signed in'

ws="$(new_ws all-owner-none)"
mv "$stub/login" "$stub/login.saved"
run_mini "$ws" 'clone-all'
mv "$stub/login.saved" "$stub/login"
assert_rc 1 'clone-all without an owner'
assert_contains "$err" 'could not determine the active gh user' 'clone-all without an owner'

# --- a name that is not a checkout is left alone; a nested checkout is found --------------------
make_remote misc/plain
make_remote misc/nest
set_repos misc misc/plain misc/nest
ws="$(new_ws all-existing)"
mkdir -p "$ws/plain"
printf 'mine\n' >"$ws/plain/note"
git clone -q "$stub/remotes/misc/nest" "$ws/misc/nest" 2>/dev/null
run_mini "$ws" 'clone-all misc'
assert_rc 0 'clone-all with existing paths'
assert_eq "$(sorted "$out")" "$(status_line 'skipped (not a git repo)' misc/plain)
$(status_line up-to-date misc/nest)" 'an existing plain folder is skipped and a nested checkout is used'
assert_eq "$(<"$ws/plain/note")" 'mine' 'the skipped folder is untouched'
[[ ! -e $ws/nest ]] || fail 'a flat copy was cloned next to the nested checkout'

# --- a checkout that was already there is prepared as well --------------------------------------
# The settings are applied after the pull, which is how an older clone gets hooks and an identity.
make_remote cuberhaus/prep1
set_repos prepowner cuberhaus/prep1
ws="$(new_ws all-prepare-existing)"
git clone -q "$stub/remotes/cuberhaus/prep1" "$ws/prep1" 2>/dev/null
assert_eq "$(git_config "$ws/prep1" core.hooksPath)" '<unset>' 'precondition: a plain clone has no hooks path'
run_mini "$ws" 'clone-all prepowner'
assert_rc 0 'clone-all over an existing checkout'
assert_eq "$out" "$(status_line up-to-date cuberhaus/prep1)" 'an existing checkout is only pulled'
assert_eq "$(git_config "$ws/prep1" core.hooksPath)" '.githooks' 'an existing checkout gets core.hooksPath'
assert_eq "$(git_config "$ws/prep1" user.email)" 'polcg10@gmail.com' 'an existing cuberhaus checkout gets the identity'

# --- failures: retries, a final failure, a pull that cannot fast-forward -------------------------
make_remote flaky/one
make_remote flaky/two
set_repos flaky flaky/one flaky/two
printf '5\n' >"$stub/clone-fail/flaky__one"
ws="$(new_ws all-fail)"
run_mini "$ws" 'clone-all flaky'
[[ $rc -ne 0 ]] || fail 'a repository that cannot be cloned must fail the command'
assert_contains "$out" "$(status_line 'clone failed' flaky/one)" 'a failed clone is reported'
assert_contains "$out" '    fatal: simulated clone failure for flaky/one' 'the error is indented under its repository'
assert_contains "$out" "$(status_line cloned flaky/two)" 'the other repository is still cloned'
assert_contains "$err" 'retrying clone (1/3) flaky/one' 'the first failed attempt is retried'
assert_contains "$err" 'retrying clone (2/3) flaky/one' 'the second failed attempt is retried'
assert_lacks "$err" '(3/3)' 'the last attempt is not announced as a retry'
[[ ! -e $ws/one/.git ]] || fail 'a failed clone left a checkout behind'

printf '5\n' >"$stub/clone-fail/flaky__one"
ws="$(new_ws all-fail-once)"
run_mini "$ws" 'clone-all flaky' CLONE_ALL_RETRIES=1
[[ $rc -ne 0 ]] || fail 'CLONE_ALL_RETRIES=1 must fail on the first failure'
assert_lacks "$err" 'retrying' 'CLONE_ALL_RETRIES=1 does not retry'

printf '2\n' >"$stub/clone-fail/flaky__one"
ws="$(new_ws all-flaky)"
run_mini "$ws" 'clone-all flaky'
assert_rc 0 'a clone that works on the third attempt'
assert_contains "$out" "$(status_line cloned flaky/one)" 'a clone that works on the third attempt is cloned'
rm -f "$stub/clone-fail/flaky__one"

make_remote div/one
set_repos div div/one
ws="$(new_ws all-diverged)"
run_mini "$ws" 'clone-all div'
assert_rc 0 'clone-all div fresh'
printf 'local\n' >>"$ws/one/file"
git_ -C "$ws/one" commit -qam local
advance_remote div/one
run_mini "$ws" 'clone-all div'
[[ $rc -ne 0 ]] || fail 'a pull that cannot fast-forward must fail the command'
assert_contains "$out" "$(status_line 'pull failed' div/one)" 'a diverged checkout reports pull failed'
assert_contains "$out" 'fast-forward' 'the git message says why the pull failed'
printf '%s\n' "$out" | grep -q '^    ' || fail "the git message is not indented under its repository: $out"

# --- an owner without repositories is fine; a listing that fails fails the command --------------
set_repos nobody
ws="$(new_ws all-none)"
run_mini "$ws" 'clone-all nobody'
assert_rc 0 'an owner without repositories'
assert_eq "$out" '' 'an owner without repositories prints nothing'
assert_eq "$(ls -A "$ws")" '' 'an owner without repositories clones nothing'
ws="$(new_ws all-list-fails)"
run_mini "$ws" 'clone-all unlisted'
[[ $rc -ne 0 ]] || fail 'clone-all must fail when gh cannot list the repositories'
assert_eq "$(ls -A "$ws")" '' 'a failed listing clones nothing'

# --- CLONE_ALL_JOBS=1 runs one repository at a time ---------------------------------------------
ws="$(new_ws all-serial)"
run_mini "$ws" 'clone-all upd' CLONE_ALL_JOBS=1
assert_rc 0 'clone-all with one job'
assert_eq "$(sorted "$out")" "$(status_line cloned upd/one)
$(status_line cloned upd/two)" 'one job clones everything'

# --- lefthook: the program on PATH, then npx ----------------------------------------------------
extra_bin="$case_dir/extra-bin"
mkdir -p "$extra_bin"
printf '#!/bin/sh\n: >lefthook-ran\n' >"$extra_bin/lefthook"
chmod +x "$extra_bin/lefthook"
make_remote lh/path lefthook.yml
set_repos lh lh/path
ws="$(new_ws all-lefthook-path)"
mini_path="$extra_bin:$stub_bin:$tools"
run_mini "$ws" 'clone-all lh'
mini_path="$stub_bin:$tools"
assert_rc 0 'clone-all with lefthook on PATH'
[[ -e $ws/path/lefthook-ran ]] || fail 'lefthook on PATH was not used'

rm -f "$extra_bin/lefthook"
printf '#!/bin/sh\nprintf "%%s\\n" "$*" >npx-args\n' >"$extra_bin/npx"
chmod +x "$extra_bin/npx"
ws="$(new_ws all-lefthook-npx)"
mini_path="$extra_bin:$stub_bin:$tools"
run_mini "$ws" 'clone-all lh'
mini_path="$stub_bin:$tools"
assert_rc 0 'clone-all with npx only'
assert_eq "$(<"$ws/path/npx-args")" '--yes lefthook install' 'npx runs lefthook install'
assert_eq "$out" "$(status_line cloned lh/path)" 'lefthook output never mixes into the status lines'
rm -f "$extra_bin/npx"

# ==================================================================================================
# clone-team
# ==================================================================================================

make_remote acme/delta
set_team_repos acme backend acme/gamma cuberhaus/alpha acme/delta
printf 'acme/backend\ncuberhaus/platform\n' >"$stub/teams"
set_team_repos cuberhaus platform cuberhaus/alpha

# --- a fresh run into a destination -------------------------------------------------------------
ws="$(new_ws team-fresh)"
reset_gh_log
run_mini "$ws" 'here=$PWD; clone-team acme/backend teamdir; echo "rc=$?"; [ "$PWD" = "$here" ] && echo "cwd unchanged"'
assert_eq "$out" "Fetching repositories for acme/backend...
Syncing 3 repo(s) into $ws/teamdir
  clone  acme/gamma
  clone  cuberhaus/alpha
  clone  acme/delta
rc=0
cwd unchanged" 'clone-team fresh output'
assert_eq "$err" '' 'clone-team fresh writes nothing to stderr'
for repo in gamma delta; do
    assert_eq "$(git_config "$ws/teamdir/$repo" user.name)" 'Pol Casacuberta Gil' "$repo gets the work name"
    assert_eq "$(git_config "$ws/teamdir/$repo" user.email)" 'pcasacubertagil@deloitte.es' "$repo gets the work email"
    assert_eq "$(git_config "$ws/teamdir/$repo" core.hooksPath)" '.githooks' "$repo gets core.hooksPath"
done
assert_eq "$(git_config "$ws/teamdir/alpha" user.name)" 'cuberhaus' 'a cuberhaus repository gets the personal name'
assert_eq "$(git_config "$ws/teamdir/alpha" user.email)" 'polcg10@gmail.com' 'a cuberhaus repository gets the personal email'

# --- again, then after a push, then a repository that is not a checkout -------------------------
run_mini "$ws" 'clone-team acme/backend teamdir'
assert_rc 0 'clone-team again'
assert_eq "$out" "Fetching repositories for acme/backend...
Syncing 3 repo(s) into $ws/teamdir
  up-to-date   acme/gamma
  up-to-date   cuberhaus/alpha
  up-to-date   acme/delta" 'clone-team again is up-to-date'
advance_remote acme/delta
run_mini "$ws" 'clone-team acme/backend teamdir'
assert_contains "$out" '  updated   acme/delta' 'a pulled change is reported as updated'
assert_contains "$out" '  up-to-date   acme/gamma' 'an unchanged repository stays up-to-date'
rm -rf "$ws/teamdir/gamma"
mkdir -p "$ws/teamdir/gamma"
run_mini "$ws" 'clone-team acme/backend teamdir'
assert_contains "$out" '  skip   acme/gamma (exists, not a git repo)' 'a plain folder is skipped'

# --- identity overrides -------------------------------------------------------------------------
ws="$(new_ws team-identity)"
run_mini "$ws" 'clone-team acme/backend' CLONE_TEAM_WORK_GIT_NAME=Work CLONE_TEAM_WORK_GIT_EMAIL=work@example.invalid
assert_eq "$(git_config "$ws/gamma" user.name)" 'Work' 'CLONE_TEAM_WORK_GIT_NAME sets the work name'
assert_eq "$(git_config "$ws/gamma" user.email)" 'work@example.invalid' 'CLONE_TEAM_WORK_GIT_EMAIL sets the work email'
assert_eq "$(git_config "$ws/alpha" user.name)" 'cuberhaus' 'the work identity never reaches a cuberhaus repository'
ws="$(new_ws team-identity-org)"
run_mini "$ws" 'clone-team acme/backend' CLONE_GIT_IDENTITY_ACME_NAME=Acme CLONE_GIT_IDENTITY_ACME_EMAIL=acme@example.invalid
assert_eq "$(git_config "$ws/gamma" user.name)" 'Acme' 'CLONE_GIT_IDENTITY_<ORG>_NAME overrides the identity'
assert_eq "$(git_config "$ws/gamma" user.email)" 'acme@example.invalid' 'CLONE_GIT_IDENTITY_<ORG>_EMAIL overrides the identity'

# --- a checkout that was already there is prepared as well --------------------------------------
ws="$(new_ws team-prepare-existing)"
git clone -q "$stub/remotes/acme/gamma" "$ws/gamma" 2>/dev/null
assert_eq "$(git_config "$ws/gamma" user.name)" '<unset>' 'precondition: a plain clone has no identity'
run_mini "$ws" 'clone-team acme/backend'
assert_rc 0 'clone-team over an existing checkout'
assert_contains "$out" '  up-to-date   acme/gamma' 'an existing checkout is only pulled'
assert_eq "$(git_config "$ws/gamma" user.name)" 'Pol Casacuberta Gil' 'an existing checkout gets the work identity'
assert_eq "$(git_config "$ws/gamma" core.hooksPath)" '.githooks' 'an existing checkout gets core.hooksPath'

# --- failures that do not stop the run ----------------------------------------------------------
make_remote bad/one
make_remote bad/two
set_team_repos bad crew bad/one bad/two
printf '1\n' >"$stub/clone-fail/bad__one"
ws="$(new_ws team-fail)"
run_mini "$ws" 'clone-team bad/crew'
assert_rc 0 'clone-team with a failing clone'
assert_contains "$out" '  clone  bad/two' 'the other repository is still cloned'
assert_contains "$err" '  clone failed   bad/one' 'a failed clone is reported on stderr'
rm -f "$stub/clone-fail/bad__one"

# --- team input ---------------------------------------------------------------------------------
for bad in nonsense a/b/c; do
    ws="$(new_ws "team-bad-$bad")"
    run_mini "$ws" "clone-team $bad"
    assert_rc 1 "clone-team $bad"
    assert_contains "$err" "team must be in 'org/team-slug' form" "clone-team $bad"
done

set_team_repos empty team
ws="$(new_ws team-empty)"
run_mini "$ws" 'clone-team empty/team'
assert_rc 0 'a team without repositories'
assert_contains "$err" 'No repositories found for empty/team' 'a team without repositories'

# Without a team, clone-team lists yours and asks for a number.
ws="$(new_ws team-pick)"
stdin_text=$'2\n'
run_mini "$ws" 'clone-team'
assert_rc 0 'clone-team picks a team'
assert_eq "$out" "Fetching your GitHub teams...

  [1] acme/backend
  [2] cuberhaus/platform

Fetching repositories for cuberhaus/platform...
Syncing 1 repo(s) into $ws
  clone  cuberhaus/alpha" 'clone-team lists the teams and uses the one picked'

for answer in q Q ''; do
    ws="$(new_ws "team-quit-$answer")"
    stdin_text="$answer"$'\n'
    run_mini "$ws" 'clone-team'
    assert_rc 0 "answer '$answer' quits"
    assert_eq "$(ls -A "$ws")" '' "answer '$answer' clones nothing"
done
ws="$(new_ws team-eof)"
run_mini "$ws" 'clone-team'
assert_rc 0 'end of input quits'
assert_eq "$(ls -A "$ws")" '' 'end of input clones nothing'
for answer in 0 3 x 1x; do
    ws="$(new_ws "team-invalid-$answer")"
    stdin_text="$answer"$'\n'
    run_mini "$ws" 'clone-team'
    assert_rc 1 "answer '$answer' is invalid"
    assert_contains "$err" 'invalid selection' "answer '$answer' is invalid"
    assert_eq "$(ls -A "$ws")" '' "answer '$answer' clones nothing"
done
ws="$(new_ws team-leading-zero)"
stdin_text=$'01\n'
run_mini "$ws" 'clone-team'
assert_rc 0 'a leading zero is read as a decimal number'
assert_contains "$out" 'Fetching repositories for acme/backend' 'a leading zero is read as a decimal number'

mv "$stub/teams" "$stub/teams.saved"
: >"$stub/teams"
ws="$(new_ws team-none)"
run_mini "$ws" 'clone-team'
mv "$stub/teams.saved" "$stub/teams"
assert_rc 0 'no teams'
assert_contains "$err" 'You are not a member of any GitHub teams' 'no teams'

# ==================================================================================================
# git-recurse, git-ahead and the shortcuts that call them
# ==================================================================================================

# plain TEXT: TEXT without colour codes.
plain() { printf '%s\n' "$1" | sed $'s/\033\\[[0-9;]*m//g'; }

# A workspace with a clean repository, one with an edit, and one whose remote has a branch two
# commits ahead of main. The PATH of run_mini has no ~/.local/scripts/bin, no tput and no timeout
# command, which is the kind of machine Mini is for.
make_remote rec/clean
make_remote rec/dirty
make_remote rec/ahead
push_branch rec/ahead feature 2
rec_ws="$(new_ws recurse)"
for repo in clean dirty ahead; do
    git clone -q "$stub/remotes/rec/$repo" "$rec_ws/$repo" 2>/dev/null
done
printf 'edit\n' >>"$rec_ws/dirty/file"

# --- both are functions of Mini, and the shortcuts reach them -----------------------------------
run_mini "$rec_ws" 'command -v tput timeout gtimeout git-recurse git-ahead; echo "rc=$?"'
assert_eq "$out" 'git-recurse
git-ahead
rc=0' 'precondition: no tput, no timeout command and no copy of the scripts on the PATH'
run_mini "$rec_ws" 'type -t git-recurse git-ahead gr gah status'
assert_rc 0 'type'
assert_eq "$out" 'function
function
alias
alias
function' 'git-recurse and git-ahead are functions of Mini, gr and gah aliases, status a function'

# --- git-recurse runs a command in every repository and summarises what it found ----------------
run_mini "$rec_ws" 'git-recurse git status'
assert_rc 0 'git-recurse git status'
assert_eq "$err" '' 'git-recurse git status writes nothing to stderr'
assert_contains "$out" 'Launching "git status" in 3 repo(s) in parallel...' 'git-recurse says what it launches'
assert_contains "$out" 'Done: 3 ok, 0 failed (of 3)' 'git-recurse counts the repositories'
assert_contains "$out" 'Repositories with changes (1):' 'a git status run is summarised'
assert_contains "$out" '  ./dirty/: 1 modified' 'the summary names the repository and what differs'
assert_lacks "$out" $'\033' 'a machine without tput gets no colour codes'

run_mini "$rec_ws" 'gr -s git status'
assert_rc 0 'gr'
assert_contains "$out" 'Done: 3 ok, 0 failed (of 3)' 'gr runs git-recurse'
assert_lacks "$out" 'Launching' 'gr -s runs one repository at a time'
run_mini "$rec_ws" 'status'
assert_rc 0 'status'
assert_contains "$out" '  ./dirty/: 1 modified' 'status summarises git status in every repository'

# -k adds an ssh key on macOS in the script; the copy leaves it out and says so like any other option.
run_mini "$rec_ws" 'git-recurse -k git status'
assert_rc 1 'git-recurse -k'
assert_contains "$err" 'illegal option -- k' 'git-recurse -k names the option it does not know'
assert_contains "$out" 'Usage: git-recurse [options] <command> [args...]' 'git-recurse -k shows the usage'
assert_lacks "$out" 'Launching' 'git-recurse -k starts nothing'

# --- git-ahead lists the remote branches that are ahead of main ---------------------------------
run_mini "$rec_ws" 'git-ahead'
assert_rc 0 'git-ahead'
assert_eq "$err" '' 'git-ahead writes nothing to stderr'
assert_eq "$(plain "$out")" 'ahead (base: origin/main)
    ↑   2  origin/feature' 'git-ahead names the repository, its base and the branch that is ahead'
run_mini "$rec_ws" 'gah -n -v'
assert_rc 0 'gah'
assert_eq "$(plain "$out")" 'Scanning 3 repos...
ahead (base: origin/main)
    ↑   2  origin/feature
✓ clean (base: origin/main, all remotes merged)
✓ dirty (base: origin/main, all remotes merged)' 'gah is git-ahead; -v also lists the repositories that are merged'

# --- nothing leaks into the interactive shell ---------------------------------------------------
# Both bodies are subshells, so their variables, helper functions, options, traps and descriptors
# stay out; a run that fails and a wrong option must clean up as well.
run_mini "$rec_ws" '
    # The bookkeeping names exist before the lists of names are taken, so they are not "new".
    fd3_state() { if { : >&3; } 2>/dev/null; then echo open; else echo closed; fi; }
    here=$PWD funcs= vars= flags= traps= fd3=
    funcs=$(declare -F | sort)
    flags=$(set +o; shopt -p)
    traps=$(trap -p)
    fd3=$(fd3_state)
    vars=$(compgen -v | sort)
    git-recurse git status >/dev/null 2>&1 || echo "git-recurse failed"
    git-recurse -s git status >/dev/null 2>&1 || echo "git-recurse -s failed"
    git-recurse -S git status >/dev/null 2>&1 || echo "git-recurse -S failed"
    git-recurse git no-such-subcommand >/dev/null 2>&1
    git-recurse -k git status >/dev/null 2>&1
    git-ahead >/dev/null 2>&1 || echo "git-ahead failed"
    git-ahead --bogus >/dev/null 2>&1
    [ "$PWD" = "$here" ] || echo "cwd changed"
    [ "$(declare -F | sort)" = "$funcs" ] || echo "functions leaked"
    [ "$(compgen -v | sort)" = "$vars" ] || echo "variables leaked"
    [ "$(set +o; shopt -p)" = "$flags" ] || echo "shell options changed"
    [ "$(trap -p)" = "$traps" ] || echo "traps leaked"
    [ "$(fd3_state)" = "$fd3" ] || echo "descriptor 3 changed"
    [ -z "$(jobs -p)" ] || echo "background jobs left behind"
    echo done
'
assert_eq "$out" 'done' 'git-recurse and git-ahead leave the shell as they found it'

# They also leave no temporary files behind, however the run ended.
mkdir -p "$case_dir/own-tmp"
run_mini "$rec_ws" '
    git-recurse git status >/dev/null 2>&1
    git-recurse -s git status >/dev/null 2>&1
    git-recurse git no-such-subcommand >/dev/null 2>&1
    git-recurse -d 1 git status >/dev/null 2>&1
    git-recurse --help >/dev/null 2>&1
    git-ahead >/dev/null 2>&1
    ls -A "$TMPDIR"
    echo done
' TMPDIR="$case_dir/own-tmp"
assert_eq "$out" 'done' 'git-recurse and git-ahead remove their temporary files'

# --- loading Mini again must not change what they do --------------------------------------------
# Aliases from the first load (rm -vI, mv -iv, grep -i, ...) would be baked into the function
# bodies parsed by the next one, which is why these functions call `command rm` and the like.
run_mini "$rec_ws" '
    first=$(git-recurse -s git status 2>&1; git-recurse -s git pull 2>&1; git-ahead -n -v 2>&1)
    . "$MINI_RC" 2>/dev/null
    . "$MINI_RC" 2>/dev/null
    second=$(git-recurse -s git status 2>&1; git-recurse -s git pull 2>&1; git-ahead -n -v 2>&1)
    [ "$first" = "$second" ] && echo same || { echo different; echo "$first"; echo "$second"; }
    body=$(declare -f git-recurse git-ahead clone-all clone-team)
    case $body in
        *"rm -vI"* | *"mv -iv"* | *"cp -iv"* | *"--color=auto"*) echo "an alias was baked into a function" ;;
    esac
'
assert_eq "$out" 'same' 'loading Mini a second and third time changes neither the output nor the function bodies'

# ==================================================================================================
# The copies agree with the originals
# ==================================================================================================

# The scripts use `mapfile`, so they need Bash 4; on an older Bash (macOS ships 3.2) the comparison
# with them is skipped and the behaviour tests above are all there is.
if ((BASH_VERSINFO[0] >= 4)); then
    # settings DIR: the settings the commands write into a checkout. `git config --list` prints keys
    # in lower case (core.hookspath), so the match ignores case.
    settings() { git -C "$1" config --local --list | grep -iE '^(user\.|core\.hookspath)' | sort; }
    # markers DIR: which of the fake hooks (see make_remote) ran in a checkout.
    markers() {
        local name
        for name in lefthook-ran skip-worktree-ran; do
            [[ ! -e $1/$name ]] || printf '%s\n' "$name"
        done
    }

    # --- clone-all: a fresh run, a second run, and a run after one remote moved on ---------------
    # Two workspaces take the same three steps, one with the script and one with Mini's copy. Every
    # step must end with the same exit status, status lines, error output, settings and hooks run.
    # That is what makes "updated" mean the same, and what proves the script also prepares every
    # checkout (hooks path, identity, lefthook, skip-worktree).
    make_remote cuberhaus/pall-one node_modules/.bin/lefthook lefthook.yml .local/scripts/apply-skip-worktree
    make_remote acme/pall-two
    set_repos parity-all cuberhaus/pall-one acme/pall-two
    ws_script="$(new_ws parity-all-script)"
    ws_mini="$(new_ws parity-all-mini)"
    for round in fresh again moved; do
        [[ $round == moved ]] && advance_remote cuberhaus/pall-one
        run_script "$ws_script" clone-all parity-all
        script_rc=$rc script_out=$out script_err=$err
        run_mini "$ws_mini" 'clone-all parity-all'
        assert_rc "$script_rc" "clone-all ($round) ends like the script"
        assert_eq "$(sorted "$out")" "$(sorted "$script_out")" "clone-all ($round) prints what the script prints"
        assert_eq "$err" "$script_err" "clone-all ($round) writes to stderr what the script writes"
        assert_eq "$script_err" '' "clone-all ($round) of the script writes nothing to stderr"
        for repo in pall-one pall-two; do
            assert_eq "$(settings "$ws_mini/$repo")" "$(settings "$ws_script/$repo")" \
                "clone-all ($round) writes the settings the script writes in $repo"
            assert_eq "$(markers "$ws_mini/$repo")" "$(markers "$ws_script/$repo")" \
                "clone-all ($round) runs the hooks the script runs in $repo"
        done
        case $round in
            fresh) expected="$(status_line cloned cuberhaus/pall-one)
$(status_line cloned acme/pall-two)" ;;
            again) expected="$(status_line up-to-date cuberhaus/pall-one)
$(status_line up-to-date acme/pall-two)" ;;
            moved) expected="$(status_line updated cuberhaus/pall-one)
$(status_line up-to-date acme/pall-two)" ;;
        esac
        assert_eq "$(sorted "$script_out")" "$(sorted "$expected")" "clone-all ($round) of the script prints the expected lines"
    done
    assert_eq "$(settings "$ws_script/pall-one")" "core.hookspath=.githooks
user.email=polcg10@gmail.com
user.name=cuberhaus" 'the script prepares a checkout of cuberhaus'
    assert_eq "$(markers "$ws_script/pall-one")" "lefthook-ran
skip-worktree-ran" 'the script runs the repository lefthook and apply-skip-worktree'

    # --- clone-team: the same three steps ---------------------------------------------------------
    ws_script="$(new_ws parity-team-script)"
    ws_mini="$(new_ws parity-team-mini)"
    for round in fresh again moved; do
        [[ $round == moved ]] && advance_remote acme/delta
        run_script "$ws_script" clone-team acme/backend d
        script_rc=$rc script_out=$out script_err=$err
        run_mini "$ws_mini" 'clone-team acme/backend d'
        assert_rc "$script_rc" "clone-team ($round) ends like the script"
        assert_eq "${out//$ws_mini/<ws>}" "${script_out//$ws_script/<ws>}" "clone-team ($round) prints what the script prints"
        assert_eq "$err" "$script_err" "clone-team ($round) writes to stderr what the script writes"
        for repo in gamma alpha delta; do
            assert_eq "$(settings "$ws_mini/d/$repo")" "$(settings "$ws_script/d/$repo")" \
                "clone-team ($round) writes the settings the script writes in $repo"
        done
    done
    assert_contains "$script_out" '  updated   acme/delta' 'clone-team of the script reports a pulled change as updated'
    assert_contains "$script_out" '  up-to-date   acme/gamma' 'clone-team of the script keeps an unchanged repository up-to-date'

    # --- git-recurse: the whole suite of the script, through Mini's copy --------------------------
    # tests/test_git_recurse.sh runs every test that goes through the command alone against
    # whatever GIT_RECURSE_UNDER_TEST names. The wrapper starts an interactive Bash, loads Mini's
    # .bashrc and runs the function, so the suite exercises the real thing: the exit status, the
    # output (stdout and stderr stay apart) and the arguments are passed on untouched. It gets the
    # normal PATH, so the copy meets a real timeout command here; the same suite also runs it
    # without one (a PATH of its own).
    wrapper="$case_dir/git-recurse-from-mini"
    {
        printf '#!%s\n' "$(command -v bash)"
        printf 'export MINI_RC=%q HISTFILE=/dev/null\n' "$mini_rc"
        printf 'exec 9>&2\n'
        printf '%s\n' 'exec bash --noprofile --norc -i -c '\''args=("$@"); . "$MINI_RC" 2>/dev/null; git-recurse "${args[@]}" 2>&9'\'' _ "$@" 2>/dev/null'
    } >"$wrapper"
    chmod +x "$wrapper"
    suite_rc=0
    suite_out="$(env HOME="$case_dir/home" GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_SYSTEM=/dev/null \
        GIT_RECURSE_UNDER_TEST="$wrapper" bash "$repo_root/tests/test_git_recurse.sh" 2>&1)" || suite_rc=$?
    [[ $suite_rc -eq 0 ]] || fail "the git-recurse suite fails through Mini's copy (exit $suite_rc): $suite_out"
    assert_contains "$suite_out" 'tests through the command passed' 'the git-recurse suite ran to its end through Mini'

    # --- git-recurse: help, and the words the two copies must keep in step ------------------------
    for flag in -h --help; do
        run_script "$rec_ws" git-recurse "$flag"
        script_help=$out
        run_mini "$rec_ws" "git-recurse $flag"
        assert_rc 0 "git-recurse $flag"
        assert_eq "$err" '' "git-recurse $flag writes nothing to stderr"
        assert_eq "$out" "$(grep -v '^  -k ' <<<"$script_help")" "git-recurse $flag prints the help of the script, without its -k line"
    done

    # The patterns that decide what is retried and what a summary says are copied, not shared. Each
    # must be in both files word for word, so a change to one shows up here until the other follows.
    while IFS= read -r pattern; do
        grep -qF -- "$pattern" "$repo_root/.local/scripts/bin/git-recurse" ||
            fail "git-recurse no longer contains '$pattern': change Mini's copy and this test with it"
        grep -qF -- "$pattern" "$mini_rc" || fail "Mini's git-recurse lacks '$pattern', which the script has"
    done <<'EOF'
gnutls|handshake failed|connection.*terminated|connection.*reset|connection.*refused|could not resolve|timed out|operation timed out|unable to access|index\.lock|\.lock|the remote end hung up unexpectedly|rpc failed|502 bad gateway|503 service|504 gateway|429 too many
^[[:space:]]*[0-9]+ files? changed
already up[- ]to[- ]date|current branch .* is up to date
Updating [0-9a-f]+\.\.[0-9a-f]+|Fast-forward|Successfully rebased and updated|Merge made by
^[[:space:]]*git[[:space:]]+status([[:space:]]|$)
(^|[[:space:]])pull([[:space:]]|$)
EOF

    # --- git-ahead: every option, over repositories the command treats differently -----------------
    # a-ahead: two branches ahead, and a third that the remote deleted after the checkout saw it
    # (a fetch prunes it). b-merged: a branch that is merged. c-master: the default branch is
    # master. d-nohead: origin/HEAD is gone, so origin/main is the base. e-nobase: the default
    # branch is trunk and origin/HEAD is gone, so there is no base. f-badremote: a remote that
    # cannot be fetched. sub/g-nested: one folder down. h-nohead-master: the default branch is
    # master and origin/HEAD is gone, so origin/master is the base.
    ahead_ws="$(new_ws parity-ahead)"
    make_remote ah/a-ahead
    push_branch ah/a-ahead feature 2
    push_branch ah/a-ahead fix 1
    push_branch ah/a-ahead gone 1
    make_remote ah/b-merged
    push_branch ah/b-merged finished 0
    REMOTE_BRANCH=master make_remote ah/c-master
    push_branch ah/c-master dev 1
    make_remote ah/d-nohead
    push_branch ah/d-nohead topic 3
    REMOTE_BRANCH=trunk make_remote ah/e-nobase
    make_remote ah/g-nested
    push_branch ah/g-nested later 1
    REMOTE_BRANCH=master make_remote ah/h-nohead-master
    push_branch ah/h-nohead-master hotfix 1
    for repo in a-ahead b-merged c-master d-nohead e-nobase h-nohead-master; do
        git clone -q "$stub/remotes/ah/$repo" "$ahead_ws/$repo" 2>/dev/null
    done
    git -C "$stub/remotes/ah/a-ahead" branch -q -D gone
    mkdir -p "$ahead_ws/sub"
    git clone -q "$stub/remotes/ah/g-nested" "$ahead_ws/sub/g-nested" 2>/dev/null
    git -C "$ahead_ws/d-nohead" remote set-head origin -d >/dev/null
    git -C "$ahead_ws/e-nobase" remote set-head origin -d >/dev/null
    git -C "$ahead_ws/h-nohead-master" remote set-head origin -d >/dev/null
    git clone -q "$stub/remotes/ah/b-merged" "$ahead_ws/f-badremote" 2>/dev/null
    git -C "$ahead_ws/f-badremote" remote set-url origin "$case_dir/does-not-exist.git"
    ahead_empty="$(new_ws parity-ahead-empty)"

    # compare_ahead DIR NAME ARGS...: git-ahead ARGS in DIR, by the script and by Mini. The status,
    # the output with its colour codes and the error output must be the same. Leaves what the script
    # said in ahead_rc, ahead_out and ahead_err.
    compare_ahead() {
        local dir=$1 name=$2
        shift 2
        run_script "$dir" git-ahead "$@"
        ahead_rc=$rc ahead_out=$out ahead_err=$err
        run_mini "$dir" "$(printf '%q ' git-ahead "$@")"
        assert_rc "$ahead_rc" "git-ahead $name ends like the script"
        assert_eq "$out" "$ahead_out" "git-ahead $name prints what the script prints"
        assert_eq "$err" "$ahead_err" "git-ahead $name writes to stderr what the script writes"
    }

    # The cached refs first: a fetch gives origin/HEAD back to the repositories that lost it (git
    # 2.48 and later), and the cases without a base are only reached without one.
    compare_ahead "$ahead_ws" '-n -v' -n -v
    ahead_view="$(plain "$ahead_out")"
    assert_rc 0 'the script scans without fetching'
    assert_eq "$ahead_err" '' 'the script scans without writing to stderr'
    assert_eq "$ahead_view" "Scanning 8 repos...
a-ahead (base: origin/main)
    ↑   2  origin/feature
    ↑   1  origin/fix
    ↑   1  origin/gone
✓ b-merged (base: origin/main, all remotes merged)
c-master (base: origin/master)
    ↑   1  origin/dev
d-nohead (base: origin/main)
    ↑   3  origin/topic
• e-nobase (no origin/main or origin/master)
✓ f-badremote (base: origin/main, all remotes merged)
h-nohead-master (base: origin/master)
    ↑   1  origin/hotfix
sub/g-nested (base: origin/main)
    ↑   1  origin/later" 'the script reports every kind of repository as expected'
    compare_ahead "$ahead_ws" '-n' -n
    assert_eq "$(plain "$ahead_out")" "a-ahead (base: origin/main)
    ↑   2  origin/feature
    ↑   1  origin/fix
    ↑   1  origin/gone
c-master (base: origin/master)
    ↑   1  origin/dev
d-nohead (base: origin/main)
    ↑   3  origin/topic
h-nohead-master (base: origin/master)
    ↑   1  origin/hotfix
sub/g-nested (base: origin/main)
    ↑   1  origin/later" 'without -v only the repositories with something ahead are named'
    cached_out=$ahead_out
    compare_ahead "$ahead_ws" '--no-fetch' --no-fetch
    assert_eq "$ahead_out" "$cached_out" '--no-fetch is -n'
    compare_ahead "$ahead_ws" '--verbose -n' --verbose -n
    assert_eq "$(plain "$ahead_out")" "$ahead_view" '--verbose is -v'
    compare_ahead "$ahead_ws" '--depth 1 -n' --depth 1 -n
    assert_eq "$ahead_out" 'No git repos found under . (depth 1).' 'a depth that finds nothing says so'
    compare_ahead "$ahead_ws" '--depth 2 -n -v' --depth 2 -n -v
    assert_contains "$ahead_out" 'Scanning 7 repos...' 'a depth of 2 stops above sub/g-nested'
    assert_lacks "$ahead_out" 'g-nested' 'a depth of 2 stops above sub/g-nested'
    compare_ahead "$ahead_ws" '--depth abc -n' --depth abc -n
    assert_eq "$ahead_out" 'No git repos found under . (depth abc).' 'a depth that is not a number finds nothing'
    compare_ahead "$ahead_ws" 'a relative folder' -n -v sub
    assert_eq "$(plain "$ahead_out")" 'Scanning 1 repos...
g-nested (base: origin/main)
    ↑   1  origin/later' 'a folder given as an argument is scanned and its path is shortened'
    compare_ahead "$ahead_empty" 'an empty folder' -n
    assert_eq "$ahead_out" 'No git repos found under . (depth 4).' 'a folder without repositories says so'
    compare_ahead "$ahead_ws" 'an absolute folder' -n -v "$ahead_ws/sub"
    compare_ahead "$ahead_ws" 'the folder last' -n -v "$ahead_empty" "$ahead_ws/sub"

    # Help, and the answers to what is not valid.
    for flag in -h --help; do
        compare_ahead "$ahead_ws" "$flag" "$flag"
        assert_contains "$ahead_out" 'git-ahead [dir]' "git-ahead $flag shows the usage"
    done
    compare_ahead "$ahead_ws" '--bogus' --bogus
    assert_eq "$ahead_rc" 2 'an unknown option ends with 2'
    assert_eq "$ahead_err" 'Unknown option: --bogus' 'an unknown option is named'
    compare_ahead "$ahead_ws" '-x -n' -x -n
    assert_eq "$ahead_rc" 2 'an unknown short option ends with 2'
    compare_ahead "$ahead_ws" 'a missing folder' -n "$case_dir/does-not-exist"
    assert_eq "$ahead_rc" 1 'a folder that is not there ends with 1'
    assert_eq "$ahead_err" "Not a directory: $case_dir/does-not-exist" 'a folder that is not there is named'
    # --depth without a value stops on an unset $2; the two shells word that message differently.
    run_script "$ahead_ws" git-ahead --depth
    script_rc=$rc script_err=$err
    run_mini "$ahead_ws" 'git-ahead --depth'
    assert_rc "$script_rc" 'git-ahead --depth without a value ends like the script'
    assert_contains "$err" 'unbound variable' 'git-ahead --depth without a value says why'
    assert_contains "$script_err" 'unbound variable' 'the script says why as well'

    # Then with fetching, which also brings origin/HEAD back to the repositories that lost it.
    compare_ahead "$ahead_ws" 'with a fetch' -v
    assert_contains "$(plain "$ahead_out")" '✗ f-badremote (fetch failed)' 'a remote that cannot be fetched is reported'
    assert_contains "$(plain "$ahead_out")" '    ↑   2  origin/feature' 'a fetch keeps what the cached refs showed'
    assert_lacks "$(plain "$ahead_out")" 'origin/gone' 'a fetch prunes a branch the remote deleted'
    assert_lacks "$(plain "$ahead_out")" 'f-badremote (base' 'a repository that cannot be fetched is not scanned'
    compare_ahead "$ahead_ws" 'a fetch without -v'
    assert_lacks "$(plain "$ahead_out")" 'b-merged' 'a repository without anything ahead is not named without -v'

    # A fetch prunes what the remote deleted. The shared checkouts above are pruned by the script's
    # run before Mini's starts, so they cannot tell whether Mini prunes: each copy gets a checkout
    # of its own that still has the stale ref.
    make_remote ah/i-stale
    push_branch ah/i-stale old 1
    stale_script="$(new_ws parity-ahead-stale-script)"
    stale_mini="$(new_ws parity-ahead-stale-mini)"
    git clone -q "$stub/remotes/ah/i-stale" "$stale_script/i-stale" 2>/dev/null
    git clone -q "$stub/remotes/ah/i-stale" "$stale_mini/i-stale" 2>/dev/null
    git -C "$stub/remotes/ah/i-stale" branch -q -D old
    run_script "$stale_script" git-ahead -v
    stale_view="$(plain "$out")"
    run_mini "$stale_mini" 'git-ahead -v'
    assert_eq "$(plain "$out")" "$stale_view" 'git-ahead prunes for Mini as it does for the script'
    assert_lacks "$out" 'origin/old' 'Mini prunes a branch the remote deleted'
    assert_contains "$(plain "$out")" '✓ i-stale (base: origin/main, all remotes merged)' 'the pruned repository has nothing left ahead'
fi

# --- the shared helper: identity and hooks for every scope, organisation and override -----------
# Both functions run on the same case in one shell; configure_tracked_repo_git is the original
# (sourced from the library) and _clone_configure_repo the copy.
ws="$(new_ws parity-helper)"
run_mini "$ws" '
    . "$LIB"
    n=0
    for scope in personal team; do
        for nwo in cuberhaus/x acme/x my-org/x Weird.Org/x; do
            for envset in none work org; do
                n=$((n + 1))
                git init -q -b main "orig$n"
                git init -q -b main "copy$n"
                mkdir -p "orig$n/.githooks" "copy$n/.githooks"
                (
                    case $envset in
                        work) export CLONE_TEAM_WORK_GIT_NAME=W CLONE_TEAM_WORK_GIT_EMAIL=w@e.invalid ;;
                        org) export CLONE_GIT_IDENTITY_MY_ORG_NAME=MyOrg CLONE_GIT_IDENTITY_MY_ORG_EMAIL=m@e.invalid \
                                CLONE_GIT_IDENTITY_WEIRD_ORG_NAME=Wd CLONE_GIT_IDENTITY_CUBERHAUS_EMAIL=c@e.invalid ;;
                    esac
                    configure_tracked_repo_git "$nwo" "orig$n" "$scope"
                    _clone_configure_repo "$nwo" "copy$n" "$scope"
                )
                a=$(git -C "orig$n" config --local --list | grep -iE "^(user\.|core\.hookspath)" | sort)
                b=$(git -C "copy$n" config --local --list | grep -iE "^(user\.|core\.hookspath)" | sort)
                [ "$a" = "$b" ] || { echo "DIFFERS: $scope $nwo $envset"; echo "original: $a"; echo "copy: $b"; }
            done
        done
    done
    # A stale core.hooksPath is removed when .githooks is gone, by both.
    git init -q -b main stale-orig; git init -q -b main stale-copy
    git -C stale-orig config core.hooksPath .githooks; git -C stale-copy config core.hooksPath .githooks
    configure_tracked_repo_git acme/x stale-orig team; _clone_configure_repo acme/x stale-copy team
    [ -z "$(git -C stale-orig config --local --get core.hooksPath)" ] || echo "original kept a stale hooksPath"
    [ -z "$(git -C stale-copy config --local --get core.hooksPath)" ] || echo "copy kept a stale hooksPath"
    # lefthook: both run the program inside the repository, given a relative path, and print nothing.
    for side in orig copy; do
        git init -q -b main "lh-$side"
        : >"lh-$side/lefthook.yml"
        mkdir -p "lh-$side/node_modules/.bin"
        printf "#!/bin/sh\necho noise\necho noise >&2\n: >lefthook-ran\n" >"lh-$side/node_modules/.bin/lefthook"
        chmod +x "lh-$side/node_modules/.bin/lefthook"
    done
    a=$(configure_tracked_repo_git acme/x lh-orig team 2>&1)
    b=$(_clone_configure_repo acme/x lh-copy team 2>&1)
    [ -e lh-orig/lefthook-ran ] || echo "original did not run lefthook"
    [ -e lh-copy/lefthook-ran ] || echo "copy did not run lefthook"
    [ -z "$a$b" ] || echo "lefthook output reached the terminal: original=$a copy=$b"
    echo "cases: $n"
' LIB="$lib"
assert_rc 0 'helper parity run'
assert_eq "$out" 'cases: 24' 'the copy of configure_tracked_repo_git agrees with the original in every case'

# --- the path helper: flat or nested, an existing checkout wins ---------------------------------
# One case per branch of resolve_repo_dir (nested checkout, flat checkout, a plain nested path, a
# plain flat path, nothing there yet) and one per tie, because the order of the branches is the
# behaviour: a nested checkout beats a flat one, any checkout beats a plain folder, and a plain
# nested folder beats a plain flat one.
ws="$(new_ws parity-paths)"
run_mini "$ws" '
    . "$LIB"
    mkdir -p org/repo/.git cv/.git o2/plain-nested plain-flat \
        tie/pair/.git pair/.git mix/item item/.git both/stuff stuff
    for nwo in org/repo cuberhaus/cv o2/plain-nested x/plain-flat new/thing \
        tie/pair mix/item both/stuff; do
        a=$(resolve_repo_dir "$nwo"); b=$(_clone_repo_dir "$nwo")
        [ "$a" = "$b" ] && echo "same: $nwo -> $a" || echo "DIFFERS: $nwo original=$a copy=$b"
    done
' LIB="$lib"
assert_rc 0 'path helper parity run'
assert_eq "$out" 'same: org/repo -> org/repo
same: cuberhaus/cv -> cv
same: o2/plain-nested -> o2/plain-nested
same: x/plain-flat -> plain-flat
same: new/thing -> thing
same: tie/pair -> tie/pair
same: mix/item -> item
same: both/stuff -> both/stuff' 'the copy of resolve_repo_dir agrees with the original in every branch and tie'

printf 'Mini .bashrc clone-all, clone-team, git-recurse and git-ahead tests passed.\n'
