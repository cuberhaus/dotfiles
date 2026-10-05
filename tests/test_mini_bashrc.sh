#!/usr/bin/env bash
# Tests for the commands that .local/Mini/.bashrc defines on its own: clone-all and clone-team.
#
# Mini is one portable file, so it carries its own copies of the clone-all and clone-team scripts
# and of the helpers they share (.local/scripts/lib/git-repo-defaults.sh). These tests run the
# copies in a real interactive Bash against real git repositories (local bare repositories stand
# in for GitHub) and a stub `gh`, and compare the shared logic with the originals so the copies
# cannot drift apart unnoticed.
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
for tool in awk basename bash cat chmod cut dirname env getconf git grep head id mkdir nproc sed sort tr uname xargs; do
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
# fake programs that leave a marker file, so a test can see that the command ran them.
make_remote() {
    local nwo=$1 work file
    shift
    git_ init -q --bare -b main "$stub/remotes/$nwo"
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
    git_ -C "$work" push -q origin main 2>/dev/null
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
# The copies agree with the originals
# ==================================================================================================

# The scripts use `mapfile`, so they need Bash 4; on an older Bash (macOS ships 3.2) the comparison
# with them is skipped and the behaviour tests above are all there is.
if ((BASH_VERSINFO[0] >= 4)); then
    # settings DIR: the settings the commands write into a checkout. `git config --list` prints keys
    # in lower case (core.hookspath), so the match ignores case.
    settings() { git -C "$1" config --local --list | grep -iE '^(user\.|core\.hookspath)' | sort; }
    # markers DIR: which of the fake hooks (see make_remote) ran in a checkout.
    markers() { (cd "$1" && ls -A | grep -E '^(lefthook|skip-worktree)-ran$' || true); }

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

printf 'Mini .bashrc clone-all and clone-team tests passed.\n'
