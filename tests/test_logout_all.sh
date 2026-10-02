#!/usr/bin/env bash
# Tests for .local/scripts/bin/logout-all.
#
# They guard what a sign-out tool must get right:
#   - Nothing changes before the answer, under --dry-run, or when there is no terminal to
#     ask on. Only an explicit "y" goes ahead: Enter, n and the end of the input do not.
#   - Exactly the listed items are deleted. Bookmarks, history, preferences, extensions and
#     Firefox's key4.db stay, and an editor keeps everything in state.vscdb except its
#     sign-in rows (no old copy of them is left in the file either).
#   - An app that is running is skipped, because it would write its session back when it
#     quits. Without pgrep nothing can be checked, so every app counts as running.
#   - A symbolic link is never followed: what it points at survives.
#   - A failing sign-out is reported and the steps after it still run. The keyring is
#     locked last, because gh needs it unlocked to delete the token it keeps there.
#   - --audit changes nothing and never prints a secret value.
#
# Every case runs in a clean `env -i` shell with a throwaway HOME. PATH holds stub commands
# (gh, docker, sudo, ssh-add, busctl, pgrep) and a few real tools, so nothing real can sign
# out, unload a key or lock the keyring, and a tool that is not linked behaves as not installed.
#
# The answers to the prompt are typed by a small Python helper on a pseudo-terminal, because
# the script only asks when stdin and stdout are terminals.

# The scripts handed to the stubs below are single-quoted on purpose: their variables must
# be expanded by the stub, not by this shell (SC2016). --audit prints paths with a literal
# ~, and the assertions look for that text, not for a home folder (SC2088).
# shellcheck disable=SC2016,SC2088

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
script="$repo_root/.local/scripts/bin/logout-all"
case_dir="$(mktemp -d)"
trap 'rm -rf "$case_dir"' EXIT

default_home="$case_dir/home"
home="$default_home"
outside="$case_dir/outside" # data that no case may touch: symbolic links point here
stubs="$case_dir/stubs"     # every stub command; a case links the ones it installs into $bin
bin="$case_dir/bin"
calls_log="$case_dir/calls.log"
bash_bin="$(command -v bash)"
python_bin="$(command -v python3 || true)"
mkdir -p "$stubs" "$outside"

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

assert_equals() {
    local expected=$1 actual=$2 message=$3
    if [[ $expected != "$actual" ]]; then
        printf 'FAIL: %s\n--- expected\n%s\n--- actual\n%s\n' "$message" "$expected" "$actual" >&2
        exit 1
    fi
}

assert_contains() {
    [[ $1 == *"$2"* ]] || fail "$3 (missing: $2). Output was:
$1"
}

assert_not_contains() {
    [[ $1 != *"$2"* ]] || fail "$3 (unexpected: $2). Output was:
$1"
}

# assert_exists PATH...: paths relative to $home that must survive.
assert_exists() {
    local path
    for path in "$@"; do
        [[ -e $home/$path || -L $home/$path ]] || fail "$path must still exist"
    done
}

# assert_missing PATH...: paths relative to $home that must be gone.
assert_missing() {
    local path
    for path in "$@"; do
        [[ ! -e $home/$path && ! -L $home/$path ]] || fail "$path must be deleted"
    done
}

###############################################################
# => Stub commands
###############################################################

# write_stub NAME: the script body comes from stdin.
write_stub() {
    {
        printf '#!%s\n' "$bash_bin"
        cat
    } > "$stubs/$1"
    chmod +x "$stubs/$1"
}

# They log their command line, and fail when "name args" starts with $STUB_FAIL.
for name in sudo docker; do
    write_stub "$name" <<'EOF'
printf '%s %s\n' "${0##*/}" "$*" >> "$STUB_LOG"
[[ "${0##*/} $*" == "${STUB_FAIL:-@none@}"* ]] && exit 1
exit 0
EOF
done

# `gh auth status` prints $STUB_GH_STATUS. Every other command succeeds.
write_stub gh <<'EOF'
printf 'gh %s\n' "$*" >> "$STUB_LOG"
if [[ "gh $*" == "${STUB_FAIL:-@none@}"* ]]; then
    printf 'gh: the request failed\n' >&2
    exit 1
fi
[[ "$1 ${2:-}" == 'auth status' ]] && printf '%s\n' "${STUB_GH_STATUS:-}"
exit 0
EOF

# `ssh-add -l` exits with $STUB_AGENT_LIST_STATUS: 0 for an agent that holds keys.
write_stub ssh-add <<'EOF'
printf 'ssh-add %s\n' "$*" >> "$STUB_LOG"
[[ ${1:-} == -l ]] && exit "${STUB_AGENT_LIST_STATUS:-1}"
[[ "ssh-add $*" == "${STUB_FAIL:-@none@}"* ]] && exit 1
exit 0
EOF

# The keyring is running unless $STUB_KEYRING is 0.
write_stub busctl <<'EOF'
printf 'busctl %s\n' "$*" >> "$STUB_LOG"
case " $* " in
    *' list '*)
        [[ ${STUB_KEYRING:-1} == 1 ]] && printf 'org.freedesktop.secrets 1234 gnome-keyring-d pol :1.45 session-3.scope -\n'
        ;;
    *' ReadAlias '*)
        printf 'o "/org/freedesktop/secrets/collection/login"\n'
        ;;
    *' Lock '*)
        [[ "busctl $*" == "${STUB_FAIL:-@none@}"* ]] && exit 1
        printf 'ao 1 "/org/freedesktop/secrets/collection/login" o "/"\n'
        ;;
esac
exit 0
EOF

# $STUB_RUNNING lists the process names that "run". Like the real pgrep, it matches a part of
# the name unless it is given -x.
write_stub pgrep <<'EOF'
exact=false
for arg; do
    [[ $arg == -x ]] && exact=true
done
for name in ${STUB_RUNNING:-}; do
    if [[ $exact == true ]]; then
        [[ $name == "${*: -1}" ]] && exit 0
    else
        [[ $name == *"${*: -1}"* ]] && exit 0
    fi
done
exit 1
EOF

# A python3 that answers like the editor-state helper (python3 - DB count|delete), for the
# cases that need a vacuum to fail or a database to be big. $STUB_COMPACT_NOTE is the second
# line the helper prints when the vacuum failed.
write_stub python3 <<'EOF'
[[ ${3:-} == count ]] && { printf '3\n'; exit 0; }
printf '3\n'
[[ -n ${STUB_COMPACT_NOTE:-} ]] && printf '%s\n' "$STUB_COMPACT_NOTE"
exit 0
EOF

# An id that says the user is root.
write_stub id <<'EOF'
[[ ${1:-} == -u ]] && { printf '0\n'; exit 0; }
printf 'root\n'
EOF

###############################################################
# => Pseudo-terminal helper
###############################################################

pty_helper="$case_dir/pty_session.py"
cat > "$pty_helper" <<'EOF'
"""Run a command on a pseudo-terminal and type the replies to its prompts.

usage: pty_session.py OUTPUT_FILE [EXPECT SEND]... -- COMMAND [ARG]...

Waits until EXPECT (plain text) appears in the output after the previous match, then
types SEND. Everything the command printed goes to OUTPUT_FILE with CRLF turned into LF.
Exits with the command's status, 124 when it hangs and 125 when it ends before every
EXPECT appeared.
"""
import os
import pty
import select
import signal
import sys
import time

TIMEOUT = 20.0


def main():
    args = sys.argv[1:]
    output_file = args[0]
    separator = args.index('--')
    steps = args[1:separator]
    command = args[separator + 1:]
    pending = [(steps[i].encode(), steps[i + 1].encode()) for i in range(0, len(steps), 2)]

    pid, fd = pty.fork()
    if pid == 0:
        try:
            os.execv(command[0], command)
        finally:
            os._exit(127)

    captured = b''
    searched_from = 0
    timed_out = False
    deadline = time.monotonic() + TIMEOUT
    while True:
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            timed_out = True
            break
        readable, _, _ = select.select([fd], [], [], min(remaining, 1.0))
        if not readable:
            continue
        try:
            chunk = os.read(fd, 4096)
        except OSError:  # Linux reports the end of the output as EIO
            break
        if not chunk:
            break
        captured += chunk
        while pending:
            index = captured.find(pending[0][0], searched_from)
            if index < 0:
                break
            searched_from = index + len(pending[0][0])
            try:
                os.write(fd, pending[0][1])
            except OSError:
                pass
            pending.pop(0)

    if timed_out:
        os.kill(pid, signal.SIGKILL)
    _, wait_status = os.waitpid(pid, 0)
    if os.WIFEXITED(wait_status):
        code = os.WEXITSTATUS(wait_status)
    else:
        code = 128 + os.WTERMSIG(wait_status)

    text = captured.decode('utf-8', errors='replace').replace('\r\n', '\n').replace('\r', '')
    with open(output_file, 'w', encoding='utf-8') as handle:
        handle.write(text)
    if timed_out:
        sys.stderr.write('pty_session: the command did not finish in time\n')
        sys.exit(124)
    if pending:
        sys.stderr.write('pty_session: the command ended before %r appeared\n' % pending[0][0].decode())
        sys.exit(125)
    sys.exit(code)


main()
EOF

###############################################################
# => Harness
###############################################################

# The real tools the script needs; the rest of PATH is empty on purpose.
real_tools=(awk cat sed grep find sort stat du head wc tr rm mkdir shred)

# set_machine: an empty home, and the default machine: every stub installed, python3 and
# ssh-keygen real, no app running. A case changes case_stubs, case_real or case_env after it.
set_machine() {
    home="$default_home"
    rm -rf "$home"
    mkdir -p "$home"
    case_stubs=(gh docker sudo ssh-add busctl pgrep)
    case_real=(python3 ssh-keygen id)
    case_env=(NO_COLOR=1)
    tty_steps=()
}

# prepare_run: the clean PATH of the next run, and an empty call log.
prepare_run() {
    local tool path
    rm -rf "$bin"
    mkdir -p "$bin"
    for tool in "${real_tools[@]}" ${case_real[@]+"${case_real[@]}"}; do
        if [[ $tool == python3 && -z $python_bin ]]; then
            continue
        fi
        path="$(command -v "$tool" || true)"
        if [[ -n $path ]]; then
            ln -s "$path" "$bin/$tool"
        fi
    done
    for tool in ${case_stubs[@]+"${case_stubs[@]}"}; do
        ln -sf "$stubs/$tool" "$bin/$tool"
    done
    : > "$calls_log"
}

# in_case_env COMMAND...: run COMMAND in an otherwise empty environment.
in_case_env() {
    env -i HOME="$home" PATH="$bin" TERM=dumb STUB_LOG="$calls_log" "${case_env[@]}" "$@"
}

# run_logout [ARGS...]
# Run `logout-all ARGS` with no terminal. Sets `status` (its exit status), `output` (stdout
# and stderr) and `calls` (what the stubs saw).
run_logout() {
    prepare_run
    status=0
    output="$(in_case_env "$bash_bin" "$script" "$@" 2>&1 < /dev/null)" || status=$?
    calls="$(< "$calls_log")"
}

# run_logout_tty
# Run `logout-all` on a pseudo-terminal and type tty_steps into it: pairs of the prompt text
# to wait for and the keys to send. Sets the same variables as run_logout.
run_logout_tty() {
    local out_file="$case_dir/tty.out"
    prepare_run
    status=0
    in_case_env "$python_bin" "$pty_helper" "$out_file" ${tty_steps[@]+"${tty_steps[@]}"} -- \
        "$bash_bin" "$script" || status=$?
    output="$(< "$out_file")"
    calls="$(< "$calls_log")"
    if [[ $status == 124 || $status == 125 ]]; then
        fail "the terminal session did not go as scripted (status $status). It printed:
$output"
    fi
}

# state_of_home: every path, type, mode, link target and modification time under $home, and
# the hash of every file, one per line.
state_of_home() {
    (
        cd "$home"
        find . -printf '%p %y %m %l %T@\n' | LC_ALL=C sort
        find . -type f -exec sha256sum {} + | LC_ALL=C sort
    )
}

# assert_same_state BEFORE MESSAGE: $home must be as state_of_home described it in BEFORE.
assert_same_state() {
    local after
    after="$(state_of_home)"
    if [[ $1 != "$after" ]]; then
        printf 'FAIL: %s\n--- what changed (- before, + after)\n' "$2" >&2
        diff <(printf '%s\n' "$1") <(printf '%s\n' "$after") >&2 || true
        exit 1
    fi
}

# put PATH [TEXT]: a file under $home with its parents.
put() {
    mkdir -p "$(dirname "$home/$1")"
    printf '%s\n' "${2:-content of ${1##*/}}" > "$home/$1"
}

# secret_is_gone FILE TEXT: succeeds when TEXT is nowhere in the raw bytes of FILE.
secret_is_gone() {
    ! grep -a -q -- "$2" "$1"
}

gh_status_two_accounts='github.com
  ✓ Logged in to github.com account alice (keyring)
  - Active account: true

  ✓ Logged in to github.com account bob (/home/test/.config/gh/hosts.yml)
  - Active account: false'

# make_editor_db DIR: a VS Code style state database with sign-in rows and rows to keep. It is
# in WAL mode, like the editors' own: a read-only connection to such a database creates its
# -wal and -shm files, and a plan or an audit must not.
make_editor_db() {
    mkdir -p "$1/User/globalStorage"
    "$python_bin" - "$1/User/globalStorage/state.vscdb" <<'PY'
import sqlite3
import sys

db = sqlite3.connect(sys.argv[1])
db.execute('PRAGMA journal_mode = WAL')
db.execute('CREATE TABLE ItemTable (key TEXT UNIQUE ON CONFLICT REPLACE, value BLOB)')
db.executemany('INSERT INTO ItemTable VALUES (?, ?)', [
    ('cursorAuth/accessToken', 'SECRET-ACCESS-TOKEN-1111'),
    ('cursorAuth/refreshToken', 'SECRET-REFRESH-TOKEN-2222'),
    ('secret://{"extensionId":"vscode.github-authentication"}', 'SECRET-EXTENSION-3333'),
    ('antigravityUnifiedStateSync.oauthToken', 'SECRET-OAUTH-TOKEN-4444'),
    ('workbench.chat.history', 'KEEP-THE-CHAT-HISTORY'),
    ('workbench.sideBar.position', 'left'),
])
db.commit()
db.close()
PY
}

# make_crashed_editor_db DIR: the same database, but its writer died without closing it, so
# every row is still only in the -wal file and the database file itself holds none of them.
make_crashed_editor_db() {
    mkdir -p "$1/User/globalStorage"
    "$python_bin" - "$1/User/globalStorage/state.vscdb" <<'PY'
import os
import sqlite3
import sys

db = sqlite3.connect(sys.argv[1])
db.execute('PRAGMA journal_mode = WAL')
db.execute('PRAGMA wal_autocheckpoint = 0')
db.execute('CREATE TABLE ItemTable (key TEXT UNIQUE ON CONFLICT REPLACE, value BLOB)')
db.executemany('INSERT INTO ItemTable VALUES (?, ?)', [
    ('cursorAuth/accessToken', 'SECRET-ACCESS-TOKEN-1111'),
    ('cursorAuth/refreshToken', 'SECRET-REFRESH-TOKEN-2222'),
    ('antigravityUnifiedStateSync.oauthToken', 'SECRET-OAUTH-TOKEN-4444'),
    ('workbench.chat.history', 'KEEP-THE-CHAT-HISTORY'),
])
db.commit()
os._exit(0)
PY
}

# editor_keys DB: the keys left in a state database, comma separated.
editor_keys() {
    "$python_bin" - "$1" <<'PY'
import sqlite3
import sys

rows = sqlite3.connect(sys.argv[1]).execute('SELECT key FROM ItemTable ORDER BY key').fetchall()
print(','.join(row[0] for row in rows))
PY
}

# build_home: a home folder with a little of everything a sign-out touches or must leave.
build_home() {
    local f
    # Chrome: one profile, signed in.
    put '.config/google-chrome/Local State' '{"profile":{"info_cache":{"Default":{"user_name":"me@example.com"}}}}'
    for f in Preferences Bookmarks History Extensions/abc/manifest.json \
        Cookies Cookies-journal 'Login Data' 'Login Data-journal' 'Web Data' \
        'Local Storage/leveldb/000003.log' 'Network/Cookies' 'Network/Trust Tokens'; do
        put ".config/google-chrome/Default/$f"
    done
    # Firefox: one profile.
    for f in prefs.js key4.db places.sqlite storage/permanent/chrome/idb.sqlite \
        cookies.sqlite cookies.sqlite-wal logins.json storage/default/https+++example.com/ls/data.sqlite; do
        put ".mozilla/firefox/abc.default/$f"
    done
    # Editors and web apps.
    put '.config/Cursor/Cookies'
    put '.config/Cursor/User/settings.json'
    put '.config/Cursor/User/globalStorage/state.vscdb.backup'
    put '.config/obsidian/obsidian.json'
    put '.config/obsidian/Cookies'
    put '.config/obsidian/Local Storage/leveldb/000003.log'
    if [[ -n $python_bin ]]; then
        make_editor_db "$home/.config/Cursor"
    fi
    # Token files, and files that logout-all must never delete.
    put '.git-credentials' 'https://alice:s3cretpassw0rd@github.com'
    put '.pi/agent/auth.json' '{"token":"abc"}'
    put '.grok/auth.json' '{"token":"abc"}'
    put '.docker/config.json' '{"auths":{"nvcr.io":{"auth":"dXNlcjpwYXNz"}}}'
    put '.ssh/id_ed25519' 'not a real key'
    put '.zsh_history' 'git status'
    put '.bash_history' 'ls'
    put 'key' 'a file of yours'
    put 'project/.env' 'API_TOKEN=abc'
}

###############################################################
# => The command itself
###############################################################

[[ -x $script ]] || fail 'logout-all must be executable'
assert_equals '#!/usr/bin/env bash' "$(sed -n 1p "$script")" \
    'logout-all must start with the env bash shebang'
[[ "$(sed -n 2p "$script")" == '# Description: '?* ]] ||
    fail 'line 2 of logout-all must be a "# Description: ..." line: the commands catalog shows it'
bash -n "$script" || fail 'logout-all must be valid bash'

set_machine
run_logout --help
assert_equals 0 "$status" '--help must succeed'
assert_contains "$output" 'Usage: logout-all' '--help must print the usage'
assert_contains "$output" 'Groups (for --only and --skip' '--help must explain the groups'
assert_contains "$output" 'Revoke tokens on the provider' '--help must say that a local sign-out is not a revocation'
assert_equals '' "$calls" '--help must run nothing'

# A fresh home with no CLI tools has nothing to sign out of.
set_machine
case_stubs=(pgrep)
run_logout
assert_equals 0 "$status" 'an empty home is not an error'
assert_contains "$output" 'Nothing to sign out of.' 'an empty home must say so'
assert_not_contains "$calls" 'logout' 'an empty home must sign out of nothing'

###############################################################
# => Bad input
###############################################################

set_machine
run_logout --bogus
assert_equals 2 "$status" 'an unknown option must be a usage error'
assert_contains "$output" 'unknown option: --bogus' 'the bad option must be named'

run_logout --only bogus
assert_equals 2 "$status" 'an unknown group must be a usage error'
assert_contains "$output" 'unknown group: bogus' 'the bad group must be named'

run_logout --only
assert_equals 2 "$status" '--only without a list must be a usage error'
assert_contains "$output" '--only needs a list of groups' '--only without a list must say what is missing'

run_logout --only files --skip files
assert_equals 2 "$status" 'skipping every selected group must be a usage error'
assert_contains "$output" 'no group is left to run' 'an empty selection must say so'

run_logout --audit --dry-run
assert_equals 2 "$status" '--audit with another option must be a usage error'
assert_contains "$output" '--audit is read-only and takes no other option' 'the conflict must be explained'

prepare_run
status=0
output="$(env -i HOME=/ PATH="$bin" NO_COLOR=1 "$bash_bin" "$script" 2>&1 < /dev/null)" || status=$?
assert_equals 2 "$status" 'HOME=/ must be refused'
assert_contains "$output" 'HOME is not set to a home folder' 'a bad HOME must be explained'

prepare_run
status=0
output="$(env -i PATH="$bin" NO_COLOR=1 "$bash_bin" "$script" 2>&1 < /dev/null)" || status=$?
assert_equals 2 "$status" 'an unset HOME must be refused'

# root: the stub id says so, and the real one is not linked.
set_machine
case_real=(python3 ssh-keygen)
case_stubs+=(id)
run_logout
assert_equals 2 "$status" 'root must be refused'
assert_contains "$output" 'not as root' 'running as root must be explained'

###############################################################
# => Nothing changes without a yes
###############################################################

set_machine
build_home
case_env+=("STUB_GH_STATUS=$gh_status_two_accounts" STUB_AGENT_LIST_STATUS=0)
before="$(state_of_home)"
run_logout --dry-run
assert_equals 0 "$status" 'a dry run must succeed'
assert_same_state "$before" 'a dry run must change nothing'
assert_contains "$output" 'Plan (nothing has been changed yet)' 'a dry run must show the plan'
assert_contains "$output" 'would run: gh auth logout --hostname github.com --user alice' 'the plan must show the gh sign-out'
assert_contains "$output" 'would run: docker logout nvcr.io' 'the plan must show the Docker sign-out'
assert_contains "$output" 'would delete .git-credentials' 'the plan must show the token files'
assert_contains "$output" 'would delete Cookies' 'the plan must show the browser data'
assert_contains "$output" 'would remove 4 sign-in token(s) and secret(s)' 'the plan must show the editor sign-ins'
assert_contains "$output" 'Dry run: nothing was changed.' 'a dry run must say it changed nothing'
for forbidden in 'auth logout' 'docker logout' 'sudo' 'ssh-add -D' ' Lock '; do
    assert_not_contains "$calls" "$forbidden" "a dry run must not run: $forbidden"
done
assert_contains "$calls" 'gh auth status' 'a dry run may read the sign-in state'
assert_not_contains "$output" 's3cretpassw0rd' 'the plan must not print a secret'

# No terminal and no --yes: it must not guess.
before="$(state_of_home)"
run_logout
assert_equals 0 "$status" 'having no terminal to ask on is not a failure'
assert_contains "$output" 'There is no terminal to ask on, so nothing was changed' 'it must say why it stopped'
assert_same_state "$before" 'without a terminal and without --yes nothing may change'
assert_not_contains "$calls" 'auth logout' 'without a terminal nothing may be signed out'

if [[ -n $python_bin ]]; then
    # Only an explicit y goes ahead.
    for answer in $'n\n' $'\n' $'no\n' $'\004'; do
        tty_steps=('[y/N] ' "$answer")
        run_logout_tty
        assert_equals 0 "$status" "the answer $(printf %q "$answer") must end without an error"
        assert_contains "$output" 'Nothing was changed.' "the answer $(printf %q "$answer") must say nothing changed"
        assert_same_state "$before" "the answer $(printf %q "$answer") must change nothing"
        assert_not_contains "$calls" 'auth logout' "the answer $(printf %q "$answer") must sign out of nothing"
    done

    tty_steps=('[y/N] ' $'y\n')
    run_logout_tty
    assert_equals 0 "$status" 'an explicit y must run to the end'
    assert_contains "$output" 'Signing out' 'an explicit y must sign out'
    assert_contains "$output" 'Done:' 'an explicit y must finish with a summary'
    assert_missing .git-credentials
else
    printf 'SKIP: python3 is not installed; the confirmation prompt is not tested.\n'
fi

###############################################################
# => The real run
###############################################################

set_machine
build_home
case_env+=("STUB_GH_STATUS=$gh_status_two_accounts" STUB_AGENT_LIST_STATUS=0)
run_logout --yes
assert_equals 0 "$status" 'a clean run must succeed'
assert_contains "$output" 'Done:' 'a clean run must end with a summary'
assert_contains "$output" 'Revoke it at the source' 'the summary must tell to revoke the tokens too'

# Gone: cookies, saved passwords, autofill, site storage, login tokens.
assert_missing \
    '.config/google-chrome/Default/Cookies' '.config/google-chrome/Default/Cookies-journal' \
    '.config/google-chrome/Default/Login Data' '.config/google-chrome/Default/Login Data-journal' \
    '.config/google-chrome/Default/Web Data' '.config/google-chrome/Default/Local Storage' \
    '.config/google-chrome/Default/Network/Cookies' '.config/google-chrome/Default/Network/Trust Tokens' \
    '.mozilla/firefox/abc.default/cookies.sqlite' '.mozilla/firefox/abc.default/cookies.sqlite-wal' \
    '.mozilla/firefox/abc.default/logins.json' \
    '.mozilla/firefox/abc.default/storage/default/https+++example.com' \
    '.config/Cursor/Cookies' '.config/Cursor/User/globalStorage/state.vscdb.backup' \
    '.config/obsidian/Cookies' '.config/obsidian/Local Storage' \
    .git-credentials .pi/agent/auth.json .grok/auth.json

# Kept: everything else, above all what is not a login.
assert_exists \
    '.config/google-chrome/Local State' '.config/google-chrome/Default/Preferences' \
    '.config/google-chrome/Default/Bookmarks' '.config/google-chrome/Default/History' \
    '.config/google-chrome/Default/Extensions/abc/manifest.json' \
    '.mozilla/firefox/abc.default/prefs.js' '.mozilla/firefox/abc.default/key4.db' \
    '.mozilla/firefox/abc.default/places.sqlite' \
    '.mozilla/firefox/abc.default/storage/permanent/chrome/idb.sqlite' \
    '.config/Cursor/User/settings.json' '.config/obsidian/obsidian.json' \
    .ssh/id_ed25519 .zsh_history .bash_history key project/.env .docker/config.json

# The sign-outs, and the order: gh before the keyring is locked.
sign_outs="$(grep -E 'auth logout|docker logout|^sudo|ssh-add -D| Lock ' <<< "$calls" | sed 's/ \/org.*//')"
assert_equals 'gh auth logout --hostname github.com --user alice
gh auth logout --hostname github.com --user bob
docker logout nvcr.io
sudo -K
ssh-add -D
busctl --user call org.freedesktop.secrets' "$sign_outs" 'every sign-out must run once, with the keyring locked last'

if [[ -n $python_bin ]]; then
    db="$home/.config/Cursor/User/globalStorage/state.vscdb"
    assert_equals 'workbench.chat.history,workbench.sideBar.position' "$(editor_keys "$db")" \
        'an editor must lose its sign-in rows and keep every other row'
    for secret in SECRET-ACCESS-TOKEN-1111 SECRET-REFRESH-TOKEN-2222 SECRET-EXTENSION-3333 SECRET-OAUTH-TOKEN-4444; do
        secret_is_gone "$db" "$secret" || fail "$secret must not stay in the state database file"
    done
    grep -a -q 'KEEP-THE-CHAT-HISTORY' "$db" || fail 'the chat history must stay in the state database'
fi

# Running it again finds nothing left on disk.
run_logout --yes
assert_equals 0 "$status" 'a second run must succeed'
assert_not_contains "$output" 'would delete' 'a second run must find no file left to delete'
assert_not_contains "$output" 'would remove' 'a second run must find no editor sign-in left'

# History is off by default and a group of its own.
set_machine
build_home
run_logout --only history --yes
assert_equals 0 "$status" '--only history must succeed'
assert_missing .zsh_history .bash_history
assert_exists .git-credentials '.config/google-chrome/Default/Cookies'
assert_contains "$output" 'Close the other terminals' 'deleting history must warn about open shells'

###############################################################
# => Editor databases the helper cannot fully clean
###############################################################

# When the vacuum fails the rows are gone but older copies may not be: say that, instead of
# calling the whole edit a failure, and do not claim the run is complete.
set_machine
put '.config/Cursor/User/globalStorage/state.vscdb' 'not read: the python3 stub answers'
case_real=(ssh-keygen id)
case_stubs+=(python3)
case_env+=('STUB_COMPACT_NOTE=could not compact the file (database or disk is full): older copies of the rows may remain in it')
run_logout --only apps --yes
assert_equals 1 "$status" 'a failed vacuum must make the run incomplete'
assert_contains "$output" 'removed 3 sign-in token(s) and secret(s)' 'a failed vacuum must still report the rows that were removed'
assert_contains "$output" 'could not compact the file (database or disk is full)' 'a failed vacuum must say why'
assert_contains "$output" 'Did not finish: Cursor' 'a failed vacuum must be named in the summary'
assert_not_contains "$output" 'FAILED to edit' 'a removed row is not a failed edit'

# A big database takes a while to vacuum: the plan says so before anything is asked.
set_machine
put '.config/Cursor/User/globalStorage/state.vscdb' 'sparse'
truncate -s 300M "$home/.config/Cursor/User/globalStorage/state.vscdb"
case_real=(ssh-keygen id)
case_stubs+=(python3)
run_logout --dry-run
assert_contains "$output" 'then compact the 300.0 MiB file, which takes a while' 'the plan must warn about a big database'
set_machine
make_editor_db "$home/.config/Cursor"
run_logout --dry-run
assert_not_contains "$output" 'which takes a while' 'the plan must not warn about a small database'

###############################################################
# => Changes that only the write-ahead log holds
###############################################################

# An editor that is running, or that crashed, leaves its newest rows in the -wal file. A reader
# that ignored it would report a signed-in editor as clean.
if [[ -n $python_bin ]]; then
    set_machine
    make_crashed_editor_db "$home/.config/Cursor"
    db="$home/.config/Cursor/User/globalStorage/state.vscdb"
    [[ -s $db-wal ]] || fail 'the fixture must leave its rows in the -wal file'
    secret_is_gone "$db" SECRET-ACCESS-TOKEN-1111 || fail 'the fixture must not have merged its rows into the database file'

    run_logout --audit
    assert_contains "$output" 'Cursor keeps 3 login token(s) in plain text' '--audit must count the tokens that only the -wal file holds'

    run_logout --dry-run
    assert_contains "$output" 'would remove 3 sign-in token(s) and secret(s)' 'the plan must count the rows that only the -wal file holds'

    run_logout --only apps --yes
    assert_equals 0 "$status" 'cleaning a database with a pending -wal file must succeed'
    assert_equals 'workbench.chat.history' "$(editor_keys "$db")" 'the sign-in rows of a pending -wal file must go, the rest stay'
    for file in "$db" "$db-wal"; do
        if [[ -e $file ]]; then
            for secret in SECRET-ACCESS-TOKEN-1111 SECRET-REFRESH-TOKEN-2222 SECRET-OAUTH-TOKEN-4444; do
                secret_is_gone "$file" "$secret" || fail "$secret must not stay in ${file##*/}"
            done
        fi
    done
fi

###############################################################
# => Groups
###############################################################

set_machine
build_home
run_logout --only files --yes
assert_equals 0 "$status" '--only files must succeed'
assert_missing .git-credentials .pi/agent/auth.json
assert_exists '.config/google-chrome/Default/Cookies' '.mozilla/firefox/abc.default/logins.json' '.config/Cursor/Cookies'
assert_not_contains "$calls" 'sudo' '--only files must not touch sudo'

set_machine
build_home
run_logout --skip=browsers,apps --yes
assert_equals 0 "$status" '--skip must succeed'
assert_missing .git-credentials
assert_exists '.config/google-chrome/Default/Cookies' '.mozilla/firefox/abc.default/logins.json' '.config/Cursor/Cookies' '.config/obsidian/Cookies'
assert_contains "$calls" 'sudo -K' '--skip browsers,apps must still run the other groups'

###############################################################
# => Running apps
###############################################################

set_machine
build_home
case_env+=(STUB_RUNNING='chrome cursor')
before_chrome="$(cd "$home/.config/google-chrome" && find . -type f -exec sha256sum {} + | LC_ALL=C sort)"
run_logout --yes
assert_equals 1 "$status" 'a skipped app must make the run incomplete'
assert_contains "$output" 'Google Chrome is running: close it first (skipped)' 'a running browser must be reported'
assert_contains "$output" 'Cursor is running: close it first (skipped)' 'a running editor must be reported'
assert_contains "$output" 'Skipped because they are running: Google Chrome, Cursor' 'the summary must name what was skipped'
assert_equals "$before_chrome" "$(cd "$home/.config/google-chrome" && find . -type f -exec sha256sum {} + | LC_ALL=C sort)" \
    'a running browser must keep every file'
assert_exists '.config/Cursor/Cookies' '.config/Cursor/User/globalStorage/state.vscdb'
assert_missing .mozilla/firefox/abc.default/cookies.sqlite
assert_missing .git-credentials
assert_not_contains "$output" 'plain terminal' 'outside an editor the hint must not talk about editor terminals'

# From an editor's own terminal the hint says to leave it.
case_env+=(TERM_PROGRAM=vscode)
run_logout --dry-run
assert_contains "$output" 'plain terminal' 'inside an editor the hint must say to open a plain terminal'

# Process names are matched exactly: chrome-sandbox or a name that merely contains chrome is not Chrome.
set_machine
build_home
case_env+=(STUB_RUNNING='chrome-helper mychrome')
run_logout --yes
assert_equals 0 "$status" 'a similar process name is not the app'
assert_missing '.config/google-chrome/Default/Cookies'

# Without pgrep nothing can be checked, and a wipe under a live app is the worse mistake.
set_machine
build_home
case_stubs=(gh docker sudo ssh-add busctl)
run_logout --yes
assert_equals 1 "$status" 'without pgrep the apps must be skipped'
assert_contains "$output" 'pgrep is not installed' 'a missing pgrep must be reported'
assert_exists '.config/google-chrome/Default/Cookies' '.mozilla/firefox/abc.default/cookies.sqlite' '.config/Cursor/Cookies'
assert_missing .git-credentials

###############################################################
# => Symbolic links
###############################################################

set_machine
build_home
printf 'cookie jar\n' > "$outside/cookies"
printf 'password\n' > "$outside/credentials"
mkdir -p "$outside/storage"
printf 'precious\n' > "$outside/storage/data"
rm -f "$home/.config/google-chrome/Default/Cookies" "$home/.git-credentials"
rm -rf "$home/.config/google-chrome/Default/Local Storage"
ln -s "$outside/cookies" "$home/.config/google-chrome/Default/Cookies"
ln -s "$outside/credentials" "$home/.git-credentials"
ln -s "$outside/storage" "$home/.config/google-chrome/Default/Local Storage"
run_logout --yes
assert_equals 0 "$status" 'links must be skipped, not treated as failures'
assert_contains "$output" 'left alone (symbolic link)' 'a link must be reported'
assert_equals 'cookie jar' "$(< "$outside/cookies")" 'a link to a file must not be followed'
assert_equals 'password' "$(< "$outside/credentials")" 'a link to a token file must not be followed'
assert_equals 'precious' "$(< "$outside/storage/data")" 'a link to a folder must not be followed'
assert_exists '.config/google-chrome/Default/Cookies' .git-credentials
assert_missing '.config/google-chrome/Default/Login Data'

###############################################################
# => Failures
###############################################################

set_machine
build_home
case_env+=("STUB_GH_STATUS=$gh_status_two_accounts" STUB_AGENT_LIST_STATUS=0
    'STUB_FAIL=gh auth logout --hostname github.com --user alice')
run_logout --yes
assert_equals 1 "$status" 'a failed sign-out must fail the run'
assert_contains "$output" 'FAILED (exit 1)' 'a failed step must be marked'
assert_contains "$output" 'gh: the request failed' "the tool's own error must be shown"
assert_contains "$output" 'Did not finish: GitHub CLI: alice on github.com' 'the failed step must be named in the summary'
assert_contains "$calls" 'gh auth logout --hostname github.com --user bob' 'a failed step must not stop the next one'
assert_contains "$calls" 'docker logout nvcr.io' 'a failed step must not stop the steps after it'
assert_missing .git-credentials '.config/google-chrome/Default/Cookies'
assert_contains "$calls" ' Lock ' 'the keyring must still be locked after a failure'

# A courtesy that cannot be done is no failure: here the keyring will not lock.
set_machine
build_home
case_env+=('STUB_FAIL=busctl --user call org.freedesktop.secrets /org/freedesktop/secrets org.freedesktop.Secret.Service Lock')
run_logout --yes
assert_equals 0 "$status" 'a keyring that will not lock is not a failure'
assert_contains "$output" 'not possible here' 'a courtesy that failed must say so'

# A keyring that is not running is not started just to be locked.
set_machine
build_home
case_env+=(STUB_KEYRING=0)
run_logout --yes
assert_not_contains "$calls" 'ReadAlias' 'a keyring that is not running must not be asked anything'
assert_not_contains "$calls" ' Lock ' 'a keyring that is not running must not be locked'

###############################################################
# => Tools that are missing, and gh that is offline
###############################################################

set_machine
build_home
case_stubs=(pgrep)
run_logout --yes
assert_equals 0 "$status" 'missing CLI tools are not an error'
assert_not_contains "$output" 'GitHub CLI' 'a tool that is not installed must not be mentioned'
assert_not_contains "$output" 'Docker' 'a tool that is not installed must not be mentioned'
assert_not_contains "$output" 'sudo' 'a tool that is not installed must not be mentioned'
assert_missing .git-credentials '.config/google-chrome/Default/Cookies'

# Without python3 the editor sign-ins cannot be edited: say so, and do the rest.
set_machine
build_home
case_real=(ssh-keygen id)
case_env+=("STUB_GH_STATUS=$gh_status_two_accounts")
run_logout --yes
assert_equals 0 "$status" 'a missing python3 is not an error'
assert_contains "$output" 'python3 is not installed' 'a missing python3 must be reported'
assert_missing .git-credentials '.config/google-chrome/Default/Cookies'

# gh checks its tokens online. Offline it lists no account, and its token file must not survive.
set_machine
put '.config/gh/hosts.yml' $'github.com:\n    oauth_token: gho_notarealtoken'
run_logout --yes
assert_equals 0 "$status" 'an offline gh must not fail the run'
assert_contains "$output" 'it listed no account (offline?)' 'the fallback must say why it deletes the file'
assert_missing .config/gh/hosts.yml

# GH_CONFIG_DIR is the folder that holds hosts.yml, and XDG_CONFIG_HOME holds a gh folder.
set_machine
put 'elsewhere/hosts.yml' $'github.com:\n    oauth_token: gho_notarealtoken'
case_env+=("GH_CONFIG_DIR=$home/elsewhere")
run_logout --yes
assert_missing elsewhere/hosts.yml
set_machine
put 'xdg/gh/hosts.yml' $'github.com:\n    oauth_token: gho_notarealtoken'
case_env+=("XDG_CONFIG_HOME=$home/xdg")
run_logout --yes
assert_missing xdg/gh/hosts.yml

###############################################################
# => A home folder whose name has a space and glob characters
###############################################################

set_machine
home="$case_dir/ho me [1]"
rm -rf "$home"
mkdir -p "$home"
build_home
run_logout --yes
assert_equals 0 "$status" 'odd characters in HOME must not break the run'
assert_missing '.config/google-chrome/Default/Login Data' '.config/google-chrome/Default/Cookies' .git-credentials
assert_exists '.config/google-chrome/Default/Bookmarks' '.mozilla/firefox/abc.default/key4.db'
rm -rf "$home"
home="$default_home"

###############################################################
# => --audit
###############################################################

set_machine
build_home
put 'polcg_nvidia_key.txt' 'nvapi-ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcd'
put 'project/.env' $'API_TOKEN=sk-ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ab\nDB_PASSWORD=hunter2hunter2\nPLAIN=1'
put 'repo/.git/config' $'[remote "origin"]\n\turl = https://ghp_ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789ab@github.com/x/y.git'
if command -v ssh-keygen > /dev/null 2>&1; then
    mkdir -p "$home/.ssh"
    ssh-keygen -q -t ed25519 -N '' -f "$home/.ssh/open_key" > /dev/null
    ssh-keygen -q -t ed25519 -N 'a passphrase' -f "$home/.ssh/locked_key" > /dev/null
fi
case_env+=("STUB_GH_STATUS=$gh_status_two_accounts")
before="$(state_of_home)"
run_logout --audit
assert_equals 0 "$status" '--audit must succeed'
assert_same_state "$before" '--audit must change nothing'
assert_equals '' "$(grep -E 'auth logout|docker logout|sudo|ssh-add -D| Lock ' <<< "$calls" || true)" '--audit must run no sign-out'
assert_contains "$output" 'Credentials on this machine' '--audit must print its title'
assert_contains "$output" '~/.git-credentials' '--audit must list the Git credential store'
assert_contains "$output" 'for github.com' '--audit must name the host of the Git credentials'
assert_contains "$output" '~/.pi/agent/auth.json' '--audit must list the AI tool logins'
assert_contains "$output" 'Docker login for nvcr.io' '--audit must list the Docker login'
assert_contains "$output" 'GitHub CLI: alice' '--audit must list the gh accounts'
assert_contains "$output" 'token is in the keyring' '--audit must say that a keyring token is protected'
assert_contains "$output" 'GitHub CLI token for bob' '--audit must flag a gh token in plain text'
assert_contains "$output" '~/project/.env' '--audit must list the .env file'
assert_contains "$output" 'API_TOKEN, DB_PASSWORD' '--audit must name the secret variables of an .env file'
assert_not_contains "$output" 'PLAIN' '--audit must not list a variable that is not a secret'
assert_contains "$output" 'looks like a NVIDIA API key' '--audit must find an API key by its shape'
assert_contains "$output" 'a git remote URL contains a password or token' '--audit must find a token in a remote URL'
assert_contains "$output" 'Revoke it at the source' '--audit must say where to revoke'
if command -v ssh-keygen > /dev/null 2>&1; then
    assert_contains "$output" 'WITHOUT a passphrase' '--audit must flag an unprotected SSH key'
    assert_contains "$output" '~/.ssh/open_key' '--audit must name the unprotected SSH key'
    assert_contains "$output" 'protected by a passphrase' '--audit must pass a protected SSH key'
fi
if [[ -n $python_bin ]]; then
    assert_contains "$output" 'Cursor keeps 3 login token(s) in plain text' '--audit must count the editor tokens'
fi
assert_contains "$output" 'Google Chrome / Default' '--audit must list the browser profiles'
assert_contains "$output" 'signed in to Google: yes' '--audit must say whether Chrome is signed in'

# Not one secret value may reach the output.
for secret in s3cretpassw0rd nvapi-ABCDEFGHIJ sk-ABCDEFGHIJ hunter2hunter2 ghp_ABCDEFGHIJ \
    SECRET-ACCESS-TOKEN SECRET-REFRESH-TOKEN SECRET-OAUTH-TOKEN dXNlcjpwYXNz; do
    assert_not_contains "$output" "$secret" "--audit must never print a secret value ($secret)"
done

printf 'All logout-all tests passed.\n'
