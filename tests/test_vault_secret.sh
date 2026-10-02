#!/usr/bin/env bash
# Tests for .local/scripts/bin/vault-secret.
#
# The vault's credentials are SOPS + age, and the age identity decides who can read them. A plain
# keys.txt can be read by anyone with sudo, so on a work machine the identity is kept
# passphrase-protected (keys.txt.gpg), and the helper must:
#   - point sops at that copy (SOPS_AGE_KEY_CMD) only when nothing else is configured and sops'
#     own default file is absent, with the path quoted the way sops reads that variable;
#   - never let the identity out: key-wrap takes it from stdin, hands it to gpg through a pipe and
#     never prints it, passes it as an argument or leaves it in a file in the clear;
#   - save a wrapped key only if that copy alone opens the store;
#   - never print a decrypted value from key-status.
#
# Part 1 runs the script against stub sops and gpg commands. Part 2 runs the real sops, age and
# gpg with a throwaway key, a throwaway GNUPGHOME and a stub pinentry that types a fixed
# passphrase, so no dialog opens and the real ~/.gnupg and its agent are never touched.

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
script="$repo_root/.local/scripts/bin/vault-secret"
bash_bin="$(command -v bash)"
env_bin="$(command -v env)"
python_bin="$(command -v python3 || true)"
case_dir="$(mktemp -d)"
real_gnupg=''

# stop_real_agent: stop the throwaway gpg-agent of part 2, in the empty environment it was started
# in. It is keyed on its own GNUPGHOME inside the case folder: never stop any other agent.
stop_real_agent() {
    case "$real_gnupg" in
        "$case_dir"/*)
            "$env_bin" -i HOME="$case_dir" GNUPGHOME="$real_gnupg" PATH=/usr/bin:/bin \
                gpgconf --kill gpg-agent >/dev/null 2>&1 || true
            ;;
    esac
}

# remove_real_socketdir: for a GNUPGHOME that is not the default one, gpg keeps the agent's sockets in
# /run/user/UID/gnupg/d.HASH and leaves that empty folder behind when the agent is gone. rmdir
# removes only an empty folder, and only the one of the throwaway GNUPGHOME is ever looked up.
remove_real_socketdir() {
    local socket_dir
    case "$real_gnupg" in
        "$case_dir"/*)
            socket_dir=$("$env_bin" -i HOME="$case_dir" GNUPGHOME="$real_gnupg" PATH=/usr/bin:/bin \
                gpgconf --list-dirs socketdir 2>/dev/null) || return 0
            case "$socket_dir" in
                */gnupg/d.*) rmdir "$socket_dir" 2>/dev/null || true ;;
            esac
            ;;
    esac
}

cleanup() {
    stop_real_agent
    remove_real_socketdir
    chmod -R u+rwX "$case_dir" 2>/dev/null || true
    rm -rf "$case_dir"
}
trap cleanup EXIT

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

assert_file() {
    [[ -f $1 ]] || fail "$2 (no such file: $1)"
}

assert_no_file() {
    [[ ! -e $1 ]] || fail "$2 (exists: $1)"
}

assert_fails() {
    [[ $1 != 0 ]] || fail "$2 (it succeeded)"
}

file_mode() {
    stat -c %a "$1" 2>/dev/null || stat -f %Lp "$1"
}

[[ -x $script ]] || fail 'vault-secret must be executable'

###############################################################
# => Part 1: the helper against stub sops and gpg
###############################################################

# The identity is built here because a literal one in the source would be a leaked-key finding.
fake_identity="AGE-SECRET-KEY-1$(printf 'Q%.0s' {1..58})"

vault="$case_dir/vault"
stubs="$case_dir/stubs"
tmp_dir="$case_dir/tmp"
log="$case_dir/calls.log"
stdin_file="$case_dir/stdin"
stdin_copy="$case_dir/gpg-stdin"
mkdir -p "$vault/Secrets" "$stubs"
cat > "$vault/Secrets/credentials.enc.yaml" <<'EOF'
credentials:
    camera_cpd:
        username: ENC[AES256_GCM,data:x,iv:x,tag:x,type:str]
        password: ENC[AES256_GCM,data:x,iv:x,tag:x,type:str]
    horse_vpn:
        context: ENC[AES256_GCM,data:x,iv:x,tag:x,type:str]
    nx_access:
        primary_account:
            username: ENC[AES256_GCM,data:x,iv:x,tag:x,type:str]
sops:
    version: 3.13.2
EOF

write_stub() {
    cat > "$stubs/$1"
    chmod +x "$stubs/$1"
}

# Stand-in for sops: records how it was called (never the value of a key), then answers.
write_stub sops <<'EOF'
#!/usr/bin/env bash
{
    printf 'ARGS:'
    printf ' <%s>' "$@"
    printf '\n'
    printf 'KEYCMD:%s\n' "${SOPS_AGE_KEY_CMD-<unset>}"
    printf 'KEYFILE:%s\n' "${SOPS_AGE_KEY_FILE-<unset>}"
    printf 'KEY:%s\n' "${SOPS_AGE_KEY+<set>}"
    printf 'XDG:%s\n' "${XDG_CONFIG_HOME-<unset>}"
} >> "$STUB_LOG"
if [ "${STUB_SOPS_RC:-0}" != 0 ]; then
    echo 'stub sops: failed' >&2
    exit "$STUB_SOPS_RC"
fi
printf 'STUB-SECRET-VALUE\n'
EOF

# Stand-in for gpg: records the arguments and the environment (never the data), keeps what it
# was given on stdin where the test can compare it, and writes a marker as the "encrypted" file.
write_stub gpg <<'EOF'
#!/usr/bin/env bash
{
    printf 'GPG-ARGS:'
    printf ' <%s>' "$@"
    printf '\n'
} >> "$STUB_LOG"
env >> "$STUB_LOG.env"
if [ "${STUB_GPG_RC:-0}" != 0 ]; then
    exit "$STUB_GPG_RC"
fi
output=''
while [ "$#" -gt 0 ]; do
    if [ "$1" = --output ]; then
        output=$2
        shift
    fi
    shift
done
cat > "$STUB_GPG_STDIN"
printf 'STUB-WRAPPED\n' > "$output"
EOF

case_env=()
home=''

# set_machine [HOME]: an empty home with no identity, no recorded calls and no extra environment.
set_machine() {
    home=${1:-$case_dir/home}
    rm -rf "${case_dir:?}/home" "${home:?}" "${tmp_dir:?}"
    mkdir -p "$home" "$tmp_dir"
    : > "$log"
    : > "$log.env"
    : > "$stdin_file"
    rm -f "$stdin_copy"
    case_env=()
}

# add_key plain|wrapped [CONFIG_DIR]: a placeholder identity where sops (or the helper) looks.
add_key() {
    local dir=${2:-$home/.config}/sops/age
    mkdir -p "$dir"
    if [[ $1 == plain ]]; then
        printf 'placeholder\n' > "$dir/keys.txt"
    else
        printf 'placeholder\n' > "$dir/keys.txt.gpg"
    fi
}

# run_vs [ARGS...]: run the script in an otherwise empty environment. Sets status, out (stdout),
# err (stderr). Standard input is the file $stdin_file, never a terminal.
run_vs() {
    local item
    local -a run_env=(
        HOME="$home" PATH="$stubs:/usr/bin:/bin" VAULT_ROOT="$vault" TMPDIR="$tmp_dir" LC_ALL=C
        STUB_LOG="$log" STUB_GPG_STDIN="$stdin_copy"
    )
    for item in ${case_env[@]+"${case_env[@]}"}; do
        run_env+=("$item")
    done
    status=0
    out="$("$env_bin" -i "${run_env[@]}" "$bash_bin" "$script" "$@" 2>"$case_dir/stderr" < "$stdin_file")" || status=$?
    err="$(< "$case_dir/stderr")"
}

calls() {
    cat "$log"
}

store_path="$vault/Secrets/credentials.enc.yaml"

# --- list, help and argument errors ------------------------------------------------------

set_machine
run_vs list
assert_equals 0 "$status" 'list must succeed'
assert_equals $'camera_cpd\nhorse_vpn\nnx_access' "$out" 'list must print the credential names'
assert_equals '' "$(calls)" 'list must not run sops: it decrypts nothing'

for flag in --help -h; do
    set_machine
    case_env=(VAULT_ROOT=/nonexistent)
    run_vs "$flag"
    assert_equals 0 "$status" "$flag must succeed even without a vault"
    assert_contains "$out" 'Usage: vault-secret' "$flag must print the usage"
    assert_contains "$out" 'key-wrap' "$flag must describe key-wrap"
    assert_contains "$out" 'key-status' "$flag must describe key-status"
done

set_machine
run_vs list edit
assert_equals 2 "$status" 'two arguments are a usage error'
assert_contains "$err" 'Usage: vault-secret' 'the usage goes to stderr on an error'

set_machine
case_env=(VAULT_ROOT=/nonexistent)
run_vs list
assert_equals 1 "$status" 'a missing vault must fail'
assert_contains "$err" 'Set VAULT_ROOT' 'a missing vault must say how to point at it'

# shellcheck disable=SC2016  # the shell metacharacters are the point: they must stay literal
for bad in 'a.b' 'a/b' 'a b' '../x' 'x;y' '$(id)' '*' ''; do
    set_machine
    add_key plain
    run_vs "$bad"
    assert_equals 2 "$status" "the entry name '$bad' must be rejected"
    assert_contains "$err" 'Invalid entry name' "the entry name '$bad' must be explained"
    assert_equals '' "$(calls)" "the entry name '$bad' must never reach sops"
done

# --- the numbered selector ---------------------------------------------------------------

set_machine
add_key plain
printf '2\n' > "$stdin_file"
run_vs
assert_equals 0 "$status" 'the selector must decrypt the chosen entry'
assert_contains "$out" '1) camera_cpd' 'the selector must list the entries'
assert_contains "$(calls)" '["credentials"]["horse_vpn"]' 'choice 2 must decrypt the second entry'

for answer in q 0 9 x ''; do
    set_machine
    add_key plain
    printf '%s\n' "$answer" > "$stdin_file"
    run_vs
    case $answer in
        q) assert_equals 0 "$status" 'q must cancel quietly' ;;
        *) assert_equals 2 "$status" "the selector answer '$answer' must be rejected" ;;
    esac
    assert_equals '' "$(calls)" "the selector answer '$answer' must not run sops"
done

# --- which identity sops is given ---------------------------------------------------------

set_machine
add_key plain
run_vs camera_cpd
assert_equals 0 "$status" 'an entry must decrypt'
assert_equals 'STUB-SECRET-VALUE' "$out" 'the entry output is what sops printed, nothing else'
assert_contains "$(calls)" "ARGS: <decrypt> <--extract> <[\"credentials\"][\"camera_cpd\"]> <$store_path>" \
    'an entry must be extracted from the store with sops decrypt --extract'
assert_contains "$(calls)" 'KEYCMD:<unset>' 'a plain keys.txt is sops default: the helper must not add a key command'

set_machine
run_vs camera_cpd
assert_equals 1 "$status" 'without any identity the helper must stop'
assert_contains "$err" 'vault-secret key-wrap' 'without any identity the helper must say how to create one'
assert_equals '' "$(calls)" 'without any identity sops must not run: it would only fail noisily'

set_machine
add_key wrapped
run_vs camera_cpd
assert_equals 0 "$status" 'a wrapped identity must be usable'
assert_contains "$(calls)" "KEYCMD:gpg --quiet --batch --decrypt '$home/.config/sops/age/keys.txt.gpg'" \
    'a wrapped identity must reach sops as a key command that runs gpg'

# The key command is read as shell words without a shell: every character of the path must survive.
set_machine "$case_dir/ho me/it's \"x\" \$HOME"
add_key wrapped
run_vs camera_cpd
assert_equals 0 "$status" 'an awkward home folder must work'
assert_contains "$(calls)" "KEYCMD:gpg --quiet --batch --decrypt '$case_dir/ho me/it'\\''s \"x\" \$HOME/.config/sops/age/keys.txt.gpg'" \
    'a quote, a space and a dollar sign in the path must be quoted for sops'

set_machine
add_key plain
add_key wrapped
run_vs camera_cpd
assert_contains "$(calls)" 'KEYCMD:<unset>' 'sops default keys.txt wins over the wrapped copy'

# What the user configured for sops is never overridden.
set_machine
add_key wrapped
case_env=(SOPS_AGE_KEY_CMD=my-own-command)
run_vs camera_cpd
assert_contains "$(calls)" 'KEYCMD:my-own-command' 'an own SOPS_AGE_KEY_CMD must be kept'

set_machine
add_key wrapped
case_env=(SOPS_AGE_KEY_FILE=/some/own/keys.txt)
run_vs camera_cpd
assert_contains "$(calls)" 'KEYCMD:<unset>' 'an own SOPS_AGE_KEY_FILE must not get a key command next to it'
assert_contains "$(calls)" 'KEYFILE:/some/own/keys.txt' 'an own SOPS_AGE_KEY_FILE must be kept'

set_machine
add_key wrapped
case_env=(SOPS_AGE_KEY=placeholder)
run_vs camera_cpd
assert_contains "$(calls)" 'KEYCMD:<unset>' 'an own SOPS_AGE_KEY must not get a key command next to it'
assert_contains "$(calls)" 'KEY:<set>' 'an own SOPS_AGE_KEY must be kept'

# XDG_CONFIG_HOME moves sops' default folder, so it moves the wrapped copy too.
set_machine
add_key plain
add_key wrapped "$case_dir/xdg"
case_env=(XDG_CONFIG_HOME="$case_dir/xdg")
run_vs camera_cpd
assert_contains "$(calls)" "KEYCMD:gpg --quiet --batch --decrypt '$case_dir/xdg/sops/age/keys.txt.gpg'" \
    'XDG_CONFIG_HOME must decide where the wrapped copy is, and a plain file in ~/.config must not count'

set_machine
add_key wrapped
run_vs edit
assert_equals 0 "$status" 'edit must run'
assert_contains "$(calls)" "ARGS: <edit> <$store_path>" 'edit must open the store with sops edit'
assert_contains "$(calls)" "KEYCMD:gpg --quiet --batch --decrypt '$home/.config/sops/age/keys.txt.gpg'" \
    'edit must use the wrapped identity too'

set_machine
run_vs edit
assert_equals 1 "$status" 'edit without any identity must stop'
assert_equals '' "$(calls)" 'edit without any identity must not run sops'

set_machine
add_key plain
case_env=(VAULT_SECRET_SOPS=/nonexistent/sops)
run_vs camera_cpd
assert_equals 127 "$status" 'a missing sops must be reported'
assert_contains "$err" 'sops is not installed' 'a missing sops must be explained'

# Without gpg a wrapped identity cannot be used, and saying so beats sops' generic failure.
nogpg_bin="$case_dir/nogpg-bin"
mkdir -p "$nogpg_bin"
for tool in uname awk grep cat mktemp rm env tty mv mkdir chmod; do
    ln -s "$(command -v "$tool")" "$nogpg_bin/$tool"
done

set_machine
add_key wrapped
case_env=(PATH="$nogpg_bin" VAULT_SECRET_SOPS="$stubs/sops")
run_vs camera_cpd
assert_equals 127 "$status" 'a wrapped identity without gpg must be reported'
assert_contains "$err" 'gpg is not installed' 'a wrapped identity without gpg must be explained'
assert_equals '' "$(calls)" 'a wrapped identity without gpg must not run sops'

set_machine
case_env=(PATH="$nogpg_bin" VAULT_SECRET_SOPS="$stubs/sops")
printf '%s\n' "$fake_identity" > "$stdin_file"
run_vs key-wrap
assert_equals 127 "$status" 'key-wrap without gpg must be reported'
assert_contains "$err" 'gpg is not installed' 'key-wrap without gpg must be explained'

# --- key-status ---------------------------------------------------------------------------

set_machine
run_vs key-status
assert_equals 1 "$status" 'key-status without an identity must fail'
assert_contains "$out" 'none found' 'key-status must say there is no identity'
assert_contains "$out" 'vault-secret key-wrap' 'key-status must say how to create one'
assert_equals '' "$(calls)" 'key-status without an identity must not run sops'

set_machine
add_key plain
run_vs key-status
assert_equals 0 "$status" 'key-status with a plain identity that opens the store must succeed'
assert_contains "$out" "plain file $home/.config/sops/age/keys.txt" 'key-status must name the plain file'
assert_contains "$out" 'anyone who can read your home folder' 'key-status must warn about a plain file'
assert_contains "$out" 'Opens store:  yes' 'key-status must report that the store opens'
assert_contains "$out" 'key-wrap < ' 'key-status must show how to wrap an existing plain file'
assert_not_contains "$out$err" 'STUB-SECRET-VALUE' 'key-status must never print what sops decrypted'

set_machine
add_key wrapped
run_vs key-status
assert_equals 0 "$status" 'key-status with a wrapped identity must succeed'
assert_contains "$out" 'passphrase-protected' 'key-status must report a wrapped identity'
assert_contains "$out" 'a passphrase prompt may appear' 'key-status must warn that testing can prompt'
assert_contains "$(calls)" 'KEYCMD:gpg --quiet --batch --decrypt' 'key-status must test the wrapped identity'
assert_not_contains "$out$err" 'STUB-SECRET-VALUE' 'key-status must never print what sops decrypted'

set_machine
case_env=(SOPS_AGE_KEY_FILE=/some/own/keys.txt)
run_vs key-status
assert_equals 0 "$status" 'key-status with an environment identity must succeed'
assert_contains "$out" 'set in the environment' 'key-status must report an environment identity'

set_machine
add_key plain
case_env=(STUB_SOPS_RC=1)
run_vs key-status
assert_equals 1 "$status" 'key-status must fail when the identity does not open the store'
assert_contains "$out" 'Opens store:  no' 'key-status must report that the store does not open'

# --- key-wrap -----------------------------------------------------------------------------

identity_input=$'# created: 2026-01-01T00:00:00Z\n# public key: age1example\n'"$fake_identity"$'\n'
wrapped_target() {
    printf '%s/.config/sops/age/keys.txt.gpg' "$home"
}

set_machine
add_key wrapped
printf '%s' "$identity_input" > "$stdin_file"
run_vs key-wrap
assert_equals 1 "$status" 'key-wrap must not overwrite an existing wrapped key'
assert_contains "$err" 'already exists' 'key-wrap must explain the refusal'
assert_equals '' "$(calls)" 'a refused key-wrap must not run gpg or sops'
assert_equals 'placeholder' "$(< "$(wrapped_target)")" 'a refused key-wrap must leave the existing key as it was'

set_machine
printf 'this is not a key, MY-SENTINEL-INPUT\n' > "$stdin_file"
run_vs key-wrap
assert_equals 2 "$status" 'key-wrap must reject input without an age secret key'
assert_contains "$err" 'No AGE-SECRET-KEY-1' 'key-wrap must explain the rejection'
assert_equals '' "$(calls)" 'rejected input must not reach gpg or sops'
assert_not_contains "$out$err" 'MY-SENTINEL-INPUT' 'key-wrap must not echo what it was given'
assert_no_file "$(wrapped_target)" 'rejected input must save nothing'

set_machine
printf '%s' "$identity_input" > "$stdin_file"
case_env=(SOPS_AGE_KEY_FILE=/elsewhere/keys.txt SOPS_AGE_KEY=ambient-key)
run_vs key-wrap
assert_equals 0 "$status" "key-wrap must succeed. Output: $out $err"
assert_file "$(wrapped_target)" 'key-wrap must save keys.txt.gpg'
assert_equals 'STUB-WRAPPED' "$(< "$(wrapped_target)")" 'key-wrap must save what gpg produced'
assert_equals 600 "$(file_mode "$(wrapped_target)")" 'the wrapped key must be private to its owner'
assert_equals 700 "$(file_mode "$home/.config/sops/age")" 'the folder created for the key must be private to its owner'
assert_equals "${identity_input}x" "$(cat "$stdin_copy"; printf x)" 'gpg must receive the input exactly'
assert_contains "$(calls)" 'GPG-ARGS: <--quiet> <--symmetric> <--cipher-algo> <AES256> <--output> <' \
    'the identity must be encrypted with a passphrase (symmetric AES256)'
assert_contains "$out" 'Saved' 'key-wrap must say it saved the key'
assert_not_contains "$(calls)" "$fake_identity" 'the identity must never be an argument'
assert_not_contains "$(< "$log.env")" "$fake_identity" 'the identity must never be in the environment of gpg'
assert_not_contains "$out$err" "$fake_identity" 'the identity must never be printed'
# The check ran on the new copy alone: not on an environment key nor on sops' default folder.
assert_contains "$(calls)" 'ARGS: <decrypt> <--extract> <["credentials"]>' 'key-wrap must check that the key opens the store'
assert_contains "$(calls)" 'KEYFILE:<unset>' 'the check must not use an environment key file'
assert_not_contains "$(calls)" 'KEY:<set>' 'the check must not use an environment key'
assert_contains "$(calls)" "XDG:$tmp_dir/" 'the check must look for sops default key in an empty folder'
assert_contains "$(calls)" "keys.txt.gpg'" 'the check must use the new copy as key command'
assert_equals '' "$(find "$tmp_dir" -mindepth 1)" 'key-wrap must clean up its scratch folder'
if grep -rqF "$fake_identity" "$home" "$tmp_dir" "$case_dir/stderr" 2>/dev/null; then
    fail 'the identity must not be written anywhere under the home folder or the temp folder'
fi

set_machine
printf '%s' "$identity_input" > "$stdin_file"
case_env=(STUB_SOPS_RC=1)
run_vs key-wrap
assert_equals 1 "$status" 'key-wrap must fail when the key does not open the store'
assert_contains "$err" 'Nothing was saved' 'key-wrap must say it saved nothing'
assert_no_file "$(wrapped_target)" 'a key that does not open the store must not be saved'
assert_equals '' "$(find "$tmp_dir" -mindepth 1)" 'a failed key-wrap must clean up its scratch folder'

set_machine
printf '%s' "$identity_input" > "$stdin_file"
case_env=(STUB_GPG_RC=2)
run_vs key-wrap
assert_equals 1 "$status" 'key-wrap must fail when gpg fails'
assert_contains "$err" 'Nothing was saved' 'key-wrap must say it saved nothing when gpg fails'
assert_no_file "$(wrapped_target)" 'a gpg failure must save nothing'
assert_not_contains "$(calls)" 'ARGS: <decrypt>' 'a gpg failure must stop before the check'
assert_equals '' "$(find "$tmp_dir" -mindepth 1)" 'a gpg failure must clean up its scratch folder'

set_machine
add_key plain
printf '%s' "$identity_input" > "$stdin_file"
run_vs key-wrap
assert_equals 0 "$status" 'key-wrap must work next to a plain file'
assert_contains "$out" 'still there' 'key-wrap must warn that the plain file is still readable'
assert_contains "$out" 'shred -u' 'key-wrap must say how to delete the plain file'
assert_equals 'placeholder' "$(< "$home/.config/sops/age/keys.txt")" 'key-wrap must never delete the plain file itself'

# The identity can be typed on a terminal: it must not be echoed.
if [[ -n $python_bin ]]; then
    pty_helper="$case_dir/pty_session.py"
    cat > "$pty_helper" <<'EOF'
"""Run a command on a pseudo-terminal, type SEND after EXPECT appears and save the output.

usage: pty_session.py OUTPUT_FILE EXPECT SEND -- COMMAND [ARG]...
"""
import os
import pty
import select
import signal
import sys
import time

output_file, expect, send = sys.argv[1:4]
command = sys.argv[sys.argv.index('--') + 1:]
pid, fd = pty.fork()
if pid == 0:
    try:
        os.execv(command[0], command)
    finally:
        os._exit(127)

captured = b''
sent = False
deadline = time.monotonic() + 20
while time.monotonic() < deadline:
    readable, _, _ = select.select([fd], [], [], 1.0)
    if not readable:
        continue
    try:
        chunk = os.read(fd, 4096)
    except OSError:
        break
    if not chunk:
        break
    captured += chunk
    if not sent and expect.encode() in captured:
        # The prompt is printed just before the script turns echo off: let it get there first.
        time.sleep(0.3)
        os.write(fd, send.encode())
        sent = True
else:
    os.kill(pid, signal.SIGKILL)
    sys.exit(124)

_, wait_status = os.waitpid(pid, 0)
with open(output_file, 'w', encoding='utf-8') as handle:
    handle.write(captured.decode('utf-8', errors='replace').replace('\r\n', '\n'))
sys.exit(os.WEXITSTATUS(wait_status) if os.WIFEXITED(wait_status) else 128 + os.WTERMSIG(wait_status))
EOF
    set_machine
    status=0
    "$python_bin" "$pty_helper" "$case_dir/tty-output" 'press Enter: ' "$fake_identity"$'\n' -- \
        "$env_bin" -i HOME="$home" PATH="$stubs:/usr/bin:/bin" VAULT_ROOT="$vault" TMPDIR="$tmp_dir" LC_ALL=C \
        STUB_LOG="$log" STUB_GPG_STDIN="$stdin_copy" "$bash_bin" "$script" key-wrap || status=$?
    tty_output="$(< "$case_dir/tty-output")"
    assert_equals 0 "$status" "key-wrap on a terminal must succeed. Output: $tty_output"
    assert_contains "$tty_output" 'Paste the age secret key' 'key-wrap on a terminal must ask for the key'
    assert_not_contains "$tty_output" "$fake_identity" 'a key typed on a terminal must not be echoed'
    assert_equals "$fake_identity" "$(< "$stdin_copy")" 'gpg must receive the typed key'
    assert_file "$(wrapped_target)" 'key-wrap on a terminal must save the key'
else
    printf 'SKIP: python3 is not installed, so the terminal prompt of key-wrap is not exercised.\n'
fi

# --- the sources --------------------------------------------------------------------------

# The identity is only ever expanded as the data of a pipe from the printf builtin: a here-string
# can become a temporary file, echo is not guaranteed to be a builtin, and tracing would print it.
if grep -nE '<<<|[[:space:]]echo[[:space:]]|set[[:space:]]+-[a-z]*x|xtrace' "$script"; then
    fail 'vault-secret must not use here-strings, echo or tracing: the identity must not leave memory and pipes'
fi
# shellcheck disable=SC2016  # the pattern is the script's own text, not an expansion
expansions="$(grep -cF '"$identity"' "$script")"
piped="$(grep -cF "printf '%s\\n' \"\$identity\" |" "$script")"
assert_equals "$piped" "$expansions" 'every use of the identity must be printf piped into a command'

###############################################################
# => Part 2: the real sops, age and gpg
###############################################################

for tool in sops age-keygen gpg gpgconf; do
    if ! command -v "$tool" >/dev/null 2>&1; then
        printf 'SKIP: %s is not installed, so the end-to-end part is not exercised.\n' "$tool"
        printf 'vault-secret tests passed (stub part only).\n'
        exit 0
    fi
done

real="$case_dir/real"
real_gnupg="$real/gnupg"
real_vault="$real/vault"
mkdir -p "$real/pin" "$real/tmp" "$real_gnupg" "$real_vault/Secrets"
chmod 700 "$real_gnupg"

age-keygen -o "$real/key.txt" >/dev/null 2>&1
age-keygen -o "$real/other-key.txt" >/dev/null 2>&1
cat > "$real/plain.yaml" <<'EOF'
credentials:
    camera_cpd:
        username: fake-user
        password: fake-pass-123
    nx_access:
        primary_account:
            password: "p@ss w0rd: with 'quotes'"
EOF
sops encrypt --age "$(age-keygen -y "$real/key.txt")" --input-type yaml --output-type yaml \
    "$real/plain.yaml" > "$real_vault/Secrets/credentials.enc.yaml"
rm -f "$real/plain.yaml"

# A pinentry that answers every passphrase question from a file, or cancels when told to.
cat > "$real/pinentry-stub" <<EOF
#!/bin/sh
dir="$real/pin"
echo "OK Pleased to meet you"
repeat=0
while IFS= read -r line; do
    case "\$line" in
        SETREPEAT*) repeat=1; echo OK ;;
        GETPIN*)
            if [ -e "\$dir/cancel" ]; then
                echo "ERR 83886179 Operation cancelled"
            else
                printf 'D %s\n' "\$(cat "\$dir/pass")"
                [ "\$repeat" = 1 ] && echo "S PIN_REPEATED"
                echo OK
            fi ;;
        BYE*) echo "OK closing connection"; exit 0 ;;
        *) echo OK ;;
    esac
done
EOF
chmod 755 "$real/pinentry-stub"
printf 'pinentry-program %s/pinentry-stub\n' "$real" > "$real_gnupg/gpg-agent.conf"

sops_dir="$(dirname "$(command -v sops)")"
set_passphrase() { printf '%s' "$1" > "$real/pin/pass"; rm -f "$real/pin/cancel"; }
cancel_passphrase() { : > "$real/pin/cancel"; }
forget_passphrases() { stop_real_agent; }

# run_real [ARGS...]: the script with the real tools, a throwaway home and the throwaway agent.
real_home="$real/home"
real_stdin="$real/stdin"
real_extra=()
run_real() {
    local item
    local -a run_env=(
        HOME="$real_home" GNUPGHOME="$real_gnupg" PATH="$sops_dir:/usr/bin:/bin" VAULT_ROOT="$real_vault"
        TMPDIR="$real/tmp" LC_ALL=C
    )
    for item in ${real_extra[@]+"${real_extra[@]}"}; do
        run_env+=("$item")
    done
    status=0
    out="$("$env_bin" -i "${run_env[@]}" "$bash_bin" "$script" "$@" 2>"$case_dir/stderr" < "$real_stdin")" || status=$?
    err="$(< "$case_dir/stderr")"
}

mkdir -p "$real_home"
: > "$real_stdin"
set_passphrase 'correct-horse-battery'

# Wrap the key, the way a user would: the key file on stdin, a passphrase typed into the pinentry.
cp "$real/key.txt" "$real_stdin"
run_real key-wrap
assert_equals 0 "$status" "key-wrap with the real tools must succeed. Output: $out $err"
wrapped="$real_home/.config/sops/age/keys.txt.gpg"
assert_file "$wrapped" 'key-wrap must save keys.txt.gpg'
assert_equals 600 "$(file_mode "$wrapped")" 'the wrapped key must be private to its owner'
if grep -aq 'AGE-SECRET-KEY' "$wrapped"; then
    fail 'the wrapped file must not contain the identity in the clear'
fi
if printf '%s' "$out$err" | grep -qF "$(grep -h '^AGE-SECRET-KEY-' "$real/key.txt")"; then
    fail 'key-wrap must never print the identity'
fi
assert_equals '' "$(find "$real/tmp" -mindepth 1)" 'key-wrap must clean up its scratch folder'
: > "$real_stdin"

run_real key-status
assert_equals 0 "$status" "key-status must succeed once the key is wrapped. Output: $out $err"
assert_contains "$out" 'passphrase-protected' 'key-status must report the wrapped key'
assert_contains "$out" 'Opens store:  yes' 'key-status must report that the store opens'

run_real camera_cpd
assert_equals 0 "$status" "an entry must decrypt through the wrapped key. Output: $err"
assert_contains "$out" 'fake-pass-123' 'the entry must be decrypted'
assert_contains "$out" 'fake-user' 'the whole entry must be decrypted'

run_real nx_access
assert_equals 0 "$status" 'a nested entry must decrypt'
# sops prints an entry as YAML, which quotes this value as 'p@ss w0rd: with ''quotes'''.
assert_contains "$out" "p@ss w0rd: with ''quotes''" 'awkward characters in a value must survive'

# gpg-agent remembers the passphrase: later reads need no prompt, even if the prompt would cancel.
cancel_passphrase
run_real camera_cpd
assert_equals 0 "$status" 'the cached passphrase must serve a second read without a prompt'

# What logout-all does: the agent forgets, so the next read needs the passphrase again.
forget_passphrases
run_real camera_cpd
assert_fails "$status" 'after the agent forgot the passphrase a cancelled prompt must fail'
assert_equals '' "$out" 'a failed read must print nothing'
assert_contains "$err" 'Failed to get the data key' 'a failed read must show the sops error'
assert_not_contains "$out$err" 'fake-pass-123' 'a failed read must not print a value'

set_passphrase 'wrong-passphrase'
forget_passphrases
run_real camera_cpd
assert_fails "$status" 'a wrong passphrase must fail'
assert_equals '' "$out" 'a wrong passphrase must print nothing'

set_passphrase 'correct-horse-battery'
forget_passphrases
run_real camera_cpd
assert_equals 0 "$status" 'the right passphrase must work again'
assert_contains "$out" 'fake-pass-123' 'the entry must be decrypted again'

real_extra=(EDITOR=true)
run_real edit
if [[ $status != 0 && $status != 200 ]]; then
    fail "edit must find the wrapped identity (status $status). Output: $out $err"
fi
assert_not_contains "$err" 'Failed to get the data key' 'edit must be able to decrypt through the wrapped key'
real_extra=()

run_real key-wrap
assert_equals 1 "$status" 'key-wrap must not overwrite the wrapped key'
assert_contains "$err" 'already exists' 'key-wrap must explain the refusal'

# A plain keys.txt must not hide a wrong key: the check looks at the new copy alone.
real_home="$real/home2"
mkdir -p "$real_home/.config/sops/age"
cp "$real/key.txt" "$real_home/.config/sops/age/keys.txt"
chmod 600 "$real_home/.config/sops/age/keys.txt"
cp "$real/other-key.txt" "$real_stdin"
run_real key-wrap
assert_equals 1 "$status" 'a key that is not a recipient must be refused even when a plain key opens the store'
assert_contains "$err" 'does not open' 'a refused key must be explained'
assert_no_file "$real_home/.config/sops/age/keys.txt.gpg" 'a refused key must not be saved'
assert_equals '' "$(find "$real/tmp" -mindepth 1)" 'a refused key-wrap must clean up its scratch folder'

run_real key-status
assert_equals 0 "$status" 'key-status must work with a plain key'
assert_contains "$out" 'plain file' 'key-status must report the plain file'
assert_contains "$out" 'anyone who can read your home folder' 'key-status must warn about the plain file'

cp "$real/key.txt" "$real_stdin"
run_real key-wrap
assert_equals 0 "$status" "the right key must be accepted next to a plain file. Output: $out $err"
assert_contains "$out" 'still there' 'key-wrap must warn about the plain file that is left'
assert_file "$real_home/.config/sops/age/keys.txt" 'key-wrap must leave the plain file for the user to delete'

printf 'vault-secret tests passed.\n'
