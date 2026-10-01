#!/usr/bin/env bash
# Unit tests for `updateall` and pip_user_upgrade in .config/zsh/aliases.
#
# They guard two failures seen on Ubuntu 26.04:
#   - nvim ran as a background job. It draws a full-screen UI, so an interactive
#     shell stopped it ("suspended (tty output)") before any plugin was updated.
#     Every step must finish before the alias returns.
#   - `pip install --user` ran on a PEP 668 "externally managed" Python, where it
#     always fails. pip may only touch an interpreter that allows it.
#
# Every case runs in a clean `env -i` shell with a throwaway HOME and stub
# commands first on PATH, so nothing real (sudo, apt-get, gcloud, nvim, pip) ever
# starts and the developer's (or an agent's) environment cannot leak in.

# The scripts handed to the child shells below are single-quoted on purpose: their
# variables must be expanded by the child shell, not by this one.
# shellcheck disable=SC2016

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
aliases="$repo_root/.config/zsh/aliases"
case_dir="$(mktemp -d)"
trap 'rm -rf "$case_dir"' EXIT

home="$case_dir/home"
bin="$case_dir/bin"
calls_log="$case_dir/calls.log"
pip_log="$case_dir/pip.log"
mkdir -p "$home" "$bin"

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
    [[ $1 == *"$2"* ]] || fail "$3 (missing: $2)"
}

assert_not_contains() {
    [[ $1 != *"$2"* ]] || fail "$3 (unexpected: $2)"
}

###############################################################
# => Stub commands
###############################################################

# sudo records the command line and never runs it, so apt-get cannot start.
cat > "$bin/sudo" <<'EOF'
#!/usr/bin/env bash
printf 'sudo %s\n' "$*" >> "$STUB_LOG"
EOF

cat > "$bin/gcloud" <<'EOF'
#!/usr/bin/env bash
printf 'gcloud %s\n' "$*" >> "$STUB_LOG"
EOF

# Nothing may call pip3 any more; recording it makes a regression visible.
cat > "$bin/pip3" <<'EOF'
#!/usr/bin/env bash
printf 'pip3 %s\n' "$*" >> "$STUB_LOG"
EOF

# The alias starts emacs as a background job: keep it inert and silent.
cat > "$bin/emacs" <<'EOF'
#!/usr/bin/env bash
exit 0
EOF

# nvim takes a moment, like a real plugin update. The last line only exists if the
# alias waited for it instead of leaving it running in the background.
cat > "$bin/nvim" <<'EOF'
#!/usr/bin/env bash
printf 'nvim %s\n' "$*" >> "$STUB_LOG"
sleep 0.3
printf 'nvim finished\n' >> "$STUB_LOG"
EOF

# python3 answers the PEP 668 probe (-c) with STUB_PROBE_STATUS and records the rest.
cat > "$bin/python3" <<'EOF'
#!/usr/bin/env bash
if [[ ${1:-} == -c ]]; then
    exit "${STUB_PROBE_STATUS:-1}"
fi
printf 'python3 %s\n' "$*" >> "$STUB_LOG"
EOF

chmod +x "$bin"/*

# run_updateall SHELL PROBE_STATUS
# Load the aliases into a clean SHELL as an Ubuntu machine and run the updateall
# alias. PROBE_STATUS is what the stub python3 answers to the PEP 668 probe: 1 means
# externally managed, 0 means pip may install. Sets `calls` to what the stubs saw
# and `output` to the shell's stdout.
run_updateall() {
    local shell_name=$1 probe=$2 shell_bin setup flags=()
    shell_bin="$(command -v "$shell_name")"
    case "$shell_name" in
        bash)
            flags=(--noprofile --norc)
            setup='shopt -s expand_aliases'
            ;;
        zsh)
            flags=(-f)
            setup=':'
            ;;
    esac

    : > "$calls_log"
    # `eval` parses `updateall` only after the aliases are loaded. zsh reads the
    # whole -c script before running it and would not expand the alias otherwise.
    output="$(
        env -i HOME="$home" PATH="$bin:/usr/bin:/bin" TERM=dumb DISTRO=ubuntu \
            STUB_LOG="$calls_log" STUB_PROBE_STATUS="$probe" \
            "$shell_bin" "${flags[@]}" -c "$setup"$'\n''
                antigen() { printf "antigen %s\n" "$*" >> "$STUB_LOG"; }
                source "$1"
                eval updateall
            ' "$shell_name" "$aliases"
    )" || fail "$shell_name: updateall exited with an error"
    calls="$(cat "$calls_log")"
}

###############################################################
# => updateall: every step runs to completion, in order
###############################################################

managed_calls='sudo apt-get update
sudo apt-get full-upgrade
gcloud components update --quiet
antigen update
nvim +PlugUpgrade +PlugUpdate +qall
nvim finished'

unmanaged_calls='sudo apt-get update
sudo apt-get full-upgrade
gcloud components update --quiet
python3 -m pip install --user --upgrade pip pynvim
antigen update
nvim +PlugUpgrade +PlugUpdate +qall
nvim finished'

shells=(bash)
if command -v zsh > /dev/null 2>&1; then
    shells+=(zsh)
else
    printf 'SKIP: zsh is not installed; testing Bash only.\n'
fi

for shell_name in "${shells[@]}"; do
    run_updateall "$shell_name" 1
    assert_equals "$managed_calls" "$calls" \
        "$shell_name: on an externally managed Python updateall must finish every step in order and never call pip"
    assert_contains "$output" 'Skipping pip upgrade' \
        "$shell_name: a skipped pip upgrade must say so"

    run_updateall "$shell_name" 0
    assert_equals "$unmanaged_calls" "$calls" \
        "$shell_name: on a Python that allows it updateall must upgrade pip and pynvim once, for the user"
    assert_not_contains "$output" 'Skipping pip upgrade' \
        "$shell_name: nothing was skipped, so nothing may say so"
done

###############################################################
# => pip_user_upgrade without python3
###############################################################

mkdir -p "$case_dir/empty"
output="$(
    env -i HOME="$home" PATH="$case_dir/empty" "$(command -v bash)" --noprofile --norc -c '
        source "$1"
        pip_user_upgrade
        printf "rc=%s\n" "$?"
    ' bash "$aliases"
)"
assert_equals 'rc=0' "$output" 'pip_user_upgrade must quietly do nothing when python3 is missing'

###############################################################
# => pip_user_upgrade against real interpreters
###############################################################
# The stubs above never run the probe, so a typo in it would pass. Run it for real
# and compare its decision with the facts read straight off the interpreter. A fake
# `pip` package first on PYTHONPATH records the install instead of performing it.

mkdir -p "$case_dir/fakepip/pip"
: > "$case_dir/fakepip/pip/__init__.py"
cat > "$case_dir/fakepip/pip/__main__.py" <<'EOF'
import os
import sys

with open(os.environ["PIP_LOG"], "a") as pip_log:
    pip_log.write(" ".join(sys.argv[1:]) + "\n")
EOF

# interpreter_allows_user_pip PYTHON
# Succeed when PYTHON is neither marked EXTERNALLY-MANAGED nor inside a virtualenv,
# fail when pip must stay away, and return 2 when PYTHON cannot be probed.
interpreter_allows_user_pip() {
    local python=$1 stdlib in_venv
    stdlib="$("$python" -c 'import sysconfig; print(sysconfig.get_path("stdlib"))')" || return 2
    in_venv="$("$python" -c 'import sys; print(sys.prefix != sys.base_prefix)')" || return 2
    [[ ! -e $stdlib/EXTERNALLY-MANAGED && $in_venv == False ]]
}

# check_real_interpreter LABEL PYTHON
check_real_interpreter() {
    local label=$1 python=$2 verdict=0 expected=''
    interpreter_allows_user_pip "$python" || verdict=$?
    case "$verdict" in
        0) expected='install --user --upgrade pip pynvim' ;;
        1) expected='' ;;
        *)
            printf 'SKIP: %s: %s cannot be probed.\n' "$label" "$python"
            return 0
            ;;
    esac

    : > "$pip_log"
    env -i HOME="$home" PATH="${python%/*}:/usr/bin:/bin" \
        PYTHONPATH="$case_dir/fakepip" PIP_LOG="$pip_log" \
        bash --noprofile --norc -c 'source "$1"; pip_user_upgrade' bash "$aliases" > /dev/null
    assert_equals "$expected" "$(cat "$pip_log")" \
        "$label: pip_user_upgrade disagrees with the interpreter's own PEP 668 facts"
}

system_python="$(command -v python3 || true)"
if [[ -z $system_python ]]; then
    printf 'SKIP: python3 is not installed; skipping the real-interpreter cases.\n'
else
    check_real_interpreter 'system python3' "$system_python"

    # A virtualenv never takes `pip install --user`, whatever the system marks.
    if "$system_python" -m venv --without-pip "$case_dir/venv" > /dev/null 2>&1; then
        check_real_interpreter 'virtualenv python3' "$case_dir/venv/bin/python3"
    else
        printf 'SKIP: python3 -m venv is unavailable; skipping the virtualenv case.\n'
    fi
fi

printf 'updateall tests passed.\n'
