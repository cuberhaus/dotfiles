#!/usr/bin/env bash
# Hermetic tests for apply_skip_worktree in bootstrap/base_functions.  The function finds the
# checkout from the location of base_functions (three levels up) and runs
# .local/scripts/apply-skip-worktree there.  It once went up two levels, looked for the script in
# <checkout>/.local/.local/scripts and, finding nothing, skipped it without a word, so no bootstrap
# ever masked the volatile config files.  Each case builds its own copy of the layout in a
# temporary directory; nothing touches the real checkout, its index, or the real home directory.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRATCH="$(mktemp -d)"
trap 'rm -rf "$SCRATCH"' EXIT

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

assert_equals() {
    [ "$1" = "$2" ] || fail "$3: expected [$1], got [$2]"
}

assert_contains() {
    case "$1" in
        *"$2"*) ;;
        *) fail "$3: [$2] not found in [$1]" ;;
    esac
}

# make_checkout NAME [stub|real|none]: <root>/.local/scripts/bootstrap/base_functions (the real
# file) and, in the same place a checkout has it, apply-skip-worktree: a stub that records its
# arguments, the real script, or nothing.  Sets CASE (the case's folder) and ROOT (the checkout,
# written the way `cd -P` prints it, which is what the function must report).
make_checkout() {
    CASE="$SCRATCH/$1"
    mkdir -p "$CASE/checkout/.local/scripts/bootstrap"
    ROOT="$(cd -P "$CASE/checkout" && pwd)"
    cp "$REPO_ROOT/.local/scripts/bootstrap/base_functions" "$ROOT/.local/scripts/bootstrap/base_functions"
    case "${2:-stub}" in
        stub)
            cat > "$ROOT/.local/scripts/apply-skip-worktree" <<STUB
#!/usr/bin/env bash
printf '%s\n' "\$*" > "$CASE/args"
STUB
            chmod +x "$ROOT/.local/scripts/apply-skip-worktree"
            ;;
        real)
            cp "$REPO_ROOT/.local/scripts/apply-skip-worktree" "$ROOT/.local/scripts/apply-skip-worktree"
            ;;
        none) ;;
    esac
}

# run_from FILE: source FILE the way a profile does and call the function.  OUTPUT holds what it
# printed and STATUS how it ended.
run_from() {
    STATUS=0
    OUTPUT="$(
        # shellcheck source=/dev/null
        source "$1"
        apply_skip_worktree 2>&1
    )" || STATUS=$?
}

test_it_runs_the_script_of_the_checkout_it_lives_in() {
    make_checkout plain

    run_from "$ROOT/.local/scripts/bootstrap/base_functions"

    assert_equals 0 "$STATUS" "the status (output: $OUTPUT)"
    assert_equals "--verbose $ROOT" "$(cat "$CASE/args")" 'the arguments the script received'
}

test_it_finds_the_checkout_through_the_stow_link() {
    make_checkout stow
    # Stow links ~/.local/scripts to the checkout, and the uninstallers source base_functions
    # from there.  A plain `..` from that path would end in the home directory.
    mkdir -p "$CASE/home/.local"
    ln -s "$ROOT/.local/scripts" "$CASE/home/.local/scripts"

    run_from "$CASE/home/.local/scripts/bootstrap/base_functions"

    assert_equals 0 "$STATUS" "the status (output: $OUTPUT)"
    assert_equals "--verbose $ROOT" "$(cat "$CASE/args")" 'the checkout, not the folder holding the link'
}

test_a_missing_script_is_reported_not_skipped_silently() {
    make_checkout missing none

    run_from "$ROOT/.local/scripts/bootstrap/base_functions"

    assert_equals 0 "$STATUS" 'a cosmetic step must not abort the bootstrap'
    assert_contains "$OUTPUT" "apply-skip-worktree not found at $ROOT/.local/scripts/apply-skip-worktree" \
        'the warning names the path it looked in'
}

test_the_real_script_masks_a_tracked_volatile_file_through_the_function() {
    make_checkout real real
    local volatile=.config/vlc/vlc-qt-interface.conf
    git -C "$ROOT" init -q
    mkdir -p "$ROOT/.config/vlc"
    printf '[General]\n' > "$ROOT/$volatile"
    git -C "$ROOT" add "$volatile"
    assert_equals "H $volatile" "$(git -C "$ROOT" ls-files -v -- "$volatile")" 'the file before the step'

    run_from "$ROOT/.local/scripts/bootstrap/base_functions"

    assert_equals 0 "$STATUS" "the status (output: $OUTPUT)"
    assert_equals "S $volatile" "$(git -C "$ROOT" ls-files -v -- "$volatile")" 'the file after the step (S = skip-worktree)'
    assert_contains "$OUTPUT" "skip-worktree: $volatile" 'the verbose report of the script'
}

test_every_profile_masks_the_volatile_files() {
    local entrypoint
    for entrypoint in arch manjaro ubuntu ubuntu_windows mac work gentoo; do
        grep -Eq '^[[:space:]]*(gentoo_apply )?apply_skip_worktree$' "$REPO_ROOT/.local/scripts/bootstrap/$entrypoint" ||
            fail "the $entrypoint profile never calls apply_skip_worktree"
    done
}

for test_name in \
    test_it_runs_the_script_of_the_checkout_it_lives_in \
    test_it_finds_the_checkout_through_the_stow_link \
    test_a_missing_script_is_reported_not_skipped_silently \
    test_the_real_script_masks_a_tracked_volatile_file_through_the_function \
    test_every_profile_masks_the_volatile_files; do
    ( "$test_name" ) || {
        printf 'not ok - %s\n' "$test_name" >&2
        exit 1
    }
    printf 'ok - %s\n' "$test_name"
done

printf 'apply_skip_worktree tests passed.\n'
