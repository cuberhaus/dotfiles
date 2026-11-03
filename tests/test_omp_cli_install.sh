#!/usr/bin/env bash
# Hermetic OMP installation/lifecycle tests: no network, sudo, or real HOME.
# Calls without arguments intentionally exercise the installer's optional-argument path.
# shellcheck disable=SC2119
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRATCH="$(mktemp -d)"
trap 'rm -rf "$SCRATCH"' EXIT
source "$ROOT/.local/scripts/bootstrap/base_functions"

VERSION=18.16.0
DIGEST=cd8f77920da58c6f56b5851c841b61d5e30b4ba8f4cab683afba1f8274b56a06
TOOLS="$SCRATCH/tools"
mkdir -p "$TOOLS"
for tool in bash cat chmod grep ln mkdir mktemp mv readlink rm rmdir; do
    ln -s "$(command -v "$tool")" "$TOOLS/$tool"
done

fail() { printf 'FAIL: %s\n' "$*" >&2; exit 1; }
sandbox() {
    export HOME="$SCRATCH/$1" CALLS="$SCRATCH/$1-calls" EXPECTED_VERSION="$VERSION"
    export PATH="$HOME/.local/bin:$TOOLS"
    mkdir -p "$HOME"
    : > "$CALLS"
}
curl() {
    printf 'curl %s\n' "$*" >> "$CALLS"
    [ "${FAKE:-}" != download-fail ] || return 22
    [ "$6" = "https://raw.githubusercontent.com/can1357/oh-my-pi/v$VERSION/scripts/install.sh" ] || return 1
    cat > "$8" <<'INSTALLER'
#!/bin/bash
[ "$*" = "--binary --ref v$EXPECTED_VERSION" ] || exit 1
[ "$PI_BUILD_URL" = https://build.stencil.so ] || exit 1
printf 'installer\n' >> "$CALLS"
[ "${FAKE:-}" != installer-fail ] || exit 1
[ "${FAKE:-}" != no-binary ] || exit 0
version="$EXPECTED_VERSION"
[ "${FAKE:-}" != wrong-version ] || version=99.0.0
printf '#!/bin/bash\nprintf "omp/%s\\n"\n' "$version" > "$PI_INSTALL_DIR/omp"
chmod +x "$PI_INSTALL_DIR/omp"
INSTALLER
}
sha256sum() {
    if [ "${FAKE:-}" = bad-checksum ]; then
        printf 'bad  %s\n' "$1"
    else
        printf '%s  %s\n' "$DIGEST" "$1"
    fi
}
shasum() { printf '%s  %s\n' "$DIGEST" "$3"; }

test_shasum_fallback() {
    sandbox shasum
    unset -f sha256sum
    omp_cli_install || fail 'shasum fallback failed'
    [ "$(omp --version)" = "omp/$VERSION" ] || fail 'fallback installed wrong version'
}

test_install_and_rerun() {
    sandbox install
    omp_cli_install || fail 'fresh install failed'
    [ "$(omp --version)" = "omp/$VERSION" ] || fail 'wrong binary version'
    [ "$(readlink "$HOME/.local/bin/omp")" = "$HOME/.local/share/omp/bin/omp" ] || fail 'wrong link'
    omp_cli_install || fail 'rerun failed'
    [ "$(grep -c '^curl ' "$CALLS")" = 1 ] || fail 'rerun downloads again'
    rm "$HOME/.local/bin/omp"
    omp_cli_install || fail 'missing-link repair failed'
    [ "$(grep -c '^curl ' "$CALLS")" = 1 ] || fail 'link repair downloads again'
    [ "$(omp --version)" = "omp/$VERSION" ] || fail 'link was not repaired'
}

test_failure_preserves_existing_binary() {
    sandbox failure
    omp_cli_install >/dev/null || fail 'fixture install failed'
    printf '#!/bin/bash\nprintf "omp/old\\n"\n' > "$HOME/.local/share/omp/bin/omp"
    local ran_before
    for FAKE in download-fail bad-checksum installer-fail no-binary wrong-version; do
        export FAKE
        ran_before="$(grep -c '^installer$' "$CALLS")"
        if omp_cli_install >/dev/null 2>&1; then fail "$FAKE reported success"; fi
        case "$FAKE" in
            download-fail|bad-checksum)
                [ "$(grep -c '^installer$' "$CALLS")" = "$ran_before" ] || fail "$FAKE executed an unverified installer" ;;
        esac
        [ "$(omp --version)" = omp/old ] || fail "$FAKE replaced the working binary"
        for stage in "$HOME/.local/share/omp/bin"/.install.*; do
            [ ! -e "$stage" ] || fail 'staging directory leaked'
        done
    done
}

test_foreign_command_is_preserved() {
    sandbox foreign
    mkdir -p "$HOME/.local/bin"
    printf '#!/bin/bash\nprintf "mine\\n"\n' > "$HOME/.local/bin/omp"
    chmod +x "$HOME/.local/bin/omp"
    if omp_cli_install >/dev/null; then fail 'foreign command accepted'; fi
    [ ! -s "$CALLS" ] || fail 'downloaded before detecting a foreign command'
    omp_cli_uninstall >/dev/null
    [ "$(omp)" = mine ] || fail 'uninstall deleted a foreign command'
}

test_uninstall_preserves_state_and_foreign_link() {
    sandbox uninstall
    omp_cli_install >/dev/null || fail 'fixture install failed'
    mkdir -p "$HOME/.omp/agent"
    printf 'fixture state\n' > "$HOME/.omp/agent/state"
    omp_cli_uninstall --dry-run >/dev/null
    [ -x "$HOME/.local/bin/omp" ] || fail 'dry-run removed binary'
    omp_cli_uninstall >/dev/null
    [ ! -L "$HOME/.local/bin/omp" ] || fail 'managed link remains'
    [ ! -e "$HOME/.local/share/omp/bin/omp" ] || fail 'managed binary remains'
    [ -f "$HOME/.omp/agent/state" ] || fail 'application state was removed'
    ln -s "$HOME/.omp/agent/state" "$HOME/.local/bin/omp"
    omp_cli_uninstall >/dev/null
    [ -L "$HOME/.local/bin/omp" ] || fail 'foreign link was removed'
}

test_help_dry_run_and_argument_validation() {
    sandbox help
    for fn in omp_cli_install omp_cli_uninstall; do
        for arg in -h --help; do
            "$fn" "$arg" | grep -Fq "Usage: $fn" || fail "$fn help failed"
        done
        "$fn" --dry-run >/dev/null
        DRY_RUN=true "$fn" >/dev/null
        if "$fn" --invalid >/dev/null 2>&1; then fail 'invalid argument accepted'; fi
        if "$fn" --dry-run extra >/dev/null 2>&1; then fail 'extra argument accepted'; fi
    done
    [ ! -d "$HOME/.local" ] || fail 'help/dry-run mutated HOME'
    [ ! -s "$CALLS" ] || fail 'help/dry-run used the network'
}

test_profile_wiring_and_pin() {
    grep -Fq "local version=\"$VERSION\"" "$ROOT/.local/scripts/bootstrap/base_functions" || fail 'version fixture drift'
    grep -Fq "local digest=\"$DIGEST\"" "$ROOT/.local/scripts/bootstrap/base_functions" || fail 'digest fixture drift'
    for profile in arch mac ubuntu work; do
        grep -Fq 'omp_cli_install || warn' "$ROOT/.local/scripts/bootstrap/${profile}_functions" || fail "$profile missing nonfatal install"
        grep -Fq 'omp_cli_uninstall' "$ROOT/.local/scripts/bootstrap/uninstall_${profile}_functions" || fail "$profile missing uninstall"
    done
    grep -Fq 'install-omp:' "$ROOT/Makefile" || fail 'missing narrow install target'
}

for test in test_install_and_rerun test_shasum_fallback test_failure_preserves_existing_binary \
    test_foreign_command_is_preserved test_uninstall_preserves_state_and_foreign_link \
    test_help_dry_run_and_argument_validation test_profile_wiring_and_pin; do
    ( "$test" )
    printf 'ok - %s\n' "$test"
done
