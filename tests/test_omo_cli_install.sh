#!/usr/bin/env bash
# Hermetic OmO bootstrap tests; optional no-argument calls are intentional.
# shellcheck disable=SC2119
set -euo pipefail
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRATCH="$(mktemp -d)"
trap 'rm -rf "$SCRATCH"' EXIT
source "$ROOT/.local/scripts/bootstrap/base_functions"

VERSION=5.1.58
DIGEST=a0aabc454243cd03cf8a817a57225cb3c1b66feb9a1f44b4b0c1b464d689d32b
TOOLS="$SCRATCH/tools"
mkdir -p "$TOOLS"
for tool in bash cat chmod grep ln mkdir mktemp mv readlink rm rmdir; do
    ln -s "$(command -v "$tool")" "$TOOLS/$tool"
done
fail() { printf 'FAIL: %s\n' "$*" >&2; exit 1; }
sandbox() {
    export HOME="$SCRATCH/$1" CALLS="$SCRATCH/$1-calls" EXPECTED_VERSION="$VERSION"
    export REAL_HOME="$HOME" PATH="$HOME/.local/bin:$TOOLS"
    mkdir -p "$HOME"
    : > "$CALLS"
}
curl() {
    printf 'curl %s\n' "$*" >> "$CALLS"
    [ "${FAKE:-}" != download-fail ] || return 22
    [ "$6" = "https://raw.githubusercontent.com/code-yeongyu/oh-my-openagent/v$VERSION/packages/get-worker/scripts/install.sh" ] || return 1
    cat > "$8" <<'INSTALLER'
#!/bin/bash
[ "$*" = "$EXPECTED_VERSION" ] || exit 1
[ "$OMO_NO_MODIFY_PATH" = 1 ] || exit 1
[ "$OMO_INSTALL_BASE_URL" = https://get.omo.dev ] || exit 1
[ "$HOME" != "$REAL_HOME" ] || exit 1
[ -z "$GITHUB_PATH" ] || exit 1
printf 'installer\n' >> "$CALLS"
[ "${FAKE:-}" != installer-fail ] || exit 1
[ "${FAKE:-}" != no-binary ] || exit 0
# Emulate upstream's receipt: it must stay in staging, not the real HOME.
mkdir -p "$HOME/.omo"
printf 'staging receipt\n' > "$HOME/.omo/install.json"
version="$EXPECTED_VERSION"
[ "${FAKE:-}" != wrong-version ] || version=99.0.0
printf '#!/bin/bash\nprintf "omo %s (engine: fixture)\\n"\n' "$version" > "$OMO_INSTALL_DIR/omo"
chmod +x "$OMO_INSTALL_DIR/omo"
INSTALLER
}
sha256sum() {
    if [ "${FAKE:-}" = bad-checksum ]; then printf 'bad  %s\n' "$1"
    else printf '%s  %s\n' "$DIGEST" "$1"; fi
}
shasum() { printf '%s  %s\n' "$DIGEST" "$3"; }

test_install_rerun_and_link_repair() {
    sandbox install
    omo_cli_install || fail 'fresh install failed'
    [ "$(omo --version)" = "omo $VERSION (engine: fixture)" ] || fail 'wrong version'
    [ ! -e "$HOME/.omo/install.json" ] || fail 'staging receipt reached real HOME'
    omo_cli_install || fail 'rerun failed'
    rm "$HOME/.local/bin/omo"
    omo_cli_install || fail 'link repair failed'
    [ "$(readlink "$HOME/.local/bin/omo")" = "$HOME/.local/share/omo/bin/omo" ] || fail 'wrong link'
    [ "$(grep -c '^curl ' "$CALLS")" = 1 ] || fail 'rerun/repair downloads again'
}
test_shasum_fallback() {
    sandbox shasum
    unset -f sha256sum
    omo_cli_install || fail 'shasum fallback failed'
}
test_failure_preserves_binary_and_state() {
    sandbox failures
    omo_cli_install >/dev/null || fail 'fixture install failed'
    mkdir -p "$HOME/.omo"
    printf 'original receipt\n' > "$HOME/.omo/install.json"
    printf '#!/bin/bash\nprintf "omo old\\n"\n' > "$HOME/.local/share/omo/bin/omo"
    local ran_before
    for FAKE in download-fail bad-checksum installer-fail no-binary wrong-version; do
        export FAKE
        ran_before="$(grep -c '^installer$' "$CALLS")"
        if omo_cli_install >/dev/null 2>&1; then fail "$FAKE reported success"; fi
        [ "$(omo --version)" = 'omo old' ] || fail "$FAKE replaced a working binary"
        [ "$(cat "$HOME/.omo/install.json")" = 'original receipt' ] || fail 'original receipt changed'
        case "$FAKE" in
            download-fail|bad-checksum)
                [ "$(grep -c '^installer$' "$CALLS")" = "$ran_before" ] || fail 'unverified installer executed' ;;
        esac
        for stage in "$HOME/.local/share/omo/bin"/.install.*; do
            [ ! -e "$stage" ] || fail 'staging directory leaked'
        done
    done
}
test_foreign_command_and_uninstall() {
    sandbox foreign
    mkdir -p "$HOME/.local/bin" "$HOME/.omo/agent"
    printf '#!/bin/bash\nprintf "mine\\n"\n' > "$HOME/.local/bin/omo"
    chmod +x "$HOME/.local/bin/omo"
    if omo_cli_install >/dev/null; then fail 'foreign command accepted'; fi
    [ ! -s "$CALLS" ] || fail 'foreign command caused a download'
    omo_cli_uninstall >/dev/null
    [ "$(omo)" = mine ] || fail 'foreign command removed'
    rm "$HOME/.local/bin/omo"
    omo_cli_install >/dev/null || fail 'fixture install failed'
    printf 'fixture state\n' > "$HOME/.omo/agent/state"
    omo_cli_uninstall --dry-run >/dev/null
    [ -x "$HOME/.local/bin/omo" ] || fail 'dry-run removed binary'
    omo_cli_uninstall >/dev/null
    [ ! -L "$HOME/.local/bin/omo" ] || fail 'managed link remains'
    [ ! -e "$HOME/.local/share/omo/bin/omo" ] || fail 'managed binary remains'
    [ -f "$HOME/.omo/agent/state" ] || fail 'app state removed'
}
test_help_dry_run_and_arguments() {
    sandbox help
    for fn in omo_cli_install omo_cli_uninstall; do
        for arg in -h --help; do
            "$fn" "$arg" | grep -Fq "Usage: $fn" || fail 'help failed'
        done
        "$fn" --dry-run >/dev/null
        DRY_RUN=true "$fn" >/dev/null
        if "$fn" --invalid >/dev/null 2>&1; then fail 'invalid argument accepted'; fi
        if "$fn" --dry-run extra >/dev/null 2>&1; then fail 'extra argument accepted'; fi
    done
    [ ! -d "$HOME/.local" ] || fail 'help/dry-run mutated HOME'
    [ ! -s "$CALLS" ] || fail 'help/dry-run downloaded'
}
test_wiring_and_pin() {
    grep -Fq "local version=\"$VERSION\"" "$ROOT/.local/scripts/bootstrap/base_functions" || fail 'version drift'
    grep -Fq "local digest=\"$DIGEST\"" "$ROOT/.local/scripts/bootstrap/base_functions" || fail 'digest drift'
    for profile in arch mac ubuntu work; do
        grep -Fq 'omo_cli_install || warn' "$ROOT/.local/scripts/bootstrap/${profile}_functions" || fail "$profile install missing"
        grep -Fq 'omo_cli_uninstall' "$ROOT/.local/scripts/bootstrap/uninstall_${profile}_functions" || fail "$profile uninstall missing"
    done
    grep -Fq 'install-omo:' "$ROOT/Makefile" || fail 'install target missing'
}
for test in test_install_rerun_and_link_repair test_shasum_fallback \
    test_failure_preserves_binary_and_state test_foreign_command_and_uninstall \
    test_help_dry_run_and_arguments test_wiring_and_pin; do
    ( "$test" )
    printf 'ok - %s\n' "$test"
done
