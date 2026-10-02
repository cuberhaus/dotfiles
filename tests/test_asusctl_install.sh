#!/usr/bin/env bash
# Hermetic tests for .local/scripts/asusctl_install.sh and its bootstrap/repair wiring.
# Fake id, sudo, apt-get, dpkg-query, cargo, rustc, ldd, systemctl, udevadm, and busctl
# binaries, env-overridden sysfs/DMI/system directories, and a local git repository
# standing in for the upstream project keep every case off the real machine, the
# network, and the real package manager. git and make are the real ones.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT="$REPO_ROOT/.local/scripts/asusctl_install.sh"
BASE_FUNCTIONS="$REPO_ROOT/.local/scripts/bootstrap/base_functions"
REPAIR_SCRIPT="$REPO_ROOT/.local/scripts/repair-installation"
TEST_ROOT="$(mktemp -d)"
trap 'rm -rf "$TEST_ROOT"' EXIT

# The throwaway repositories must not pick up the user's git configuration or hooks.
export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1

readonly TAG=6.3.8
readonly ALL_PACKAGES='ca-certificates git make build-essential pkg-config cargo libudev-dev libusb-1.0-0-dev libclang-dev libusb-1.0-0'
readonly ANIME_FILE="usr/share/asusd/anime/asus/festive/Mother's day.gif"

CASE=''
OUTPUT=''
STATUS=0
UPSTREAM=''
UPSTREAM_COMMIT=''
# What run_installer executes. A test that must start the installer another way (for
# example with one helper replaced) overrides it for a single run.
INSTALLER_COMMAND=(bash "$SCRIPT")

fail() {
    printf 'FAIL: %s\n' "$1" >&2
    shift
    if [[ $# -gt 0 ]]; then
        printf '%s\n' "$@" >&2
    fi
    exit 1
}

## Create a fake upstream project in $1 tagged $2. Optional flags: drop-anime (omit one
## data file), escape (install outside /usr), no-rules (omit the udev rule).
make_upstream() {
    local dir="$1" tag="$2" flag
    shift 2
    mkdir -p "$dir/data" "$dir/rog-aura/data" "$dir/rog-anime/data/anime/asus/festive" "$dir/rog-anime/data/anime/custom"
    printf '[Service]\nExecStart=/usr/bin/asusd\n' >"$dir/data/asusd.service"
    printf '[Service]\nExecStart=/usr/bin/asus-shutdown\n' >"$dir/data/asus-shutdown.service"
    printf '# udev rule\n' >"$dir/data/asusd.rules"
    printf '<busconfig/>\n' >"$dir/data/asusd.conf"
    printf '()\n' >"$dir/rog-aura/data/aura_support.ron"
    printf 'gif\n' >"$dir/rog-anime/data/anime/asus/festive/Mother's day.gif"
    printf 'png\n' >"$dir/rog-anime/data/anime/custom/rust.png"
    # Mirrors the real Makefile: the binaries depend on the sources, so make would try
    # to rebuild them unless it is told with -o that they are up to date.
    cat >"$dir/Makefile" <<'EOF'
.RECIPEPREFIX := >
prefix = /usr
target/release/%: Makefile
>@echo "make must not rebuild $@" >&2; exit 1
install-asusd: target/release/asusd
>install -D -m 0755 target/release/asusd "$(DESTDIR)$(prefix)/bin/asusd"
install-asus-shutdown: target/release/asus-shutdown
>install -D -m 0755 target/release/asus-shutdown "$(DESTDIR)$(prefix)/bin/asus-shutdown"
install-asusctl: target/release/asusctl
>install -D -m 0755 target/release/asusctl "$(DESTDIR)$(prefix)/bin/asusctl"
install-data-asusd:
>install -D -m 0644 data/asusd.rules "$(DESTDIR)$(prefix)/lib/udev/rules.d/99-asusd.rules"
>install -D -m 0644 rog-aura/data/aura_support.ron "$(DESTDIR)$(prefix)/share/asusd/aura_support.ron"
>install -D -m 0644 data/asusd.conf "$(DESTDIR)$(prefix)/share/dbus-1/system.d/asusd.conf"
>install -D -m 0644 data/asusd.service "$(DESTDIR)$(prefix)/lib/systemd/system/asusd.service"
>install -D -m 0644 data/asus-shutdown.service "$(DESTDIR)$(prefix)/lib/systemd/system/asus-shutdown.service"
>cd rog-anime/data && find "./anime" -type f -exec install -D -m 0644 "{}" "$(DESTDIR)$(prefix)/share/asusd/{}" \;
EOF
    for flag in "$@"; do
        case "$flag" in
            drop-anime) rm "$dir/rog-anime/data/anime/custom/rust.png" ;;
            escape)
                # shellcheck disable=SC2016 # make expands $(DESTDIR), the shell must not
                printf '>install -D -m 0644 data/asusd.conf "$(DESTDIR)/etc/escaped.conf"\n' >>"$dir/Makefile"
                ;;
            no-rules) sed -i '/99-asusd.rules/d' "$dir/Makefile" ;;
        esac
    done
    git -C "$dir" init -q -b main
    git -C "$dir" add -A
    git -C "$dir" -c user.name=test -c user.email=test@example.invalid -c commit.gpgsign=false \
        commit -q -m "release $tag"
    git -C "$dir" tag "$tag"
}

write_fakes() {
    local bin="$CASE/bin"

    cat >"$bin/id" <<'EOF'
#!/usr/bin/env bash
case "${1:-}" in
    -u) printf '%s\n' "${FAKE_UID:-1000}" ;;
    -un) printf 'tester\n' ;;
    *) /usr/bin/id "$@" ;;
esac
EOF
    # One log line per call: whether sudo would have prompted, then the command it runs.
    cat >"$bin/sudo" <<'EOF'
#!/usr/bin/env bash
mode=interactive
if [[ "${1:-}" == -n ]]; then
    mode=noninteractive
    shift
fi
printf '%s %s\n' "$mode" "${1:-}" >>"$FAKE_LOG_DIR/sudo.log"
[[ "${FAKE_SUDO_FAIL:-false}" != true ]] || exit 1
exec "$@"
EOF
    cat >"$bin/apt-get" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >>"$FAKE_LOG_DIR/apt-get.log"
[[ "${FAKE_APT_FAIL:-false}" != true ]] || exit 100
for argument in "$@"; do
    case "$argument" in
        -* | install | update) ;;
        *) printf '%s\n' "$argument" >>"$FAKE_LOG_DIR/installed.list" ;;
    esac
done
EOF
    cat >"$bin/dpkg-query" <<'EOF'
#!/usr/bin/env bash
case "$1" in
    -W)
        package="${*: -1}"
        if grep -Fxq -- "$package" "$FAKE_LOG_DIR/installed.list" 2>/dev/null; then
            printf 'installed'
        else
            exit 1
        fi
        ;;
    -S)
        read -r owner owned_path <"$FAKE_LOG_DIR/dpkg-owner" 2>/dev/null || exit 1
        [[ "$2" == "$owned_path" ]] || exit 1
        printf '%s: %s\n' "$owner" "$owned_path"
        ;;
esac
EOF
    cat >"$bin/cargo" <<'EOF'
#!/usr/bin/env bash
printf '%s|%s\n' "$PWD" "$*" >>"$FAKE_LOG_DIR/cargo.log"
[[ "${FAKE_CARGO_FAIL:-false}" != true ]] || { echo 'error: could not compile asusd' >&2; exit 101; }
[[ "$*" == 'build --release --locked -p asusctl -p asusd -p asus-shutdown' ]] ||
    { echo "unexpected cargo arguments: $*" >&2; exit 2; }
mkdir -p target/release
for binary in asusctl asusd asus-shutdown; do
    printf '#!/bin/sh\necho %s\n' "$binary" >"target/release/$binary"
    chmod 755 "target/release/$binary"
    # Older than every source file, as a real incremental build could leave them.
    touch -d '2000-01-01' "target/release/$binary"
done
EOF
    cat >"$bin/rustc" <<'EOF'
#!/usr/bin/env bash
printf 'rustc %s (fake)\n' "${FAKE_RUSTC_VERSION:-1.93.1}"
EOF
    cat >"$bin/ldd" <<'EOF'
#!/usr/bin/env bash
printf '\tlibc.so.6 => /lib/libc.so.6 (0x0)\n'
[[ "${FAKE_LDD_MISSING:-false}" != true ]] || printf '\tlibmissing.so.1 => not found\n'
EOF
    cat >"$bin/systemctl" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >>"$FAKE_LOG_DIR/systemctl.log"
state="$FAKE_LOG_DIR/active"
mkdir -p "$state"
case "$1" in
    is-active)
        shift
        quiet=false
        if [[ "${1:-}" == --quiet ]]; then quiet=true; shift; fi
        if [[ -e "$state/$1" ]]; then
            [[ "$quiet" == true ]] || echo active
            exit 0
        fi
        [[ "$quiet" == true ]] || echo inactive
        exit 3
        ;;
    start | restart)
        [[ "${FAKE_ASUSD_FAIL:-false}" != true ]] || exit 1
        touch "$state/$2"
        if [[ "$1" == restart && -n "${FAKE_ASUSD_SETS_PROFILE:-}" ]]; then
            printf '%s\n' "$FAKE_ASUSD_SETS_PROFILE" >"$FAKE_PROFILE_FILE"
        fi
        ;;
    enable) touch "$state/${*: -1}" ;;
    disable) rm -f "$state/${*: -1}" ;;
    stop) rm -f "$state/$2" ;;
esac
EOF
    cat >"$bin/udevadm" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >>"$FAKE_LOG_DIR/udevadm.log"
EOF
    cat >"$bin/busctl" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >>"$FAKE_LOG_DIR/busctl.log"
mkdir -p "$FAKE_LOG_DIR/dbus"
[[ "${FAKE_BUSCTL_UNAVAILABLE:-false}" != true ]] || { echo 'Failed to get property' >&2; exit 1; }
# --system get-property NAME OBJECT INTERFACE PROPERTY | --system set-property ... PROPERTY b VALUE
case "$2" in
    get-property)
        value="$(cat "$FAKE_LOG_DIR/dbus/$6" 2>/dev/null || echo "${FAKE_DBUS_DEFAULT:-true}")"
        printf 'b %s\n' "$value"
        ;;
    set-property) printf '%s\n' "$8" >"$FAKE_LOG_DIR/dbus/$6" ;;
esac
EOF
    chmod +x "$bin"/*
}

## Create a fresh fake machine in $CASE: a supported ROG laptop on a new-enough kernel,
## with systemd running, power-profiles-daemon active, and nothing installed yet.
new_case() {
    CASE="$TEST_ROOT/$1"
    mkdir -p "$CASE/bin" "$CASE/dmi" "$CASE/platform" "$CASE/root" "$CASE/home" "$CASE/cache" \
        "$CASE/systemd" "$CASE/active" "$CASE/dbus"
    printf 'ASUSTeK COMPUTER INC.\n' >"$CASE/dmi/sys_vendor"
    printf 'ROG Strix SCAR 16 G635LX_G635LX\n' >"$CASE/dmi/product_name"
    printf 'ROG Strix SCAR 16\n' >"$CASE/dmi/product_family"
    ln -s "$CASE/bin" "$CASE/platform/driver"
    printf '7.0.0-34-generic\n' >"$CASE/osrelease"
    printf 'balanced\n' >"$CASE/profile"
    : >"$CASE/active/power-profiles-daemon.service"
    : >"$CASE/installed.list"
    write_fakes
}

## Run the installer in $CASE with the given arguments; set OUTPUT and STATUS.
run_installer() {
    local -a overrides=()
    if [[ -n "${VERSION_UNDER_TEST:-}" ]]; then
        overrides+=("ASUSCTL_INSTALL_VERSION=$VERSION_UNDER_TEST")
    fi
    STATUS=0
    OUTPUT="$(
        env PATH="$CASE/bin:$PATH" \
            HOME="$CASE/home" \
            XDG_CACHE_HOME="$CASE/cache" \
            FAKE_LOG_DIR="$CASE" \
            FAKE_PROFILE_FILE="$CASE/profile" \
            ASUSCTL_INSTALL_ROOT="$CASE/root" \
            ASUSCTL_INSTALL_DMI_DIR="$CASE/dmi" \
            ASUSCTL_INSTALL_PLATFORM_DIR="$CASE/platform" \
            ASUSCTL_INSTALL_OSRELEASE_FILE="$CASE/osrelease" \
            ASUSCTL_INSTALL_SYSTEMD_DIR="$CASE/systemd" \
            ASUSCTL_INSTALL_PROFILE_FILE="$CASE/profile" \
            ASUSCTL_INSTALL_SUDO="$CASE/bin/sudo" \
            ASUSCTL_INSTALL_DAEMON_WAIT=0 \
            ASUSCTL_INSTALL_REPO_URL="file://$UPSTREAM" \
            ASUSCTL_INSTALL_COMMIT="${COMMIT_UNDER_TEST:-$UPSTREAM_COMMIT}" \
            ${overrides[@]+"${overrides[@]}"} \
            "${INSTALLER_COMMAND[@]}" "$@" 2>&1
    )" || STATUS=$?
}

assert_status() {
    [[ "$STATUS" -eq "$1" ]] || fail "$2 (exit $STATUS, expected $1)" "$OUTPUT"
}

assert_output() {
    [[ "$OUTPUT" == *"$1"* ]] || fail "$2: output lacks '$1'" "$OUTPUT"
}

assert_output_matches() {
    grep -Eq -- "$1" <<<"$OUTPUT" || fail "$2: output does not match /$1/" "$OUTPUT"
}

refute_output() {
    [[ "$OUTPUT" != *"$1"* ]] || fail "$2: output must not contain '$1'" "$OUTPUT"
}

log_lines() {
    if [[ -f "$CASE/$1.log" ]]; then wc -l <"$CASE/$1.log"; else echo 0; fi
}

assert_log_lines() {
    [[ "$(log_lines "$1")" -eq "$2" ]] ||
        fail "$3: expected $2 line(s) in $1.log" "$(cat "$CASE/$1.log" 2>/dev/null || echo '(no log)')"
}

assert_log_has() {
    grep -Fxq -- "$2" "$CASE/$1.log" 2>/dev/null ||
        fail "$3: $1.log lacks the line '$2'" "$(cat "$CASE/$1.log" 2>/dev/null || echo '(no log)')"
}

## Nothing was escalated, installed, or built; only read-only systemd and D-Bus queries
## may have happened.
assert_read_only() {
    local name
    for name in sudo apt-get cargo udevadm; do
        assert_log_lines "$name" 0 "$1 must not run $name"
    done
    if grep -qv '^is-active ' "$CASE/systemctl.log" 2>/dev/null; then
        fail "$1: systemctl may only query" "$(cat "$CASE/systemctl.log")"
    fi
    if grep -qv ' get-property ' "$CASE/busctl.log" 2>/dev/null; then
        fail "$1: busctl may only read" "$(cat "$CASE/busctl.log")"
    fi
}

## Nothing at all happened: no command ran and no file or build directory exists.
assert_untouched() {
    assert_read_only "$1"
    assert_log_lines systemctl 0 "$1 must not query systemd"
    assert_log_lines busctl 0 "$1 must not query D-Bus"
    [[ -z "$(find "$CASE/root" -type f 2>/dev/null)" ]] || fail "$1: no file may be installed" "$(find "$CASE/root" -type f)"
    [[ -z "$(ls -A "$CASE/cache")" ]] || fail "$1: no build directory may be left behind" "$(ls -A "$CASE/cache")"
}

assert_nothing_installed() {
    [[ -z "$(find "$CASE/root" -type f)" ]] || fail "$1: nothing may be installed" "$(find "$CASE/root" -type f)"
    [[ -z "$(ls -A "$CASE/cache")" ]] || fail "$1: no build directory may be left behind" "$(ls -A "$CASE/cache")"
}

assert_file_mode() {
    local mode
    mode="$(stat -c '%a' "$CASE/root/$1" 2>/dev/null)" || fail "$3: $1 was not installed"
    [[ "$mode" == "$2" ]] || fail "$3: $1 has mode $mode, expected $2"
}

## Run the installer and expect success.
install_ok() {
    run_installer "$@"
    assert_status 0 install
}

test_other_machines_are_skipped() {
    new_case not-asus
    printf 'LENOVO\n' >"$CASE/dmi/sys_vendor"
    run_installer
    assert_status 0 'non-ASUS vendor'
    assert_output 'not an ASUS machine (DMI vendor: LENOVO)' 'non-ASUS vendor'
    assert_untouched 'non-ASUS vendor'

    new_case missing-dmi
    rm -r "$CASE/dmi"
    run_installer
    assert_status 0 'machine without DMI data'
    assert_output 'DMI vendor: unknown' 'machine without DMI data'
    assert_untouched 'machine without DMI data'

    new_case unsupported-family
    printf 'PRIME\n' >"$CASE/dmi/product_family"
    run_installer
    assert_status 0 'ASUS machine outside the supported families'
    assert_output "DMI family 'PRIME' is not one that asusd supports" 'unsupported family'
    assert_untouched 'unsupported family'

    new_case old-kernel
    printf '6.8.0-90-generic\n' >"$CASE/osrelease"
    run_installer
    assert_status 0 'kernel older than 6.19'
    assert_output 'older than 6.19' 'old kernel'
    assert_untouched 'old kernel'

    new_case numeric-kernel-compare
    printf '6.9.1\n' >"$CASE/osrelease"
    run_installer
    assert_output 'older than 6.19' '6.9 is older than 6.19 (numeric, not textual, comparison)'
    assert_untouched 'kernel 6.9'

    new_case unbound-driver
    rm "$CASE/platform/driver"
    run_installer
    assert_status 0 'asus-nb-wmi not bound'
    assert_output 'asus-nb-wmi driver is not bound' 'unbound driver'
    assert_untouched 'unbound driver'
}

test_every_supported_family_is_accepted() {
    local family
    local -a families=('ROG Strix SCAR 16' 'TUF Gaming A15' 'ROG Zephyrus G14' 'Vivobook S 15' 'ASUS Vivo Book Pro'
        'ASUSLaptop' 'Zenbook 14' 'ProArt PX13' 'TX Air' 'TX Gaming' 'EXPERTBOOK B9')
    new_case families
    for family in "${families[@]}"; do
        printf '%s\n' "$family" >"$CASE/dmi/product_family"
        run_installer --dry-run
        refute_output 'Skipping asusctl' "family '$family' must be a target"
        assert_output 'Dry run' "family '$family' must reach the plan"
    done
    printf '6.19.0-rc1\n' >"$CASE/osrelease"
    run_installer --dry-run
    refute_output 'Skipping asusctl' 'kernel 6.19 is new enough'
}

test_package_manager_gate() {
    local shell_path
    # shellcheck disable=SC2016 # the child shell, not this one, expands $1
    local absent='source "$1"; ! has_apt' present='source "$1"; has_apt'

    new_case no-apt
    mkdir -p "$CASE/empty"
    # The installer is sourced (its main only runs when executed). The shell is given
    # by absolute path so the child still starts when its PATH holds no commands; $BASH
    # is not used because launchers that set $_ can make it point at the launcher.
    shell_path="$(command -v bash)"
    env PATH="$CASE/empty" "$shell_path" -c "$absent" _ "$SCRIPT" ||
        fail 'has_apt must fail without apt-get and dpkg-query'
    env PATH="$CASE/bin:$PATH" "$shell_path" -c "$present" _ "$SCRIPT" ||
        fail 'has_apt must succeed with apt-get and dpkg-query'

    # The install path must honour the gate: on a supported laptop whose package manager
    # is not apt it says so and stops before escalating, building, or writing anything.
    # The machine here has apt, so the installer is sourced with has_apt replaced.
    # shellcheck disable=SC2016 # the child shell, not this one, expands $1
    INSTALLER_COMMAND=(bash -c 'source "$1"; has_apt() { return 1; }; install_main' _ "$SCRIPT")
    run_installer
    INSTALLER_COMMAND=(bash "$SCRIPT")
    assert_status 0 'system without apt'
    assert_output 'Skipping asusctl: the automatic install supports apt-based systems only.' 'system without apt'
    assert_untouched 'system without apt'
}

test_dry_run_changes_nothing() {
    new_case dry-run
    run_installer --dry-run
    assert_status 0 'dry run'
    assert_output "Dry run - would install the build dependencies: $ALL_PACKAGES" 'dry run lists the missing packages'
    assert_output "would clone asusctl $TAG from file://$UPSTREAM and require commit $UPSTREAM_COMMIT" 'dry run names the pin'
    assert_output 'would build it as tester with: cargo build --release --locked -p asusctl -p asusd -p asus-shutdown' 'dry run names the build'
    assert_output 'would turn off asusd' 'dry run names the profile policy'
    assert_untouched 'dry run'

    printf 'git\nmake\n' >"$CASE/installed.list"
    FAKE_UID=0 run_installer --dry-run
    assert_status 0 'dry run works as root'
    assert_output 'build dependencies: ca-certificates build-essential pkg-config' 'dry run lists only the missing packages'
    refute_output 'ca-certificates git' 'installed packages are not listed'
    assert_untouched 'dry run as root'
}

test_root_is_refused() {
    new_case root
    FAKE_UID=0 run_installer
    assert_status 1 'running as root'
    assert_output 'Run this as your normal user' 'root must be told why'
    assert_untouched 'root'
}

test_full_installation() {
    local manifest marker
    new_case install
    install_ok
    assert_output "asusctl $TAG is installed." 'full installation'

    assert_log_has apt-get "install -y --no-install-recommends $ALL_PACKAGES" 'the missing packages are installed'
    assert_log_lines cargo 1 'one build'
    grep -Fq '|build --release --locked -p asusctl -p asusd -p asus-shutdown' "$CASE/cargo.log" ||
        fail 'only the daemon, CLI, and shutdown helper are built (no GUI)'

    assert_file_mode usr/bin/asusd 755 'binary mode'
    assert_file_mode usr/bin/asusctl 755 'binary mode'
    assert_file_mode usr/bin/asus-shutdown 755 'binary mode'
    assert_file_mode usr/lib/systemd/system/asusd.service 644 'unit mode'
    assert_file_mode usr/lib/udev/rules.d/99-asusd.rules 644 'udev rule mode'
    assert_file_mode usr/share/dbus-1/system.d/asusd.conf 644 'D-Bus policy mode'
    assert_file_mode "$ANIME_FILE" 644 'a name with a space and an apostrophe survives'

    manifest="$CASE/root/var/lib/cuberhaus/asusctl/manifest"
    marker="$CASE/root/var/lib/cuberhaus/asusctl/version"
    [[ "$(cat "$marker")" == "$TAG $UPSTREAM_COMMIT" ]] || fail 'the marker records the pinned release' "$(cat "$marker")"
    grep -Fxq "/$ANIME_FILE" "$manifest" || fail 'the manifest lists the file with the awkward name'
    grep -Fxq /usr/bin/asusd "$manifest" || fail 'the manifest lists the daemon'
    if grep -qv '^/usr/' "$manifest"; then fail 'every recorded file is below /usr' "$(cat "$manifest")"; fi
    [[ "$(LC_ALL=C sort "$manifest")" == "$(cat "$manifest")" ]] || fail 'the manifest is sorted'
    [[ -z "$(ls -A "$CASE/cache")" ]] || fail 'the build directory is removed afterwards' "$(ls -A "$CASE/cache")"

    assert_log_has systemctl 'daemon-reload' 'systemd reloads its units'
    assert_log_has systemctl 'restart asusd.service' 'asusd starts'
    assert_log_has systemctl 'enable --now asus-shutdown.service' 'the shutdown helper is enabled'
    assert_log_has udevadm 'control --reload' 'udev reloads its rules'
    assert_log_has busctl '--system set-property xyz.ljones.Asusd /xyz/ljones xyz.ljones.Platform ChangePlatformProfileOnAc b false' 'AC switching off'
    assert_log_has busctl '--system set-property xyz.ljones.Asusd /xyz/ljones xyz.ljones.Platform ChangePlatformProfileOnBattery b false' 'battery switching off'
    assert_log_has busctl '--system set-property xyz.ljones.Asusd /xyz/ljones xyz.ljones.Platform PlatformProfileLinkedEpp b false' 'linked EPP off'
    if grep -v '^is-active ' "$CASE/systemctl.log" | grep -q 'power-profiles-daemon'; then
        fail 'the installer must never stop, disable, or mask power-profiles-daemon' "$(cat "$CASE/systemctl.log")"
    fi
    assert_output 'power-profiles-daemon keeps the Power Mode' 'the policy is explained'
    refute_output 'changed the firmware platform profile' 'no profile change was caused'
    assert_output 'asusctl battery limit 80' 'next steps are printed'
}

test_second_run_changes_nothing() {
    new_case idempotent
    install_ok
    run_installer
    assert_status 0 'second run'
    assert_output "asusctl $TAG is already installed" 'second run is a no-op'
    assert_log_lines cargo 1 'the second run must not rebuild'
    assert_log_lines apt-get 1 'the second run must not install packages again'
    assert_log_has systemctl 'start asusd.service' 'the second run only makes sure asusd runs'
    [[ "$(grep -c 'set-property' "$CASE/busctl.log")" -eq 3 ]] || fail 'the policy is not applied twice' "$(cat "$CASE/busctl.log")"
    refute_output 'changed the firmware platform profile' 'the second run is quiet'

    rm "$CASE/root/usr/bin/asusctl"
    run_installer
    assert_status 0 'repair after a deleted file'
    assert_log_lines cargo 2 'a recorded file that vanished triggers a reinstall'
    assert_file_mode usr/bin/asusctl 755 'repaired file'
}

test_force_rebuilds() {
    new_case force
    install_ok
    run_installer --force
    assert_status 0 '--force on a current install'
    assert_log_lines cargo 2 '--force rebuilds'
}

test_force_bypasses_the_hardware_gate() {
    new_case force-gate
    printf '6.8.0-90-generic\n' >"$CASE/osrelease"
    run_installer --force
    assert_status 0 '--force on an old kernel'
    assert_output 'Continuing although' '--force explains what it ignored'
    assert_log_lines cargo 1 '--force builds despite the kernel check'
}

test_pin_mismatch_aborts_before_building() {
    new_case moved-tag
    COMMIT_UNDER_TEST=0000000000000000000000000000000000000001 run_installer
    assert_status 1 'tag that no longer matches the pin'
    assert_output 'not at the pinned 0000000000000000000000000000000000000001' 'pin mismatch'
    assert_log_lines cargo 0 'nothing is built from an unpinned tree'
    assert_nothing_installed 'unpinned tree'
}

test_build_problems_install_nothing() {
    new_case build-failure
    FAKE_CARGO_FAIL=true run_installer
    assert_status 1 'failing build'
    assert_output 'The build failed' 'failing build'
    assert_nothing_installed 'failing build'

    new_case old-rust
    FAKE_RUSTC_VERSION=1.70.0 run_installer
    assert_status 1 'old Rust'
    assert_output "needs Rust 1.82 or newer; found '1.70.0'" 'old Rust'
    assert_log_lines cargo 0 'old Rust is rejected before building'
    assert_nothing_installed 'old Rust'

    new_case missing-library
    FAKE_LDD_MISSING=true run_installer
    assert_status 1 'binary with a missing library'
    assert_output 'libmissing.so.1 => not found' 'missing library is named'
    assert_nothing_installed 'missing library'

    new_case apt-failure
    FAKE_APT_FAIL=true run_installer
    assert_status 1 'failing apt'
    assert_output 'Installing the build dependencies failed' 'failing apt'
    assert_log_lines cargo 0 'nothing is built when the dependencies fail'
    assert_nothing_installed 'failing apt'
}

test_unexpected_upstream_layouts_are_rejected() {
    local escaped="$TEST_ROOT/upstream-escape" missing="$TEST_ROOT/upstream-no-rules" commit

    make_upstream "$escaped" "$TAG" escape
    commit="$(git -C "$escaped" rev-parse HEAD)"
    new_case escaping-makefile
    UPSTREAM="$escaped" COMMIT_UNDER_TEST="$commit" run_installer
    assert_status 1 'Makefile that installs outside /usr'
    assert_output 'files outside /usr' 'escape is reported'
    assert_nothing_installed 'Makefile that escapes /usr'

    make_upstream "$missing" "$TAG" no-rules
    commit="$(git -C "$missing" rev-parse HEAD)"
    new_case missing-udev-rule
    UPSTREAM="$missing" COMMIT_UNDER_TEST="$commit" run_installer
    assert_status 1 'staged install without the udev rule'
    assert_output 'lacks /usr/lib/udev/rules.d/99-asusd.rules' 'missing required file is reported'
    assert_nothing_installed 'staged install without the udev rule'
}

test_unmanaged_and_packaged_installs_are_left_alone() {
    new_case manual-install
    mkdir -p "$CASE/root/usr/bin"
    printf 'manual\n' >"$CASE/root/usr/bin/asusd"
    run_installer
    assert_status 0 'unmanaged asusd'
    assert_output 'was not installed by this script' 'unmanaged copy is reported'
    assert_log_lines cargo 0 'an unmanaged copy is not replaced'
    [[ "$(cat "$CASE/root/usr/bin/asusd")" == manual ]] || fail 'an unmanaged copy is untouched'

    run_installer --force
    assert_status 0 '--force over an unmanaged copy'
    assert_output 'Replacing the asusctl files that this script did not install' '--force explains the replacement'
    [[ "$(cat "$CASE/root/usr/bin/asusd")" != manual ]] || fail '--force replaces an unmanaged copy'

    new_case packaged-install
    mkdir -p "$CASE/root/usr/bin"
    printf 'packaged\n' >"$CASE/root/usr/bin/asusd"
    printf 'asusctl /usr/bin/asusd\n' >"$CASE/dpkg-owner"
    run_installer
    assert_status 0 'packaged asusd'
    assert_output "already provided by the package 'asusctl'" 'package is named'
    assert_log_lines cargo 0 'a packaged copy is not replaced'

    run_installer --force
    assert_status 1 '--force over a packaged copy'
    assert_output "belongs to the package 'asusctl'" '--force never overwrites package files'
    assert_log_lines cargo 0 'a packaged copy is not rebuilt over'
    [[ "$(cat "$CASE/root/usr/bin/asusd")" == packaged ]] || fail 'package files are never overwritten'

    new_case foreign-unit
    mkdir -p "$CASE/root/etc/systemd/system"
    : >"$CASE/root/etc/systemd/system/asusd.service"
    run_installer
    assert_status 0 'unit override from another installation'
    assert_output '/etc/systemd/system/asusd.service exists' 'a unit override from another install is detected'
    assert_log_lines cargo 0 'another installation owns the daemon'
}

test_profile_policy() {
    new_case no-profile-daemon
    rm "$CASE/active/power-profiles-daemon.service"
    install_ok
    assert_output 'No other power-profile daemon is running' 'without another daemon asusd keeps managing profiles'
    assert_log_lines busctl 0 'asusd keeps its defaults when nothing else manages profiles'

    new_case tuned
    rm "$CASE/active/power-profiles-daemon.service"
    : >"$CASE/active/tuned.service"
    install_ok
    assert_output 'tuned keeps the Power Mode' 'tuned also owns the Power Mode'

    new_case already-off
    FAKE_DBUS_DEFAULT=false install_ok
    assert_output 'already off' 'a policy that is already applied is left alone'
    [[ "$(grep -c 'set-property' "$CASE/busctl.log" || true)" -eq 0 ]] || fail 'nothing is set when all flags are off'

    new_case no-platform-control
    FAKE_BUSCTL_UNAVAILABLE=true install_ok
    assert_output 'offers no platform-profile control' 'hardware without platform profiles needs no policy'

    new_case profile-changed
    FAKE_ASUSD_SETS_PROFILE=performance install_ok
    assert_output "changed the firmware platform profile from 'balanced' to 'performance'" 'the first-start switch is reported'
    assert_output 'Choose your Power Mode again' 'the report says what to do'

    new_case profile-changed-without-owner
    rm "$CASE/active/power-profiles-daemon.service"
    FAKE_ASUSD_SETS_PROFILE=performance install_ok
    refute_output 'changed the firmware platform profile' 'asusd switching is intended when it owns the profile'
}

test_service_failures_are_reported() {
    new_case asusd-fails
    FAKE_ASUSD_FAIL=true run_installer
    assert_status 1 'asusd that cannot start'
    assert_output 'journalctl -u asusd.service' 'the failure points at the journal'
    assert_output 'service setup needs attention' 'the failure is summarised'
    assert_file_mode usr/bin/asusd 755 'the files stay installed after a service failure'
    [[ -f "$CASE/root/var/lib/cuberhaus/asusctl/manifest" ]] || fail 'the installation is recorded even when asusd fails'

    new_case no-systemd
    rmdir "$CASE/systemd"
    install_ok
    assert_output 'systemd is not running here' 'no systemd'
    assert_file_mode usr/bin/asusd 755 'files are installed without systemd'
    assert_log_lines systemctl 0 'systemctl is never called without systemd'
    assert_log_lines busctl 0 'D-Bus is never called without systemd'
}

test_unattended_never_prompts() {
    new_case unattended
    install_ok --unattended
    [[ -s "$CASE/sudo.log" ]] || fail 'the installation used sudo'
    if grep -q '^interactive ' "$CASE/sudo.log"; then
        fail 'every sudo call must be noninteractive' "$(cat "$CASE/sudo.log")"
    fi

    new_case unattended-without-credentials
    FAKE_SUDO_FAIL=true run_installer --unattended
    assert_status 1 'unattended without cached credentials'
    assert_output "Run 'sudo -v'" 'the failure explains how to authorize'
    assert_log_lines cargo 0 'the credentials are checked before the long build'
    assert_log_lines apt-get 0 'the credentials are checked before installing packages'
}

test_upgrade_removes_stale_files() {
    local next="$TEST_ROOT/upstream-next" commit
    new_case upgrade
    install_ok
    assert_file_mode usr/share/asusd/anime/custom/rust.png 644 'file shipped by the first release'

    make_upstream "$next" 6.3.9 drop-anime
    commit="$(git -C "$next" rev-parse HEAD)"
    mkdir -p "$CASE/root/usr/share/asusd/anime/custom"
    printf 'mine\n' >"$CASE/root/usr/share/asusd/anime/custom/personal.gif"
    UPSTREAM="$next" VERSION_UNDER_TEST=6.3.9 COMMIT_UNDER_TEST="$commit" run_installer
    assert_status 0 'upgrade to the next release'
    [[ ! -e "$CASE/root/usr/share/asusd/anime/custom/rust.png" ]] || fail 'a file the new release no longer ships is removed'
    [[ -f "$CASE/root/usr/share/asusd/anime/custom/personal.gif" ]] || fail 'a file the user added is kept'
    assert_file_mode "$ANIME_FILE" 644 'files of the new release are present'
    [[ "$(cat "$CASE/root/var/lib/cuberhaus/asusctl/version")" == "6.3.9 $commit" ]] || fail 'the marker records the new release'
    [[ "$(grep -cx 'restart asusd.service' "$CASE/systemctl.log")" -eq 2 ]] || fail 'the new daemon is restarted' "$(cat "$CASE/systemctl.log")"
}

test_uninstall() {
    local before
    new_case uninstall
    install_ok
    mkdir -p "$CASE/root/etc/asusd" "$CASE/root/usr/share/asusd/anime/custom"
    printf '(settings)\n' >"$CASE/root/etc/asusd/asusd.ron"
    printf 'mine\n' >"$CASE/root/usr/share/asusd/anime/custom/personal.gif"
    before="$(find "$CASE/root" -type f | wc -l)"

    run_installer --uninstall --dry-run
    assert_status 0 'uninstall dry run'
    assert_output 'would remove the' 'dry run names the removal'
    [[ "$(find "$CASE/root" -type f | wc -l)" -eq "$before" ]] || fail 'uninstall --dry-run removes nothing'

    run_installer --uninstall
    assert_status 0 'uninstall'
    assert_output 'The daemon settings in /etc/asusd were kept' 'settings are kept by default'
    assert_log_has systemctl 'disable --now asus-shutdown.service' 'the shutdown helper is disabled'
    assert_log_has systemctl 'stop asusd.service' 'asusd is stopped'
    [[ ! -e "$CASE/root/usr/bin/asusd" && ! -e "$CASE/root/$ANIME_FILE" ]] || fail 'installed files are removed'
    [[ ! -d "$CASE/root/var/lib/cuberhaus" ]] || fail 'the state directory is removed'
    [[ -f "$CASE/root/etc/asusd/asusd.ron" ]] || fail 'the daemon settings are kept'
    [[ -f "$CASE/root/usr/share/asusd/anime/custom/personal.gif" ]] || fail 'files the user added are kept'

    run_installer --uninstall
    assert_status 0 'second uninstall'
    assert_output 'Nothing to uninstall' 'second uninstall is a no-op'

    new_case purge
    install_ok
    mkdir -p "$CASE/root/etc/asusd"
    printf '(settings)\n' >"$CASE/root/etc/asusd/asusd.ron"
    run_installer --uninstall --purge
    assert_status 0 'uninstall --purge'
    [[ ! -e "$CASE/root/etc/asusd" ]] || fail '--purge deletes the daemon settings'
    [[ -z "$(find "$CASE/root" -type f)" ]] || fail 'nothing is left behind' "$(find "$CASE/root" -type f)"
}

test_status_is_read_only() {
    new_case status-fresh
    run_installer --status
    assert_status 0 'status of a fresh machine'
    assert_output_matches 'Supported target: +yes' 'status recognises the laptop'
    assert_output 'not installed by this script' 'status reports the missing install'
    assert_output 'asusctl is not installed' 'status says how to install'
    assert_read_only 'status'
    assert_nothing_installed 'status'

    new_case status-installed
    install_ok
    : >"$CASE/sudo.log"
    run_installer --status
    assert_status 0 'status of a healthy installation'
    assert_output "$TAG (up to date)" 'status reports the current version'
    assert_output 'asusd leaves profile switching to power-profiles-daemon' 'status reports the profile policy'
    assert_output "asusctl $TAG is installed and asusd is running" 'status verdict'
    assert_log_lines sudo 0 'status never uses sudo'

    printf 'true\n' >"$CASE/dbus/ChangePlatformProfileOnAc"
    run_installer --status
    assert_output 'will fight power-profiles-daemon' 'status flags a profile conflict'
    assert_output 'Apply the profile policy by rerunning' 'the verdict names the repair for a conflict'
    printf 'false\n' >"$CASE/dbus/ChangePlatformProfileOnAc"

    FAKE_BUSCTL_UNAVAILABLE=true run_installer --status
    assert_output 'asusd is not answering on D-Bus' 'status survives a silent daemon'

    rm "$CASE/active/asusd.service"
    run_installer --status
    assert_output_matches 'asusd.service: +inactive' 'status reports a stopped service'
    assert_output 'asusd is not running' 'the verdict says asusd is down'
    refute_output 'is installed and asusd is running' 'a stopped service is never reported as healthy'
    : >"$CASE/active/asusd.service"

    rm "$CASE/root/usr/bin/asusctl"
    run_installer --status
    assert_output 'recorded files are missing' 'status reports a damaged installation'
    assert_output 'The installation is out of date or damaged' 'the verdict says to repair a damaged installation'
    assert_log_lines sudo 0 'none of the status runs used sudo'

    new_case status-other-machine
    printf 'LENOVO\n' >"$CASE/dmi/sys_vendor"
    run_installer --status
    assert_status 0 'status on another machine'
    assert_output 'Nothing to do on this machine' 'status on other hardware'

    new_case status-old-install
    install_ok
    printf '6.0.0 deadbeef\n' >"$CASE/root/var/lib/cuberhaus/asusctl/version"
    run_installer --status
    assert_output "6.0.0 (pinned release is $TAG)" 'status reports an outdated install'
}

test_usage_errors() {
    new_case usage
    run_installer --bogus
    assert_status 2 'unknown option'
    run_installer --status --dry-run
    assert_status 2 '--status with --dry-run'
    run_installer --status --force
    assert_status 2 '--status with --force'
    run_installer --uninstall --status
    assert_status 2 '--uninstall with --status'
    run_installer --purge
    assert_status 2 '--purge without --uninstall'
    run_installer --uninstall --force
    assert_status 2 '--uninstall with --force'
    run_installer --help
    assert_status 0 '--help'
    assert_output 'Usage: asusctl_install.sh' '--help prints the usage'
    assert_untouched 'usage errors'
}

## Write a stand-in installer to $1 that records its arguments and sudo setting.
write_recording_installer() {
    cat >"$1" <<'EOF'
#!/usr/bin/env bash
printf 'args=%s sudo=%s\n' "$*" "${ASUSCTL_INSTALL_SUDO:-}" >>"$WRAPPER_LOG"
EOF
}

test_bootstrap_and_repair_wiring() {
    local checkout="$TEST_ROOT/checkout" home="$TEST_ROOT/wrapper-home" log="$TEST_ROOT/wrapper.log" repo="$TEST_ROOT/repair-repo"
    local usage_text

    mkdir -p "$checkout/.local/scripts" "$home/.local/scripts" "$repo/.local/scripts"
    write_recording_installer "$checkout/.local/scripts/asusctl_install.sh"
    write_recording_installer "$home/.local/scripts/asusctl_install.sh"
    write_recording_installer "$repo/.local/scripts/asusctl_install.sh"
    cp "$REPAIR_SCRIPT" "$repo/.local/scripts/repair-installation"
    : >"$log"

    (
        # shellcheck source=/dev/null
        source "$BASE_FUNCTIONS"
        export WRAPPER_LOG="$log"
        unset SUDO_COMMAND ASUSCTL_INSTALL_SUDO UNATTENDED DOTFILES_ROOT
        DOTFILES_ROOT="$checkout" UNATTENDED=true SUDO_COMMAND=privilege-runner asusctl_install
        DOTFILES_ROOT="$checkout" UNATTENDED=false asusctl_install
        DOTFILES_ROOT="$checkout" asusctl_uninstall
        HOME="$home" asusctl_install
        # A failing uninstall must not abort the uninstall menu.
        printf '#!/usr/bin/env bash\nexit 1\n' >"$checkout/.local/scripts/asusctl_install.sh"
        DOTFILES_ROOT="$checkout" asusctl_uninstall
    ) || fail 'the bootstrap wrappers must run and tolerate a failing uninstall'
    [[ "$(cat "$log")" == $'args=--unattended sudo=privilege-runner\nargs= sudo=sudo\nargs=--uninstall sudo=\nargs= sudo=sudo' ]] ||
        fail 'asusctl_install must pass --unattended and the sudo command, and fall back to HOME' "$(cat "$log")"

    : >"$log"
    WRAPPER_LOG="$log" DRY_RUN=true bash "$repo/.local/scripts/repair-installation" asusctl auto
    WRAPPER_LOG="$log" bash "$repo/.local/scripts/repair-installation" asusctl auto
    [[ "$(cat "$log")" == $'args=--dry-run sudo=\nargs= sudo=' ]] ||
        fail 'make repair REPAIR=asusctl must run the installer and honour DRY_RUN' "$(cat "$log")"
    usage_text="$(bash "$repo/.local/scripts/repair-installation" bogus 2>&1 || true)"
    [[ "$usage_text" == *'ide-repos|asusctl'* ]] || fail 'the repair usage text must list asusctl' "$usage_text"

    grep -Fq 'asusctl_install ||' "$REPO_ROOT/.local/scripts/bootstrap/ubuntu" ||
        fail 'the ubuntu bootstrap must install asusctl without aborting on failure'
    grep -Fq 'make repair REPAIR=asusctl PROFILE=ubuntu' "$REPO_ROOT/.local/scripts/bootstrap/ubuntu" ||
        fail 'the ubuntu bootstrap must name the repair command'
    grep -Fq 'asusctl_uninstall' "$REPO_ROOT/.local/scripts/bootstrap/uninstall_work" ||
        fail 'the work uninstaller must offer asusctl'
    grep -Fq 'asusctl_uninstall' "$REPO_ROOT/.local/scripts/bootstrap/uninstall_ubuntu" ||
        fail 'the ubuntu uninstaller must offer asusctl'
    grep -Fq 'bash tests/test_asusctl_install.sh' "$REPO_ROOT/Makefile" ||
        fail 'make test must run this suite'
}

UPSTREAM="$TEST_ROOT/upstream"
make_upstream "$UPSTREAM" "$TAG"
UPSTREAM_COMMIT="$(git -C "$UPSTREAM" rev-parse HEAD)"

test_other_machines_are_skipped
test_every_supported_family_is_accepted
test_package_manager_gate
test_dry_run_changes_nothing
test_root_is_refused
test_full_installation
test_second_run_changes_nothing
test_force_rebuilds
test_force_bypasses_the_hardware_gate
test_pin_mismatch_aborts_before_building
test_build_problems_install_nothing
test_unexpected_upstream_layouts_are_rejected
test_unmanaged_and_packaged_installs_are_left_alone
test_profile_policy
test_service_failures_are_reported
test_unattended_never_prompts
test_upgrade_removes_stale_files
test_uninstall
test_status_is_read_only
test_usage_errors
test_bootstrap_and_repair_wiring

printf 'PASS: asusctl_install gates, builds from the pin, installs reversibly, and leaves profiles to their owner\n'
