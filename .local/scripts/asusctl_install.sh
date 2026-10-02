#!/usr/bin/env bash
# Install asusctl and its daemon asusd on ASUS laptops: platform profiles, fan curves,
# battery charge limit and keyboard lighting. The pinned upstream release is built as
# the invoking user, copied under /usr with sudo, and every file is recorded so
# --uninstall removes exactly what was added. Anything that is not a supported ASUS
# laptop is skipped. While power-profiles-daemon (or tuned) is running, asusd's own
# AC/battery profile switching is turned off so the two never fight over the profile.
# Rationale, trade-offs, and recovery: docs/ASUSCTL.md
set -euo pipefail

# Pinned upstream release. The commit is checked after cloning, so a moved tag or a
# tampered mirror aborts the build. To bump: change both, compare is_supported_family
# with data/asusd.rules of the new release, and rerun the tests (docs/ASUSCTL.md).
readonly VERSION="${ASUSCTL_INSTALL_VERSION:-6.3.8}"
readonly COMMIT="${ASUSCTL_INSTALL_COMMIT:-47bf9b134b4e702e0b5e18db7cc66839e42d9035}"
readonly REPO_URL="${ASUSCTL_INSTALL_REPO_URL:-https://gitlab.com/asus-linux/asusctl.git}"
readonly MIN_KERNEL='6.19'
readonly MIN_RUST='1.82'

# Test seams: every system location can be redirected so the tests never touch the
# real machine. ASUSCTL_INSTALL_ROOT behaves like DESTDIR.
readonly ROOT_DIR="${ASUSCTL_INSTALL_ROOT:-}"
readonly DMI_DIR="${ASUSCTL_INSTALL_DMI_DIR:-/sys/class/dmi/id}"
readonly PLATFORM_DEVICE_DIR="${ASUSCTL_INSTALL_PLATFORM_DIR:-/sys/devices/platform/asus-nb-wmi}"
readonly KERNEL_RELEASE_FILE="${ASUSCTL_INSTALL_OSRELEASE_FILE:-/proc/sys/kernel/osrelease}"
readonly SYSTEMD_DIR="${ASUSCTL_INSTALL_SYSTEMD_DIR:-/run/systemd/system}"
readonly PLATFORM_PROFILE_FILE="${ASUSCTL_INSTALL_PROFILE_FILE:-/sys/firmware/acpi/platform_profile}"
readonly SUDO_PROGRAM="${ASUSCTL_INSTALL_SUDO:-sudo}"
readonly DAEMON_WAIT_SECONDS="${ASUSCTL_INSTALL_DAEMON_WAIT:-10}"

readonly STATE_DIR='/var/lib/cuberhaus/asusctl'
readonly CONFIG_DIR='/etc/asusd'

# Build tools, then the libraries asusd links against. libclang-dev is needed by the
# git dependency "sg", whose build script runs bindgen. libusb-1.0-0 is listed on its
# own so a later "apt autoremove" of the -dev package cannot remove the runtime library.
readonly BUILD_PACKAGES=(
    ca-certificates git make build-essential pkg-config cargo
    libudev-dev libusb-1.0-0-dev libclang-dev libusb-1.0-0
)

# Files the staged install must contain, as a guard against an upstream layout change.
readonly REQUIRED_FILES=(
    /usr/bin/asusd /usr/bin/asusctl /usr/bin/asus-shutdown
    /usr/lib/systemd/system/asusd.service /usr/lib/systemd/system/asus-shutdown.service
    /usr/lib/udev/rules.d/99-asusd.rules /usr/share/dbus-1/system.d/asusd.conf
)

# asusd exposes these as read/write D-Bus properties and persists a change in its own
# config. All three default to true: asusd then switches the firmware platform profile
# (and the CPU energy preference) whenever the power source changes and at every start.
readonly DAEMON_BUS_NAME='xyz.ljones.Asusd'
readonly DAEMON_OBJECT='/xyz/ljones'
readonly PLATFORM_INTERFACE='xyz.ljones.Platform'
readonly SWITCHING_PROPERTIES=(
    ChangePlatformProfileOnAc ChangePlatformProfileOnBattery PlatformProfileLinkedEpp
)
readonly PROFILE_DAEMON_UNITS=(power-profiles-daemon.service tuned.service)

MODE=apply
DRY_RUN=false
FORCE=false
PURGE=false
UNATTENDED=false
SKIP_REASON=''
SKIP_SEVERITY=info
WORK_DIR=''
SRC_DIR=''
STAGE_DIR=''
MANIFEST=''

info() { printf '\033[1;34m[INFO]\033[0m  %s\n' "$*"; }
success() { printf '\033[1;32m[ OK ]\033[0m  %s\n' "$*"; }
warn() { printf '\033[1;33m[WARN]\033[0m  %s\n' "$*"; }
error() { printf '\033[1;31m[ERROR]\033[0m %s\n' "$*" >&2; }

## Print one aligned "label value" line.
field() { printf '  %-25s %s\n' "$1" "$2"; }

## Announce a step that a dry run does not perform.
plan() { info "Dry run - would $*"; }

usage() {
    cat <<EOF
Usage: ${0##*/} [--dry-run] [--force] [--unattended]
       ${0##*/} --uninstall [--purge] [--dry-run]
       ${0##*/} --status

Install asusctl $VERSION and its daemon asusd on a supported ASUS laptop, built from
the pinned upstream release. Other machines are skipped.

Options:
  --dry-run     Show every step without changing anything (no sudo needed).
  --force       Skip the hardware and kernel checks, rebuild an up-to-date install,
                and replace an asusctl that this script did not install.
  --unattended  Never prompt: use 'sudo -n' and fail when credentials are not cached.
  --uninstall   Stop the services and remove exactly the files this script installed.
  --purge       With --uninstall, also delete $CONFIG_DIR (daemon settings such as
                the battery charge limit).
  --status      Report hardware, installation, services, and profile ownership
                (read-only).
  -h, --help    Show this help.

Run it as your normal user: it builds as you and calls sudo only to install.
See docs/ASUSCTL.md for the design, the trade-offs, and the recovery steps.
EOF
}

set_mode() {
    if [[ "$MODE" != apply && "$MODE" != "$1" ]]; then
        error "Choose only one of --uninstall or --status."
        exit 2
    fi
    MODE="$1"
}

parse_args() {
    local argument
    for argument in "$@"; do
        case "$argument" in
            --dry-run) DRY_RUN=true ;;
            --force) FORCE=true ;;
            --unattended) UNATTENDED=true ;;
            --uninstall) set_mode uninstall ;;
            --purge) PURGE=true ;;
            --status) set_mode status ;;
            -h | --help)
                usage
                exit 0
                ;;
            *)
                error "Unknown option: $argument"
                usage >&2
                exit 2
                ;;
        esac
    done

    if [[ "$MODE" == status && ("$DRY_RUN" == true || "$FORCE" == true || "$PURGE" == true) ]]; then
        error "--status is read-only and cannot be combined with --dry-run, --force, or --purge."
        exit 2
    fi
    if [[ "$PURGE" == true && "$MODE" != uninstall ]]; then
        error "--purge only applies to --uninstall."
        exit 2
    fi
    if [[ "$MODE" == uninstall && "$FORCE" == true ]]; then
        error "--force does not apply to --uninstall."
        exit 2
    fi
}

## Print the first line of a file, or nothing when it is unreadable.
read_value() {
    local value=''
    if [[ -r "$1" ]]; then
        IFS= read -r value <"$1" || true
    fi
    printf '%s' "$value"
}

## Print the real location of the absolute system path $1 (prefixed in the tests).
target_path() { printf '%s%s' "$ROOT_DIR" "$1"; }

## Succeed when version $1 is at least version $2 (dotted numbers).
version_at_least() {
    [[ "$(printf '%s\n%s\n' "$2" "$1" | sort -V | head -n 1)" == "$2" ]]
}

## Print the major.minor of the running kernel.
kernel_version() {
    local release
    release="$(read_value "$KERNEL_RELEASE_FILE")"
    if [[ "$release" =~ ^([0-9]+\.[0-9]+) ]]; then
        printf '%s' "${BASH_REMATCH[1]}"
    fi
}

## Run a command as root: directly when already root, otherwise through sudo, which
## never prompts in unattended mode.
as_root() {
    if [[ "$(id -u)" -eq 0 ]]; then
        "$@"
    elif [[ "$UNATTENDED" == true ]]; then
        command "$SUDO_PROGRAM" -n "$@"
    else
        command "$SUDO_PROGRAM" "$@"
    fi
}

## Succeed when systemd is the running init system.
systemd_running() { [[ -d "$SYSTEMD_DIR" ]]; }

## Succeed for the DMI product families that the udev rule of the pinned release
## starts asusd for (data/asusd.rules); asusd never runs on other families.
is_supported_family() {
    case "$1" in
        *TUF* | *ROG* | *Zephyrus* | *Strix* | *Vivo*ook* | *ASUSLaptop* | *Zen*ook* | \
            *ProArt* | *'TX Air'* | *'TX Gaming'* | *EXPERTBOOK*) return 0 ;;
        *) return 1 ;;
    esac
}

## Succeed on a supported ASUS laptop. Otherwise set SKIP_REASON and SKIP_SEVERITY:
## info when the machine is simply not a target, warn when it is one asusctl cannot use.
check_target() {
    local vendor family kernel
    SKIP_REASON=''
    SKIP_SEVERITY=info

    vendor="$(read_value "$DMI_DIR/sys_vendor")"
    family="$(read_value "$DMI_DIR/product_family")"
    kernel="$(kernel_version)"

    if [[ "$vendor" != ASUS* ]]; then
        SKIP_REASON="this is not an ASUS machine (DMI vendor: ${vendor:-unknown})"
        return 1
    fi
    if ! is_supported_family "$family"; then
        SKIP_REASON="DMI family '${family:-unknown}' is not one that asusd supports"
        return 1
    fi

    SKIP_SEVERITY=warn
    if ! version_at_least "$kernel" "$MIN_KERNEL"; then
        SKIP_REASON="the running kernel (${kernel:-unknown}) is older than $MIN_KERNEL, which asusctl requires"
        return 1
    fi
    if [[ ! -L "$PLATFORM_DEVICE_DIR/driver" ]]; then
        SKIP_REASON="the asus-nb-wmi driver is not bound, so asusd would never be started"
        return 1
    fi
}

## Succeed on a system whose packages are managed by apt.
has_apt() {
    command -v apt-get >/dev/null 2>&1 && command -v dpkg-query >/dev/null 2>&1
}

## Print the first asusctl file on this machine that this script did not install.
foreign_install() {
    local path
    for path in /usr/bin/asusd /usr/bin/asusctl /usr/local/bin/asusd /usr/local/bin/asusctl \
        /etc/systemd/system/asusd.service; do
        if [[ -e "$(target_path "$path")" ]]; then
            printf '%s' "$path"
            return 0
        fi
    done
    return 1
}

## Print the dpkg package that owns the file $1, or fail when none does.
package_owner() {
    local owner
    owner="$(dpkg-query -S "$1" 2>/dev/null)" || return 1
    printf '%s' "${owner%%:*}"
}

## Succeed when the pinned release is installed and every recorded file is still there.
install_is_current() {
    local manifest path
    manifest="$(target_path "$STATE_DIR/manifest")"
    [[ "$(read_value "$(target_path "$STATE_DIR/version")")" == "$VERSION $COMMIT" ]] || return 1
    [[ -f "$manifest" ]] || return 1
    while IFS= read -r path || [[ -n "$path" ]]; do
        [[ -e "$(target_path "$path")" ]] || return 1
    done <"$manifest"
}

## Print the build packages that are not installed yet, one per line.
missing_packages() {
    local package
    for package in "${BUILD_PACKAGES[@]}"; do
        # shellcheck disable=SC2016 # dpkg-query format string, not a shell expansion
        if [[ "$(dpkg-query -W -f='${db:Status-Status}' "$package" 2>/dev/null)" != installed ]]; then
            printf '%s\n' "$package"
        fi
    done
}

install_build_packages() {
    local -a missing=()
    mapfile -t missing < <(missing_packages)
    if [[ "${#missing[@]}" -eq 0 ]]; then
        info "Build dependencies are already installed."
        return 0
    fi
    if [[ "$DRY_RUN" == true ]]; then
        plan "install the build dependencies: ${missing[*]}"
        return 0
    fi
    info "Installing build dependencies: ${missing[*]}"
    as_root env DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends "${missing[@]}" || {
        error "Installing the build dependencies failed. Try 'sudo apt-get update', then rerun."
        return 1
    }
}

## Fail early, before any long build, unless commands can run as root.
require_privileges() {
    if ! as_root true; then
        if [[ "$UNATTENDED" == true ]]; then
            error "Unattended mode needs cached sudo credentials. Run 'sudo -v' in this terminal, then retry."
        fi
        return 1
    fi
}

## Fail unless the installed Rust compiler can build the pinned release.
require_rust() {
    local version
    version="$(rustc --version 2>/dev/null | awk '{ print $2 }')" || version=''
    if ! version_at_least "$version" "$MIN_RUST"; then
        error "asusctl $VERSION needs Rust $MIN_RUST or newer; found '${version:-none}'."
        error "Install a newer toolchain (for example with rustup), then rerun."
        return 1
    fi
}

fetch_source() {
    local actual
    info "Fetching asusctl $VERSION from $REPO_URL..."
    GIT_TERMINAL_PROMPT=0 git -c advice.detachedHead=false clone --quiet --depth 1 \
        --branch "$VERSION" "$REPO_URL" "$SRC_DIR" || {
        error "Cloning $REPO_URL failed. Check the network and rerun."
        return 1
    }
    actual="$(git -C "$SRC_DIR" rev-parse HEAD)"
    if [[ "$actual" != "$COMMIT" ]]; then
        error "The upstream tag $VERSION points at $actual, not at the pinned $COMMIT."
        error "Refusing to build. Inspect the tag before changing the pin."
        return 1
    fi
}

build_source() {
    local binary
    info "Compiling asusctl $VERSION (a few minutes; fetches Rust crates from crates.io and GitHub)..."
    (cd "$SRC_DIR" && cargo build --release --locked -p asusctl -p asusd -p asus-shutdown) || {
        error "The build failed. Check the network and the messages above, then rerun."
        return 1
    }
    for binary in asusctl asusd asus-shutdown; do
        if [[ ! -x "$SRC_DIR/target/release/$binary" ]]; then
            error "The build did not produce $binary."
            return 1
        fi
    done
}

## Install the built files into a staging directory with the upstream Makefile, as the
## invoking user, and record their absolute paths in $MANIFEST. "-o" tells make that the
## binaries are up to date, so it never starts a build of its own (which would also
## compile the GUI).
stage_files() {
    local required
    mkdir -p "$STAGE_DIR"
    (
        umask 022
        make --no-print-directory -C "$SRC_DIR" \
            -o target/release/asusd -o target/release/asus-shutdown -o target/release/asusctl \
            install-asusd install-asus-shutdown install-asusctl install-data-asusd \
            DESTDIR="$STAGE_DIR" prefix=/usr >/dev/null
    ) || {
        error "Staging the build with the upstream Makefile failed."
        return 1
    }
    (cd "$STAGE_DIR" && find . \( -type f -o -type l \) -print | LC_ALL=C sort | sed 's|^\.||') >"$MANIFEST"

    for required in "${REQUIRED_FILES[@]}"; do
        if ! grep -Fxq -- "$required" "$MANIFEST"; then
            error "The staged install lacks $required; the upstream layout changed."
            return 1
        fi
    done
    if grep -qv '^/usr/' "$MANIFEST"; then
        error "The staged install contains files outside /usr:"
        grep -v '^/usr/' "$MANIFEST" >&2
        return 1
    fi
}

## Fail when a staged binary needs a shared library that is not installed.
verify_binaries() {
    local binary missing
    for binary in asusctl asusd asus-shutdown; do
        if missing="$(ldd "$STAGE_DIR/usr/bin/$binary" 2>&1 | grep 'not found')"; then
            error "$binary needs libraries that are not installed:"
            printf '%s\n' "$missing" >&2
            return 1
        fi
    done
}

## Runs as root. Copy every file listed in manifest $3 from staging directory $1 to
## prefix $2 with its staged mode, remove files that an older manifest in state
## directory $4 listed but this release no longer ships, then record the manifest and
## the version marker file $5 as the new state.
copy_staged_files() {
    local stage="$1" root="$2" manifest="$3" state="$4" marker="$5"
    local path stale
    local -a owner=()
    if [[ "$(id -u)" -eq 0 ]]; then
        owner=(-o root -g root)
    fi
    while IFS= read -r path; do
        install -D "${owner[@]}" -m "$(stat -c '%a' "$stage$path")" "$stage$path" "$root$path"
    done <"$manifest"
    if [[ -f "$root$state/manifest" ]]; then
        while IFS= read -r stale; do
            rm -f -- "$root$stale"
        done < <(LC_ALL=C comm -23 "$root$state/manifest" "$manifest")
    fi
    install -d "${owner[@]}" -m 0755 "$root$state"
    install "${owner[@]}" -m 0644 "$manifest" "$root$state/manifest"
    install "${owner[@]}" -m 0644 "$marker" "$root$state/version"
}

## Runs as root. Remove every file listed in manifest $2 below prefix $1, the directories
## left empty in the data directory that only asusd uses, and the state directory $3.
remove_installed_files() {
    local root="$1" manifest="$2" state="$3" path
    while IFS= read -r path; do
        rm -f -- "$root$path"
    done <"$manifest"
    find "$root/usr/share/asusd" -depth -type d -empty -delete 2>/dev/null || true
    rm -f -- "$root$state/manifest" "$root$state/version"
    rmdir "$root$state" 2>/dev/null || true
    rmdir "$(dirname "$root$state")" 2>/dev/null || true
}

install_files() {
    local count
    count="$(wc -l <"$MANIFEST")"
    printf '%s %s\n' "$VERSION" "$COMMIT" >"$WORK_DIR/version"
    info "Installing $count files under /usr (and recording them in $STATE_DIR)..."
    as_root bash -c "set -euo pipefail; $(declare -f copy_staged_files); copy_staged_files \"\$@\"" _ \
        "$STAGE_DIR" "$ROOT_DIR" "$MANIFEST" "$STATE_DIR" "$WORK_DIR/version" || {
        error "Copying the files into place failed."
        return 1
    }
}

## Print the active unit that manages the platform profile besides asusd, if any.
other_profile_daemon() {
    local unit
    for unit in "${PROFILE_DAEMON_UNITS[@]}"; do
        if systemctl is-active --quiet "$unit" 2>/dev/null; then
            printf '%s' "${unit%.service}"
            return 0
        fi
    done
    return 1
}

## Print "b true" or "b false" for an asusd platform property, as the invoking user.
daemon_property() {
    busctl --system get-property "$DAEMON_BUS_NAME" "$DAEMON_OBJECT" "$PLATFORM_INTERFACE" "$1" 2>/dev/null
}

## Wait for asusd to answer on D-Bus; succeed once it exposes the platform interface.
wait_for_platform_interface() {
    local attempt
    for ((attempt = 0; attempt <= DAEMON_WAIT_SECONDS; attempt++)); do
        if as_root busctl --system get-property "$DAEMON_BUS_NAME" "$DAEMON_OBJECT" \
            "$PLATFORM_INTERFACE" "${SWITCHING_PROPERTIES[0]}" >/dev/null 2>&1; then
            return 0
        fi
        if [[ "$attempt" -lt "$DAEMON_WAIT_SECONDS" ]]; then
            sleep 1
        fi
    done
    return 1
}

## While another daemon owns the Power Mode, turn off asusd's own profile switching.
## Without one, asusd keeps managing profiles on power-source changes, as upstream intends.
apply_profile_policy() {
    local daemon property current changed=false
    if ! daemon="$(other_profile_daemon)"; then
        info "No other power-profile daemon is running: asusd manages profiles itself."
        return 0
    fi
    if ! wait_for_platform_interface; then
        info "asusd offers no platform-profile control on this machine; nothing to switch off."
        return 0
    fi
    for property in "${SWITCHING_PROPERTIES[@]}"; do
        current="$(as_root busctl --system get-property "$DAEMON_BUS_NAME" "$DAEMON_OBJECT" \
            "$PLATFORM_INTERFACE" "$property")" || {
            warn "Could not read the asusd property $property."
            return 1
        }
        if [[ "$current" == 'b true' ]]; then
            as_root busctl --system set-property "$DAEMON_BUS_NAME" "$DAEMON_OBJECT" \
                "$PLATFORM_INTERFACE" "$property" b false || {
                warn "Could not switch off the asusd property $property."
                return 1
            }
            changed=true
        fi
    done
    if [[ "$changed" == true ]]; then
        success "Switched off asusd's automatic profile switching: $daemon keeps the Power Mode."
    else
        info "asusd's automatic profile switching is already off; $daemon keeps the Power Mode."
    fi
}

## asusd applies its AC/battery profile once while it starts, before the policy above can
## take effect. Say so when that changed the firmware profile that the user had selected.
report_profile_change() {
    local before="$1" after
    after="$(read_value "$PLATFORM_PROFILE_FILE")"
    if [[ -n "$before" && -n "$after" && "$before" != "$after" ]]; then
        warn "asusd changed the firmware platform profile from '$before' to '$after' while it first started."
        info "Choose your Power Mode again (GNOME Settings > Power, or: powerprofilesctl set balanced)."
    fi
}

## Start asusd and the shutdown helper. $1 is "restart" after files changed, otherwise "start".
start_services() {
    if ! systemd_running; then
        warn "systemd is not running here: the files are installed, but the services were not started."
        return 0
    fi
    if [[ "$1" == restart ]]; then
        as_root systemctl daemon-reload
        as_root udevadm control --reload
    fi
    if ! as_root systemctl "$1" asusd.service; then
        error "asusd.service failed to start. Inspect it with: journalctl -u asusd.service -n 50 --no-pager"
        return 1
    fi
    as_root systemctl enable --now asus-shutdown.service || {
        warn "asus-shutdown.service could not be enabled; inspect it with: systemctl status asus-shutdown.service"
        return 1
    }
}

## Start the services and apply the profile policy. $1 is "restart" or "start".
configure_runtime() {
    local before status=0 managed=false
    before="$(read_value "$PLATFORM_PROFILE_FILE")"
    start_services "$1" || return 1
    if systemd_running; then
        if other_profile_daemon >/dev/null; then
            managed=true
        fi
        apply_profile_policy || status=1
        if [[ "$managed" == true && "$1" == restart ]]; then
            report_profile_change "$before"
        fi
    fi
    return "$status"
}

print_next_steps() {
    echo "--------------------------------------------------------"
    info "Next steps (all optional):"
    info "  asusctl info                   # model and supported features"
    info "  asusctl battery limit 80       # stop charging at 80%, kept across reboots"
    info "  asusctl profile list           # firmware platform profiles"
    info "Check the installation any time with: $0 --status"
}

install_main() {
    local existing owner status=0 build_root

    if ! check_target; then
        if [[ "$FORCE" != true ]]; then
            if [[ "$SKIP_SEVERITY" == warn ]]; then
                warn "Skipping asusctl: $SKIP_REASON."
            else
                info "Skipping asusctl: $SKIP_REASON."
            fi
            return 0
        fi
        warn "Continuing although $SKIP_REASON, because --force was given."
    fi
    if ! has_apt; then
        info "Skipping asusctl: the automatic install supports apt-based systems only."
        return 0
    fi

    if [[ "$DRY_RUN" != true && "$(id -u)" -eq 0 ]]; then
        error "Run this as your normal user, not as root: it builds third-party code and uses sudo only to install."
        return 1
    fi

    if [[ ! -f "$(target_path "$STATE_DIR/manifest")" ]] && existing="$(foreign_install)"; then
        if owner="$(package_owner "$existing")"; then
            if [[ "$FORCE" == true ]]; then
                error "$existing belongs to the package '$owner'; remove the package instead of overwriting its files."
                return 1
            fi
            info "Skipping asusctl: it is already provided by the package '$owner' ($existing)."
            return 0
        fi
        if [[ "$FORCE" != true ]]; then
            info "Skipping asusctl: $existing exists and was not installed by this script (--force replaces it)."
            return 0
        fi
        warn "Replacing the asusctl files that this script did not install, because --force was given."
    fi

    if [[ "$FORCE" != true ]] && install_is_current; then
        success "asusctl $VERSION is already installed."
        if [[ "$DRY_RUN" == true ]]; then
            plan "make sure asusd is running and the profile policy is applied"
            return 0
        fi
        require_privileges || return 1
        configure_runtime start || status=1
        return "$status"
    fi

    if [[ "$DRY_RUN" != true ]]; then
        require_privileges || return 1
    fi

    install_build_packages || return 1
    if [[ "$DRY_RUN" == true ]]; then
        plan "clone asusctl $VERSION from $REPO_URL and require commit $COMMIT"
        plan "build it as $(id -un) with: cargo build --release --locked -p asusctl -p asusd -p asus-shutdown"
        plan "stage the files with the upstream Makefile, then copy them under /usr and record them in $STATE_DIR"
        plan "reload systemd and udev, start asusd.service, and enable asus-shutdown.service"
        plan "turn off asusd's automatic profile switching while power-profiles-daemon or tuned is running"
        return 0
    fi
    require_rust || return 1

    build_root="${XDG_CACHE_HOME:-$HOME/.cache}"
    mkdir -p "$build_root"
    WORK_DIR="$(mktemp -d "$build_root/asusctl-install.XXXXXX")"
    trap 'rm -rf "$WORK_DIR"' EXIT
    SRC_DIR="$WORK_DIR/src"
    STAGE_DIR="$WORK_DIR/stage"
    MANIFEST="$WORK_DIR/manifest"

    fetch_source || return 1
    build_source || return 1
    stage_files || return 1
    verify_binaries || return 1
    install_files || return 1

    configure_runtime restart || status=1
    if [[ "$status" -eq 0 ]]; then
        success "asusctl $VERSION is installed."
    else
        warn "asusctl $VERSION is installed, but the service setup needs attention (see above)."
        info "Rerun after fixing it with: $0"
    fi
    print_next_steps
    return "$status"
}

uninstall_main() {
    local manifest count
    manifest="$(target_path "$STATE_DIR/manifest")"
    if [[ ! -f "$manifest" ]]; then
        info "Nothing to uninstall: this script has not installed asusctl on this machine."
        return 0
    fi
    count="$(wc -l <"$manifest")"

    if [[ "$DRY_RUN" == true ]]; then
        plan "stop and disable asusd.service and asus-shutdown.service"
        plan "remove the $count files recorded in $manifest"
        if [[ "$PURGE" == true ]]; then
            plan "delete $CONFIG_DIR, including the daemon settings"
        else
            plan "keep $CONFIG_DIR (the daemon settings); add --purge to delete it"
        fi
        return 0
    fi

    as_root true || return 1
    if systemd_running; then
        as_root systemctl disable --now asus-shutdown.service ||
            warn "asus-shutdown.service could not be disabled; it may not have been enabled."
        as_root systemctl stop asusd.service || warn "asusd.service could not be stopped; it may not be running."
    fi
    as_root bash -c "set -euo pipefail; $(declare -f remove_installed_files); remove_installed_files \"\$@\"" _ \
        "$ROOT_DIR" "$manifest" "$STATE_DIR" || {
        error "Removing the installed files failed."
        return 1
    }
    if systemd_running; then
        as_root systemctl daemon-reload
        as_root udevadm control --reload
    fi
    if [[ "$PURGE" == true ]]; then
        as_root rm -rf -- "$(target_path "$CONFIG_DIR")"
        success "Removed asusctl and the daemon settings in $CONFIG_DIR."
    else
        success "Removed asusctl. The daemon settings in $CONFIG_DIR were kept (--purge deletes them)."
    fi
    info "The build dependencies stay installed; they are ordinary development packages."
}

status_report() {
    local daemon marker installed_state service_state policy_state property value reason
    local conflict=false unreadable=false

    info "Hardware"
    field 'Machine:' "$(read_value "$DMI_DIR/sys_vendor") $(read_value "$DMI_DIR/product_name")"
    field 'Product family:' "$(read_value "$DMI_DIR/product_family")"
    field 'Kernel:' "$(read_value "$KERNEL_RELEASE_FILE") (asusctl needs $MIN_KERNEL or newer)"
    if [[ -L "$PLATFORM_DEVICE_DIR/driver" ]]; then
        field 'asus-nb-wmi driver:' bound
    else
        field 'asus-nb-wmi driver:' 'not bound'
    fi
    if check_target; then
        field 'Supported target:' yes
    else
        field 'Supported target:' "no - $SKIP_REASON"
    fi

    info "Installation"
    field 'Pinned release:' "$VERSION"
    marker="$(read_value "$(target_path "$STATE_DIR/version")")"
    if [[ -z "$marker" ]]; then
        installed_state='not installed by this script'
        if reason="$(foreign_install)"; then
            installed_state="not installed by this script (found $reason)"
        fi
    elif install_is_current; then
        installed_state="$VERSION (up to date)"
    elif [[ "${marker%% *}" == "$VERSION" ]]; then
        installed_state="$VERSION, but recorded files are missing or the commit differs"
    else
        installed_state="${marker%% *} (pinned release is $VERSION)"
    fi
    field 'Installed:' "$installed_state"

    service_state="$(systemctl is-active asusd.service 2>/dev/null)" || true
    field 'asusd.service:' "${service_state:-unknown (systemd is not running)}"

    info "Power profiles"
    if daemon="$(other_profile_daemon)"; then
        field 'Power Mode owner:' "$daemon"
        for property in "${SWITCHING_PROPERTIES[@]}"; do
            if value="$(daemon_property "$property")"; then
                if [[ "$value" == 'b true' ]]; then
                    conflict=true
                fi
            else
                unreadable=true
            fi
        done
        if [[ "$unreadable" == true ]]; then
            policy_state='unknown (asusd is not answering on D-Bus)'
        elif [[ "$conflict" == true ]]; then
            policy_state="asusd still switches profiles and will fight $daemon - rerun $0"
        else
            policy_state="asusd leaves profile switching to $daemon"
        fi
    else
        field 'Power Mode owner:' 'asusd (no other daemon is running)'
        policy_state='asusd switches profiles when the power source changes'
    fi
    field 'Profile switching:' "$policy_state"

    info "Verdict"
    if ! check_target; then
        info "Nothing to do on this machine."
    elif [[ -z "$marker" ]]; then
        warn "asusctl is not installed. Install it with: $0"
    elif ! install_is_current; then
        warn "The installation is out of date or damaged. Repair it with: $0"
    elif [[ "$service_state" != active ]]; then
        warn "asusd is not running. Start it with: sudo systemctl start asusd.service"
    elif [[ "$conflict" == true ]]; then
        warn "Apply the profile policy by rerunning: $0"
    else
        success "asusctl $VERSION is installed and asusd is running."
    fi
}

main() {
    parse_args "$@"
    case "$MODE" in
        status) status_report ;;
        uninstall) uninstall_main ;;
        *) install_main ;;
    esac
}

if [[ "${BASH_SOURCE[0]}" == "$0" ]]; then
    main "$@"
fi
