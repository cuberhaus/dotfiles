#!/usr/bin/env bash
# Install the free edition of DaVinci Resolve (Blackmagic Design) on Ubuntu from the ZIP that
# you download from Blackmagic yourself: the download sits behind a registration form, so this
# script never fetches it. It finds the ZIP (or the .run installer inside it), installs the
# libraries Resolve needs, runs Blackmagic's installer in terminal mode, moves the glib
# libraries Resolve bundles out of its way so the system ones are used, and reports every
# library that is still missing. Machines without an NVIDIA GPU are skipped unless --force.
# Before Blackmagic's installer runs, it also limits the vendor's control-panel udev rule to
# Blackmagic's own USB devices, because the vendor's file makes every /dev/hidraw* device
# writable by every user (--keep-vendor-udev-rules skips that).
#
# Rationale, trade-offs, and recovery: .local/README.md ("DaVinci Resolve")
set -euo pipefail

# Every system location can be redirected, so the tests never touch the real machine.
# DAVINCI_RESOLVE_ROOT works like DESTDIR for the files outside the prefix (launchers, udev rules).
readonly PREFIX="${DAVINCI_RESOLVE_PREFIX:-/opt/resolve}"
readonly ROOT_DIR="${DAVINCI_RESOLVE_ROOT:-}"
readonly DEV_DIR="${DAVINCI_RESOLVE_DEV_DIR:-/dev}"
readonly PCI_DEVICES_DIR="${DAVINCI_RESOLVE_PCI_DIR:-/sys/bus/pci/devices}"
readonly SUDO_PROGRAM="${DAVINCI_RESOLVE_SUDO:-sudo}"
readonly SEARCH_DIRS="${DAVINCI_RESOLVE_SEARCH_DIRS:-}"
readonly WORK_PARENT="${DAVINCI_RESOLVE_WORK_DIR:-${XDG_CACHE_HOME:-$HOME/.cache}}"
readonly MIN_FREE_MB="${DAVINCI_RESOLVE_MIN_FREE_MB:-12288}"
readonly REQUIRE_TERMINAL="${DAVINCI_RESOLVE_REQUIRE_TERMINAL:-true}"
readonly SUPPORT_URL='https://www.blackmagicdesign.com/support/family/davinci-resolve-and-fusion'
readonly TAB=$'\t'

# The libraries Blackmagic's installer requires on Ubuntu (check_ubuntu_package_deps in the
# AppRun of DaVinci Resolve 21.1.1), under the package names of Ubuntu 24.04 and later: the t64
# transition renamed libapr1, libaprutil1, libasound2, and libglib2.0-0, and the old names no
# longer install. unzip opens the download and libfuse2t64 runs the .run file, an AppImage.
# The audit reads this array: one name per line, no comments inside it.
readonly PREREQUISITE_PACKAGES=(
    unzip
    libfuse2t64
    dbus
    fontconfig
    libapr1t64
    libaprutil1t64
    libasound2t64
    libbz2-1.0
    libexpat1
    libfreetype6
    libglib2.0-0t64
    libglu1-mesa
    libglvnd0
    libgomp1
    libice6
    libnuma1
    libsm6
    libstdc++6
    libsystemd0
    libuuid1
    libx11-6
    libxau6
    libxcb-composite0
    libxcb-cursor0
    libxcb-damage0
    libxcb-glx0
    libxcb-icccm4
    libxcb-image0
    libxcb-keysyms1
    libxcb-randr0
    libxcb-render-util0
    libxcb-render0
    libxcb-shape0
    libxcb-shm0
    libxcb-sync1
    libxcb-util1
    libxcb-xfixes0
    libxcb-xinerama0
    libxcb-xinput0
    libxcb-xkb1
    libxcb1
    libxcursor1
    libxext6
    libxfixes3
    libxi6
    libxinerama1
    libxkbcommon-x11-0
    libxkbcommon0
    libxrandr2
    libxrender1
    libxtst6
    libxxf86vm1
    ocl-icd-libopencl1
)

# What identifies a udev rules file as Blackmagic's (its USB vendor ID, and the one of the
# Feitian dongle that Resolve Studio uses) or as this script's, so that --uninstall never
# removes a file of someone else that happens to have the same name.
readonly BLACKMAGIC_VENDOR_MATCH='ATTRS{idVendor}=="1edb"'
readonly DONGLE_VENDOR_MATCH='ATTRS{idVendor}=="096e"'
readonly OVERRIDE_MARKER='Written by davinci_resolve_install.sh'

# Resolve bundles older copies of these glib libraries than the ones the system and the Qt
# plugins use, which ends in "symbol lookup error". Moved aside, Resolve loads the system ones.
readonly BUNDLED_GLIB_LIBRARIES=(libglib-2.0 libgobject-2.0 libgio-2.0 libgmodule-2.0)

MODE=install
DRY_RUN=false
FORCE=false
UNATTENDED=false
KEEP_VENDOR_UDEV_RULES=false
INSTALLER_ARGUMENT=''
INSTALLER=''
INSTALLER_VERSION=''
RUN_FILE=''
WORK_DIR=''

info() { printf '\033[34m[INFO]\033[0m %s\n' "$*"; }
success() { printf '\033[32m[OK]\033[0m %s\n' "$*"; }
warn() { printf '\033[33m[WARN]\033[0m %s\n' "$*" >&2; }
error() { printf '\033[31m[ERROR]\033[0m %s\n' "$*" >&2; }
plan() { printf '\033[34m[DRY RUN]\033[0m %s\n' "$*"; }

usage() {
    cat <<'EOF'
Usage: davinci_resolve_install.sh [--installer FILE] [--dry-run] [--force] [--unattended]
                                  [--keep-vendor-udev-rules]
       davinci_resolve_install.sh --uninstall [--dry-run] [--unattended]

Install the free DaVinci Resolve (Blackmagic Design) on Ubuntu. Blackmagic offers the
download only behind a registration form, so download the Linux ZIP yourself first
(DaVinci_Resolve_<version>_Linux.zip, in ~/Downloads or your home folder); this script
never fetches it. It then installs the libraries Resolve needs, limits the vendor's udev
rule for control panels to Blackmagic's own USB devices, runs Blackmagic's own installer in
terminal mode (it asks questions, so run this from a terminal), moves the glib libraries
Resolve bundles out of its way, and reports every library that is still missing.
Machines without an NVIDIA GPU, and ones that already have Resolve, are skipped.

Options:
  --installer FILE  Use this ZIP or .run file instead of searching the download folders.
  --dry-run         Show every step and change nothing (no sudo, nothing unpacked).
  --force           Skip the NVIDIA check and install again over an existing installation.
  --unattended      Never prompt for the sudo password, and stop before Blackmagic's installer,
                    which cannot run without a person.
  --keep-vendor-udev-rules
                    Leave Blackmagic's udev rules as its installer writes them. By default the
                    script first writes /etc/udev/rules.d/75-davincipanel.rules with only the
                    Blackmagic USB rule: the vendor's file of that name also makes every
                    /dev/hidraw* device (touchpad, keyboard interfaces, security keys)
                    writable by every user.
  --uninstall       Remove /opt/resolve and the launchers, menu entries, and udev rules that
                    belong to it. Projects and settings in your home folder stay.
  -h, --help        Show this help.

Run it as your own user: it uses sudo for the steps that need it.
EOF
}

usage_error() {
    error "$1"
    usage >&2
    exit 2
}

parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -h | --help)
                usage
                exit 0
                ;;
            --dry-run) DRY_RUN=true ;;
            --force) FORCE=true ;;
            --unattended) UNATTENDED=true ;;
            --keep-vendor-udev-rules) KEEP_VENDOR_UDEV_RULES=true ;;
            --uninstall) MODE=uninstall ;;
            --installer)
                [[ $# -ge 2 ]] || usage_error '--installer needs a file'
                INSTALLER_ARGUMENT="$2"
                shift
                ;;
            --installer=*) INSTALLER_ARGUMENT="${1#--installer=}" ;;
            *) usage_error "unknown option: $1" ;;
        esac
        shift
    done
    if [[ "$MODE" == uninstall ]]; then
        [[ "$FORCE" != true ]] || usage_error '--uninstall does not take --force'
        [[ -z "$INSTALLER_ARGUMENT" ]] || usage_error '--uninstall does not take --installer'
        [[ "$KEEP_VENDOR_UDEV_RULES" != true ]] || usage_error '--uninstall does not take --keep-vendor-udev-rules'
    fi
}

cleanup() {
    if [[ -n "$WORK_DIR" && -d "$WORK_DIR" ]]; then
        rm -rf -- "$WORK_DIR" || warn "Could not remove the temporary folder $WORK_DIR"
    fi
}

## Run a command as root through sudo; --unattended never prompts for a password.
as_root() {
    if [[ "$UNATTENDED" == true ]]; then
        command "$SUDO_PROGRAM" -n "$@"
    else
        command "$SUDO_PROGRAM" "$@"
    fi
}

resolve_installed() {
    [[ -x "$PREFIX/bin/resolve" ]]
}

## True when the machine has an NVIDIA display controller (vendor 0x10de, class 0x03xxxx), which
## sysfs shows before the driver loads and which leaves out the card's HDMI audio function.
nvidia_gpu_present() {
    local device vendor class
    for device in "$PCI_DEVICES_DIR"/*; do
        if [[ ! -r "$device/vendor" || ! -r "$device/class" ]]; then
            continue
        fi
        vendor="$(<"$device/vendor")"
        class="$(<"$device/class")"
        if [[ "$vendor" == 0x10de && "$class" == 0x03* ]]; then
            return 0
        fi
    done
    return 1
}

## The folders searched for the installer, one per line.
search_dirs() {
    local dir download
    if [[ -n "$SEARCH_DIRS" ]]; then
        tr ':' '\n' <<<"$SEARCH_DIRS"
        return 0
    fi
    download="$(xdg-user-dir DOWNLOAD 2>/dev/null || true)"
    for dir in "$download" "$HOME/Downloads" "$HOME"; do
        if [[ -n "$dir" ]]; then
            printf '%s\n' "$dir"
        fi
    done | awk '!seen[$0]++'
}

## The version in an installer's file name: DaVinci_Resolve_21.1.1_Linux.zip gives 21.1.1.
## A file that is not named like Blackmagic's (one passed with --installer) has no version.
installer_version() {
    local name="${1##*/}"
    case "$name" in
        DaVinci_Resolve_*_Linux*) ;;
        *) return 0 ;;
    esac
    name="${name#DaVinci_Resolve_}"
    name="${name#Studio_}"
    printf '%s' "${name%%_Linux*}"
}

## One line per free-edition installer in the search folders: version, 0 for a ZIP or 1 for the
## .run (so an already unpacked installer beats its ZIP), and the path.
list_installers() {
    local dir file priority
    while IFS= read -r dir; do
        if [[ ! -d "$dir" ]]; then
            continue
        fi
        for file in "$dir"/DaVinci_Resolve_[0-9]*_Linux.zip "$dir"/DaVinci_Resolve_[0-9]*_Linux.run; do
            if [[ ! -f "$file" ]]; then
                continue
            fi
            priority=0
            if [[ "$file" == *.run ]]; then
                priority=1
            fi
            printf '%s\t%s\t%s\n' "$(installer_version "$file")" "$priority" "$file"
        done
    done < <(search_dirs)
}

explain_download() {
    error "No DaVinci Resolve installer was found in: $(search_dirs | awk 'NR > 1 { printf ", " } { printf "%s", $0 } END { print "" }')"
    printf '%s\n' \
        'Blackmagic Design offers the download only behind a registration form, so this script cannot fetch it:' \
        "  1. Open $SUPPORT_URL" \
        '  2. Download the free "DaVinci Resolve" for Linux (a ZIP named DaVinci_Resolve_<version>_Linux.zip)' \
        '  3. Save it in ~/Downloads (or pass its path with --installer FILE)' \
        '  4. Run: make repair REPAIR=davinci-resolve' >&2
}

## Choose the installer: the file named by --installer, else the newest one found. Sets
## INSTALLER and INSTALLER_VERSION.
select_installer() {
    local candidate
    if [[ -n "$INSTALLER_ARGUMENT" ]]; then
        if [[ ! -f "$INSTALLER_ARGUMENT" ]]; then
            error "Installer not found: $INSTALLER_ARGUMENT"
            return 1
        fi
        case "$INSTALLER_ARGUMENT" in
            *.zip | *.run) INSTALLER="$INSTALLER_ARGUMENT" ;;
            *)
                error "$INSTALLER_ARGUMENT is neither a .zip nor a .run file."
                return 1
                ;;
        esac
    else
        candidate="$(list_installers | sort -t "$TAB" -k1,1V -k2,2n | tail -n 1 | cut -f3-)"
        if [[ -z "$candidate" ]]; then
            explain_download
            return 1
        fi
        INSTALLER="$candidate"
    fi
    INSTALLER_VERSION="$(installer_version "$INSTALLER")"
}

## Free megabytes on the filesystem that holds $1, or on its nearest existing parent.
free_mb() {
    local dir="$1"
    while [[ ! -d "$dir" && "$dir" != / && "$dir" != . ]]; do
        dir="$(dirname -- "$dir")"
    done
    df -Pk -- "$dir" 2>/dev/null | awk 'NR == 2 { print int($4 / 1024) }' || true
}

check_free_space() {
    local -a places=("$(dirname -- "$PREFIX")")
    local place available
    if [[ "$INSTALLER" == *.zip ]]; then
        places+=("$WORK_PARENT")
    fi
    for place in "${places[@]}"; do
        available="$(free_mb "$place")"
        if [[ ! "$available" =~ ^[0-9]+$ ]]; then
            warn "Could not read the free space of $place; continuing."
        elif ((available < MIN_FREE_MB)); then
            error "$place has $available MB free, and DaVinci Resolve needs about $MIN_FREE_MB MB there."
            return 1
        fi
    done
}

## The prerequisite packages that are not installed, one per line.
missing_prerequisites() {
    local package status
    for package in "${PREREQUISITE_PACKAGES[@]}"; do
        # shellcheck disable=SC2016 # dpkg-query expands ${Status}, the shell must not
        status="$(dpkg-query -W -f='${Status}' "$package" 2>/dev/null || true)"
        if [[ "$status" != *'ok installed'* ]]; then
            printf '%s\n' "$package"
        fi
    done
}

plan_install() {
    local -a missing=()
    local run_description="$INSTALLER"
    if [[ "$INSTALLER" == *.zip ]]; then
        plan "would unpack $INSTALLER into a temporary folder under $WORK_PARENT and remove it afterwards"
        run_description='<the .run file from the ZIP>'
    fi
    mapfile -t missing < <(missing_prerequisites)
    if [[ ${#missing[@]} -gt 0 ]]; then
        plan "would install with apt-get: ${missing[*]}"
    else
        plan 'every prerequisite library is already installed'
    fi
    plan_panel_udev_rule
    plan "would run: sudo env SKIP_PACKAGE_CHECK=1 $run_description -i"
    plan "would move the glib libraries that Resolve bundles to $PREFIX/libs/not_used"
    plan 'nothing was changed'
}

## Blackmagic's installer asks questions: it needs a person and a terminal.
require_terminal() {
    if [[ "$UNATTENDED" == true ]]; then
        warn "Blackmagic's installer asks questions, so it cannot run unattended."
        warn 'Run it from a terminal when you are at the machine: make repair REPAIR=davinci-resolve'
        return 1
    fi
    if [[ "$REQUIRE_TERMINAL" == true && ! -t 0 ]]; then
        error "Blackmagic's installer asks questions and needs a terminal; run this from one."
        return 1
    fi
}

install_prerequisites() {
    local -a missing=()
    mapfile -t missing < <(missing_prerequisites)
    if [[ ${#missing[@]} -eq 0 ]]; then
        info 'Every library DaVinci Resolve needs is already installed.'
        return 0
    fi
    info "Installing the libraries DaVinci Resolve needs: ${missing[*]}"
    if ! as_root env DEBIAN_FRONTEND=noninteractive apt-get install -y "${missing[@]}"; then
        error "apt-get could not install them. If it cannot find a package, run 'sudo apt-get update' and try again."
        return 1
    fi
}

## The rules file that replaces Blackmagic's 75-davincipanel.rules: udev lets a file in
## /etc/udev/rules.d replace the file of the same name in /usr/lib/udev/rules.d.
panel_rules_override() {
    printf '%s\n' "$ROOT_DIR/etc/udev/rules.d/75-davincipanel.rules"
}

## What goes into that file: the one rule for Blackmagic's USB devices and nothing else.
panel_rules_override_content() {
    cat <<EOF
# $OVERRIDE_MARKER from the dotfiles repository. Delete this file to undo it.
# Blackmagic's installer writes /usr/lib/udev/rules.d/75-davincipanel.rules with the rule
# below and one more, KERNEL=="hidraw*", MODE="0777", which would make every raw HID device
# (touchpad, keyboard interfaces, security keys) writable by every user. A file of the same
# name in /etc/udev/rules.d replaces that one, so only the Blackmagic rule applies.
SUBSYSTEM=="usb", $BLACKMAGIC_VENDOR_MATCH, MODE="0666"
EOF
}

## Write that file before Blackmagic's installer runs, so its world-writable hidraw rule never
## applies and no device is exposed in between. A file of that name that already exists is the
## user's own and stays. When the file cannot be written the installation stops here, instead
## of installing the rule it was meant to prevent.
scope_panel_udev_rule() {
    local override
    override="$(panel_rules_override)"
    if [[ "$KEEP_VENDOR_UDEV_RULES" == true ]]; then
        warn "Keeping Blackmagic's udev rules as its installer writes them: 75-davincipanel.rules makes every /dev/hidraw* device writable by every user."
        return 0
    fi
    if [[ -e "$override" || -L "$override" ]]; then
        info "$override already exists and replaces Blackmagic's panel udev rule; leaving it as it is."
        return 0
    fi
    info "Limiting Blackmagic's panel udev rule to Blackmagic USB devices: $override (delete it to use Blackmagic's own)."
    if ! as_root mkdir -p -- "$(dirname -- "$override")" ||
        ! panel_rules_override_content | as_root tee -- "$override" >/dev/null ||
        ! as_root chmod 644 -- "$override"; then
        error "Could not write $override, so Blackmagic's installer was not started: its own rule would make every /dev/hidraw* device writable by every user. Fix the cause, or rerun with --keep-vendor-udev-rules to accept that."
        return 1
    fi
}

plan_panel_udev_rule() {
    local override
    override="$(panel_rules_override)"
    if [[ "$KEEP_VENDOR_UDEV_RULES" == true ]]; then
        plan "would keep Blackmagic's udev rules as its installer writes them (--keep-vendor-udev-rules)"
    elif [[ -e "$override" || -L "$override" ]]; then
        plan "$override exists and would stay as it is"
    else
        plan "would write $override, which limits Blackmagic's panel udev rule to Blackmagic USB devices (its own rule also makes every /dev/hidraw* device writable by every user)"
    fi
}

## After the installation: list every raw HID device that any user can write to, which is what
## Blackmagic's own udev rule causes. Read-only.
report_world_writable_hidraw() {
    local nodes node
    nodes="$(find "$DEV_DIR" -maxdepth 1 -name 'hidraw*' -perm -0002 2>/dev/null | sort || true)"
    if [[ -z "$nodes" ]]; then
        info "No $DEV_DIR/hidraw* device is writable by every user."
        return 0
    fi
    warn 'These raw HID devices are writable by every user:'
    while IFS= read -r node; do
        warn "  $node"
    done <<<"$nodes"
    warn 'Find the udev rule that does it with: grep -l hidraw /etc/udev/rules.d/* /usr/lib/udev/rules.d/*'
    warn 'Then fix it and replug the devices or reboot: udev sets permissions when a device appears.'
}

## Unpack the ZIP into a temporary folder and set RUN_FILE to the installer inside it.
extract_zip() {
    local status=0
    mkdir -p -- "$WORK_PARENT"
    WORK_DIR="$(mktemp -d "$WORK_PARENT/davinci-resolve.XXXXXX")"
    info "Unpacking $(basename -- "$INSTALLER") (the installer is several gigabytes)..."
    unzip -q -o -- "$INSTALLER" -d "$WORK_DIR" || status=$?
    if ((status > 1)); then
        error "unzip could not unpack $INSTALLER (exit $status). Is the download complete?"
        return 1
    fi
    RUN_FILE="$(find "$WORK_DIR" -maxdepth 3 -type f -name 'DaVinci_Resolve_*_Linux.run' -print -quit)"
    if [[ -z "$RUN_FILE" ]]; then
        error "$INSTALLER does not contain a DaVinci_Resolve_*_Linux.run installer."
        return 1
    fi
}

prepare_run_file() {
    if [[ "$INSTALLER" == *.zip ]]; then
        extract_zip || return 1
    else
        RUN_FILE="$INSTALLER"
    fi
    if [[ ! -x "$RUN_FILE" ]]; then
        chmod +x -- "$RUN_FILE" || {
            error "Cannot make $RUN_FILE executable."
            return 1
        }
    fi
}

run_installer() {
    info "Starting Blackmagic's installer in terminal mode. Answer its questions when it asks."
    if ! as_root env SKIP_PACKAGE_CHECK=1 "$RUN_FILE" -i; then
        error "Blackmagic's installer failed, so $PREFIX may be incomplete. Fix the cause and rerun with --force."
        return 1
    fi
    if ! resolve_installed; then
        error "The installer finished, but $PREFIX/bin/resolve does not exist."
        return 1
    fi
}

## Move the bundled glib libraries into libs/not_used, where Resolve does not look for them.
park_bundled_glib() {
    local libs="$PREFIX/libs" parked="$PREFIX/libs/not_used" library file moved=0
    if [[ ! -d "$libs" ]]; then
        warn "$libs does not exist; the glib libraries were not moved."
        return 0
    fi
    for library in "${BUNDLED_GLIB_LIBRARIES[@]}"; do
        for file in "$libs/$library".so*; do
            if [[ ! -e "$file" && ! -L "$file" ]]; then
                continue
            fi
            as_root mkdir -p -- "$parked"
            as_root mv -f -- "$file" "$parked/"
            moved=$((moved + 1))
        done
    done
    if ((moved > 0)); then
        info "Moved $moved bundled glib file(s) to $parked, so Resolve uses the system libraries."
    else
        info 'The bundled glib libraries were already moved aside.'
    fi
}

## Warn about every shared library that Resolve, or its Qt platform plugin, cannot find.
report_missing_libraries() {
    local target unresolved=''
    for target in "$PREFIX/bin/resolve" "$PREFIX/libs/plugins/platforms/libqxcb.so"; do
        if [[ -e "$target" ]]; then
            unresolved+="$(ldd "$target" 2>/dev/null | awk '/not found/ { print $1 }' || true)"$'\n'
        fi
    done
    unresolved="$(printf '%s' "$unresolved" | sort -u | sed '/^$/d')"
    if [[ -z "$unresolved" ]]; then
        info 'Every shared library Resolve links against was found.'
        return 0
    fi
    warn 'Resolve will not start until these libraries are installed:'
    while IFS= read -r target; do
        warn "  $target"
    done <<<"$unresolved"
    warn 'Find the package that ships one with: apt-file search <library>  (sudo apt install apt-file && sudo apt-file update)'
}

next_steps() {
    info "Start Resolve from the application menu, or run: $PREFIX/bin/resolve"
    info "If its window does not open on Wayland, start it through XWayland: QT_QPA_PLATFORM=xcb $PREFIX/bin/resolve"
    info 'The free edition on Linux does not decode H.264/H.265 video or AAC audio; convert such clips first, for example:'
    info '  ffmpeg -i clip.mp4 -c:v dnxhd -profile:v dnxhr_hq -pix_fmt yuv422p -c:a pcm_s16le clip.mov'
}

apply() {
    if ! command -v apt-get >/dev/null 2>&1 || ! command -v dpkg-query >/dev/null 2>&1; then
        error 'This installer is for Debian and Ubuntu (apt-get and dpkg-query are missing). On Arch, build the davinci-resolve AUR package.'
        return 1
    fi
    if [[ "$FORCE" != true ]]; then
        if ! nvidia_gpu_present; then
            info 'No NVIDIA GPU found, so DaVinci Resolve is skipped (--force installs it anyway).'
            return 0
        fi
        if resolve_installed; then
            info "DaVinci Resolve is already installed in $PREFIX (--force installs it again)."
            return 0
        fi
    fi

    select_installer || return 1
    info "Using $INSTALLER${INSTALLER_VERSION:+ (DaVinci Resolve $INSTALLER_VERSION)}."
    check_free_space || return 1

    if [[ "$DRY_RUN" == true ]]; then
        plan_install
        return 0
    fi

    require_terminal || return 1
    # A truncated download is the likeliest failure, so the ZIP is unpacked and checked before
    # anything on the system changes.
    prepare_run_file || return 1
    install_prerequisites || return 1
    scope_panel_udev_rule || return 1
    run_installer || return 1
    park_bundled_glib
    report_missing_libraries
    report_world_writable_hidraw
    success "DaVinci Resolve${INSTALLER_VERSION:+ $INSTALLER_VERSION} is installed in $PREFIX."
    next_steps
}

## Print file $1 when it exists and contains the fixed text $2, the mark that shows it is
## Blackmagic's or this script's. A file of someone else with the same name is not listed.
list_if_marked() {
    if [[ -f "$1" ]] && grep -Fq -- "$2" "$1"; then
        printf '%s\n' "$1"
    fi
}

## The files outside the prefix that belong to Resolve, one per line: the launchers whose Exec
## line starts a program inside the prefix (also the one on the Desktop), its menu entries, and
## the udev rules that Blackmagic's installer, or this script, wrote.
system_files() {
    local file directory
    local rules="$ROOT_DIR/usr/lib/udev/rules.d" admin_rules="$ROOT_DIR/etc/udev/rules.d"
    for file in "$ROOT_DIR"/usr/share/applications/*.desktop "$HOME/Desktop/com.blackmagicdesign.resolve.desktop"; do
        if [[ -f "$file" ]] && awk -v prefix="$PREFIX/" '/^Exec=/ && index($0, prefix) { found = 1 } END { exit !found }' "$file"; then
            printf '%s\n' "$file"
        fi
    done
    for file in "$ROOT_DIR/usr/share/desktop-directories/com.blackmagicdesign.resolve.directory" \
        "$ROOT_DIR/etc/xdg/menus/applications-merged/com.blackmagicdesign.resolve.menu"; do
        if [[ -f "$file" ]]; then
            printf '%s\n' "$file"
        fi
    done
    list_if_marked "$rules/75-davincipanel.rules" "$BLACKMAGIC_VENDOR_MATCH"
    list_if_marked "$rules/75-davincikb.rules" "$BLACKMAGIC_VENDOR_MATCH"
    list_if_marked "$rules/75-sdx.rules" "$DONGLE_VENDOR_MATCH"
    list_if_marked "$admin_rules/75-davincipanel.rules" "$OVERRIDE_MARKER"
    # The names the rules have in Blackmagic's payload, which an older release or a manual
    # setup may have installed as they are.
    for directory in "$admin_rules" "$rules"; do
        list_if_marked "$directory/99-BlackmagicDevices.rules" "$BLACKMAGIC_VENDOR_MATCH"
        list_if_marked "$directory/99-ResolveKeyboardHID.rules" "$BLACKMAGIC_VENDOR_MATCH"
    done
}

uninstall() {
    local -a files=()
    local file
    if [[ "$PREFIX" != /?* ]]; then
        error "Refusing to remove '$PREFIX': it is not an absolute path inside the file system."
        return 1
    fi
    # Only a folder that holds Resolve's program is removed, so a mistyped prefix deletes nothing.
    if ! resolve_installed; then
        info "DaVinci Resolve is not installed in $PREFIX; nothing to remove."
        return 0
    fi
    while IFS= read -r file; do
        files+=("$file")
    done < <(system_files)

    if [[ "$DRY_RUN" == true ]]; then
        plan "would remove $PREFIX"
        for file in ${files[@]+"${files[@]}"}; do
            plan "would remove $file"
        done
        plan 'nothing was changed'
        return 0
    fi

    as_root rm -rf -- "$PREFIX"
    if [[ ${#files[@]} -gt 0 ]]; then
        as_root rm -f -- "${files[@]}"
    fi
    success "Removed DaVinci Resolve from $PREFIX (${#files[@]} launcher, menu, and rules file(s) outside it)."
    info 'Kept: your projects and settings (~/.local/share/DaVinciResolve), and the libraries apt installed for it.'
    info "Not removed: what Blackmagic's installer put in shared places (panel libraries in /usr/lib64 or /usr/lib, /var/BlackmagicDesign, icons, MIME types, the OFX renderer). See .local/README.md."
}

main() {
    parse_args "$@"
    if [[ "$(id -u)" -eq 0 ]]; then
        error 'Run this as your own user, not as root: it uses sudo for the steps that need it.'
        return 1
    fi
    trap cleanup EXIT
    if [[ "$MODE" == uninstall ]]; then
        uninstall
    else
        apply
    fi
}

main "$@"
