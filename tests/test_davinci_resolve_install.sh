#!/usr/bin/env bash
# Hermetic tests for .local/scripts/davinci_resolve_install.sh, its bootstrap and repair wiring,
# and the OpenShot and Blender entries that ship beside it. Fake id, sudo, apt-get, dpkg-query,
# ldd, and xdg-user-dir binaries, an env-redirected PCI tree, prefix, and system root, and a real
# ZIP (built with Python's zipfile, unpacked by the real unzip) that holds a fake Blackmagic
# installer keep every case off the real machine, the network, and the real package manager.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT="$REPO_ROOT/.local/scripts/davinci_resolve_install.sh"
BOOTSTRAP_DIR="$REPO_ROOT/.local/scripts/bootstrap"
BASE_FUNCTIONS="$BOOTSTRAP_DIR/base_functions"
REPAIR_SCRIPT="$REPO_ROOT/.local/scripts/repair-installation"

for tool in python3 unzip; do
    if ! command -v "$tool" >/dev/null 2>&1; then
        printf 'SKIP: %s is not installed\n' "$tool"
        exit 0
    fi
done

TEST_ROOT="$(mktemp -d)"
trap 'rm -rf "$TEST_ROOT"' EXIT

# What a fresh Ubuntu desktop lacks of the script's prerequisite packages.
readonly MISSING_AT_START=(libapr1t64 libaprutil1t64 libglu1-mesa)
readonly GLIB_FILES=(libglib-2.0.so.0 libglib-2.0.so.0.7800.0 libgobject-2.0.so.0 libgio-2.0.so.0 libgmodule-2.0.so.0)

CASE=''
OUTPUT=''
STATUS=0
# KEY=value pairs for the next run_script, on top of the defaults; new_case clears them.
ENV_OVERRIDES=()

fail() {
    printf 'FAIL: %s\n' "$1" >&2
    shift
    if [[ $# -gt 0 ]]; then
        printf '%s\n' "$@" >&2
    fi
    exit 1
}

## The package names of the script's PREREQUISITE_PACKAGES array, one per line.
prerequisite_packages() {
    awk '/^readonly PREREQUISITE_PACKAGES=\(/ { in_array = 1; next }
         in_array && /^\)/ { exit }
         in_array { gsub(/^[ \t]+|[ \t]+$/, ""); if ($0 != "") print }' "$SCRIPT"
}

write_fakes() {
    local bin="$CASE/bin"

    cat >"$bin/id" <<'EOF'
#!/usr/bin/env bash
case "${1:-}" in
    -u) printf '%s\n' "${FAKE_UID:-1000}" ;;
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
printf '%s %s\n' "$mode" "$*" >>"$FAKE_LOG_DIR/sudo.log"
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
package="${*: -1}"
if grep -Fxq -- "$package" "$FAKE_LOG_DIR/installed.list" 2>/dev/null; then
    printf 'install ok installed'
else
    exit 1
fi
EOF
    cat >"$bin/ldd" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$1" >>"$FAKE_LOG_DIR/ldd.log"
printf '\tlibc.so.6 => /lib/libc.so.6 (0x0)\n'
if [[ "${FAKE_LDD_MISSING:-false}" == true ]]; then
    printf '\tlibmissing.so.1 => not found\n'
    printf '\tlibother.so.2 => not found\n'
fi
EOF
    cat >"$bin/xdg-user-dir" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "${FAKE_DOWNLOAD_DIR:-$HOME/Downloads}"
EOF
    chmod +x "$bin"/*
}

## Write the stand-in for Blackmagic's .run installer to $1. It records how it was started and
## installs a Resolve tree under the prefix, plus a launcher and the two udev rules.
write_fake_run() {
    cat >"$1" <<'EOF'
#!/usr/bin/env bash
printf 'args=%s skip=%s\n' "$*" "${SKIP_PACKAGE_CHECK:-}" >>"$FAKE_LOG_DIR/installer.log"
[[ "${FAKE_INSTALLER_FAIL:-false}" != true ]] || exit 1
[[ "${FAKE_INSTALLER_NOOP:-false}" != true ]] || exit 0
prefix="$DAVINCI_RESOLVE_PREFIX"
root="$DAVINCI_RESOLVE_ROOT"
mkdir -p "$prefix/bin" "$prefix/libs/plugins/platforms" "$root/usr/share/applications" "$root/usr/lib/udev/rules.d"
printf '#!/bin/sh\n' >"$prefix/bin/resolve"
chmod 755 "$prefix/bin/resolve"
for library in libglib-2.0.so.0 libglib-2.0.so.0.7800.0 libgobject-2.0.so.0 libgio-2.0.so.0 \
    libgmodule-2.0.so.0 libQt5Core.so.5 libglibmm-2.4.so.1; do
    : >"$prefix/libs/$library"
done
: >"$prefix/libs/plugins/platforms/libqxcb.so"
printf '[Desktop Entry]\nName=DaVinci Resolve\nExec=%s/bin/resolve %%u\n' "$prefix" \
    >"$root/usr/share/applications/com.blackmagicdesign.resolve.desktop"
: >"$root/usr/lib/udev/rules.d/99-BlackmagicDevices.rules"
: >"$root/usr/lib/udev/rules.d/99-ResolveKeyboardHID.rules"
EOF
}

## make_zip ZIP SOURCE NAME [MODE]: a ZIP with SOURCE stored as NAME (permissions MODE, octal,
## default 755) next to a text file, like the real download.
make_zip() {
    python3 - "$1" "$2" "$3" "${4:-755}" <<'PY'
import sys
import zipfile

zip_path, source, name, mode = sys.argv[1:5]
with zipfile.ZipFile(zip_path, "w") as archive:
    info = zipfile.ZipInfo(name)
    info.external_attr = (0o100000 | int(mode, 8)) << 16
    with open(source, "rb") as handle:
        archive.writestr(info, handle.read())
    archive.writestr("Linux_Installation_Instructions.txt", "read the instructions\n")
PY
}

## Put a ZIP that holds the fake installer into the download folder.
add_zip() {
    local version="${1:-21.1.1}"
    make_zip "$CASE/downloads/DaVinci_Resolve_${version}_Linux.zip" "$CASE/fake.run" "DaVinci_Resolve_${version}_Linux.run"
}

## A fresh fake machine in $CASE: an NVIDIA card with its audio function, a fresh Ubuntu that
## lacks MISSING_AT_START, an empty download folder, and nothing installed.
new_case() {
    CASE="$TEST_ROOT/$1"
    ENV_OVERRIDES=()
    mkdir -p "$CASE/bin" "$CASE/home/Downloads" "$CASE/cache" "$CASE/root" "$CASE/downloads" \
        "$CASE/pci/0000:01:00.0" "$CASE/pci/0000:01:00.1"
    printf '0x10de\n' >"$CASE/pci/0000:01:00.0/vendor"
    printf '0x030000\n' >"$CASE/pci/0000:01:00.0/class"
    printf '0x10de\n' >"$CASE/pci/0000:01:00.1/vendor"
    printf '0x040300\n' >"$CASE/pci/0000:01:00.1/class"
    prerequisite_packages | grep -Fvx -f <(printf '%s\n' "${MISSING_AT_START[@]}") >"$CASE/installed.list" || true
    write_fakes
    write_fake_run "$CASE/fake.run"
}

## Run the script in $CASE with the given arguments; set OUTPUT (stdout and stderr) and STATUS.
## Standard input is /dev/null, so a developer's terminal never leaks into a case.
run_script() {
    STATUS=0
    OUTPUT="$(
        env PATH="$CASE/bin:$PATH" \
            HOME="$CASE/home" \
            XDG_CACHE_HOME="$CASE/cache" \
            FAKE_LOG_DIR="$CASE" \
            DAVINCI_RESOLVE_PREFIX="$CASE/opt/resolve" \
            DAVINCI_RESOLVE_ROOT="$CASE/root" \
            DAVINCI_RESOLVE_PCI_DIR="$CASE/pci" \
            DAVINCI_RESOLVE_SUDO="$CASE/bin/sudo" \
            DAVINCI_RESOLVE_SEARCH_DIRS="$CASE/downloads" \
            DAVINCI_RESOLVE_MIN_FREE_MB=1 \
            DAVINCI_RESOLVE_REQUIRE_TERMINAL=false \
            ${ENV_OVERRIDES[@]+"${ENV_OVERRIDES[@]}"} \
            bash "$SCRIPT" "$@" 2>&1 </dev/null
    )" || STATUS=$?
}

assert_status() {
    [[ "$STATUS" -eq "$1" ]] || fail "$2 (exit $STATUS, expected $1)" "$OUTPUT"
}

assert_output() {
    [[ "$OUTPUT" == *"$1"* ]] || fail "$2: output lacks '$1'" "$OUTPUT"
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

assert_log_matches() {
    grep -Eq -- "$2" "$CASE/$1.log" 2>/dev/null ||
        fail "$3: $1.log has no line matching /$2/" "$(cat "$CASE/$1.log" 2>/dev/null || echo '(no log)')"
}

## Nothing was escalated, installed, unpacked, or written: no command ran and no file exists.
assert_untouched() {
    local name
    for name in sudo apt-get installer; do
        assert_log_lines "$name" 0 "$1 must not run $name"
    done
    [[ ! -e "$CASE/opt" ]] || fail "$1: nothing may be installed" "$(find "$CASE/opt")"
    [[ -z "$(find "$CASE/root" -type f 2>/dev/null)" ]] || fail "$1: no system file may be written" "$(find "$CASE/root" -type f)"
    [[ -z "$(ls -A "$CASE/cache")" ]] || fail "$1: no temporary folder may be left behind" "$(ls -A "$CASE/cache")"
}

assert_no_leftovers() {
    [[ -z "$(ls -A "$CASE/cache")" ]] || fail "$1: the temporary folder must be removed" "$(ls -A "$CASE/cache")"
}

## Run a full installation from a ZIP and expect success.
install_ok() {
    add_zip
    run_script
    assert_status 0 install
}

test_help_and_usage_errors() {
    new_case usage
    run_script --help
    assert_status 0 '--help'
    assert_output 'Usage: davinci_resolve_install.sh' '--help prints the usage'
    assert_output 'registration form' '--help explains the manual download'
    bash "$SCRIPT" --help >/dev/null 2>"$CASE/stderr"
    [[ ! -s "$CASE/stderr" ]] || fail '--help must write nothing to stderr' "$(cat "$CASE/stderr")"

    run_script --dry-run -h
    assert_status 0 '-h after another option'
    assert_output 'Usage: davinci_resolve_install.sh' '-h after another option'

    run_script --bogus
    assert_status 2 'unknown option'
    assert_output 'unknown option: --bogus' 'unknown option'
    assert_output 'Usage: davinci_resolve_install.sh' 'a usage error shows the usage'
    run_script --installer
    assert_status 2 '--installer without a file'
    run_script --uninstall --force
    assert_status 2 '--uninstall with --force'
    run_script --uninstall --installer "$CASE/x.zip"
    assert_status 2 '--uninstall with --installer'
    assert_untouched 'usage errors'
}

test_other_machines_are_skipped() {
    new_case amd
    add_zip
    printf '0x1002\n' >"$CASE/pci/0000:01:00.0/vendor"
    printf '0x1002\n' >"$CASE/pci/0000:01:00.1/vendor"
    run_script
    assert_status 0 'AMD machine'
    assert_output 'No NVIDIA GPU found' 'AMD machine'
    assert_untouched 'AMD machine'

    new_case audio-function-only
    add_zip
    rm -r "$CASE/pci/0000:01:00.0"
    run_script
    assert_status 0 'only the HDMI audio function of an NVIDIA card'
    assert_output 'No NVIDIA GPU found' 'audio function only'
    assert_untouched 'audio function only'

    new_case no-pci-tree
    add_zip
    rm -r "$CASE/pci"
    run_script
    assert_status 0 'machine without a PCI tree'
    assert_output 'No NVIDIA GPU found' 'no PCI tree'
    assert_untouched 'no PCI tree'

    new_case force-bypasses-the-gpu-gate
    add_zip
    printf '0x1002\n' >"$CASE/pci/0000:01:00.0/vendor"
    run_script --force
    assert_status 0 '--force on a machine without an NVIDIA GPU'
    assert_log_lines installer 1 '--force installs anyway'
}

test_not_debian_is_refused() {
    new_case no-apt
    add_zip
    # A PATH with the basics but neither apt-get nor dpkg-query.
    mkdir -p "$CASE/basic"
    local tool
    for tool in bash env id dirname basename mktemp mkdir rm cat awk sed sort tail cut tr find chmod df unzip xdg-user-dir; do
        ln -s "$(command -v "$tool")" "$CASE/basic/$tool" 2>/dev/null || true
    done
    STATUS=0
    OUTPUT="$(env PATH="$CASE/basic" HOME="$CASE/home" DAVINCI_RESOLVE_PCI_DIR="$CASE/pci" \
        DAVINCI_RESOLVE_SEARCH_DIRS="$CASE/downloads" DAVINCI_RESOLVE_PREFIX="$CASE/opt/resolve" \
        "$(command -v bash)" "$SCRIPT" 2>&1 </dev/null)" || STATUS=$?
    assert_status 1 'machine without apt-get'
    assert_output 'for Debian and Ubuntu' 'machine without apt-get'
    assert_untouched 'machine without apt-get'
}

test_existing_installation_is_left_alone() {
    new_case installed
    add_zip
    mkdir -p "$CASE/opt/resolve/bin"
    printf '#!/bin/sh\n' >"$CASE/opt/resolve/bin/resolve"
    chmod 755 "$CASE/opt/resolve/bin/resolve"
    run_script
    assert_status 0 'Resolve already installed'
    assert_output 'already installed' 'Resolve already installed'
    assert_log_lines sudo 0 'an existing installation needs no sudo'
    assert_log_lines installer 0 'an existing installation is not reinstalled'

    run_script --force
    assert_status 0 '--force over an existing installation'
    assert_log_lines installer 1 '--force installs again'
}

test_missing_installer_explains_the_download() {
    new_case no-zip
    run_script
    assert_status 1 'no installer'
    assert_output 'No DaVinci Resolve installer was found in' 'no installer'
    assert_output "$CASE/downloads" 'the message names the searched folder'
    assert_output 'registration form' 'no installer'
    assert_output 'https://www.blackmagicdesign.com/support/family/davinci-resolve-and-fusion' 'no installer'
    assert_output 'make repair REPAIR=davinci-resolve' 'no installer'
    assert_untouched 'no installer'

    # Lookalikes are not the free Linux installer: Studio, another platform, a partial download.
    : >"$CASE/downloads/DaVinci_Resolve_Studio_21.1.1_Linux.zip"
    : >"$CASE/downloads/DaVinci_Resolve_21.1.1_Windows.zip"
    : >"$CASE/downloads/DaVinci_Resolve_21.1.1_Linux.zip.part"
    : >"$CASE/downloads/DaVinci_Resolve_Linux.zip"
    run_script
    assert_status 1 'lookalike downloads'
    assert_output 'No DaVinci Resolve installer was found in' 'lookalike downloads'
    assert_untouched 'lookalike downloads'
}

test_default_search_folders() {
    new_case default-search
    # An empty value falls back to the real defaults: the XDG download folder, ~/Downloads, ~.
    ENV_OVERRIDES=(DAVINCI_RESOLVE_SEARCH_DIRS=)

    printf 'zip\n' >"$CASE/home/Downloads/DaVinci_Resolve_21.1.1_Linux.zip"
    run_script --dry-run
    assert_status 0 'the Downloads folder'
    assert_output "Using $CASE/home/Downloads/DaVinci_Resolve_21.1.1_Linux.zip (DaVinci Resolve 21.1.1)" 'the Downloads folder'
    rm "$CASE/home/Downloads/DaVinci_Resolve_21.1.1_Linux.zip"

    printf 'zip\n' >"$CASE/home/DaVinci_Resolve_20.3_Linux.zip"
    run_script --dry-run
    assert_status 0 'the home folder'
    assert_output "Using $CASE/home/DaVinci_Resolve_20.3_Linux.zip" 'the home folder'
    rm "$CASE/home/DaVinci_Resolve_20.3_Linux.zip"

    mkdir -p "$CASE/media"
    printf 'zip\n' >"$CASE/media/DaVinci_Resolve_21.1.1_Linux.zip"
    ENV_OVERRIDES=(DAVINCI_RESOLVE_SEARCH_DIRS= "FAKE_DOWNLOAD_DIR=$CASE/media")
    run_script --dry-run
    assert_status 0 'the XDG download folder'
    assert_output "Using $CASE/media/DaVinci_Resolve_21.1.1_Linux.zip" 'the XDG download folder'

    mkdir -p "$CASE/second"
    printf 'zip\n' >"$CASE/second/DaVinci_Resolve_21.2_Linux.zip"
    ENV_OVERRIDES=("DAVINCI_RESOLVE_SEARCH_DIRS=$CASE/first:$CASE/second")
    run_script --dry-run
    assert_status 0 'a colon-separated folder list'
    assert_output "Using $CASE/second/DaVinci_Resolve_21.2_Linux.zip" 'a colon-separated folder list'
    assert_untouched 'folder search'
}

test_newest_installer_is_chosen() {
    new_case newest
    local version
    for version in 9.0 20.3 21.1.1; do
        printf 'zip\n' >"$CASE/downloads/DaVinci_Resolve_${version}_Linux.zip"
    done
    printf 'run\n' >"$CASE/downloads/DaVinci_Resolve_21.1.1_Linux.run"
    : >"$CASE/downloads/DaVinci_Resolve_Studio_99.0_Linux.zip"

    run_script --dry-run
    assert_status 0 'several installers'
    assert_output "Using $CASE/downloads/DaVinci_Resolve_21.1.1_Linux.run (DaVinci Resolve 21.1.1)" 'an unpacked installer beats its ZIP, and 9.0 < 20.3 < 21.1.1 by number, not by name'
    refute_output 'would unpack' 'a .run file needs no unpacking'
    assert_output "would run: sudo env SKIP_PACKAGE_CHECK=1 $CASE/downloads/DaVinci_Resolve_21.1.1_Linux.run -i" 'a .run file runs as it is'

    rm "$CASE/downloads/DaVinci_Resolve_21.1.1_Linux.run"
    run_script --dry-run
    assert_output "Using $CASE/downloads/DaVinci_Resolve_21.1.1_Linux.zip" 'the newest ZIP'
    assert_output 'would unpack' 'a ZIP is unpacked'

    printf 'zip\n' >"$CASE/downloads/DaVinci_Resolve_21.10_Linux.zip"
    run_script --dry-run
    assert_output "Using $CASE/downloads/DaVinci_Resolve_21.10_Linux.zip" '21.10 is newer than 21.1.1'
    assert_untouched 'installer choice'
}

test_dry_run_changes_nothing() {
    new_case dry-run
    add_zip
    run_script --dry-run
    assert_status 0 'dry run'
    assert_output 'would unpack' 'dry run'
    assert_output "would install with apt-get: ${MISSING_AT_START[*]}" 'dry run lists only the missing libraries'
    assert_output 'would run: sudo env SKIP_PACKAGE_CHECK=1 <the .run file from the ZIP> -i' 'dry run'
    assert_output 'would move the glib libraries' 'dry run'
    assert_output 'nothing was changed' 'dry run'
    assert_untouched 'dry run'

    printf '%s\n' "${MISSING_AT_START[@]}" >>"$CASE/installed.list"
    run_script --dry-run
    assert_status 0 'dry run with every prerequisite present'
    assert_output 'every prerequisite library is already installed' 'dry run with every prerequisite present'
    refute_output 'would install with apt-get' 'dry run with every prerequisite present'
}

test_full_installation() {
    new_case full
    install_ok
    assert_output 'DaVinci Resolve 21.1.1 is installed in' 'install'

    # Prerequisites: one apt-get call that names only what was missing, through sudo.
    assert_log_lines apt-get 1 'install'
    assert_log_has apt-get "install -y ${MISSING_AT_START[*]}" 'install'
    assert_log_has sudo "interactive env DEBIAN_FRONTEND=noninteractive apt-get install -y ${MISSING_AT_START[*]}" 'install'

    # Blackmagic's installer: root through sudo, terminal mode, package check skipped, once.
    assert_log_lines installer 1 'install'
    assert_log_has installer 'args=-i skip=1' 'install'
    assert_log_matches sudo '^interactive env SKIP_PACKAGE_CHECK=1 .*/DaVinci_Resolve_21\.1\.1_Linux\.run -i$' 'install'

    # The bundled glib libraries are moved aside, everything else stays.
    local file
    for file in "${GLIB_FILES[@]}"; do
        [[ ! -e "$CASE/opt/resolve/libs/$file" ]] || fail "$file must leave the library folder"
        [[ -e "$CASE/opt/resolve/libs/not_used/$file" ]] || fail "$file must be kept in libs/not_used"
    done
    [[ -e "$CASE/opt/resolve/libs/libQt5Core.so.5" ]] || fail 'other bundled libraries must stay'
    [[ -e "$CASE/opt/resolve/libs/libglibmm-2.4.so.1" ]] || fail 'a library that only starts like a glib one must stay'
    assert_output 'Moved 5 bundled glib file(s)' 'install'

    # Both the program and the Qt platform plugin were checked for missing libraries.
    assert_log_lines ldd 2 'install'
    assert_output 'Every shared library Resolve links against was found' 'install'
    refute_output 'Resolve will not start' 'install'

    # What the user needs next.
    assert_output "$CASE/opt/resolve/bin/resolve" 'install'
    assert_output 'QT_QPA_PLATFORM=xcb' 'the Wayland hint'
    assert_output 'H.264/H.265' 'the codec limit of the free edition'
    assert_output 'dnxhr_hq' 'the ffmpeg conversion example'

    # The download stays, the unpacked copy goes.
    [[ -f "$CASE/downloads/DaVinci_Resolve_21.1.1_Linux.zip" ]] || fail 'the downloaded ZIP must stay'
    assert_no_leftovers 'install'
}

test_second_run_changes_nothing() {
    new_case second-run
    install_ok
    local sudo_lines apt_lines
    sudo_lines="$(log_lines sudo)"
    apt_lines="$(log_lines apt-get)"

    run_script
    assert_status 0 'second run'
    assert_output 'already installed' 'second run'
    assert_log_lines sudo "$sudo_lines" 'second run'
    assert_log_lines apt-get "$apt_lines" 'second run'
    assert_log_lines installer 1 'second run'
}

test_every_prerequisite_present() {
    new_case prerequisites-present
    printf '%s\n' "${MISSING_AT_START[@]}" >>"$CASE/installed.list"
    install_ok
    assert_output 'Every library DaVinci Resolve needs is already installed' 'prerequisites present'
    assert_log_lines apt-get 0 'nothing to install'
    assert_log_lines installer 1 'prerequisites present'
}

test_zip_layouts() {
    new_case zip-subfolder
    # The installer one folder down and without the executable bit, as a ZIP made elsewhere may be.
    make_zip "$CASE/downloads/DaVinci_Resolve_21.1.1_Linux.zip" "$CASE/fake.run" \
        'DaVinci_Resolve_21.1.1_Linux/DaVinci_Resolve_21.1.1_Linux.run' 644
    run_script
    assert_status 0 'installer in a subfolder without the executable bit'
    assert_log_lines installer 1 'the installer is found and made executable'
    assert_no_leftovers 'subfolder layout'
}

test_explicit_installer() {
    new_case explicit
    mkdir -p "$CASE/elsewhere"
    make_zip "$CASE/elsewhere/blackmagic.zip" "$CASE/fake.run" 'DaVinci_Resolve_21.1.1_Linux.run'
    run_script --installer "$CASE/elsewhere/blackmagic.zip"
    assert_status 0 '--installer with a ZIP'
    assert_output "Using $CASE/elsewhere/blackmagic.zip." 'a file that is not named like the download has no version'
    refute_output 'DaVinci Resolve blackmagic' 'no made-up version'
    assert_output 'DaVinci Resolve is installed in' 'no version in the final message'
    assert_log_lines installer 1 '--installer with a ZIP'

    new_case explicit-equals-form-and-run
    cp "$CASE/fake.run" "$CASE/elsewhere-installer.run"
    chmod 644 "$CASE/elsewhere-installer.run"
    run_script "--installer=$CASE/elsewhere-installer.run"
    assert_status 0 '--installer=FILE with a .run file'
    assert_output "Using $CASE/elsewhere-installer.run." '--installer=FILE'
    assert_log_lines installer 1 '--installer=FILE'
    assert_log_matches sudo "^interactive env SKIP_PACKAGE_CHECK=1 $CASE/elsewhere-installer\\.run -i\$" '--installer=FILE'

    new_case explicit-errors
    run_script --installer "$CASE/missing.zip"
    assert_status 1 '--installer with a missing file'
    assert_output "Installer not found: $CASE/missing.zip" 'missing file'
    : >"$CASE/notes.txt"
    run_script --installer "$CASE/notes.txt"
    assert_status 1 '--installer with another kind of file'
    assert_output 'neither a .zip nor a .run file' 'another kind of file'
    run_script --installer "$CASE/downloads"
    assert_status 1 '--installer with a folder'
    assert_output 'Installer not found' 'a folder'
    assert_untouched 'explicit installer errors'
}

test_broken_downloads_fail_before_the_system_changes() {
    new_case corrupt-zip
    printf 'this is not a zip file\n' >"$CASE/downloads/DaVinci_Resolve_21.1.1_Linux.zip"
    run_script
    assert_status 1 'corrupt ZIP'
    assert_output 'unzip could not unpack' 'corrupt ZIP'
    assert_output 'Is the download complete' 'corrupt ZIP'
    assert_untouched 'corrupt ZIP'

    new_case zip-without-installer
    printf 'readme\n' >"$CASE/readme.txt"
    make_zip "$CASE/downloads/DaVinci_Resolve_21.1.1_Linux.zip" "$CASE/readme.txt" 'README.txt'
    run_script
    assert_status 1 'ZIP without the installer'
    assert_output 'does not contain a DaVinci_Resolve_*_Linux.run installer' 'ZIP without the installer'
    assert_untouched 'ZIP without the installer'
}

test_installer_problems() {
    new_case apt-fails
    ENV_OVERRIDES=(FAKE_APT_FAIL=true)
    add_zip
    run_script
    assert_status 1 'apt-get failure'
    assert_output 'sudo apt-get update' 'the hint for a package that is not found'
    assert_log_lines installer 0 'the installer must not start without its libraries'
    assert_no_leftovers 'apt-get failure'

    new_case installer-fails
    ENV_OVERRIDES=(FAKE_INSTALLER_FAIL=true)
    add_zip
    run_script
    assert_status 1 'failing installer'
    assert_output "Blackmagic's installer failed" 'failing installer'
    refute_output 'is installed in' 'failing installer'
    [[ ! -e "$CASE/opt/resolve/libs/not_used" ]] || fail 'a failed installation must not move libraries'
    assert_no_leftovers 'failing installer'

    new_case installer-installs-nothing
    ENV_OVERRIDES=(FAKE_INSTALLER_NOOP=true)
    add_zip
    run_script
    assert_status 1 'installer that installs nothing'
    assert_output 'bin/resolve does not exist' 'installer that installs nothing'
    refute_output 'is installed in' 'installer that installs nothing'
    assert_no_leftovers 'installer that installs nothing'
}

test_missing_libraries_are_reported() {
    new_case missing-libraries
    ENV_OVERRIDES=(FAKE_LDD_MISSING=true)
    install_ok
    assert_output 'Resolve will not start until these libraries are installed' 'missing libraries'
    assert_output 'libmissing.so.1' 'missing libraries'
    assert_output 'libother.so.2' 'missing libraries'
    [[ "$(printf '%s\n' "$OUTPUT" | grep -c 'libmissing\.so\.1')" -eq 1 ]] ||
        fail 'a library that both checks miss is listed once' "$OUTPUT"
    assert_output 'apt-file search' 'missing libraries'
    assert_output 'is installed in' 'the files are installed even when a library is missing'
}

test_unattended_and_terminal() {
    new_case unattended
    add_zip
    run_script --unattended
    assert_status 1 'unattended install'
    assert_output 'cannot run unattended' 'unattended install'
    assert_output 'make repair REPAIR=davinci-resolve' 'unattended install'
    assert_untouched 'unattended install'

    run_script --unattended --dry-run
    assert_status 0 'unattended dry run'
    assert_output 'nothing was changed' 'unattended dry run'
    assert_untouched 'unattended dry run'

    # Without a terminal on standard input the installer's questions could not be answered.
    ENV_OVERRIDES=(DAVINCI_RESOLVE_REQUIRE_TERMINAL=true)
    run_script
    assert_status 1 'no terminal'
    assert_output 'needs a terminal' 'no terminal'
    assert_untouched 'no terminal'
}

test_root_is_refused() {
    new_case root
    add_zip
    ENV_OVERRIDES=(FAKE_UID=0)
    run_script
    assert_status 1 'run as root'
    assert_output 'not as root' 'run as root'
    run_script --uninstall
    assert_status 1 'uninstall as root'
    assert_output 'not as root' 'uninstall as root'
    run_script --help
    assert_status 0 '--help as root'
    assert_untouched 'run as root'
}

test_free_space_is_checked() {
    new_case low-disk
    add_zip
    ENV_OVERRIDES=(DAVINCI_RESOLVE_MIN_FREE_MB=999999999)
    run_script
    assert_status 1 'full disk'
    assert_output 'MB free' 'full disk'
    assert_untouched 'full disk'
    run_script --dry-run
    assert_status 1 'full disk in a dry run'
    assert_output 'MB free' 'full disk in a dry run'
}

test_uninstall() {
    new_case uninstall
    install_ok
    local prefix="$CASE/opt/resolve"
    local launcher="$CASE/root/usr/share/applications/com.blackmagicdesign.resolve.desktop"
    local rules_dir="$CASE/root/usr/lib/udev/rules.d"
    # Neighbours that must survive: another app's launcher and rules, the user's data, and a
    # launcher of the same name pattern that starts a program elsewhere.
    printf '[Desktop Entry]\nExec=/usr/bin/other\n' >"$CASE/root/usr/share/applications/other.desktop"
    printf '[Desktop Entry]\nExec=/opt/resolve-extra/bin/tool\n' >"$CASE/root/usr/share/applications/lookalike.desktop"
    : >"$rules_dir/60-other.rules"
    mkdir -p "$CASE/home/.local/share/DaVinciResolve"
    : >"$CASE/home/.local/share/DaVinciResolve/project.db"
    local sudo_lines
    sudo_lines="$(log_lines sudo)"

    run_script --uninstall --dry-run
    assert_status 0 'uninstall dry run'
    assert_output "would remove $prefix" 'uninstall dry run'
    assert_output "would remove $launcher" 'uninstall dry run'
    assert_output "would remove $rules_dir/99-BlackmagicDevices.rules" 'uninstall dry run'
    assert_output "would remove $rules_dir/99-ResolveKeyboardHID.rules" 'uninstall dry run'
    refute_output 'other.desktop' 'uninstall dry run'
    refute_output 'lookalike.desktop' 'uninstall dry run'
    [[ -x "$prefix/bin/resolve" ]] || fail 'a dry run must not remove Resolve'
    assert_log_lines sudo "$sudo_lines" 'uninstall dry run'

    run_script --uninstall --unattended
    assert_status 0 'uninstall'
    assert_output 'Removed DaVinci Resolve' 'uninstall'
    [[ ! -e "$prefix" ]] || fail 'the prefix must be removed'
    [[ ! -e "$launcher" ]] || fail 'the launcher must be removed'
    [[ ! -e "$rules_dir/99-BlackmagicDevices.rules" && ! -e "$rules_dir/99-ResolveKeyboardHID.rules" ]] ||
        fail 'the udev rules must be removed'
    [[ -f "$CASE/root/usr/share/applications/other.desktop" ]] || fail "another app's launcher must stay"
    [[ -f "$CASE/root/usr/share/applications/lookalike.desktop" ]] || fail 'a launcher that starts a program outside the prefix must stay'
    [[ -f "$rules_dir/60-other.rules" ]] || fail "another package's udev rules must stay"
    [[ -f "$CASE/home/.local/share/DaVinciResolve/project.db" ]] || fail 'the projects and settings must stay'
    assert_log_matches sudo "^noninteractive rm -rf -- $prefix\$" 'uninstall never prompts when unattended'

    sudo_lines="$(log_lines sudo)"
    run_script --uninstall
    assert_status 0 'second uninstall'
    assert_output 'not installed' 'second uninstall'
    assert_log_lines sudo "$sudo_lines" 'second uninstall'
}

test_uninstall_removes_only_what_is_resolve() {
    new_case foreign-folder
    mkdir -p "$CASE/opt/resolve/keep"
    : >"$CASE/opt/resolve/keep/file"
    run_script --uninstall
    assert_status 0 'a folder without Resolve'
    assert_output 'nothing to remove' 'a folder without Resolve'
    [[ -f "$CASE/opt/resolve/keep/file" ]] || fail 'a folder that holds no Resolve program must not be deleted'
    assert_log_lines sudo 0 'a folder without Resolve'

    new_case relative-prefix
    ENV_OVERRIDES=(DAVINCI_RESOLVE_PREFIX=relative/resolve)
    run_script --uninstall
    assert_status 1 'a relative prefix'
    assert_output 'Refusing to remove' 'a relative prefix'
    assert_log_lines sudo 0 'a relative prefix'
}

## Write a stand-in script to $1 that records its arguments and sudo setting.
write_recording_script() {
    cat >"$1" <<'EOF'
#!/usr/bin/env bash
printf 'args=%s sudo=%s\n' "$*" "${DAVINCI_RESOLVE_SUDO:-}" >>"$WRAPPER_LOG"
EOF
}

test_bootstrap_and_repair_wiring() {
    local checkout="$TEST_ROOT/checkout" home="$TEST_ROOT/wrapper-home" log="$TEST_ROOT/wrapper.log" repo="$TEST_ROOT/repair-repo"
    local usage_text file

    mkdir -p "$checkout/.local/scripts" "$home/.local/scripts" "$repo/.local/scripts"
    write_recording_script "$checkout/.local/scripts/davinci_resolve_install.sh"
    write_recording_script "$home/.local/scripts/davinci_resolve_install.sh"
    write_recording_script "$repo/.local/scripts/davinci_resolve_install.sh"
    cp "$REPAIR_SCRIPT" "$repo/.local/scripts/repair-installation"
    : >"$log"

    (
        # shellcheck source=/dev/null
        source "$BASE_FUNCTIONS"
        export WRAPPER_LOG="$log"
        unset SUDO_COMMAND DAVINCI_RESOLVE_SUDO UNATTENDED DOTFILES_ROOT
        DOTFILES_ROOT="$checkout" UNATTENDED=true SUDO_COMMAND=privilege-runner davinci_resolve_install
        DOTFILES_ROOT="$checkout" UNATTENDED=false davinci_resolve_install
        DOTFILES_ROOT="$checkout" davinci_uninstall
        HOME="$home" davinci_resolve_install
        # A failing uninstall must not abort the uninstall menu.
        printf '#!/usr/bin/env bash\nexit 1\n' >"$checkout/.local/scripts/davinci_resolve_install.sh"
        DOTFILES_ROOT="$checkout" davinci_uninstall
    ) || fail 'the bootstrap wrappers must run and tolerate a failing uninstall'
    [[ "$(cat "$log")" == $'args=--unattended sudo=privilege-runner\nargs= sudo=sudo\nargs=--uninstall sudo=\nargs= sudo=sudo' ]] ||
        fail 'davinci_resolve_install must pass --unattended and the sudo command, and fall back to HOME' "$(cat "$log")"

    : >"$log"
    WRAPPER_LOG="$log" DRY_RUN=true bash "$repo/.local/scripts/repair-installation" davinci-resolve auto
    WRAPPER_LOG="$log" bash "$repo/.local/scripts/repair-installation" davinci-resolve auto
    [[ "$(cat "$log")" == $'args=--dry-run sudo=\nargs= sudo=' ]] ||
        fail 'make repair REPAIR=davinci-resolve must run the installer and honour DRY_RUN' "$(cat "$log")"
    usage_text="$(bash "$repo/.local/scripts/repair-installation" bogus 2>&1 || true)"
    [[ "$usage_text" == *'nvidia-container-toolkit|davinci-resolve'* ]] || fail 'the repair usage text must list davinci-resolve' "$usage_text"
    grep -Fq 'davinci-resolve' "$REPO_ROOT/Makefile" || fail 'the make repair help must list davinci-resolve'

    for file in ubuntu work; do
        grep -Fq 'davinci_resolve_install ||' "$BOOTSTRAP_DIR/$file" ||
            fail "the $file bootstrap must run the Resolve step without aborting on failure"
        grep -Fq 'make repair REPAIR=davinci-resolve' "$BOOTSTRAP_DIR/$file" ||
            fail "the $file bootstrap must name the repair command"
    done
    for file in uninstall_ubuntu uninstall_work; do
        grep -Fq 'davinci_uninstall' "$BOOTSTRAP_DIR/$file" || fail "$file must offer the Resolve removal"
    done
    # Only the desktop profiles that can run a GUI editor call it; not WSL, Arch, or macOS.
    for file in ubuntu_windows arch manjaro mac; do
        if grep -Fq 'davinci_resolve_install' "$BOOTSTRAP_DIR/$file"; then
            fail "the $file bootstrap must not install DaVinci Resolve"
        fi
    done
    grep -Fq 'bash tests/test_davinci_resolve_install.sh' "$REPO_ROOT/Makefile" || fail 'make test must run this suite'
}

test_video_apps_are_declared() {
    local file
    for file in ubuntu_functions work_functions; do
        grep -Eq 'snap install openshot-qt( |$)' "$BOOTSTRAP_DIR/$file" ||
            fail "$file must install the official OpenShot snap"
        grep -Eq 'snap install blender --classic' "$BOOTSTRAP_DIR/$file" ||
            fail "$file must install the official Blender snap with --classic"
        # The PPA is not the choice (70 packages, release upgrades disable it) and neither is the
        # archive build (two releases behind).
        # shellcheck disable=SC2016 # grep reads \$apt as text, the shell must not expand it
        if grep -Eq 'ppa:openshot|(\$apt|apt(-get)? install)[^#]*openshot' "$BOOTSTRAP_DIR/$file"; then
            fail "$file must not install OpenShot from the PPA or the Ubuntu archive"
        fi
    done
    grep -Eq '^[[:space:]]+blender[[:space:]]+#' "$BOOTSTRAP_DIR/arch_functions" || fail 'the Arch profile must install blender'
    grep -Eq '^[[:space:]]+openshot[[:space:]]+#' "$BOOTSTRAP_DIR/arch_functions" || fail 'the Arch profile must install openshot'
    grep -Eq '^[[:space:]]+blender[[:space:]]+#' "$BOOTSTRAP_DIR/mac_functions" || fail 'the mac profile must install the blender cask'
    grep -Eq '^[[:space:]]+openshot-video-editor[[:space:]]+#' "$BOOTSTRAP_DIR/mac_functions" ||
        fail 'the mac profile must install the openshot-video-editor cask'

    for file in uninstall_ubuntu_functions uninstall_work_functions; do
        grep -Fq 'snap remove openshot-qt' "$BOOTSTRAP_DIR/$file" || fail "$file must remove the OpenShot snap"
        grep -Fq 'snap remove blender' "$BOOTSTRAP_DIR/$file" || fail "$file must remove the Blender snap"
    done
    grep -Eq '(^|[[:space:]])blender([[:space:]]|$)' "$BOOTSTRAP_DIR/uninstall_arch_functions" || fail 'the Arch uninstaller must remove blender'
    grep -Eq '(^|[[:space:]])openshot([[:space:]]|$)' "$BOOTSTRAP_DIR/uninstall_arch_functions" || fail 'the Arch uninstaller must remove openshot'
    grep -Eq 'blender openshot-video-editor' "$BOOTSTRAP_DIR/uninstall_mac_functions" || fail 'the mac uninstaller must remove both casks'
}

test_help_and_usage_errors
test_other_machines_are_skipped
test_not_debian_is_refused
test_existing_installation_is_left_alone
test_missing_installer_explains_the_download
test_default_search_folders
test_newest_installer_is_chosen
test_dry_run_changes_nothing
test_full_installation
test_second_run_changes_nothing
test_every_prerequisite_present
test_zip_layouts
test_explicit_installer
test_broken_downloads_fail_before_the_system_changes
test_installer_problems
test_missing_libraries_are_reported
test_unattended_and_terminal
test_root_is_refused
test_free_space_is_checked
test_uninstall
test_uninstall_removes_only_what_is_resolve
test_bootstrap_and_repair_wiring
test_video_apps_are_declared

printf 'PASS: davinci_resolve_install gates, finds the download, installs reversibly, and the video editors are declared in every profile\n'
