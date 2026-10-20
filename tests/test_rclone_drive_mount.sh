#!/usr/bin/env bash
# Hermetic tests for rclone_drive_mount_install and rclone_drive_mount_uninstall
# in bootstrap/base_functions, the cuberhaus-drive-mount.service unit, the
# DRIVE_ROOT export in .zshenv and .config/zsh/.zshenv, and how profiles,
# uninstallers, and repair-installation wire them up.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BOOTSTRAP_DIR="$REPO_ROOT/.local/scripts/bootstrap"
SCRATCH="$(mktemp -d)"
RM_COMMAND="$(command -v rm)"
ORIGINAL_PATH="$PATH"
trap '"$RM_COMMAND" -rf "$SCRATCH"' EXIT

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

# shellcheck source=/dev/null
source "$BOOTSTRAP_DIR/base_functions"

new_case() {
    CASE="$SCRATCH/$1"
    export HOME="$CASE/home"
    export DRIVE_ROOT="$HOME/Google Drive"
    mkdir -p "$CASE/bin" "$HOME/.config/systemd/user" "$HOME/.config/rclone"
    cp "$REPO_ROOT/.config/systemd/user/cuberhaus-drive-mount.service" \
        "$HOME/.config/systemd/user/cuberhaus-drive-mount.service"
    CALLS_FILE="$CASE/calls.log"
    REMOTES_FILE="$CASE/remotes"
    : > "$CALLS_FILE"
    : > "$REMOTES_FILE"

    cat > "$CASE/bin/uname" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "${FAKE_UNAME:-Linux}"
EOF
    cat > "$CASE/bin/systemctl" <<EOF
#!/usr/bin/env bash
printf 'systemctl %s\n' "\$*" >> "$CALLS_FILE"
if [ "\${FAKE_SYSTEMCTL_FAIL:-false}" = true ]; then
    exit 1
fi
EOF
    cat > "$CASE/bin/rclone" <<EOF
#!/usr/bin/env bash
printf 'rclone %s\n' "\$*" >> "$CALLS_FILE"
case "\${1:-}" in
    listremotes)
        cat "$REMOTES_FILE"
        ;;
    config)
        if [ "\${FAKE_RCLONE_CONFIG_FAIL:-false}" = true ]; then
            exit 1
        fi
        printf 'drive:\n' > "$REMOTES_FILE"
        printf '[drive]\ntype = drive\n' > "$HOME/.config/rclone/rclone.conf"
        ;;
esac
EOF
    cat > "$CASE/bin/fusermount3" <<EOF
#!/usr/bin/env bash
printf 'fusermount3 %s\n' "\$*" >> "$CALLS_FILE"
EOF
    chmod +x "$CASE/bin/uname" "$CASE/bin/systemctl" "$CASE/bin/rclone" "$CASE/bin/fusermount3"
    export PATH="$CASE/bin:$ORIGINAL_PATH"
    is_wsl() { [ "${FAKE_WSL:-false}" = true ]; }
}

test_enables_service_when_drive_remote_already_configured() {
    new_case configured
    printf 'drive:\n' > "$REMOTES_FILE"

    UNATTENDED=true rclone_drive_mount_install >/dev/null || fail 'install with existing drive: remote failed'

    [ -d "$HOME/Google Drive" ] || fail 'mount directory must be created'
    grep -Fxq 'systemctl --user daemon-reload' "$CALLS_FILE" ||
        fail 'must reload user systemd daemon'
    grep -Fxq 'systemctl --user enable --now cuberhaus-drive-mount.service' "$CALLS_FILE" ||
        fail 'must enable and start cuberhaus-drive-mount.service'
    if grep -Fq 'rclone config create' "$CALLS_FILE"; then
        fail 'must not run rclone config create when drive: is already configured'
    fi
}

test_unattended_skips_oauth_cleanly_when_remote_is_unconfigured() {
    new_case unattended-unconfigured
    local output

    output="$(UNATTENDED=true rclone_drive_mount_install)" ||
        fail 'unattended install without drive: remote must succeed and print repair hint'

    assert_contains "$output" 'make repair REPAIR=drive-mount' \
        'unattended message when drive: remote is not configured'
    if grep -Fq 'rclone config' "$CALLS_FILE" || grep -Fq 'systemctl' "$CALLS_FILE"; then
        fail 'unattended install without drive: must not launch OAuth or enable the unit'
    fi
}

test_interactive_configures_oauth_with_gmail_client_credentials() {
    new_case interactive-oauth
    mkdir -p "$HOME/.cursor"
    cat > "$HOME/.cursor/gmail-oauth-client.json" <<'EOF'
{"installed":{"client_id":"test-client-id.apps.googleusercontent.com","client_secret":"test-secret"}}
EOF
    bootstrap_stdin_is_interactive() { return 0; }

    UNATTENDED=false rclone_drive_mount_install >/dev/null ||
        fail 'interactive install with OAuth client credentials failed'

    grep -Fxq 'rclone config create drive drive scope=drive client_id=test-client-id.apps.googleusercontent.com client_secret=test-secret' "$CALLS_FILE" ||
        fail 'interactive install must pass client_id and client_secret from ~/.cursor/gmail-oauth-client.json'
    grep -Fxq 'systemctl --user enable --now cuberhaus-drive-mount.service' "$CALLS_FILE" ||
        fail 'interactive install must enable cuberhaus-drive-mount.service after OAuth'
}

test_fails_when_unit_file_is_not_stowed() {
    new_case missing-unit
    local status=0 output
    rm -f "$HOME/.config/systemd/user/cuberhaus-drive-mount.service"

    output="$(UNATTENDED=true rclone_drive_mount_install 2>&1)" || status=$?

    [ "$status" -ne 0 ] || fail 'must fail when unit file is missing'
    assert_contains "$output" 'make restow' 'missing unit message must point to make restow'
}

test_skips_on_non_linux_and_wsl() {
    new_case skip-platforms
    printf 'drive:\n' > "$REMOTES_FILE"

    FAKE_UNAME=Darwin UNATTENDED=true rclone_drive_mount_install >/dev/null ||
        fail 'must succeed on macOS'
    FAKE_WSL=true UNATTENDED=true rclone_drive_mount_install >/dev/null ||
        fail 'must succeed on WSL'

    [ ! -s "$CALLS_FILE" ] || fail 'must not call rclone or systemctl on macOS or WSL'
}

test_uninstall_disables_service_and_unmounts() {
    new_case uninstall

    rclone_drive_mount_uninstall >/dev/null || fail 'uninstall failed'

    grep -Fxq 'systemctl --user disable --now cuberhaus-drive-mount.service' "$CALLS_FILE" ||
        fail 'uninstall must disable and stop cuberhaus-drive-mount.service'
    grep -Fxq "fusermount3 -uz $HOME/Google Drive" "$CALLS_FILE" ||
        fail 'uninstall must lazy-unmount ~/Google Drive via fusermount3'
}

test_profiles_uninstallers_repair_and_shell_env_wire_up_drive_mount() {
    local entry_scripts=(
        "$BOOTSTRAP_DIR/ubuntu"
        "$BOOTSTRAP_DIR/work"
        "$BOOTSTRAP_DIR/arch"
        "$BOOTSTRAP_DIR/manjaro"
    )
    local uninstall_functions=(
        "$BOOTSTRAP_DIR/uninstall_ubuntu_functions"
        "$BOOTSTRAP_DIR/uninstall_work_functions"
        "$BOOTSTRAP_DIR/uninstall_arch_functions"
    )
    local file dry_output

    for file in "${entry_scripts[@]}"; do
        grep -Eq '^[[:space:]]*rclone_drive_mount_install \|\|$' "$file" ||
            fail "$file must run rclone_drive_mount_install without aborting on failure"
    done
    if grep -Fq 'rclone_drive_mount_install' "$BOOTSTRAP_DIR/ubuntu_windows"; then
        fail 'WSL bootstrap must not run rclone_drive_mount_install'
    fi
    for file in "${uninstall_functions[@]}"; do
        grep -Eq '^[[:space:]]*rclone_drive_mount_uninstall( \|\| true)?$' "$file" ||
            fail "$file must undo what rclone_drive_mount_install set"
    done

    dry_output="$(DRY_RUN=true bash "$REPO_ROOT/.local/scripts/repair-installation" drive-mount)" ||
        fail 'repair-installation drive-mount DRY_RUN=true failed'
    assert_contains "$dry_output" 'rclone_drive_mount_install' \
        'repair-installation drive-mount dry-run output'

    # shellcheck disable=SC2016
    grep -Fq 'export DRIVE_ROOT="$HOME/Google Drive"' "$REPO_ROOT/.zshenv" ||
        fail '.zshenv must export DRIVE_ROOT on Linux/macOS'
    # shellcheck disable=SC2016
    grep -Fq 'export DRIVE_ROOT="${DRIVE_ROOT:-$HOME/Google Drive}"' "$REPO_ROOT/.config/zsh/.zshenv" ||
        fail '.config/zsh/.zshenv must export DRIVE_ROOT for editor/agent shells'
}

for test_name in \
    test_enables_service_when_drive_remote_already_configured \
    test_unattended_skips_oauth_cleanly_when_remote_is_unconfigured \
    test_interactive_configures_oauth_with_gmail_client_credentials \
    test_fails_when_unit_file_is_not_stowed \
    test_skips_on_non_linux_and_wsl \
    test_uninstall_disables_service_and_unmounts \
    test_profiles_uninstallers_repair_and_shell_env_wire_up_drive_mount; do
    ( "$test_name" ) || {
        printf 'not ok - %s\n' "$test_name" >&2
        exit 1
    }
    printf 'ok - %s\n' "$test_name"
done

printf 'Google Drive rclone mount bootstrap tests passed.\n'
