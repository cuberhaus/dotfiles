#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
WORK_BOOTSTRAP="$REPO_ROOT/.local/scripts/bootstrap/work"
WORK_FUNCTIONS="$REPO_ROOT/.local/scripts/bootstrap/work_functions"
CASE_DIR="$(mktemp -d)"
EVENT_LOG="$CASE_DIR/events.log"
RM_COMMAND="$(command -v rm)"
trap '"$RM_COMMAND" -rf "$CASE_DIR"' EXIT

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

grep -Fq "if [[ \"\${BASH_SOURCE[0]}\" == \"\$0\" ]]; then" "$WORK_BOOTSTRAP" ||
    fail 'work bootstrap must be sourceable without provisioning the machine'

export HOME="$CASE_DIR/home"
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_STATE_HOME="$HOME/.local/state"
export SHELL=/bin/zsh
mkdir -p "$XDG_CONFIG_HOME"
: > "$EVENT_LOG"

source "$WORK_BOOTSTRAP"

test_unattended_environment_is_noninteractive() {
    local original_path="$PATH"
    local fake_bin="$CASE_DIR/fake-bin"
    local sudo_log="$CASE_DIR/sudo.log"
    local error_log="$CASE_DIR/sudo-error.log"
    mkdir -p "$fake_bin"
    cat > "$fake_bin/privilege-runner" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$FAKE_SUDO_LOG"
[ "${FAKE_SUDO_FAIL:-false}" != true ]
EOF
    printf '#!/usr/bin/env bash\n' > "$fake_bin/zsh"
    chmod +x "$fake_bin/privilege-runner" "$fake_bin/zsh"

    export PATH="$fake_bin:$PATH"
    export FAKE_SUDO_LOG="$sudo_log"
    export SUDO_COMMAND=privilege-runner
    UNATTENDED=true
    unset DEBIAN_FRONTEND

    work_prepare_environment || fail 'cached sudo credentials should allow unattended mode'
    [ "$DEBIAN_FRONTEND" = noninteractive ] ||
        fail 'unattended mode must export DEBIAN_FRONTEND=noninteractive'
    sudo apt-get update
    $apt example-package
    grep -Fxq -- '-n true' "$sudo_log" || fail 'unattended mode did not preflight sudo -n'
    grep -Fxq -- '-n apt-get update' "$sudo_log" ||
        fail 'unattended mode did not keep subsequent sudo calls noninteractive'
    grep -Fxq -- '-n env DEBIAN_FRONTEND=noninteractive apt-get install -y example-package' "$sudo_log" ||
        fail 'apt installs did not preserve noninteractive Debian behavior through sudo'

    local SHELL=/bin/bash
    local USER=bootstrap-user
    work_configure_default_shell
    grep -Fxq -- "-n chsh -s $fake_bin/zsh bootstrap-user" "$sudo_log" ||
        fail 'unattended shell change did not use noninteractive sudo'

    export FAKE_SUDO_FAIL=true
    if work_prepare_environment 2> "$error_log"; then
        fail 'unattended mode accepted unavailable sudo credentials'
    fi
    grep -Fq 'sudo -v' "$error_log" ||
        fail 'unattended sudo failure did not explain how to authorize it'

    unset -f sudo
    unset FAKE_SUDO_FAIL FAKE_SUDO_LOG DEBIAN_FRONTEND SUDO_COMMAND
    export PATH="$original_path"
}

test_unattended_environment_is_noninteractive

test_dev_tools_preserve_git_credentials() {
    local apt=true
    local fake_bin="$CASE_DIR/git-bin"
    local git_log="$CASE_DIR/git.log"
    local original_path="$PATH"
    mkdir -p "$fake_bin"
    cat > "$fake_bin/git" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$FAKE_GIT_LOG"
EOF
    chmod +x "$fake_bin/git"
    export FAKE_GIT_LOG="$git_log"
    export PATH="$fake_bin:$PATH"

    dev_tools_install
    export PATH="$original_path"
    unset FAKE_GIT_LOG
    [ ! -s "$git_log" ] || fail 'dev tools must not configure plaintext Git credential storage'
}

test_cursor_install_detection() {
    local original_path="$PATH"
    local fake_bin="$CASE_DIR/cursor-bin"
    local cursor_app="$HOME/Applications/cursor.AppImage"
    local chmod_command rm_command stat_command truncate_command
    chmod_command="$(command -v chmod)"
    rm_command="$(command -v rm)"
    stat_command="$(command -v stat)"
    truncate_command="$(command -v truncate)"
    mkdir -p "$fake_bin" "$HOME/Applications"
    ln -s "$stat_command" "$fake_bin/stat"
    export PATH="$fake_bin"

    : > "$cursor_app"
    if cursor_is_installed; then
        fail 'a partial Cursor AppImage must not count as installed'
    fi

    "$truncate_command" -s 1048576 "$cursor_app"
    cursor_is_installed || fail 'a complete Cursor AppImage was not detected'

    "$rm_command" -f "$cursor_app"
    printf '#!/usr/bin/env bash\n' > "$fake_bin/cursor"
    "$chmod_command" +x "$fake_bin/cursor"
    cursor_is_installed || fail 'the installed Cursor command was not detected'

    export PATH="$original_path"
    [ "$(grep -c 'cursor_is_installed' "$WORK_FUNCTIONS")" -ge 3 ] ||
        fail 'both Cursor installer branches must use the shared installed check'
}

test_workspace_is_cloned_before_sync() {
    local fake_bin="$CASE_DIR/workspace-bin"
    local gh_log="$CASE_DIR/gh.log"
    local restore_attempt_log="$CASE_DIR/workspace-restore-attempt.log"
    local restore_failure_marker="$CASE_DIR/workspace-restore-failed"
    local restore_log="$CASE_DIR/workspace-restore.log"
    local sync_log="$CASE_DIR/workspace-sync.log"
    local workspace_dir="$CASE_DIR/cuberhaus-workspace"
    local make_command
    make_command="$(command -v make)"
    mkdir -p "$fake_bin"
    cat > "$fake_bin/gh" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$FAKE_GH_LOG"
mkdir -p "$4/.git"
cat > "$4/Makefile" <<'WORKSPACE_MAKEFILE'
.RECIPEPREFIX := >
skills-restore:
>@printf 'attempt\n' >> "$$FAKE_WORKSPACE_RESTORE_ATTEMPT_LOG"
>@test -e "$$FAKE_WORKSPACE_RESTORE_FAILURE_MARKER" || { touch "$$FAKE_WORKSPACE_RESTORE_FAILURE_MARKER"; exit 1; }
>@mkdir -p .agents/skills
>@printf 'restore\n' >> "$$FAKE_WORKSPACE_RESTORE_LOG"
WORKSPACE_MAKEFILE
cat > "$4/sync.sh" <<'SYNC_SCRIPT'
#!/usr/bin/env bash
[ -s "$FAKE_WORKSPACE_RESTORE_LOG" ] || {
    printf 'skills must be restored before sync\n' >&2
    exit 1
}
printf 'sync\n' >> "$FAKE_WORKSPACE_SYNC_LOG"
SYNC_SCRIPT
chmod +x "$4/sync.sh"
EOF
    chmod +x "$fake_bin/gh"

    export FAKE_GH_LOG="$gh_log"
    export FAKE_WORKSPACE_RESTORE_ATTEMPT_LOG="$restore_attempt_log"
    export FAKE_WORKSPACE_RESTORE_FAILURE_MARKER="$restore_failure_marker"
    export FAKE_WORKSPACE_RESTORE_LOG="$restore_log"
    export FAKE_WORKSPACE_SYNC_LOG="$sync_log"
    if PATH="$fake_bin:$PATH" "$make_command" --no-print-directory -C "$REPO_ROOT" \
        bootstrap-workspace CUBERHAUS_WORKSPACE_DIR="$workspace_dir"; then
        fail 'bootstrap must fail when initial workspace skill restoration fails'
    fi
    [ ! -s "$sync_log" ] || fail 'workspace sync must not run after failed skill restoration'

    PATH="$fake_bin:$PATH" "$make_command" --no-print-directory -C "$REPO_ROOT" \
        bootstrap-workspace CUBERHAUS_WORKSPACE_DIR="$workspace_dir"
    PATH="$fake_bin:$PATH" "$make_command" --no-print-directory -C "$REPO_ROOT" \
        bootstrap-workspace CUBERHAUS_WORKSPACE_DIR="$workspace_dir"

    [ "$(grep -c '^repo clone cuberhaus/cuberhaus-workspace ' "$gh_log")" -eq 1 ] ||
        fail 'workspace repository must be cloned exactly once'
    [ "$(grep -c '^attempt$' "$restore_attempt_log")" -eq 2 ] ||
        fail 'workspace skill restoration must retry after an interrupted first attempt'
    [ "$(grep -c '^restore$' "$restore_log")" -eq 1 ] ||
        fail 'workspace skills must not be reinstalled after restoration succeeds'
    [ "$(grep -c '^sync$' "$sync_log")" -eq 2 ] ||
        fail 'workspace repository must be synced on every invocation'
    [ "$(grep -c 'bootstrap-workspace' "$REPO_ROOT/Makefile")" -ge 7 ] ||
        fail 'all OS bootstrap targets must use the shared workspace prerequisite'
    grep -Fq 'RESTORE_WORKSPACE_SKILLS ?= 1' "$REPO_ROOT/Makefile" ||
        fail 'workspace skill installation must be enabled by default'

    unset FAKE_GH_LOG FAKE_WORKSPACE_RESTORE_ATTEMPT_LOG
    unset FAKE_WORKSPACE_RESTORE_FAILURE_MARKER FAKE_WORKSPACE_RESTORE_LOG
    unset FAKE_WORKSPACE_SYNC_LOG
}

test_dev_tools_preserve_git_credentials
test_cursor_install_detection
test_workspace_is_cloned_before_sync

git check-ignore -q \
    .config/systemd/user/timers.target.wants/cuberhaus-workspace-pull.timer ||
    fail 'runtime systemd timer links must be ignored'

record() {
    printf '%s\n' "$1" >> "$EVENT_LOG"
}

bootstrap_enable_logging() { record 'logging'; }
work_prepare_environment() { record 'prepare-environment'; }
configure_dual_boot_utc_rtc() { record 'dual-boot'; }
work_update_system() { record 'system-update'; }
bootstrap_stow_checkout() { record "stow:$1:$SKIP_STOW"; }
install_preparation() { record 'preparation'; }
shutdown_fix() { record 'shutdown-fix'; }
nvidia_install() { record 'nvidia-install'; }
nvidia_display_config() { record 'nvidia-display'; }
dev_tools_install() { record 'dev-tools'; }
sops_install() { record 'sops'; }
work_configure_default_shell() { record 'default-shell'; }
node_install() { record 'node'; }
python_install() { record 'python'; }
docker_install() { record 'docker'; }
gcloud_install() { record 'gcloud'; }
gui_apps_install() { record 'gui-apps'; }
obsidian_vault_install() { record 'obsidian-vault'; }
resolve_high_dpi_choice() {
    record "high-dpi-choice:$HIGH_DPI_CHOICE"
    HIGH_DPI=false
}
high_dpi_screen() { record 'high-dpi-apply'; }
apply_skip_worktree() { record 'skip-worktree'; }
gsettings() { printf '1.0\n'; }
info() { :; }

work_main --unattended --no-stow --high-dpi=no </dev/null

[ "$UNATTENDED" = true ] || fail '--unattended was not parsed'
[ "$SKIP_STOW" = true ] || fail '--no-stow was not parsed'
[ "$HIGH_DPI_CHOICE" = no ] || fail '--high-dpi was not parsed'

expected_events=$'logging\nprepare-environment\ndual-boot\nsystem-update\nstow:work:true\npreparation\nshutdown-fix\nnvidia-install\nnvidia-display\ndev-tools\nsops\ndefault-shell\nnode\npython\ndocker\ngcloud\ngui-apps\nobsidian-vault\nhigh-dpi-choice:no\nskip-worktree'
actual_events="$(cat "$EVENT_LOG")"
[ "$actual_events" = "$expected_events" ] ||
    fail "unexpected work bootstrap stages:\n$actual_events"

grep -Fqx 'export DISTRO=ubuntu' "$XDG_CONFIG_HOME/distro" ||
    fail 'work bootstrap did not write the Ubuntu distro marker'

printf 'Work bootstrap orchestration tests passed.\n'