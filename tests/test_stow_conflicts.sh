#!/usr/bin/env bash
# Tests that `make restow` can clear the conflicts `make audit-installation` sends people to it
# for, and that it never moves the live GNOME settings database.
#
# The Stow scenarios run the real GNU Stow against a scratch package and a scratch HOME, and are
# skipped where `stow` is not installed. The Makefile checks only read the recipes with `make -n`.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
BACKUP_SCRIPT=.local/scripts/stow-backup-conflicts
ORIGINAL_HOME="$HOME"
CASE_DIR=
STOW_AVAILABLE=false
command -v stow >/dev/null 2>&1 && STOW_AVAILABLE=true

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

# A scratch Stow package shaped like the real checkout ($CASE_DIR/cuberhaus/dotfiles, so Stow
# links through relative paths) with this repository's real backup script and ignore list,
# and an empty HOME that the script and Stow both treat as their target.
setup_case() {
    CASE_DIR="$(mktemp -d)"
    unset STOW_TARGET
    export HOME="$CASE_DIR/home"
    PACKAGE="$CASE_DIR/cuberhaus/dotfiles"
    mkdir -p "$HOME" "$PACKAGE/.local/scripts" "$PACKAGE/.config/app"
    cp "$REPO_ROOT/$BACKUP_SCRIPT" "$PACKAGE/$BACKUP_SCRIPT"
    cp "$REPO_ROOT/.stow-local-ignore" "$PACKAGE/.stow-local-ignore"
    printf 'repository settings\n' > "$PACKAGE/.config/app/settings.json"
}

teardown_case() {
    export HOME="$ORIGINAL_HOME"
    rm -rf "$CASE_DIR"
}

run_stow() {
    stow -R -t "$HOME" -d "$CASE_DIR/cuberhaus" dotfiles
}

run_backup() {
    bash "$PACKAGE/$BACKUP_SCRIPT" > "$CASE_DIR/backup.log" 2>&1 \
        || { cat "$CASE_DIR/backup.log" >&2; fail "$BACKUP_SCRIPT failed"; }
}

# The audit reports "Stow reports N pending action/conflict line(s)" for this exact situation, and
# `stow -R` cannot get past it because Stow owns only the relative links it created itself.
test_absolute_link_to_the_repository_file_is_replaced_by_a_stow_link() {
    setup_case
    mkdir -p "$HOME/.config/app"
    ln -s "$PACKAGE/.config/app/settings.json" "$HOME/.config/app/settings.json"

    if run_stow 2> "$CASE_DIR/stow.err"; then
        fail 'Expected Stow to reject an absolute link that it did not create'
    fi
    grep -Fq 'existing target is not owned by stow: .config/app/settings.json' "$CASE_DIR/stow.err" \
        || fail "Stow did not report the link as not owned: $(cat "$CASE_DIR/stow.err")"

    run_backup
    run_stow || fail 'Stow must succeed once the conflicting link has been backed up'

    local link backed_up
    link="$(readlink "$HOME/.config/app/settings.json")"
    case "$link" in
        /*) fail "Expected a relative link made by Stow, got: $link" ;;
    esac
    [ "$HOME/.config/app/settings.json" -ef "$PACKAGE/.config/app/settings.json" ] \
        || fail 'The new link must resolve to the repository file'
    backed_up="$(find "$HOME/.dotfiles-backup" -type l -path '*/.config/app/settings.json' -print -quit)"
    [ -n "$backed_up" ] || fail 'The replaced absolute link must be kept in the backup folder'
    [ "$(readlink "$backed_up")" = "$PACKAGE/.config/app/settings.json" ] \
        || fail 'The backed-up link must keep pointing at its original target'
    grep -Fq 'repository settings' "$PACKAGE/.config/app/settings.json" \
        || fail 'The repository file must be untouched'

    # Nothing is left to do, so the audit would report no drift.
    stow -n -v -t "$HOME" -d "$CASE_DIR/cuberhaus" dotfiles > "$CASE_DIR/plan.out" 2>&1 \
        || fail "A second Stow run still reports drift: $(cat "$CASE_DIR/plan.out")"
    teardown_case
}

test_plain_file_in_the_way_is_backed_up() {
    setup_case
    mkdir -p "$HOME/.config/app"
    printf 'my own settings\n' > "$HOME/.config/app/settings.json"

    run_backup
    run_stow || fail 'Stow must succeed once the conflicting file has been backed up'

    [ "$HOME/.config/app/settings.json" -ef "$PACKAGE/.config/app/settings.json" ] \
        || fail 'The new link must resolve to the repository file'
    find "$HOME/.dotfiles-backup" -type f -path '*/.config/app/settings.json' -print -quit | grep -q . \
        || fail 'The replaced file must be kept in the backup folder'
    grep -Fq 'my own settings' "$(find "$HOME/.dotfiles-backup" -type f -path '*/.config/app/settings.json' -print -quit)" \
        || fail 'The backed-up file must keep its content'
    teardown_case
}

# The backup folder can now hold a symbolic link, so uninstalling must put it back as it was.
test_uninstall_restores_a_backed_up_link() {
    setup_case
    cp "$REPO_ROOT/.local/scripts/stow-uninstall" "$PACKAGE/.local/scripts/stow-uninstall"
    mkdir -p "$HOME/.config/app"
    ln -s "$PACKAGE/.config/app/settings.json" "$HOME/.config/app/settings.json"

    run_backup
    run_stow || fail 'Stow must succeed once the conflicting link has been backed up'
    bash "$PACKAGE/.local/scripts/stow-uninstall" > "$CASE_DIR/uninstall.log" 2>&1 \
        || { cat "$CASE_DIR/uninstall.log" >&2; fail 'stow-uninstall failed'; }

    [ -L "$HOME/.config/app/settings.json" ] || fail 'Uninstalling must restore the original link'
    [ "$(readlink "$HOME/.config/app/settings.json")" = "$PACKAGE/.config/app/settings.json" ] \
        || fail 'The restored link must point where the original did'
    teardown_case
}

# dconf rewrites its database and gnome-shell keeps it mapped, so Stow can never own it, and the
# backup step would otherwise move the user's real desktop settings aside for a stale placeholder.
test_live_dconf_database_is_left_alone() {
    setup_case
    mkdir -p "$PACKAGE/.config/dconf" "$HOME/.config/dconf"
    printf 'placeholder\n' > "$PACKAGE/.config/dconf/user"
    printf 'live desktop settings\n' > "$HOME/.config/dconf/user"

    run_backup
    run_stow || fail 'Stow must succeed while a live dconf database exists'

    { [ -f "$HOME/.config/dconf/user" ] && [ ! -L "$HOME/.config/dconf/user" ]; } \
        || fail 'The live dconf database must stay a regular file'
    [ "$(cat "$HOME/.config/dconf/user")" = 'live desktop settings' ] \
        || fail 'The live dconf database must keep its content'
    [ ! -e "$HOME/.dotfiles-backup" ] || fail 'Nothing may be backed up for dconf'
    teardown_case
}

# Without the ignore rule Stow would link the whole directory into the repository on a fresh
# machine, and dconf would then write its database inside the checkout.
test_dconf_directory_is_never_linked_into_the_repository() {
    setup_case
    mkdir -p "$PACKAGE/.config/dconf" "$HOME/.config"
    printf 'placeholder\n' > "$PACKAGE/.config/dconf/user"
    printf 'unrelated\n' > "$PACKAGE/.config/dconf-settings.ini"

    run_stow || fail 'Stow must succeed on a machine without a dconf directory'

    { [ ! -e "$HOME/.config/dconf" ] && [ ! -L "$HOME/.config/dconf" ]; } \
        || fail 'Stow must not create ~/.config/dconf'
    [ "$HOME/.config/dconf-settings.ini" -ef "$PACKAGE/.config/dconf-settings.ini" ] \
        || fail 'The ignore rule must not swallow a sibling whose name only starts with "dconf"'
    teardown_case
}

# DaVinci Resolve keeps its Project Library, LUTs and logs next to the one preference file that is
# tracked. Where ~/.local/share/DaVinciResolve is missing, Stow links the whole folder into the
# checkout, so Resolve would then write the user's projects inside the repository.
RESOLVE_CONFIG=.local/share/DaVinciResolve/configs/config.user.xml

setup_resolve_case() {
    setup_case
    mkdir -p "$HOME/.local/share" "$PACKAGE/$(dirname "$RESOLVE_CONFIG")"
    printf '<DisplayScale>200</DisplayScale>\n' > "$PACKAGE/$RESOLVE_CONFIG"
}

test_stow_folds_the_resolve_folder_without_the_backup_step() {
    setup_resolve_case

    run_stow || fail 'Stow must succeed on a machine without a Resolve folder'

    # The control: this is the hazard the backup step removes, so if Stow ever stops folding,
    # the test below would pass for the wrong reason.
    [ -L "$HOME/.local/share/DaVinciResolve" ] \
        || fail 'Expected Stow to fold the Resolve folder when it does not exist (control)'
    teardown_case
}

test_resolve_folder_stays_real_so_its_data_stays_out_of_the_checkout() {
    setup_resolve_case

    run_backup
    run_stow || fail 'Stow must succeed once the real folders exist'

    { [ -d "$HOME/.local/share/DaVinciResolve" ] && [ ! -L "$HOME/.local/share/DaVinciResolve" ]; } \
        || fail 'The Resolve folder must be a real folder, not a link into the checkout'
    { [ -d "$HOME/.local/share/DaVinciResolve/configs" ] && [ ! -L "$HOME/.local/share/DaVinciResolve/configs" ]; } \
        || fail 'The configs folder must be a real folder'
    [ -L "$HOME/$RESOLVE_CONFIG" ] || fail 'The preference file must be a Stow link'
    [ "$HOME/$RESOLVE_CONFIG" -ef "$PACKAGE/$RESOLVE_CONFIG" ] \
        || fail 'The link must resolve to the repository file'

    # What Resolve does on its first run: it fills its folder with data of its own.
    mkdir -p "$HOME/.local/share/DaVinciResolve/Resolve Disk Database"
    printf 'a project\n' > "$HOME/.local/share/DaVinciResolve/Resolve Disk Database/project.db"
    printf 'a log\n' > "$HOME/.local/share/DaVinciResolve/configs/UI.preset"
    [ ! -e "$PACKAGE/.local/share/DaVinciResolve/Resolve Disk Database" ] \
        || fail 'Resolve data must not appear inside the checkout'
    [ ! -e "$PACKAGE/.local/share/DaVinciResolve/configs/UI.preset" ] \
        || fail 'Resolve files must not appear inside the checkout'

    # Nothing is left to do, so the audit would report no drift.
    stow -n -v -t "$HOME" -d "$CASE_DIR/cuberhaus" dotfiles > "$CASE_DIR/plan.out" 2>&1 \
        || fail "A second Stow run still reports drift: $(cat "$CASE_DIR/plan.out")"
    teardown_case
}

test_dry_run_creates_no_resolve_folder() {
    setup_resolve_case

    bash "$PACKAGE/$BACKUP_SCRIPT" --dry-run > "$CASE_DIR/backup.log" 2>&1 \
        || { cat "$CASE_DIR/backup.log" >&2; fail "$BACKUP_SCRIPT --dry-run failed"; }

    grep -Fq "Would create the real folder $HOME/.local/share/DaVinciResolve/configs" "$CASE_DIR/backup.log" \
        || fail "The preview must name the folder it would create: $(cat "$CASE_DIR/backup.log")"
    { [ ! -e "$HOME/.local/share/DaVinciResolve" ] && [ ! -L "$HOME/.local/share/DaVinciResolve" ]; } \
        || fail '--dry-run must create nothing'
    teardown_case
}

test_existing_resolve_folder_is_left_alone() {
    setup_resolve_case
    mkdir -p "$HOME/.local/share/DaVinciResolve/configs"
    printf 'my own preferences\n' > "$HOME/.local/share/DaVinciResolve/configs/config.user.xml"
    printf 'a project\n' > "$HOME/.local/share/DaVinciResolve/project.db"

    run_backup
    grep -Fq 'Created the real folder' "$CASE_DIR/backup.log" \
        && fail 'An existing folder must not be created again'
    [ "$(cat "$HOME/.local/share/DaVinciResolve/project.db")" = 'a project' ] \
        || fail 'Existing Resolve data must keep its content'

    # The live preference file is a plain conflict, backed up like any other.
    run_stow || fail 'Stow must succeed once the live preference file has been backed up'
    [ "$HOME/$RESOLVE_CONFIG" -ef "$PACKAGE/$RESOLVE_CONFIG" ] \
        || fail 'The link must resolve to the repository file'
    grep -Fq 'my own preferences' "$(find "$HOME/.dotfiles-backup" -type f -path "*/$RESOLVE_CONFIG" -print -quit)" \
        || fail 'The replaced preference file must be kept in the backup folder'
    teardown_case
}

# The Makefile passes TARGET so that a scratch target never moves anything out of the real home.
test_backup_follows_stow_target_instead_of_home() {
    setup_case
    local scratch="$CASE_DIR/scratch"
    mkdir -p "$scratch/.config/app" "$HOME/.config/app"
    printf 'in the scratch target\n' > "$scratch/.config/app/settings.json"
    printf 'in home\n' > "$HOME/.config/app/settings.json"

    STOW_TARGET="$scratch" run_backup

    [ ! -e "$scratch/.config/app/settings.json" ] || fail 'The conflict in the target must be moved'
    find "$scratch/.dotfiles-backup" -type f -path '*/.config/app/settings.json' -print -quit | grep -q . \
        || fail 'The backup must live inside the target'
    grep -Fq 'in home' "$HOME/.config/app/settings.json" || fail 'HOME must not be touched'
    [ ! -e "$HOME/.dotfiles-backup" ] || fail 'No backup may be created in HOME'
    teardown_case
}

test_make_restow_backs_up_conflicts_before_stowing() {
    local plan backup_line stow_line
    plan="$(make --no-print-directory -n -C "$REPO_ROOT" restow)"
    # "|| true": no match is the failure under test, and it must reach the message below, not set -e.
    backup_line="$(grep -n -F 'stow-backup-conflicts' <<< "$plan" | head -n 1 | cut -d: -f1 || true)"
    stow_line="$(grep -n -F 'stow -v -R' <<< "$plan" | head -n 1 | cut -d: -f1 || true)"
    [ -n "$backup_line" ] || fail 'make restow must back up conflicts first, as make install does'
    [ -n "$stow_line" ] || fail 'make restow must run stow -R'
    [ "$backup_line" -lt "$stow_line" ] || fail 'Conflicts must be backed up before Stow runs'
}

test_make_passes_its_target_to_the_backup_step() {
    local goal plan
    for goal in install restow dry-run; do
        plan="$(make --no-print-directory -n -C "$REPO_ROOT" "$goal" TARGET=/scratch/target)"
        grep -q -F 'STOW_TARGET="/scratch/target"' <<< "$plan" \
            || fail "make $goal must hand its TARGET to $BACKUP_SCRIPT"
    done
}

test_make_dry_run_previews_the_backup_and_keeps_the_exit_status() {
    local plan
    plan="$(make --no-print-directory -n -C "$REPO_ROOT" dry-run)"
    grep -q -F 'stow-backup-conflicts --dry-run' <<< "$plan" \
        || fail 'make dry-run must preview the backup'
    # The recipe must end with the literal shell text `exit $status`, which make leaves for the shell.
    # shellcheck disable=SC2016
    grep -q -F 'exit $status' <<< "$plan" \
        || fail 'make dry-run must keep the exit status of the Stow simulation'
}

test_make_restow_backs_up_conflicts_before_stowing
test_make_passes_its_target_to_the_backup_step
test_make_dry_run_previews_the_backup_and_keeps_the_exit_status

if [ "$STOW_AVAILABLE" = true ]; then
    test_absolute_link_to_the_repository_file_is_replaced_by_a_stow_link
    test_plain_file_in_the_way_is_backed_up
    test_uninstall_restores_a_backed_up_link
    test_live_dconf_database_is_left_alone
    test_dconf_directory_is_never_linked_into_the_repository
    test_stow_folds_the_resolve_folder_without_the_backup_step
    test_resolve_folder_stays_real_so_its_data_stays_out_of_the_checkout
    test_dry_run_creates_no_resolve_folder
    test_existing_resolve_folder_is_left_alone
    test_backup_follows_stow_target_instead_of_home
else
    printf 'SKIP: GNU Stow is not installed; the real-Stow scenarios were not run.\n'
fi

printf 'Stow conflict tests passed.\n'
