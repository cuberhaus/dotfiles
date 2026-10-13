#!/usr/bin/env bash
# Shared patterns for shell script linting (used by lint.sh and pre-commit).
# Single source of truth — edit here, both tools pick it up.

# Glob patterns passed to `git ls-files` to find tracked and untracked shell scripts
LINT_PATTERNS=(
    '*.sh'
    '.zshenv'
    '.config/zsh/.zshenv'
    '.bashrc'
    '.bash_profile'
    '.xinitrc'
    '.xprofile'
    '.config/zsh/aliases'
    '.config/zsh/functions'
    '.config/i3-layout-manager/layouts/*.sh'
    '.config/i3/*.sh'
    '.config/xmobar/*.sh'
    '.local/scripts/automation/*'
    '.local/scripts/bootstrap/*'
    '.local/scripts/bin/*'
    '.local/scripts/*.sh'
    '.local/scripts/apply-skip-worktree'
    '.local/scripts/hooks/pre-commit'
    '.local/scripts/obsidian-reinstall-plugins'
    '.local/scripts/repair-installation'
    '.local/scripts/restore-app-data'
    '.local/scripts/stow-backup-conflicts'
    '.local/scripts/stow-uninstall'
    '.config/i3blocks/scripts/backlight/backlight'
    '.local/scripts/applets'
    '.local/scripts/toggle_theme'
    '.local/scripts/cinnamon_path/*'
    '.local/Mini/.bashrc'
)

# Substrings to exclude (vendored / submodule paths)
LINT_EXCLUDES=(
    'BigBagKbdTrixXKB'
    'base16-shell'
    'i3blocks-contrib'
    'i3blocks_extra/battery-plus' # vendored: hastinbe/i3blocks-battery-plus (GPLv2)
    'i3blocks-spotify'
    'i3-layout-manager/i3'
    '\.terminfo$'
    'bootstrap/gentoo\.packages$' # package manifest, not a shell script
)

# Print all nonignored shell scripts that should be linted (one per line).
lint_get_scripts() {
    local grep_args=()
    for pattern in "${LINT_EXCLUDES[@]}"; do
        grep_args+=(-e "$pattern")
    done
    git ls-files --cached --others --exclude-standard -- "${LINT_PATTERNS[@]}" |
        grep -v "${grep_args[@]}"
}

# Print the files with a sh/bash/dash/ksh shebang that lint_get_scripts does not
# return (one per line), so a script outside LINT_PATTERNS cannot go unlinted
# unnoticed. Zsh scripts are left out on purpose: shellcheck cannot read them.
lint_get_unlisted_scripts() {
    local grep_args=() pattern file first
    local shell_shebang='^#![[:space:]]*([^[:space:]]*/)?(env[[:space:]]+(-S[[:space:]]+)?)?(ba|da|k)?sh([[:space:]]|$)'
    for pattern in "${LINT_EXCLUDES[@]}"; do
        grep_args+=(-e "$pattern")
    done
    comm -13 <(lint_get_scripts | LC_ALL=C sort) <(
        git grep --untracked -lIz -e '^#!' -- . |
            while IFS= read -r -d '' file; do
                IFS= read -r first < "$file" || true
                [[ $first =~ $shell_shebang ]] && printf '%s\n' "$file"
            done |
            { grep -v "${grep_args[@]}" || true; } | LC_ALL=C sort
    )
}

# Pathspecs for files allowed to contain `/home/<user>/` or `/Users/<user>/`.
# Shared by lint.yml's hardcoded-paths job and the pre-commit hook.
HARDCODED_PATHS_EXCLUDES=(
    ':!*.md'
    ':!.gitignore'
    ':!SECURITY.md'
    ':!.config/warp-terminal/user_preferences.json'
    ':!.config/birdtray-config.json'
    ':!.config/texstudio/texstudio.ini'
    ':!.config/texstudio/cache/'
    ':!.cinnamon/backgrounds/user-folders.lst'
    ':!.config/cinnamon/dark/gnome.dconf'
    ':!.config/cinnamon/light/gnome.dconf'
    ':!.config/.p10k.zsh'
    ':!.config/zsh/.p10k.zsh'
    ':!.config/vlc/vlc-qt-interface.conf'
    ':!.vim/.netrwhist'
    ':!.local/scripts/bootstrap/arch_functions'
    ':!tests/'
)
HARDCODED_PATHS_REGEX='(/home/[a-z][a-z0-9_-]*/|/Users/[a-z][a-z0-9_-]*/)'

# Vendored Markdown that markdownlint skips (basic regexes on the path): font
# documentation, texstudio data, and third-party licence and readme text.
LINT_MARKDOWN_EXCLUDES=(
    '/fonts/'
    'texstudio/'
    '^\.config/Dictionary/'
    '^\.config/i3blocks/scripts/backlight/'
)

# Print every Markdown file markdownlint should check (one per line): all tracked
# and not-ignored ones except the vendored text above. Used by check-formatting.sh
# and by the Markdown workflow, so a new document is checked without editing a list.
lint_get_markdown() {
    local grep_args=() pattern
    for pattern in "${LINT_MARKDOWN_EXCLUDES[@]}"; do
        grep_args+=(-e "$pattern")
    done
    git ls-files --cached --others --exclude-standard -- '*.md' |
        { grep -v "${grep_args[@]}" || true; }
}
