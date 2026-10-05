###############################################################
# => Configuration
###############################################################

# Enable Readline not waiting for additional input when a key is pressed.
set keyseq-timeout 50

export HISTCONTROL=ignoredups:erasedups   # no duplicate entries
export EDITOR=vim
export VISUAL=vim
export LESSHISTFILE="-"

# Enable vim mode
set -o vi

# Ctrl+L to clear screen in vi mode
bind -m vi-command 'Control-l: clear-screen'
bind -m vi-insert 'Control-l: clear-screen'

# If there are multiple matches for completion, Tab should cycle through them
bind 'TAB':menu-complete

# Display a list of the matching files
bind "set show-all-if-ambiguous on"

# Perform partial completion on the first Tab press,
# only start cycling full results on the second Tab press
bind "set menu-complete-display-prefix on"

# Complete backwards
bind '"\e[Z":menu-complete-backward'

# append to the history file, don't overwrite it
shopt -s histappend

# After each command, append to the history file and reread it
PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND$'\n'}history -a; history -c; history -r"

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=8192
HISTFILESIZE=16384

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# cd into a directory by just typing its name
shopt -s autocd

# Auto-correct minor typos in cd directory names
shopt -s cdspell

###############################################################
# => Path
###############################################################

[ -d "$HOME/.local/bin" ] && PATH="$HOME/.local/bin:$PATH"
[ -d "$HOME/.local/scripts/bin" ] && PATH="$HOME/.local/scripts/bin:$PATH"

###############################################################
# => Prompt (with git branch)
###############################################################

__git_branch() {
    local branch
    branch=$(git symbolic-ref --short HEAD 2>/dev/null || git rev-parse --short HEAD 2>/dev/null)
    [[ -n "$branch" ]] && printf ' (%s)' "$branch"
}

if [ "$(id -u)" -eq 0 ]; then
    PS1='\[\e[1;31m\]\u\[\e[0m\]@\[\e[1;33m\]\h\[\e[0m\]:\[\e[1;34m\]\w\[\e[0;35m\]$(__git_branch)\[\e[0m\]\$ '
else
    PS1='\[\e[1;32m\]\u\[\e[0m\]@\[\e[1;33m\]\h\[\e[0m\]:\[\e[1;34m\]\w\[\e[0;35m\]$(__git_branch)\[\e[0m\]\$ '
fi

###############################################################
# => Functions
###############################################################

mkcd() { mkdir -pv "$1" && cd "$1" || return; }

extract() {
    if [ ! -f "$1" ]; then
        echo "'$1' is not a valid file"
        return 1
    fi
    case "$1" in
        *.tar.bz2) tar xjf "$1"   ;;
        *.tar.gz)  tar xzf "$1"   ;;
        *.tar.xz)  tar xf "$1"    ;;
        *.tar.zst) unzstd "$1"    ;;
        *.bz2)     bunzip2 "$1"   ;;
        *.rar)     unrar x "$1"   ;;
        *.gz)      gunzip "$1"    ;;
        *.tar)     tar xf "$1"    ;;
        *.tbz2)    tar xjf "$1"   ;;
        *.tgz)     tar xzf "$1"   ;;
        *.zip)     unzip "$1"     ;;
        *.Z)       uncompress "$1";;
        *.7z)      7z x "$1"      ;;
        *.deb)     ar x "$1"      ;;
        *)         echo "'$1' cannot be extracted via extract()" ;;
    esac
}

# Normalize `open` across Linux, macOS, and Windows
if [ ! "$(uname -s)" = 'Darwin' ]; then
    if grep -q Microsoft /proc/version 2>/dev/null; then
        alias open='explorer.exe'
    else
        alias open='xdg-open'
    fi
fi

# `o` with no arguments opens the current directory, otherwise opens the given location
o() {
    if [ $# -eq 0 ]; then
        open . </dev/null &>/dev/null &
    else
        open "$@" </dev/null &>/dev/null &
    fi
}

# Pull current repo or recursively pull child repos (parallel)
pull() {
    if [ -d .git ]; then
        git pull "$@"
    else
        local tmpdir pids=() repos=() failures=0
        printf "\033[34mdepth: 2 \033[0m\n"
        tmpdir=$(mktemp -d)
        set +m  # disable job control notifications
        
        while IFS= read -r -d $'\0' dot_git; do
            local dir
            dir=$(dirname "$dot_git")
            repos+=("$dir")
            printf "\033[34mDownloading %s...\033[0m\n" "$dir"
            (
                git -C "$dir" pull > "$tmpdir/$(echo "$dir" | tr '/' '_').out" 2>&1
            ) &
            pids+=($!)
        done < <(find . -maxdepth 2 -type d -name .git -print0 2>/dev/null)
        
        local updated_repos=()
        local updated_summaries=()
        for i in "${!pids[@]}"; do
            local pid="${pids[$i]}" repo="${repos[$i]}"
            local outfile
            outfile="$tmpdir/$(echo "$repo" | tr '/' '_').out"
            if wait "$pid"; then
                printf "\033[32m✓ %s\033[0m\n" "$repo"
                local summary
                summary=$(grep -E '^[[:space:]]*[0-9]+ files? changed' "$outfile" 2>/dev/null | tail -n 1 | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')
                if [ -n "$summary" ]; then
                    updated_repos+=("$repo")
                    updated_summaries+=("$summary")
                fi
            else
                printf "\033[31m✗ %s\033[0m\n" "$repo"
                ((failures++)) || true
            fi
            cat "$outfile" 2>/dev/null
        done
        set -m  # re-enable job control
        rm -rf "$tmpdir"
        if [ "${#updated_repos[@]}" -gt 0 ]; then
            printf "\n\033[32mUpdated repositories (%d):\033[0m\n" "${#updated_repos[@]}"
            for (( j=0; j < ${#updated_repos[@]}; j++ )); do
                printf "  \033[32m%s\033[0m: %s\n" "${updated_repos[j]}" "${updated_summaries[j]}"
            done
        fi
        if ((failures > 0)); then
            printf "\n\033[31m%d repo(s) failed\033[0m\n" "$failures"
            return 1
        fi
    fi
}

# Recursively add a GitHub PAT token to all repositories
add_pat() {
    local pat="$1"
    if [ -z "$pat" ]; then
        echo "Usage: add_pat <token>"
        echo "Recursively changes all https://github.com/... remotes to use the defined PAT."
        return 1
    fi
    printf "\033[34mdepth: 2 \033[0m\n"
    while IFS= read -r -d $'\0' dot_git; do
        local dir
        dir=$(dirname "$dot_git")
        local remote_url
        remote_url=$(git -C "$dir" remote get-url origin 2>/dev/null)
        if [ -n "$remote_url" ]; then
            if [[ "$remote_url" == *"github.com"* && "$remote_url" == https://* ]]; then
                # Remove any existing credentials and insert new formatting
                local new_url
                new_url=$(echo "$remote_url" | sed -E "s|https://([^@]+@)?github\\.com|https://$pat@github.com|")
                if [ "$remote_url" != "$new_url" ]; then
                    git -C "$dir" remote set-url origin "$new_url"
                    printf "\033[32m✓ %s\033[0m (remote updated)\n" "$dir"
                else
                    printf "\033[34m- %s\033[0m (already using this token)\n" "$dir"
                fi
            else
                printf "\033[33m! %s\033[0m (ignored: not an HTTPS GitHub remote)\n" "$dir"
            fi
        fi
    done < <(find . -maxdepth 2 -type d -name .git -print0 2>/dev/null)
}

###############################################################
# => Cloning GitHub repositories (clone-all, clone-team)
###############################################################

# Self-contained versions of the clone-all and clone-team scripts in .local/scripts/bin, and of
# the helpers they share (.local/scripts/lib/git-repo-defaults.sh), for a machine that has only
# this file. Both need the GitHub CLI (gh) signed in; clone-team also needs the read:org scope.
# They stay above the aliases on purpose: Bash builds the aliases that exist when a function is
# defined into its body, so a later alias for grep, rm, ... cannot change what runs here.

# Print where a repository lives: an existing checkout wins, flat (./cv) or nested
# (./cuberhaus/cv); with none, the flat name.
_clone_repo_dir() {
    local nwo="$1" name="${1##*/}"
    if [ -d "$nwo/.git" ]; then
        printf '%s\n' "$nwo"
    elif [ -d "$name/.git" ]; then
        printf '%s\n' "$name"
    elif [ -e "$nwo" ]; then
        printf '%s\n' "$nwo"
    else
        printf '%s\n' "$name"
    fi
}

# Prepare a checkout after a clone or pull: git hooks, lefthook, skip-worktree masks and the
# commit identity. SCOPE is "personal" (clone-all: only cuberhaus/* gets an identity) or "team"
# (clone-team: cuberhaus/* is personal, every other organisation gets the work identity from
# CLONE_TEAM_WORK_GIT_NAME and CLONE_TEAM_WORK_GIT_EMAIL). CLONE_GIT_IDENTITY_<ORG>_NAME and
# _EMAIL (ORG upper-cased) override the identity of any organisation.
_clone_configure_repo() {
    local nwo="$1" dir="$2" scope="${3:-personal}"
    local org="${nwo%%/*}" git_name="" git_email="" org_key var_name var_email

    [ -d "$dir/.git" ] || return 0

    if [ -d "$dir/.githooks" ]; then
        git -C "$dir" config core.hooksPath .githooks
    elif [ "$(git -C "$dir" config --get core.hooksPath 2>/dev/null)" = ".githooks" ]; then
        git -C "$dir" config --unset core.hooksPath 2>/dev/null || true
    fi

    if [ -f "$dir/lefthook.yml" ] || [ -f "$dir/.lefthook.yml" ]; then
        (
            cd "$dir" || exit 0
            if [ -x ./node_modules/.bin/lefthook ]; then
                ./node_modules/.bin/lefthook install
            elif command -v lefthook >/dev/null 2>&1; then
                lefthook install
            elif command -v npx >/dev/null 2>&1; then
                npx --yes lefthook install
            fi
        ) >/dev/null 2>&1 || true
    fi

    if [ -x "$dir/.local/scripts/apply-skip-worktree" ]; then
        "$dir/.local/scripts/apply-skip-worktree" "$dir"
    fi

    case "$scope" in
        team)
            if [ "$org" = "cuberhaus" ]; then
                git_name="cuberhaus"
                git_email="polcg10@gmail.com"
            else
                git_name="${CLONE_TEAM_WORK_GIT_NAME:-Pol Casacuberta Gil}"
                git_email="${CLONE_TEAM_WORK_GIT_EMAIL:-pcasacubertagil@deloitte.es}"
            fi
            ;;
        *)
            if [[ "$nwo" == cuberhaus/* ]]; then
                git_name="cuberhaus"
                git_email="polcg10@gmail.com"
            fi
            ;;
    esac

    if [ -n "$org" ]; then
        org_key=$(printf '%s' "$org" | tr '[:lower:]' '[:upper:]' | tr -c 'A-Z0-9' '_')
        var_name="CLONE_GIT_IDENTITY_${org_key}_NAME"
        var_email="CLONE_GIT_IDENTITY_${org_key}_EMAIL"
        if [ -n "${!var_name:-}" ]; then git_name="${!var_name}"; fi
        if [ -n "${!var_email:-}" ]; then git_email="${!var_email}"; fi
    fi

    if [ -n "$git_name" ]; then git -C "$dir" config user.name "$git_name"; fi
    if [ -n "$git_email" ]; then git -C "$dir" config user.email "$git_email"; fi
    return 0
}

# Clone every repository of a GitHub owner, or fast-forward the ones already present, with one
# status line per repository (in parallel). Run it from the directory that holds the checkouts.
# The body is a subshell, so its variables, helper functions and options never reach the shell.
clone-all() (
    case ${1:-} in
        -h | --help)
            cat <<'EOF'
Usage: clone-all [OWNER]

Clone every repository of a GitHub owner, or fast-forward the ones already present,
with one status line per repository. Run it from the directory that should hold the
checkouts (for example ~/cuberhaus). A repository may live flat (./cv) or nested
(./cuberhaus/cv), and an existing .git wins.

OWNER defaults to $CLONE_ALL_OWNER, then to the account gh is signed in as.

Environment:
  CLONE_ALL_OWNER    owner to use when no argument is given
  CLONE_ALL_JOBS     parallel jobs (default: CPU count, at least 4, at most 32)
  CLONE_ALL_RETRIES  attempts per repository (default: 3)
EOF
            exit 0
            ;;
        -*)
            printf 'clone-all: unknown option: %s (try clone-all --help)\n' "$1" >&2
            exit 2
            ;;
    esac

    if ! command -v gh >/dev/null 2>&1; then
        echo "error: gh (GitHub CLI) is not installed" >&2
        exit 1
    fi
    set -o pipefail

    # Cloning and pulling wait on the network and the disk, not on the CPU, so far more jobs than
    # cores is fine: the detected cores, at least 4 and at most 32.
    max_jobs=${CLONE_ALL_JOBS:-}
    if [ -z "$max_jobs" ]; then
        max_jobs=$(nproc 2>/dev/null) ||
            max_jobs=$(getconf _NPROCESSORS_ONLN 2>/dev/null) ||
            max_jobs=$(sysctl -n hw.ncpu 2>/dev/null) ||
            max_jobs=4
        if [ "$max_jobs" -lt 4 ]; then max_jobs=4; fi
        if [ "$max_jobs" -gt 32 ]; then max_jobs=32; fi
    fi
    clone_retries=${CLONE_ALL_RETRIES:-3}

    # Owner to clone: the argument, then CLONE_ALL_OWNER, then the account gh is signed in as.
    owner=${1:-${CLONE_ALL_OWNER:-}}
    if [ -z "$owner" ]; then
        owner=$(gh api user --jq '.login' 2>/dev/null)
        if [ -z "$owner" ]; then
            echo "error: could not determine the active gh user (run 'gh auth login' or pass an owner)" >&2
            exit 1
        fi
    fi

    if [ -t 1 ]; then
        C_RESET=$'\033[0m' C_GREEN=$'\033[32m' C_BLUE=$'\033[34m'
        C_YELLOW=$'\033[33m' C_RED=$'\033[31m' C_DIM=$'\033[2m'
    else
        C_RESET='' C_GREEN='' C_BLUE='' C_YELLOW='' C_RED='' C_DIM=''
    fi

    # One repository: clone it, or fast-forward it when it is already there. It runs in its own
    # Bash (xargs below), so it and the helpers it calls are exported.
    process_repo() {
        local nwo="$1" dir attempt output status label before after
        dir=$(_clone_repo_dir "$nwo")

        if [ -d "$dir/.git" ]; then
            # A quiet pull prints nothing even when it moves HEAD, so compare HEAD instead.
            before=$(git -C "$dir" rev-parse HEAD 2>/dev/null)
            if output=$(git -C "$dir" pull --ff-only --quiet --jobs=4 2>&1); then
                after=$(git -C "$dir" rev-parse HEAD 2>/dev/null)
                if [ "$before" = "$after" ]; then
                    label="${C_DIM}up-to-date${C_RESET}"
                else
                    label="${C_GREEN}updated${C_RESET}"
                fi
                status=0
            else
                label="${C_RED}pull failed${C_RESET}"
                status=1
            fi
        elif [ -e "$dir" ]; then
            label="${C_YELLOW}skipped (not a git repo)${C_RESET}"
            output=""
            status=0
        else
            attempt=1
            status=1
            while [ "$attempt" -le "$clone_retries" ]; do
                if output=$(gh repo clone "$nwo" "$dir" -- --quiet 2>&1); then
                    status=0
                    break
                fi
                if [ "$attempt" -lt "$clone_retries" ]; then
                    printf 'retrying clone (%s/%s) %s\n' "$attempt" "$clone_retries" "$nwo" >&2
                fi
                attempt=$((attempt + 1))
            done

            if [ "$status" -eq 0 ]; then
                label="${C_BLUE}cloned${C_RESET}"
            else
                label="${C_RED}clone failed${C_RESET}"
            fi
        fi

        if [ "$status" -eq 0 ]; then
            _clone_configure_repo "$nwo" "$dir" personal
        fi

        printf '%-22b %s\n' "$label" "$nwo"
        if [ -n "$output" ] && [ "$status" -ne 0 ]; then
            printf '%s\n' "$output" | sed 's/^/    /'
        fi
        return "$status"
    }
    export -f _clone_repo_dir _clone_configure_repo process_repo
    export C_RESET C_GREEN C_BLUE C_YELLOW C_RED C_DIM clone_retries

    gh repo list "$owner" --limit 1000 --json nameWithOwner -q '.[].nameWithOwner' |
        xargs -P "$max_jobs" -I {} bash -c 'process_repo "$@"' _ {}
)

# Clone every repository that a GitHub team you belong to can access, or fast-forward the ones
# already present. With no team it lists yours and asks for a number.
clone-team() (
    team="" dest="."
    while [ $# -gt 0 ]; do
        case "$1" in
            -h | --help)
                cat <<'EOF'
Usage: clone-team [ORG/TEAM [DEST]]

Clone every repository that a GitHub team you belong to can access, or fast-forward the ones
already present. Without a team it lists yours and asks for a number.

  clone-team                       pick a team interactively, clone into .
  clone-team org/team-slug         clone that team's repos into .
  clone-team org/team-slug ~/src   clone into ~/src

Needs the GitHub CLI (gh) signed in with the read:org scope. After each clone or pull it sets
core.hooksPath when the repository has a .githooks folder, and a repository-local commit identity:
  cuberhaus/*  ->  cuberhaus <polcg10@gmail.com>
  other orgs   ->  $CLONE_TEAM_WORK_GIT_NAME <$CLONE_TEAM_WORK_GIT_EMAIL>
                   (default: Pol Casacuberta Gil <pcasacubertagil@deloitte.es>)
CLONE_GIT_IDENTITY_<ORG>_NAME and _EMAIL (ORG upper-cased) override either one.
EOF
                exit 0
                ;;
            -*)
                printf 'clone-team: unknown option: %s (try clone-team --help)\n' "$1" >&2
                exit 2
                ;;
            *)
                if [ -z "$team" ]; then team="$1"; else dest="$1"; fi
                shift
                ;;
        esac
    done

    if ! command -v gh >/dev/null 2>&1; then
        echo "error: gh (GitHub CLI) is not installed. See https://cli.github.com/" >&2
        exit 1
    fi
    set -uo pipefail

    if [ -t 1 ]; then
        green=$'\033[32m' yellow=$'\033[33m' red=$'\033[31m'
        cyan=$'\033[36m' dim=$'\033[2m' reset=$'\033[0m'
    else
        green='' yellow='' red='' cyan='' dim='' reset=''
    fi

    org="" slug=""
    if [ -n "$team" ]; then
        if [[ "$team" != */* || "$team" == */*/* ]]; then
            echo "${red}error:${reset} team must be in 'org/team-slug' form, e.g. myorg/backend." >&2
            exit 1
        fi
        org="${team%%/*}"
        slug="${team#*/}"
    else
        echo "${cyan}Fetching your GitHub teams...${reset}"
        teams=()
        while IFS= read -r line; do
            teams+=("$line")
        done < <(gh api --paginate /user/teams \
            --jq '.[] | "\(.organization.login)/\(.slug)"' 2>/dev/null)
        if [ "${#teams[@]}" -eq 0 ]; then
            echo "${yellow}You are not a member of any GitHub teams (or gh is not authed with read:org).${reset}" >&2
            exit 0
        fi
        echo
        for i in "${!teams[@]}"; do
            printf '  [%d] %s\n' "$((i + 1))" "${teams[$i]}"
        done
        echo
        read -rp "Select a team by number (or 'q' to quit): " choice
        case "$choice" in
            q | Q | "") exit 0 ;;
            *[!0-9]*)
                echo "${red}error:${reset} invalid selection." >&2
                exit 1
                ;;
        esac
        idx=$((10#$choice - 1))
        if [ "$idx" -lt 0 ] || [ "$idx" -ge "${#teams[@]}" ]; then
            echo "${red}error:${reset} invalid selection." >&2
            exit 1
        fi
        org="${teams[$idx]%%/*}"
        slug="${teams[$idx]#*/}"
    fi

    echo "${cyan}Fetching repositories for $org/$slug...${reset}"
    repos=()
    while IFS= read -r line; do
        repos+=("$line")
    done < <(gh api --paginate "/orgs/$org/teams/$slug/repos" --jq '.[].full_name' 2>/dev/null)
    if [ "${#repos[@]}" -eq 0 ]; then
        echo "${yellow}No repositories found for $org/$slug (or you lack access).${reset}" >&2
        exit 0
    fi

    mkdir -p "$dest" || {
        echo "${red}error:${reset} could not create $dest" >&2
        exit 1
    }
    target=$(cd "$dest" && pwd) || exit 1

    echo "${cyan}Syncing ${#repos[@]} repo(s) into $target${reset}"
    cd "$target" || exit 1
    for full in "${repos[@]}"; do
        dir=$(_clone_repo_dir "$full")

        if [ -d "$dir/.git" ]; then
            # A quiet pull prints nothing even when it moves HEAD, so compare HEAD instead.
            before=$(git -C "$dir" rev-parse HEAD 2>/dev/null)
            if output=$(git -C "$dir" pull --ff-only --quiet 2>&1); then
                after=$(git -C "$dir" rev-parse HEAD 2>/dev/null)
                if [ "$before" = "$after" ]; then
                    printf '  %s%s%s   %s\n' "$dim" "up-to-date" "$reset" "$full"
                else
                    printf '  %s%s%s   %s\n' "$green" "updated" "$reset" "$full"
                fi
            else
                printf '  %s%s%s   %s\n' "$red" "pull failed" "$reset" "$full"
                if [ -n "$output" ]; then
                    printf '%s\n' "$output" | sed 's/^/    /'
                fi
            fi
            _clone_configure_repo "$full" "$dir" team
            continue
        fi

        if [ -e "$dir" ]; then
            printf '  %s%s%s   %s (exists, not a git repo)\n' "$yellow" "skip" "$reset" "$full"
            continue
        fi

        printf '  %s%s%s  %s\n' "$green" "clone" "$reset" "$full"
        if gh repo clone "$full" "$dir" -- --quiet; then
            _clone_configure_repo "$full" "$dir" team
        else
            printf '  %s%s%s   %s\n' "$red" "clone failed" "$reset" "$full" >&2
        fi
    done
)

###############################################################
# => Git across repositories (git-recurse, git-ahead)
###############################################################

# Self-contained versions of the git-recurse and git-ahead scripts in .local/scripts/bin, for a
# machine that has only this file: gr, gah and status below call them. Same options, environment
# variables, output and exit codes as the scripts, except that git-recurse has no -k (it adds the
# ssh key of one Mac). They stay above the aliases like the clone commands do, and put `command`
# in front of grep, mv and rm, which the aliases below change (-i, -v, -I) and which Bash would
# otherwise build into a function that is defined again when this file is sourced a second time.
# Bash 3.2 (macOS) is enough. tests/test_mini_bashrc.sh runs tests/test_git_recurse.sh against
# git-recurse here and compares git-ahead with its script, so the copies cannot drift unnoticed.

# Report, in every Git repository below a directory, the remote branches that have commits
# ahead of origin's main (or master). The body is a subshell, so nothing leaks into the shell.
git-ahead() (
    set -uo pipefail
    depth=4 fetch=1 verbose=0 root="."

    while [ $# -gt 0 ]; do
        case "$1" in
            -n | --no-fetch)
                fetch=0
                shift
                ;;
            --depth)
                depth="$2"
                shift 2
                ;;
            -v | --verbose)
                verbose=1
                shift
                ;;
            -h | --help)
                cat <<'EOF'
git-ahead: scan all git repos under a directory, fetch all remotes, and
report any remote branch that has commits ahead of remote main/master.

Usage:
  git-ahead [dir]            # scan dir (default: .), fetching all remotes
  git-ahead -n|--no-fetch    # skip fetch (use cached refs)
  git-ahead --depth N        # max search depth for repos (default: 4)

Output per repo: only branches that are ahead of the base. Silent for clean
repos unless --verbose. Exit code is non-zero only on fatal error.
EOF
                exit 0
                ;;
            -*)
                echo "Unknown option: $1" >&2
                exit 2
                ;;
            *)
                root="$1"
                shift
                ;;
        esac
    done

    if [ ! -d "$root" ]; then
        echo "Not a directory: $root" >&2
        exit 1
    fi

    green=$'\033[32m' yellow=$'\033[33m' red=$'\033[31m'
    dim=$'\033[2m' bold=$'\033[1m' reset=$'\033[0m'

    # The ref to compare branches against: origin/HEAD, then origin/main, then origin/master.
    detect_base() {
        local base
        base=$(git symbolic-ref -q refs/remotes/origin/HEAD 2>/dev/null) || true
        if [ -n "$base" ]; then
            echo "${base#refs/remotes/}"
            return
        fi
        if git rev-parse --verify --quiet origin/main >/dev/null; then
            echo "origin/main"
            return
        fi
        if git rev-parse --verify --quiet origin/master >/dev/null; then
            echo "origin/master"
            return
        fi
        echo ""
    }

    scan_repo() {
        local repo="$1" rel base report="" ref ahead
        rel="${repo#"$root"/}"

        if [ "$fetch" -eq 1 ]; then
            if ! git -C "$repo" fetch --all --prune --quiet 2>/dev/null; then
                printf "%s✗%s %s %s(fetch failed)%s\n" "$red" "$reset" "$rel" "$dim" "$reset"
                return
            fi
        fi

        base=$(cd "$repo" && detect_base)
        if [ -z "$base" ]; then
            if [ "$verbose" -eq 1 ]; then
                printf "%s•%s %s %s(no origin/main or origin/master)%s\n" \
                    "$dim" "$reset" "$rel" "$dim" "$reset"
            fi
            return
        fi

        # Every remote branch except the base itself, with the commits it has ahead of it.
        while IFS= read -r ref; do
            [ "$ref" = "$base" ] && continue
            # Skip remote HEAD pointers like origin/HEAD
            case "$ref" in */HEAD) continue ;; esac
            ahead=$(git -C "$repo" rev-list --count "$base..$ref" 2>/dev/null) || continue
            if [ "$ahead" -gt 0 ]; then
                report+=$(printf "    %s↑ %3d%s  %s\n" "$yellow" "$ahead" "$reset" "$ref")
                report+=$'\n'
            fi
        done < <(git -C "$repo" for-each-ref --format='%(refname:short)' refs/remotes/)

        if [ -n "$report" ]; then
            printf "%s%s%s %s(base: %s)%s\n" "$bold" "$rel" "$reset" "$dim" "$base" "$reset"
            printf "%s" "$report"
        elif [ "$verbose" -eq 1 ]; then
            printf "%s✓%s %s %s(base: %s, all remotes merged)%s\n" \
                "$green" "$reset" "$rel" "$dim" "$base" "$reset"
        fi
    }

    # Every Git repository under root, down to the depth (find does not enter a .git it found).
    repos=()
    while IFS= read -r line; do
        repos+=("$line")
    done < <(find "$root" -maxdepth "$depth" -type d -name .git -prune 2>/dev/null |
        sed 's|/\.git$||' | sort)

    if [ ${#repos[@]} -eq 0 ]; then
        echo "No git repos found under $root (depth $depth)."
        exit 0
    fi

    [ "$verbose" -eq 1 ] && echo "Scanning ${#repos[@]} repos..."

    for r in "${repos[@]}"; do
        scan_repo "$r"
    done
)

# Run a command in every Git repository below the current directory, in parallel by default,
# retrying network failures, with a time limit per repository. After a pull it lists the
# repositories that changed; after `git status`, the ones with changes. The body is a subshell.
git-recurse() (
    DEFAULT_DEPTH=3 DEFAULT_JOBS=0 DEFAULT_RETRIES=2 DEFAULT_TIMEOUT=12

    timeout_bin=""
    if command -v timeout >/dev/null 2>&1; then
        timeout_bin="timeout"
    elif command -v gtimeout >/dev/null 2>&1; then
        timeout_bin="gtimeout"
    fi

    usage() {
        cat <<EOF
git-recurse - Run a git command in all repositories under the current directory

Usage: git-recurse [options] <command> [args...]

The command and its args are passed as positional arguments. Examples:
  git-recurse git status
  git-recurse git pull
  git-recurse -j 6 git fetch
  git-recurse -s git status

Options:
  -h, --help      Show this help
  -p              Run in parallel (default)
  -s              Run sequentially
  -j <jobs>       Max concurrent jobs in parallel mode (0 = all at once, default: $DEFAULT_JOBS, env: GIT_RECURSE_JOBS)
  -r <retries>    Max retries on transient network/TLS failures (default: $DEFAULT_RETRIES, env: GIT_RECURSE_RETRIES)
  -t <seconds>    Timeout per repository in seconds (0 = disabled, default: $DEFAULT_TIMEOUT, env: GIT_RECURSE_TIMEOUT)
  -d <depth>      Max depth to search for .git dirs (default: $DEFAULT_DEPTH)
  -S              Show a summary: the repositories a pull updated, or the ones with changes
                  for git status (automatic for both; env: GIT_RECURSE_SUMMARY=0 turns it off,
                  =1 turns it on for any command)
EOF
    }

    # Whether a failure is worth another attempt: the network or the server, not the command.
    is_transient_error() {
        if [ "$2" -eq 124 ] || [ "$2" -eq 137 ]; then
            return 0
        fi
        command grep -qiE "gnutls|handshake failed|connection.*terminated|connection.*reset|connection.*refused|could not resolve|timed out|operation timed out|unable to access|index\.lock|\.lock|the remote end hung up unexpectedly|rpc failed|502 bad gateway|503 service|504 gateway|429 too many" "$1" 2>/dev/null
    }

    # Describe what differs in a repository from its `git status --porcelain --branch`, for
    # example "2 modified, 1 untracked, ahead 1". A path both staged and modified counts in each.
    # Print nothing and return 1 when the working tree is clean and the branch is neither ahead
    # of nor behind its upstream, and 2 when the file cannot be read: a missing probe must not
    # look like a clean repository. Plain shell on purpose: every repository runs at once, so a
    # program started per repository lengthens the whole run.
    describe_status() {
        local probe_file="$1" line x y part description=""
        local conflicted=0 staged=0 modified=0 untracked=0 ahead=0 behind=0
        local ahead_re='ahead ([0-9]+)' behind_re='behind ([0-9]+)'
        local parts=()

        [ -r "$probe_file" ] || return 2

        while IFS= read -r line || [ -n "$line" ]; do
            case "$line" in
                '## '*)
                    [[ "$line" =~ $ahead_re ]] && ahead="${BASH_REMATCH[1]}"
                    [[ "$line" =~ $behind_re ]] && behind="${BASH_REMATCH[1]}"
                    ;;
                '?? '*)
                    untracked=$((untracked + 1))
                    ;;
                '!! '*) ;;
                ???*)
                    x="${line:0:1}"
                    y="${line:1:1}"
                    if [ "$x" = U ] || [ "$y" = U ] || [ "$x$y" = AA ] || [ "$x$y" = DD ]; then
                        conflicted=$((conflicted + 1))
                    else
                        [ "$x" != " " ] && staged=$((staged + 1))
                        [ "$y" != " " ] && modified=$((modified + 1))
                    fi
                    ;;
            esac
        done <"$probe_file"

        [ "$conflicted" -gt 0 ] && parts+=("$conflicted conflicted")
        [ "$staged" -gt 0 ] && parts+=("$staged staged")
        [ "$modified" -gt 0 ] && parts+=("$modified modified")
        [ "$untracked" -gt 0 ] && parts+=("$untracked untracked")
        [ "$ahead" -gt 0 ] && parts+=("ahead $ahead")
        [ "$behind" -gt 0 ] && parts+=("behind $behind")

        [ ${#parts[@]} -gt 0 ] || return 1
        for part in "${parts[@]}"; do
            description="$description${description:+, }$part"
        done
        printf '%s\n' "$description"
    }

    # What a successful pull changed, as one line (the diffstat of the pull, else of HEAD before
    # and after, else git's own "Updating"/"Fast-forward" line); return 1 when nothing changed.
    pull_summary() {
        local dir="$1" out="$2" before="$3" after="$4" summary shortstat update_line
        summary=$(command grep -E '^[[:space:]]*[0-9]+ files? changed' "$out" 2>/dev/null | tail -n 1 |
            sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')
        if [ -n "$summary" ]; then
            printf '%s\n' "$summary"
            return 0
        fi

        if [ -n "$before" ] && [ -n "$after" ] && [ "$before" != "$after" ]; then
            shortstat=$(git -C "$dir" diff --shortstat "$before" "$after" 2>/dev/null |
                sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')
            if [ -n "$shortstat" ]; then
                printf '%s\n' "$shortstat"
            else
                printf 'updated (%s..%s)\n' "${before:0:7}" "${after:0:7}"
            fi
            return 0
        fi

        if command grep -qiE 'already up[- ]to[- ]date|current branch .* is up to date' "$out" 2>/dev/null; then
            return 1
        fi

        update_line=$(command grep -E 'Updating [0-9a-f]+\.\.[0-9a-f]+|Fast-forward|Successfully rebased and updated|Merge made by' "$out" 2>/dev/null |
            head -n 1 | sed -e 's/^[[:space:]]*//' -e 's/[[:space:]]*$//')
        if [ -n "$update_line" ]; then
            printf '%s\n' "$update_line"
            return 0
        fi
        return 1
    }

    # The status summary of one repository, written to "$out.updated" when it differs. The text
    # of the user's own command cannot be summarised (it is localized and may be --short), while
    # the porcelain format is stable, so this asks git again. GIT_OPTIONAL_LOCKS=0 keeps the
    # read-only probe from taking the index lock another git process may hold. Returns non-zero
    # when the state cannot be read: describe_status answers 1 for "clean", so any other failure
    # is real and must not be taken for a clean repository.
    summarize_status() {
        local dir="$1" out="$2" description rc=0
        if [ -n "$timeout_bin" ] && [ "$timeout_sec" -gt 0 ]; then
            GIT_OPTIONAL_LOCKS=0 "$timeout_bin" -k 3s "${timeout_sec}s" \
                git -C "$dir" status --porcelain --branch >"$out.porcelain" 2>/dev/null || rc=$?
        else
            GIT_OPTIONAL_LOCKS=0 git -C "$dir" status --porcelain --branch >"$out.porcelain" 2>/dev/null || rc=$?
        fi
        [ "$rc" -eq 0 ] || return "$rc"
        description=$(describe_status "$out.porcelain") || rc=$?
        case "$rc" in
            0) printf '%s\n' "$description" >"$out.updated" ;;
            1) rc=0 ;;
        esac
        return "$rc"
    }

    # Run the command in one repository, retrying transient failures, and leave its output in
    # "$out" (plus "$out.updated" when there is something for the summary). Returns its status.
    run_repo() {
        local dir="$1" out="$2" try="$2.attempt" attempt=0 status=0 before="" after="" summary
        if [ "$summary_mode" = pull ]; then
            before=$(git -C "$dir" rev-parse HEAD 2>/dev/null || true)
        fi

        while true; do
            attempt=$((attempt + 1))
            (
                cd "$dir" 2>/dev/null || exit 1
                if [ -n "$timeout_bin" ] && [ "$timeout_sec" -gt 0 ]; then
                    "$timeout_bin" -k 3s "${timeout_sec}s" bash -c "$cmd"
                else
                    bash -c "$cmd"
                fi
            ) >"$try" 2>&1
            status=$?

            if [ "$status" -eq 0 ]; then
                if [ "$attempt" -gt 1 ]; then
                    printf '[retry %d/%d succeeded]\n' "$((attempt - 1))" "$max_retries" >"$out"
                    cat "$try" >>"$out"
                else
                    command mv "$try" "$out"
                fi
                command rm -f "$try"

                if [ "$summary_mode" = status ]; then
                    # Without its state this repository would be left out of the summary and
                    # reported as clean, so a failure here fails the repository.
                    summarize_status "$dir" "$out" || {
                        status=$?
                        printf 'git-recurse: could not read the repository state for the summary (exit %d)\n' "$status" >>"$out"
                        return "$status"
                    }
                elif [ "$summary_mode" = pull ]; then
                    after=$(git -C "$dir" rev-parse HEAD 2>/dev/null || true)
                    if summary=$(pull_summary "$dir" "$out" "$before" "$after"); then
                        printf '%s\n' "$summary" >"$out.updated"
                    fi
                fi
                return 0
            fi

            if [ "$status" -eq 124 ] || [ "$status" -eq 137 ]; then
                printf 'Operation timed out after %ds (stalled connection killed)\n' "$timeout_sec" >>"$try"
            fi

            if [ "$attempt" -gt "$max_retries" ] || ! is_transient_error "$try" "$status"; then
                if [ "$attempt" -gt 1 ]; then
                    printf '[failed after %d retries]\n' "$((attempt - 1))" >"$out"
                    cat "$try" >>"$out"
                else
                    command mv "$try" "$out"
                fi
                command rm -f "$try"
                return "$status"
            fi

            sleep "$attempt"
            command rm -f "$try"
        done
    }

    parallel=true summary_flag=false
    depth=$DEFAULT_DEPTH
    max_jobs="${GIT_RECURSE_JOBS:-$DEFAULT_JOBS}"
    max_retries="${GIT_RECURSE_RETRIES:-$DEFAULT_RETRIES}"
    timeout_sec="${GIT_RECURSE_TIMEOUT:-$DEFAULT_TIMEOUT}"

    # getopts reads only one-letter options, so it would reject --help as an illegal option.
    if [ "${1:-}" = "--help" ]; then
        usage
        exit 0
    fi

    # A leading colon makes getopts silent, so the messages below name this command.
    OPTIND=1
    while getopts ":hpsj:r:d:t:S" option; do
        case "$option" in
            h)
                usage
                exit 0
                ;;
            p) parallel=true ;;
            s) parallel=false ;;
            S) summary_flag=true ;;
            j) max_jobs="$OPTARG" ;;
            r) max_retries="$OPTARG" ;;
            d) depth="$OPTARG" ;;
            t) timeout_sec="$OPTARG" ;;
            :)
                printf 'git-recurse: option requires an argument -- %s\n' "$OPTARG" >&2
                usage
                exit 1
                ;;
            *)
                printf 'git-recurse: illegal option -- %s\n' "$OPTARG" >&2
                usage
                exit 1
                ;;
        esac
    done
    shift $((OPTIND - 1))
    cmd="$*"

    if [ -z "${cmd// }" ]; then
        echo "Error: missing command. Pass a command after the options, e.g. git status"
        echo ""
        usage
        exit 1
    fi

    if ! [[ "$max_jobs" =~ ^[0-9]+$ ]]; then
        echo "Error: -j requires a non-negative integer (0 for unlimited)."
        exit 1
    fi
    if ! [[ "$max_retries" =~ ^[0-9]+$ ]]; then
        echo "Error: -r requires a non-negative integer."
        exit 1
    fi
    if ! [[ "$timeout_sec" =~ ^[0-9]+$ ]]; then
        echo "Error: -t requires a non-negative integer (0 for disabled)."
        exit 1
    fi
    if ! [[ "$depth" =~ ^[1-9][0-9]*$ ]]; then
        echo "Error: -d requires a positive integer."
        exit 1
    fi

    # A git status command gets the "repositories with changes" summary; any other command gets
    # the "updated repositories" one, which only a pull has data for.
    status_re='^[[:space:]]*git[[:space:]]+status([[:space:]]|$)'
    pull_re='(^|[[:space:]])pull([[:space:]]|$)'
    is_status_cmd=false
    if [[ "$cmd" =~ $status_re ]]; then
        is_status_cmd=true
    fi
    want_summary=false
    if [ "$summary_flag" = true ]; then
        want_summary=true
    elif [ "${GIT_RECURSE_SUMMARY:-auto}" != "0" ]; then
        if [ "$is_status_cmd" = true ] || [[ "$cmd" =~ $pull_re ]] || [ "${GIT_RECURSE_SUMMARY:-auto}" = "1" ]; then
            want_summary=true
        fi
    fi
    summary_mode=none
    if [ "$want_summary" = true ]; then
        if [ "$is_status_cmd" = true ]; then
            summary_mode=status
        else
            summary_mode=pull
        fi
    fi

    # Whatever colours tput knows for this terminal; none for a dumb one or without tput.
    blue=$(tput setaf 4 2>/dev/null)
    green=$(tput setaf 2 2>/dev/null)
    yellow=$(tput setaf 3 2>/dev/null)
    red=$(tput setaf 1 2>/dev/null)
    normal=$(tput sgr0 2>/dev/null)
    dim=$(tput dim 2>/dev/null)

    echo "${blue}depth: $depth ${normal}"
    if [ "$depth" -eq 1 ]; then
        printf '%sExecuting "%s" in %s %s\n' "$blue" "$cmd" "$(pwd)" "$normal"
        bash -c "$cmd"
        exit $?
    fi

    dirs=()
    while IFS= read -r -d '' dir; do
        dirs+=("${dir%.git}")
    done < <(find . -maxdepth "$depth" -type d -name .git -print0 2>/dev/null)

    if [ ${#dirs[@]} -eq 0 ]; then
        echo "${red}No git repositories found${normal}"
        exit 0
    fi

    total=${#dirs[@]}
    width=${#total}
    failures=0

    # Sequential is one slot; in parallel, 0 or more than there are repositories means all at once.
    slots=$max_jobs
    if [ "$parallel" = true ]; then
        if [ "$slots" -le 0 ] || [ "$slots" -gt "$total" ]; then
            slots=$total
            printf '%sLaunching "%s" in %d repo(s) in parallel...%s\n' "$blue" "$cmd" "$total" "$normal"
        else
            printf '%sLaunching "%s" in %d repo(s) in parallel (concurrency: %d)...%s\n' "$blue" "$cmd" "$total" "$slots" "$normal"
        fi
    else
        slots=1
    fi

    # Each job reports on a FIFO when it ends, so the next one can start and its output is
    # printed in one piece. Both the FIFO and the files below live in a folder removed at the end.
    tmpdir=$(mktemp -d)
    pipe="$tmpdir/completion.fifo"
    mkfifo "$pipe"
    exec 3<>"$pipe"
    command rm -f "$pipe"

    cleanup() {
        exec 3>&-
        command rm -rf "$tmpdir" 2>/dev/null
        local pids pid_list=()
        pids=$(jobs -p 2>/dev/null)
        if [ -n "$pids" ]; then
            read -r -a pid_list <<<"$pids"
            kill "${pid_list[@]}" 2>/dev/null
        fi
    }
    trap cleanup EXIT INT TERM

    launch_job() {
        local i="$1"
        (
            trap 'echo "$i" >&3 2>/dev/null' EXIT
            run_repo "${dirs[$i]}" "$tmpdir/$i.out"
            echo $? >"$tmpdir/$i.status"
        ) &
    }

    next_to_launch=0 running=0 completed=0
    while [ "$next_to_launch" -lt "$total" ] && [ "$running" -lt "$slots" ]; do
        launch_job "$next_to_launch"
        next_to_launch=$((next_to_launch + 1))
        running=$((running + 1))
    done

    while [ "$completed" -lt "$total" ]; do
        read -r idx <&3 || break
        completed=$((completed + 1))
        running=$((running - 1))

        dir="${dirs[$idx]}"
        status=1
        if [ -f "$tmpdir/$idx.status" ]; then
            status=$(cat "$tmpdir/$idx.status")
        fi

        if [ "$status" -eq 0 ]; then
            printf '[%*d/%d] %s✓ %s%s\n' "$width" "$completed" "$total" "$green" "$dir" "$normal"
        else
            printf '[%*d/%d] %s✗ %s (exit %d)%s\n' "$width" "$completed" "$total" "$red" "$dir" "$status" "$normal"
            failures=$((failures + 1))
        fi

        # The captured output goes under its status line, so it is grouped per repository
        # instead of arriving as the jobs happen to write it.
        if [ -s "$tmpdir/$idx.out" ]; then
            sed 's/^/    /' "$tmpdir/$idx.out"
        else
            printf '    %s(no output)%s\n' "$dim" "$normal"
        fi

        if [ "$next_to_launch" -lt "$total" ]; then
            launch_job "$next_to_launch"
            next_to_launch=$((next_to_launch + 1))
            running=$((running + 1))
        fi
    done

    wait
    exec 3>&-

    # What the summary lists, and the failures, in the order the repositories were found.
    summary_repos=() summary_details=() failed_repos=() failed_statuses=()
    i=0
    while [ "$i" -lt "$total" ]; do
        status=1
        if [ -f "$tmpdir/$i.status" ]; then
            status=$(cat "$tmpdir/$i.status")
        fi
        if [ -f "$tmpdir/$i.out.updated" ]; then
            summary_repos+=("${dirs[$i]}")
            summary_details+=("$(cat "$tmpdir/$i.out.updated")")
        fi
        if [ "$status" -ne 0 ]; then
            failed_repos+=("${dirs[$i]}")
            failed_statuses+=("$status")
        fi
        i=$((i + 1))
    done

    command rm -rf "$tmpdir"
    trap - EXIT INT TERM

    printf '\n%sDone: %d ok, %d failed (of %d)%s\n' "$blue" "$((total - failures))" "$failures" "$total" "$normal"

    if [ "$summary_mode" != none ]; then
        if [ "$summary_mode" = status ]; then
            heading="Repositories with changes"
            found_color="$yellow"
            all_quiet="All repositories clean."
            none_found="No repositories with changes."
        else
            heading="Updated repositories"
            found_color="$green"
            all_quiet="All repositories up to date."
            none_found="No repositories updated."
        fi

        if [ ${#summary_repos[@]} -gt 0 ]; then
            printf '\n%s%s (%d):%s\n' "$found_color" "$heading" "${#summary_repos[@]}" "$normal"
            i=0
            while [ "$i" -lt ${#summary_repos[@]} ]; do
                printf '  %s%s%s: %s\n' "$found_color" "${summary_repos[$i]}" "$normal" "${summary_details[$i]}"
                i=$((i + 1))
            done
        elif [ "$failures" -eq 0 ]; then
            printf '\n%s%s%s\n' "$dim" "$all_quiet" "$normal"
        else
            printf '\n%s%s%s\n' "$dim" "$none_found" "$normal"
        fi
    fi

    if [ "$failures" -gt 0 ]; then
        printf '\n%sFailed repositories (%d):%s\n' "$red" "$failures" "$normal"
        i=0
        while [ "$i" -lt ${#failed_repos[@]} ]; do
            printf '  %s%s%s (exit %d)\n' "$red" "${failed_repos[$i]}" "$normal" "${failed_statuses[$i]}"
            i=$((i + 1))
        done
        exit 1
    fi
)

###############################################################
# => Colored man pages
###############################################################

export LESS_TERMCAP_mb=$'\e[1;31m'
export LESS_TERMCAP_md=$'\e[1;34m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[1;33m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[1;32m'

###############################################################
# => Aliases
###############################################################

# Prefer nvim over vim
if command -v nvim &>/dev/null; then
    alias vim="nvim"
    export EDITOR=nvim
    export VISUAL=nvim
fi

# Easier navigation: .., ..., ...., ....., ~ and -
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias .....="cd ../../../.."
alias cr='cd $HOME/repos'

## Colorize the grep command output for ease of use (good for log files)##
alias grep='grep -i --color=auto'
alias egrep='egrep -i --color=auto'
alias fgrep='fgrep -i --color=auto'

# adding flags
alias cp="cp -iv"          # confirm before overwriting something
alias mv="mv -iv"
alias rm="rm -vI"
alias mkd="mkdir -pv"
alias df="df -h"          # human-readable sizes
alias free="free -m"      # show sizes in MB

# Clear
alias c="clear"

# Git
alias g="git"
alias gs="git status"
alias gf="git fetch"
alias gl="git pull"
alias ga="git add "
alias gm="git merge"
alias gc="git commit -m "
alias gp="git push"
alias gpf="git push --force-with-lease"
alias gitsync="git submodule sync; git submodule update --init --recursive"
alias gsu="git submodule update --recursive --remote"
alias gr="git-recurse"
alias gah="git-ahead"
## Alternative (makes easier finding out which commits have no message)
alias yolo='git add -A; git commit -m "This is a placeholder"; git push'

# Recursively find and delete local branches with no remote and no commits ahead
gclean() {
    local dry_run=false
    if [ "$1" = "--dry-run" ] || [ "$1" = "-n" ]; then
        dry_run=true
        shift
    fi
    local search_path="${1:-.}"
    local total_deleted=0

    if $dry_run; then
        printf "\033[32m[DRY RUN] Scanning for stale local branches...\033[0m\n"
    else
        printf "\033[32m[CLEAN] Scanning for stale local branches...\033[0m\n"
    fi

    while IFS= read -r -d $'\0' dot_git; do
        local dir
        dir=$(dirname "$dot_git")
        local repo_name
        repo_name=$(basename "$dir")

        git -C "$dir" fetch --all --prune --quiet 2>/dev/null

        # Detect default branch
        local default_branch
        default_branch=$(git -C "$dir" symbolic-ref refs/remotes/origin/HEAD 2>/dev/null | sed 's|^refs/remotes/origin/||')
        if [ -z "$default_branch" ]; then
            if git -C "$dir" rev-parse --verify --quiet origin/main &>/dev/null; then
                default_branch="main"
            else
                default_branch="master"
            fi
        fi

        local current
        current=$(git -C "$dir" rev-parse --abbrev-ref HEAD 2>/dev/null)

        local printed_header=false
        while IFS= read -r branch; do
            [ -z "$branch" ] && continue
            [ "$branch" = "$current" ] && continue
            [ "$branch" = "$default_branch" ] && continue

            # Skip if remote tracking branch exists
            if git -C "$dir" rev-parse --verify --quiet "origin/$branch" &>/dev/null; then
                continue
            fi

            # Check commits ahead of default
            local ahead
            if ! ahead=$(git -C "$dir" rev-list --count "$default_branch..$branch" 2>/dev/null); then
                ahead=$(git -C "$dir" rev-list --count "origin/$default_branch..$branch" 2>/dev/null)
            fi

            if [ "$ahead" = "0" ]; then
                if ! $printed_header; then
                    printf "\033[37m%s\033[0m (default: %s, current: %s)\n" "$repo_name" "$default_branch" "$current"
                    printed_header=true
                fi
                if $dry_run; then
                    printf "    \033[33mwould delete: %s\033[0m\n" "$branch"
                else
                    git -C "$dir" branch -d "$branch" &>/dev/null
                    printf "    \033[31mdeleted: %s\033[0m\n" "$branch"
                fi
                ((total_deleted++)) || true
            fi
        done < <(git -C "$dir" for-each-ref --format='%(refname:short)' refs/heads/ 2>/dev/null)
    done < <(find "$search_path" -maxdepth 3 -type d -name .git -print0 2>/dev/null)

    if [ "$total_deleted" -eq 0 ]; then
        printf "\033[32mNo stale branches found.\033[0m\n"
    else
        printf "\n\033[36m%d branch(es) processed.\033[0m\n" "$total_deleted"
    fi
}

# Git functions
add-pat() {
    local pat="$1"
    if [ -z "$pat" ]; then
        echo "Usage: add-pat <token>"
        echo "Recursively changes all https://github.com/... remotes to use the defined PAT."
        return 1
    fi
    printf "\033[34mdepth: 2 \033[0m\n"
    while IFS= read -r -d $'\0' dot_git; do
        local dir
        dir=$(dirname "$dot_git")
        local remote_url
        remote_url=$(git -C "$dir" remote get-url origin 2>/dev/null)
        if [ -n "$remote_url" ]; then
            if [[ "$remote_url" == *"github.com"* && "$remote_url" == https://* ]]; then
                local new_url
                new_url=$(echo "$remote_url" | sed -E "s|https://([^@]+@)?github\\.com|https://$pat@github.com|")
                if [ "$remote_url" != "$new_url" ]; then
                    git -C "$dir" remote set-url origin "$new_url"
                    printf "\033[32m✓ %s\033[0m (remote updated)\n" "$dir"
                else
                    printf "\033[34m- %s\033[0m (already using this token)\n" "$dir"
                fi
            else
                printf "\033[33m! %s\033[0m (ignored: not an HTTPS GitHub remote)\n" "$dir"
            fi
        fi
    done < <(find . -maxdepth 2 -type d -name .git -print0 2>/dev/null)
}
alias add-pat="add-pat"
status() { git-recurse "$@" git status; }

# System maintenance
update() {
    if command -v apt-get &>/dev/null; then
        sudo apt-get update && sudo apt-get full-upgrade -y
    elif command -v pacman &>/dev/null; then
        sudo pacman -Syu
    elif command -v brew &>/dev/null; then
        brew update && brew upgrade
    else
        echo "No supported package manager found"
        return 1
    fi
}

updateall() {
    update
    if command -v nvim &>/dev/null; then
        nvim +PlugUpgrade +PlugUpdate +qall 2>/dev/null || true
    fi
}

cleanup() {
    if command -v choco-cleaner &>/dev/null; then
        choco-cleaner
    fi
    if command -v nvim &>/dev/null; then
        nvim +PlugClean +qall 2>/dev/null || true
    fi
    if command -v apt-get &>/dev/null; then
        sudo apt-get autoclean && sudo apt-get autoremove -y
    elif command -v pacman &>/dev/null; then
        sudo pacman -Sc
    elif command -v brew &>/dev/null; then
        brew cleanup
    fi
}

# List files -- prefer eza > exa > ls
if command -v eza &>/dev/null; then
    alias ls="eza --group-directories-first"
    alias la="eza --group-directories-first -a"
    alias l="eza -a -F --long --header --links --group --group-directories-first --git"
elif command -v exa &>/dev/null; then
    alias ls="exa --group-directories-first"
    alias la="exa --group-directories-first -a"
    alias l="exa -a -F --long --header --links --group --group-directories-first --git"
else
    alias ls="ls --color=auto"
    alias la="ls -a --color=auto"
    alias l="ls"
fi

# Print each PATH entry on a separate line
alias path='echo -e ${PATH//:/\\n}'

# Userlist
alias userlist="cut -d: -f1 /etc/passwd"

# Switch between bash and zsh
# shellcheck disable=SC2139
alias tobash="sudo chsh $USER -s /bin/bash && echo 'Now log out.'"
# shellcheck disable=SC2139
alias tozsh="sudo chsh $USER -s /bin/zsh && echo 'Now log out.'"

# Sleep management
alias disableSleep="sudo systemctl mask sleep.target suspend.target hibernate.target hybrid-sleep.target"
alias enableSleep="sudo systemctl unmask sleep.target suspend.target hibernate.target hybrid-sleep.target"

# Get error messages from journalctl
alias jctl="journalctl -p 3 -xb"

# Lock screen (Linux only - picks random animation if available)
if [ "$(uname -s)" = 'Linux' ]; then
    LockScreens=()
    command -v pipes.sh &>/dev/null && LockScreens+=("pipes.sh")
    command -v cmatrix &>/dev/null && LockScreens+=("cmatrix")
    if [ ${#LockScreens[@]} -gt 0 ]; then
        lock() { ${LockScreens[$((RANDOM % ${#LockScreens[@]}))]]}; }
        export -f lock
    fi
    
    # Mirror update (distro-specific)
    if command -v reflector &>/dev/null; then
        # Arch Linux
        alias mirror="sudo reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist"
    elif command -v pacman-mirrors &>/dev/null; then
        # Manjaro
        alias mirror="sudo pacman-mirrors -f && sudo pacman -Syyu"
    elif command -v apt-get &>/dev/null; then
        # Ubuntu/Debian - no mirror alias needed, apt handles it
        alias mirror="sudo apt-get update"
    fi
fi

###############################################################
# => Tool integrations
###############################################################

# fzf defaults and keybindings (Ctrl+R for history, Ctrl+T for files)
if command -v fzf &>/dev/null; then
    export FZF_DEFAULT_OPTS='--preview "bat --style=numbers --color=always --line-range :500 {}" --height 60% --border -m'
    if command -v ag &>/dev/null; then
        export FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g ""'
    fi
    eval "$(fzf --bash 2>/dev/null)" || {
        [ -f /usr/share/doc/fzf/examples/key-bindings.bash ] && source /usr/share/doc/fzf/examples/key-bindings.bash
        [ -f /usr/share/doc/fzf/examples/completion.bash ] && source /usr/share/doc/fzf/examples/completion.bash
    }
fi
