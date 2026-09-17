#!/usr/bin/env bash
# Shared post-clone/pull git configuration for clone-all, clone-team, etc.

resolve_repo_dir() {
    local nameWithOwner="$1"
    local repoName="${nameWithOwner##*/}"

    if [ -d "$nameWithOwner/.git" ]; then
        printf '%s\n' "$nameWithOwner"
    elif [ -d "$repoName/.git" ]; then
        printf '%s\n' "$repoName"
    elif [ -e "$nameWithOwner" ]; then
        printf '%s\n' "$nameWithOwner"
    elif [ -e "$repoName" ]; then
        printf '%s\n' "$repoName"
    else
        printf '%s\n' "$repoName"
    fi
}

# identity_scope:
#   personal — cuberhaus/* only (clone-all)
#   team     — cuberhaus/* personal; other orgs work identity (clone-team)
configure_tracked_repo_git() {
    local nameWithOwner="$1"
    local dir="$2"
    local identity_scope="${3:-personal}"

    if [ ! -d "$dir/.git" ]; then
        return 0
    fi

    if [ -d "$dir/.githooks" ]; then
        git -C "$dir" config core.hooksPath .githooks
    elif [ "$(git -C "$dir" config --get core.hooksPath 2>/dev/null)" = ".githooks" ]; then
        git -C "$dir" config --unset core.hooksPath 2>/dev/null || true
    fi

    if [ -f "$dir/lefthook.yml" ] || [ -f "$dir/.lefthook.yml" ]; then
        if [ -x "$dir/node_modules/.bin/lefthook" ]; then
            (cd "$dir" && "$dir/node_modules/.bin/lefthook" install 2>/dev/null) || true
        elif command -v lefthook >/dev/null 2>&1; then
            (cd "$dir" && lefthook install 2>/dev/null) || true
        elif command -v npx >/dev/null 2>&1; then
            (cd "$dir" && npx --yes lefthook install 2>/dev/null) || true
        fi
    fi

    if [ -x "$dir/.local/scripts/apply-skip-worktree" ]; then
        "$dir/.local/scripts/apply-skip-worktree" "$dir"
    fi

    local org="${nameWithOwner%%/*}"
    local git_name="" git_email=""

    case "$identity_scope" in
        team)
            if [ "$org" = "cuberhaus" ]; then
                git_name="cuberhaus"
                git_email="polcg10@gmail.com"
            else
                git_name="${CLONE_TEAM_WORK_GIT_NAME:-Pol Casacuberta Gil}"
                git_email="${CLONE_TEAM_WORK_GIT_EMAIL:-pcasacubertagil@deloitte.es}"
            fi
            ;;
        personal|*)
            if [[ "$nameWithOwner" == cuberhaus/* ]]; then
                git_name="cuberhaus"
                git_email="polcg10@gmail.com"
            fi
            ;;
    esac

    if [ -n "$org" ]; then
        local org_key
        org_key=$(printf '%s' "$org" | tr '[:lower:]' '[:upper:]' | tr -c 'A-Z0-9' '_')
        local var_name="CLONE_GIT_IDENTITY_${org_key}_NAME"
        local var_email="CLONE_GIT_IDENTITY_${org_key}_EMAIL"
        # shellcheck disable=SC2154,SC2295
        if [ -n "${!var_name:-}" ]; then git_name="${!var_name}"; fi
        # shellcheck disable=SC2154,SC2295
        if [ -n "${!var_email:-}" ]; then git_email="${!var_email}"; fi
    fi

    if [ -n "$git_name" ]; then
        git -C "$dir" config user.name "$git_name"
    fi
    if [ -n "$git_email" ]; then
        git -C "$dir" config user.email "$git_email"
    fi
}
