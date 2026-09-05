#!/usr/bin/env bash
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
MIRROR_COMMAND="$REPO_ROOT/server/pol-server/github-mirror"
CASE_DIR="$(mktemp -d)"
REMOTE_DIR="$CASE_DIR/remotes"
MIRROR_DIR="$CASE_DIR/mirrors"
FAKE_BIN="$CASE_DIR/bin"
CREDENTIALS_DIR="$CASE_DIR/credentials"
API_PAGE_ONE="$CASE_DIR/api-page-1.json"
LFS_LOG="$CASE_DIR/lfs.log"
SYSTEMCTL_LOG="$CASE_DIR/systemctl.log"
FAKE_ROOT="$CASE_DIR/root"
REAL_GIT="$(command -v git)"
trap 'rm -rf "$CASE_DIR"' EXIT

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

mkdir -p "$REMOTE_DIR" "$MIRROR_DIR" "$FAKE_BIN" "$CREDENTIALS_DIR"
printf 'test-token\n' > "$CREDENTIALS_DIR/github-token"
: > "$LFS_LOG"
: > "$SYSTEMCTL_LOG"

git init --quiet --bare "$REMOTE_DIR/private-archive.git"
git init --quiet --initial-branch=main "$CASE_DIR/source"
git -C "$CASE_DIR/source" config user.name 'Mirror Test'
git -C "$CASE_DIR/source" config user.email 'mirror-test@example.invalid'
printf 'recoverable content\n' > "$CASE_DIR/source/README.md"
git -C "$CASE_DIR/source" add README.md
git -C "$CASE_DIR/source" commit --quiet -m 'Initial test content'
git -C "$CASE_DIR/source" remote add origin "$REMOTE_DIR/private-archive.git"
git -C "$CASE_DIR/source" push --quiet origin main

cat > "$API_PAGE_ONE" <<'JSON'
[
  {
    "name": "private-archive",
    "full_name": "cuberhaus/private-archive",
    "private": true,
    "archived": true,
    "fork": true,
    "owner": {"login": "cuberhaus"}
  }
]
JSON
export API_PAGE_ONE
export LFS_LOG REAL_GIT

cat > "$FAKE_BIN/curl" <<'EOF'
#!/usr/bin/env bash
case "$*" in
        *'&page=1&'*) cat "$API_PAGE_ONE" ;;
    *'&page=2&'*) printf '[]\n' ;;
    *'https://api.github.test/user'*) printf '{"login":"cuberhaus"}\n' ;;
    *) exit 22 ;;
esac
EOF

cat > "$FAKE_BIN/git" <<'EOF'
#!/usr/bin/env bash
case "$*" in
    *'clone --mirror'*|*'remote update --prune'*|*'lfs fetch --all origin'*)
        [ "${GIT_CONFIG_COUNT:-}" = 1 ] || exit 97
        [ "${GIT_CONFIG_KEY_0:-}" = 'http.https://github.com/.extraHeader' ] || exit 97
        expected_auth="Authorization: Basic $(printf 'x-access-token:test-token' | base64 -w0)"
        [ "${GIT_CONFIG_VALUE_0:-}" = "$expected_auth" ] || exit 97
        [[ "$*" != *test-token* ]] || exit 97
        ;;
esac
if [[ "$*" == *' lfs fetch --all origin' ]]; then
    printf '%s\n' "$*" >> "$LFS_LOG"
    exit 0
fi
exec "$REAL_GIT" "$@"
EOF

cat > "$FAKE_BIN/systemctl" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$*" >> "$SYSTEMCTL_LOG"
EOF
export SYSTEMCTL_LOG
chmod +x "$FAKE_BIN/curl" "$FAKE_BIN/git" "$FAKE_BIN/systemctl"

PATH="$FAKE_BIN:$PATH" \
    CREDENTIALS_DIRECTORY="$CREDENTIALS_DIR" \
    GITHUB_MIRROR_API_URL='https://api.github.test/user/repos' \
    GITHUB_MIRROR_GIT_BASE_URL="file://$REMOTE_DIR" \
    GITHUB_MIRROR_STATE_DIRECTORY="$MIRROR_DIR" \
    POL_SERVER_ALLOW_UNPRIVILEGED=true \
    "$MIRROR_COMMAND" --sync

mirror="$MIRROR_DIR/private-archive.git"
[ "$(git -C "$mirror" rev-parse --is-bare-repository)" = true ] ||
    fail 'sync must create a bare repository'
[ "$(git -C "$mirror" show main:README.md)" = 'recoverable content' ] ||
    fail 'the mirror must preserve repository history'
grep -Fq -- "-C $mirror lfs fetch --all origin" "$LFS_LOG" ||
    fail 'sync must fetch every Git LFS object after cloning'
check_output="$(
    PATH="$FAKE_BIN:$PATH" \
        CREDENTIALS_DIRECTORY="$CREDENTIALS_DIR" \
        GITHUB_MIRROR_STATE_DIRECTORY="$MIRROR_DIR" \
        POL_SERVER_ALLOW_UNPRIVILEGED=true \
        "$MIRROR_COMMAND" --check
)"
grep -Fq 'Mirrors healthy: 1' <<< "$check_output" ||
    fail 'check must verify and count healthy bare mirrors'
grep -Fq 'Active repositories mirrored: 1' <<< "$check_output" ||
    fail 'check must report the complete active repository inventory'

git clone --quiet --mirror "$REMOTE_DIR/private-archive.git" "$MIRROR_DIR/removed-from-github.git"
printf 'updated content\n' > "$CASE_DIR/source/README.md"
git -C "$CASE_DIR/source" add README.md
git -C "$CASE_DIR/source" commit --quiet -m 'Update test content'
git -C "$CASE_DIR/source" push --quiet origin main

PATH="$FAKE_BIN:$PATH" \
    CREDENTIALS_DIRECTORY="$CREDENTIALS_DIR" \
    GITHUB_MIRROR_API_URL='https://api.github.test/user/repos' \
    GITHUB_MIRROR_GIT_BASE_URL="file://$REMOTE_DIR" \
    GITHUB_MIRROR_STATE_DIRECTORY="$MIRROR_DIR" \
    POL_SERVER_ALLOW_UNPRIVILEGED=true \
    "$MIRROR_COMMAND" --sync

[ "$(git -C "$mirror" show main:README.md)" = 'updated content' ] ||
    fail 'a later sync must update existing mirrors'
[ -d "$MIRROR_DIR/removed-from-github.git" ] ||
    fail 'sync must preserve mirrors no longer returned by GitHub'

git clone --quiet --bare "$REMOTE_DIR/private-archive.git" "$REMOTE_DIR/healthy.git"
cat > "$API_PAGE_ONE" <<'JSON'
[
    {
        "name": "missing",
        "full_name": "cuberhaus/missing",
        "private": true,
        "archived": false,
        "fork": false,
        "owner": {"login": "cuberhaus"}
    },
    {
        "name": "healthy",
        "full_name": "cuberhaus/healthy",
        "private": false,
        "archived": false,
        "fork": false,
        "owner": {"login": "cuberhaus"}
    }
]
JSON

if PATH="$FAKE_BIN:$PATH" \
        CREDENTIALS_DIRECTORY="$CREDENTIALS_DIR" \
        GITHUB_MIRROR_API_URL='https://api.github.test/user/repos' \
        GITHUB_MIRROR_GIT_BASE_URL="file://$REMOTE_DIR" \
        GITHUB_MIRROR_STATE_DIRECTORY="$MIRROR_DIR" \
        POL_SERVER_ALLOW_UNPRIVILEGED=true \
        "$MIRROR_COMMAND" --sync; then
        fail 'sync must report failure when any repository cannot be mirrored'
fi
[ -d "$MIRROR_DIR/healthy.git" ] ||
        fail 'sync must continue after an individual repository fails'

printf 'configured-token\n' |
    PATH="$FAKE_BIN:$PATH" \
        GITHUB_MIRROR_USER_API_URL='https://api.github.test/user' \
        POL_SERVER_ALLOW_UNPRIVILEGED=true \
        POL_SERVER_ROOT="$FAKE_ROOT" \
        "$MIRROR_COMMAND" --configure-token
token_file="$FAKE_ROOT/etc/cuberhaus/github-mirror-token"
[ "$(cat "$token_file")" = 'configured-token' ] ||
    fail 'configure-token must store the validated credential outside the repository'
[ "$(stat -c %a "$token_file")" = 600 ] ||
    fail 'the GitHub credential must be readable only by root'
grep -Fq 'enable --now pol-server-github-mirror.timer' "$SYSTEMCTL_LOG" ||
    fail 'configure-token must enable the mirror timer after validation'

printf 'pol-server GitHub mirror tests passed.\n'