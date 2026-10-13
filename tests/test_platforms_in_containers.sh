#!/usr/bin/env bash
# Opt-in (make test-platforms), not part of `make test`: runs the Java and LanguageTool steps
# where the hermetic tests only use stubs, in throwaway Docker containers with this checkout
# mounted read-only.
#
#   Arch    a real Arch userland: pacman installs the packages arch_functions declares, the real
#           languagetool_install downloads and unpacks LanguageTool, and the server answers on
#           that JRE.
#   macOS   the shell macOS ships (bash 3.2) with no sha256sum, so the step falls back to
#           `shasum -a 256`, run with the real shasum of this machine.  It is not a Mac: Homebrew,
#           the temurin cask and `java` stubs are out of reach, only the shell and checksum
#           behavior are exercised.
#
# It needs Docker, the network, and downloads LanguageTool (240 MB) once per container.
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

fail() {
    printf 'FAIL: %s\n' "$*" >&2
    exit 1
}

command -v docker >/dev/null 2>&1 || fail 'docker is not installed'
docker info >/dev/null 2>&1 || fail 'docker is not usable by this user'
SHASUM="$(command -v shasum)" || fail 'shasum is not installed; the macOS case runs this machine'"'"'s shasum'

WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

cat > "$WORK/arch.sh" <<'INNER'
set -euo pipefail
pacman -Syu --noconfirm --needed --disable-sandbox unzip jre-openjdk-headless >/tmp/pacman.log 2>&1 ||
    { tail -20 /tmp/pacman.log; exit 1; }
pacman -Q unzip jre-openjdk-headless
java -version 2>&1 | head -1
export HOME=/root
source /d/.local/scripts/bootstrap/base_functions
languagetool_install
languagetool_install # an installed copy is skipped, and Java is checked again
jar="$HOME/.config/texstudio/dictionaries/LanguageTool-6.6/languagetool-server.jar"
java -jar "$jar" --port 8081 >/tmp/server.log 2>&1 &
for _ in $(seq 1 60); do
    curl -fs http://localhost:8081/v2/languages > /tmp/languages.json 2>/dev/null && break
    sleep 1
done
grep -q '"longCode"' /tmp/languages.json || { echo 'the server did not list languages'; tail -20 /tmp/server.log; exit 1; }
echo 'the server answered on the Arch JRE'
INNER

cat > "$WORK/macos.sh" <<'INNER'
set -eu
apk add --no-cache curl unzip perl >/dev/null 2>&1
rm -f /usr/bin/sha256sum /bin/sha256sum # macOS has none
cp /host-shasum /usr/local/bin/shasum
chmod +x /usr/local/bin/shasum
bash --version | head -1
if command -v sha256sum >/dev/null; then echo 'sha256sum is still present'; exit 1; fi
for f in mac mac_functions base_functions uninstall_mac uninstall_mac_functions; do
    bash -n "/d/.local/scripts/bootstrap/$f" || exit 1
done
echo 'the files of the mac profile parse'
export HOME=/root
source /d/.local/scripts/bootstrap/base_functions
mkdir -p /fake
fake_java() {
    printf '#!/bin/sh\nprintf "%%s\\n" "%s" >&2\nexit %s\n' "$1" "$2" > /fake/java
    chmod +x /fake/java
}
expect() { # LABEL WANTED_STATUS OUTPUT JAVA_STATUS
    fake_java "$3" "$4"
    got=0
    PATH="/fake:$PATH" languagetool_java_check >/dev/null 2>&1 || got=1
    [ "$got" = "$2" ] || { echo "wrong answer for: $1"; exit 1; }
}
expect 'java 17' 0 'openjdk version \"17.0.9\" 2023-10-17' 0
expect 'java 21' 0 'openjdk version \"21.0.2\" 2024-01-16' 0
expect 'java 11' 1 'openjdk version \"11.0.22\" 2024-01-16' 0
expect 'java 1.8' 1 'java version \"1.8.0_391\"' 0
expect 'the macOS stub without a JDK' 1 "The operation couldn't be completed. Unable to locate a Java Runtime." 1
echo 'the Java check answers correctly under bash 3.2'
fake_java 'openjdk version \"17.0.9\" 2023-10-17' 0
PATH="/fake:$PATH" languagetool_install
test -f "$HOME/.config/texstudio/dictionaries/LanguageTool-6.6/languagetool-server.jar"
rm -rf "$HOME/.config/texstudio"
zeros=0000000000000000000000000000000000000000000000000000000000000000
sed "s/local digest=\"[0-9a-f]*\"/local digest=\"$zeros\"/" /d/.local/scripts/bootstrap/base_functions > /tmp/base_wrong_digest
grep -q "$zeros" /tmp/base_wrong_digest
if (source /tmp/base_wrong_digest; PATH="/fake:$PATH" languagetool_install >/dev/null 2>&1); then
    echo 'a wrong digest was accepted'; exit 1
fi
if [ -e "$HOME/.config/texstudio/dictionaries/LanguageTool-6.6" ]; then echo 'the refused install left files'; exit 1; fi
echo 'shasum installs the pinned digest and refuses a wrong one'
INNER

printf '== Arch (archlinux:latest)\n'
docker run --rm -v "$REPO_ROOT":/d:ro -v "$WORK/arch.sh":/arch.sh:ro archlinux:latest bash /arch.sh ||
    fail 'the Arch run failed'

printf '== macOS shell (bash:3.2, shasum -a 256)\n'
docker run --rm -v "$REPO_ROOT":/d:ro -v "$SHASUM":/host-shasum:ro -v "$WORK/macos.sh":/macos.sh:ro bash:3.2 bash /macos.sh ||
    fail 'the macOS-shell run failed'

printf 'Platform container tests passed.\n'
