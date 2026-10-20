#!/usr/bin/env bash
# Launch DaVinci Resolve and enable native X11 window decorations on its windows.
#
# Resolve creates its top-level windows and dialogs with Qt::FramelessWindowHint, which
# sets _MOTIF_WM_HINTS decorations=0 and _KDE_NET_WM_WINDOW_TYPE_OVERRIDE on X11/XWayland.
# Under Mutter/GNOME that leaves both the Project Manager and the main editing window
# without a native title bar, so they cannot be dragged by the top bar. Setting
# _MOTIF_WM_HINTS to "0x2, 0x0, 0x1, 0x0, 0x0" on titled Resolve normal/dialog windows
# makes the window manager attach a standard title bar while leaving splash screens,
# popups, and menus alone.
set -euo pipefail

readonly PREFIX="${DAVINCI_RESOLVE_PREFIX:-/opt/resolve}"
readonly POLL_INTERVAL="${DAVINCI_RESOLVE_POLL_INTERVAL:-2}"

usage() {
    cat <<'EOF'
Usage: davinci_resolve_run.sh [--decorate-only] [RESOLVE_BINARY [ARGS...]]

Start DaVinci Resolve while watching the X11 root window for newly mapped or focused
Resolve windows and enabling native window-manager title bars (_MOTIF_WM_HINTS) on them.

Options:
  --decorate-only   Decorate any currently open DaVinci Resolve windows and exit.
  -h, --help        Show this help.
EOF
}

## Candidate Resolve window IDs from the X11 window tree: titled windows with WM_CLASS
## ("resolve" "resolve"), excluding the splash screen and Qt's hidden helper windows
## whose title is "resolve" or "Qt Selection Owner for resolve".
candidate_windows() {
    xwininfo -root -tree 2>/dev/null | awk '
        /"resolve" "resolve"/ &&
        !/\(has no name\)/ &&
        !/^[[:space:]]*0x[0-9a-fA-F]+[[:space:]]+"(resolve|Qt Selection Owner for resolve)":/ {
            print $1
        }
    ' || true
}

## Enable native decorations on every candidate Resolve window that is a normal or dialog
## window and currently has _MOTIF_WM_HINTS decorations set to 0.
decorate_resolve_windows() {
    local wid props
    if [[ -z "${DISPLAY:-}" ]] || ! command -v xprop >/dev/null 2>&1 || ! command -v xwininfo >/dev/null 2>&1; then
        return 0
    fi
    while IFS= read -r wid; do
        [[ -n "$wid" ]] || continue
        props="$(xprop -id "$wid" _MOTIF_WM_HINTS _NET_WM_WINDOW_TYPE 2>/dev/null || true)"
        [[ -n "$props" ]] || continue
        if ! grep -Eq '_NET_WM_WINDOW_TYPE\([^)]*\).*(_NET_WM_WINDOW_TYPE_NORMAL|_NET_WM_WINDOW_TYPE_DIALOG)' <<<"$props"; then
            continue
        fi
        if grep -Eq '_NET_WM_WINDOW_TYPE\([^)]*\).*(_NET_WM_WINDOW_TYPE_SPLASH|_NET_WM_WINDOW_TYPE_TOOLTIP|_NET_WM_WINDOW_TYPE_POPUP_MENU|_NET_WM_WINDOW_TYPE_DROPDOWN_MENU|_NET_WM_WINDOW_TYPE_COMBO)' <<<"$props"; then
            continue
        fi
        if grep -Eq '_MOTIF_WM_HINTS\([^)]*\) = 0x2, 0x[0-9a-fA-F]+, 0x0,' <<<"$props"; then
            xprop -id "$wid" -f _MOTIF_WM_HINTS 32c -set _MOTIF_WM_HINTS '0x2, 0x0, 0x1, 0x0, 0x0' >/dev/null 2>&1 || true
        fi
    done < <(candidate_windows)
}

## Watch the X11 root window for client-list and active-window changes while process $1 lives.
watch_resolve_windows() {
    local resolve_pid="$1"
    local spy_pid=''
    decorate_resolve_windows
    if [[ -z "${DISPLAY:-}" ]] || ! command -v xprop >/dev/null 2>&1 || ! command -v xwininfo >/dev/null 2>&1; then
        return 0
    fi
    exec 3< <(exec xprop -root -spy _NET_CLIENT_LIST _NET_ACTIVE_WINDOW 2>/dev/null)
    spy_pid=$!
    # shellcheck disable=SC2064 # spy_pid is fixed once xprop starts
    trap "kill '$spy_pid' 2>/dev/null || true; exec 3<&-" EXIT
    while kill -0 "$resolve_pid" 2>/dev/null; do
        if read -r -t "$POLL_INTERVAL" -u 3 _; then
            while read -r -t 0.05 -u 3 _; do :; done
            decorate_resolve_windows
        fi
    done
}

main() {
    local decorate_only=false
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -h | --help)
                usage
                exit 0
                ;;
            --decorate-only)
                decorate_only=true
                shift
                ;;
            --)
                shift
                break
                ;;
            -*)
                printf 'davinci_resolve_run.sh: unknown option: %s\n' "$1" >&2
                usage >&2
                exit 2
                ;;
            *)
                break
                ;;
        esac
    done

    if [[ "$decorate_only" == true ]]; then
        decorate_resolve_windows
        exit 0
    fi

    local resolve_bin="$PREFIX/bin/resolve"
    if [[ $# -gt 0 ]]; then
        resolve_bin="$1"
        shift
    fi

    if [[ -n "${DISPLAY:-}" ]] && command -v xprop >/dev/null 2>&1 && command -v xwininfo >/dev/null 2>&1; then
        (watch_resolve_windows "$$") </dev/null >/dev/null 2>&1 &
    fi

    exec "$resolve_bin" "$@"
}

main "$@"
