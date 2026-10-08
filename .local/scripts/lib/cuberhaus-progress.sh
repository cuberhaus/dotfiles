# shellcheck shell=bash
# Desktop notifications for the scheduled automations: a progress bar while a job runs and a
# summary when it ends. The Linux counterpart of WinDotfiles' my_progress.psm1.
#
#   progress_start TASK TITLE [VERB] [UNIT] [DELAY_SECONDS]
#       Begin a run. Nothing shows before DELAY_SECONDS have passed, so a run that ends sooner
#       only produces its summary. VERB ("Upgrading") and UNIT ("package") word the status line.
#   progress_update [current=TEXT] [index=N] [done=N] [total=N] [percent=N]
#       Change what the bar says. The bar is done/total, or percent when that is given.
#   progress_finish success|warning|failure [--quiet] [--message TEXT] [SUMMARY_LINE...]
#       End the run with one summary notification, which replaces the bar. --quiet applies to
#       success only: nothing is shown and a bar already on screen is closed, so a run with
#       nothing to do stays out of the way. Warnings and failures always show.
#   progress_abort STATUS
#       For an EXIT trap: a run that is still open when the script dies ends as a failure.
#
# Every function returns 0 and does nothing before progress_start, so a script calls progress_*
# only when it was asked to notify, and a notification problem can never fail the job.
#
# Delivery. gdbus talks to the notification daemon directly (replaces_id, the `value` hint that
# dunst and KDE draw as a bar); notify-send is the fallback, with the synchronous hint that
# GNOME and dunst use to replace a notification in place. As root, the notification goes to every
# active graphical session through runuser and that user's session bus; nothing is shown when no
# one is logged in. Ids of shown notifications live in a private temp directory, so a progress
# update sent from the subshell of a pipeline is still replaced by the summary of its parent.
#
# Test seams: CUBERHAUS_NOTIFY_BACKEND=auto|gdbus|notify-send|none, CUBERHAUS_PROGRESS_NOW=<epoch>.
# Plain bash 3.2: the user-level automations also run on macOS, where this library shows nothing.

_PROGRESS_ACTIVE=0
_PROGRESS_FINISHED=0
_PROGRESS_DIR=''
_PROGRESS_TASK=''
_PROGRESS_TITLE=''
_PROGRESS_VERB='Working'
_PROGRESS_UNIT=''
_PROGRESS_DELAY=0
_PROGRESS_START=0
_PROGRESS_CURRENT=''
_PROGRESS_INDEX=0
_PROGRESS_DONE=0
_PROGRESS_TOTAL=0
_PROGRESS_PERCENT=-1
_PROGRESS_SENT_SIGNATURE=''
_PROGRESS_SENT_AT=0

_progress_now() {
    printf '%s\n' "${CUBERHAUS_PROGRESS_NOW:-$(date +%s)}"
}

# _progress_duration SECONDS: "42 s", "3 min 5 s", "1 h 2 min".
_progress_duration() {
    local seconds=$1
    if ((seconds < 60)); then
        printf '%d s' "$seconds"
    elif ((seconds < 3600)); then
        printf '%d min %d s' $((seconds / 60)) $((seconds % 60))
    else
        printf '%d h %d min' $((seconds / 3600)) $(((seconds % 3600) / 60))
    fi
}

# _progress_markup TEXT: notification bodies may carry Pango-like markup, so & < > must be escaped.
_progress_markup() {
    local text=$1 out='' char i
    for ((i = 0; i < ${#text}; i++)); do
        char=${text:i:1}
        case $char in
        '&') out+='&amp;' ;;
        '<') out+='&lt;' ;;
        '>') out+='&gt;' ;;
        *) out+=$char ;;
        esac
    done
    printf '%s' "$out"
}

# _progress_gvariant TEXT: TEXT as a single-quoted GVariant string, the way gdbus reads arguments.
_progress_gvariant() {
    local text=$1 out='' char i
    for ((i = 0; i < ${#text}; i++)); do
        char=${text:i:1}
        case $char in
        "'") out+="\\'" ;;
        \\) out+="\\\\" ;;
        $'\n') out+='\n' ;;
        *) out+=$char ;;
        esac
    done
    printf "'%s'" "$out"
}

_progress_backend() {
    local wanted=${CUBERHAUS_NOTIFY_BACKEND:-auto}
    case $wanted in
    none) printf 'none\n' ;;
    gdbus | notify-send)
        if command -v "$wanted" >/dev/null 2>&1; then printf '%s\n' "$wanted"; else printf 'none\n'; fi
        ;;
    *)
        if command -v gdbus >/dev/null 2>&1; then
            printf 'gdbus\n'
        elif command -v notify-send >/dev/null 2>&1; then
            printf 'notify-send\n'
        else
            printf 'none\n'
        fi
        ;;
    esac
}

# _progress_targets: one "UID USER" line for each person to notify. A normal user notifies
# themselves. Root notifies the owner of every active graphical session (x11, wayland, mir).
_progress_targets() {
    local uid session key value type state name user_id seen=' '
    uid=$(id -u)
    if ((uid != 0)); then
        printf '%s %s\n' "$uid" "$(id -un)"
        return 0
    fi
    command -v loginctl >/dev/null 2>&1 || return 0
    while read -r session _; do
        [[ -n $session ]] || continue
        type='' state='' name='' user_id=''
        while IFS='=' read -r key value; do
            case $key in
            Type) type=$value ;;
            State) state=$value ;;
            Name) name=$value ;;
            User) user_id=$value ;;
            esac
        done < <(loginctl show-session "$session" -p Type -p State -p Name -p User 2>/dev/null)
        case $type in x11 | wayland | mir) ;; *) continue ;; esac
        [[ $state == active && $user_id =~ ^[0-9]+$ && -n $name && $name != root ]] || continue
        case $seen in *" $user_id "*) continue ;; esac
        seen+="$user_id "
        printf '%s %s\n' "$user_id" "$name"
    done < <(loginctl list-sessions --no-legend 2>/dev/null)
}

# _progress_as_user UID USER COMMAND...: run COMMAND where UID's session bus is reachable.
_progress_as_user() {
    local uid=$1 user=$2 bus=${DBUS_SESSION_BUS_ADDRESS:-}
    shift 2
    if (($(id -u) == 0)); then
        runuser -u "$user" -- env "DBUS_SESSION_BUS_ADDRESS=unix:path=/run/user/$uid/bus" \
            "XDG_RUNTIME_DIR=/run/user/$uid" "$@"
        return
    fi
    if [[ -z $bus && -S /run/user/$uid/bus ]]; then
        bus=unix:path=/run/user/$uid/bus
    fi
    if [[ -n $bus ]]; then
        env "DBUS_SESSION_BUS_ADDRESS=$bus" "$@"
    else
        "$@"
    fi
}

# _progress_known_id UID: the id of the notification already shown to UID, 0 when none.
_progress_known_id() {
    local file="$_PROGRESS_DIR/id.$1" id=0
    if [[ -n $_PROGRESS_DIR && -r $file ]]; then
        id=$(<"$file")
    fi
    [[ $id =~ ^[0-9]+$ ]] || id=0
    printf '%s\n' "$id"
}

_progress_remember_id() {
    if [[ -n $_PROGRESS_DIR && -d $_PROGRESS_DIR ]]; then
        printf '%s\n' "$2" >"$_PROGRESS_DIR/id.$1"
    fi
}

# _progress_deliver TITLE BODY VALUE ICON: VALUE is the bar (0-100), or -1 for no bar.
_progress_deliver() {
    local title=$1 body=$2 value=$3 icon=$4 backend uid user id reply hints
    backend=$(_progress_backend)
    [[ $backend != none ]] || return 0
    title=$(_progress_markup "$title")
    body=$(_progress_markup "$body")
    while read -r uid user; do
        [[ -n $uid ]] || continue
        id=$(_progress_known_id "$uid")
        if [[ $backend == gdbus ]]; then
            hints="{'urgency': <byte 1>, 'x-canonical-private-synchronous': <'cuberhaus-$_PROGRESS_TASK'>"
            ((value < 0)) || hints+=", 'value': <int32 $value>"
            hints+='}'
            reply=$(_progress_as_user "$uid" "$user" gdbus call --session --timeout 5 \
                --dest org.freedesktop.Notifications --object-path /org/freedesktop/Notifications \
                --method org.freedesktop.Notifications.Notify \
                "'Cuberhaus'" "uint32 $id" "$(_progress_gvariant "$icon")" \
                "$(_progress_gvariant "$title")" "$(_progress_gvariant "$body")" '[]' "$hints" 'int32 -1' 2>/dev/null) || continue
            if [[ $reply =~ uint32\ ([0-9]+) ]]; then
                _progress_remember_id "$uid" "${BASH_REMATCH[1]}"
            fi
        else
            local args=(--app-name=Cuberhaus --urgency=normal "--icon=$icon"
                "--hint=string:x-canonical-private-synchronous:cuberhaus-$_PROGRESS_TASK")
            ((value < 0)) || args+=("--hint=int:value:$value")
            _progress_as_user "$uid" "$user" notify-send "${args[@]}" -- "$title" "$body" >/dev/null 2>&1 || continue
        fi
    done < <(_progress_targets)
    return 0
}

# _progress_close: take the bar off the screen (gdbus only; notify-send cannot close one).
_progress_close() {
    [[ $(_progress_backend) == gdbus ]] || return 0
    local uid user id
    while read -r uid user; do
        [[ -n $uid ]] || continue
        id=$(_progress_known_id "$uid")
        ((id > 0)) || continue
        _progress_as_user "$uid" "$user" gdbus call --session --timeout 5 \
            --dest org.freedesktop.Notifications --object-path /org/freedesktop/Notifications \
            --method org.freedesktop.Notifications.CloseNotification "uint32 $id" >/dev/null 2>&1 || true
    done < <(_progress_targets)
    return 0
}

_progress_cleanup() {
    if [[ -n $_PROGRESS_DIR ]]; then
        rm -rf -- "$_PROGRESS_DIR"
        _PROGRESS_DIR=''
    fi
}

progress_start() {
    _PROGRESS_TASK=${1:?progress_start needs a task}
    _PROGRESS_TITLE=${2:?progress_start needs a title}
    _PROGRESS_VERB=${3:-Working}
    _PROGRESS_UNIT=${4:-}
    _PROGRESS_DELAY=${5:-0}
    [[ $_PROGRESS_DELAY =~ ^[0-9]+$ ]] || _PROGRESS_DELAY=0
    _PROGRESS_START=$(_progress_now)
    _PROGRESS_CURRENT=''
    _PROGRESS_INDEX=0
    _PROGRESS_DONE=0
    _PROGRESS_TOTAL=0
    _PROGRESS_PERCENT=-1
    _PROGRESS_SENT_SIGNATURE=''
    _PROGRESS_SENT_AT=0
    _PROGRESS_FINISHED=0
    _PROGRESS_DIR=$(mktemp -d "${TMPDIR:-/tmp}/cuberhaus-progress.XXXXXX" 2>/dev/null) || _PROGRESS_DIR=''
    _PROGRESS_ACTIVE=1
    return 0
}

progress_update() {
    ((_PROGRESS_ACTIVE == 1 && _PROGRESS_FINISHED == 0)) || return 0
    local pair value
    for pair in "$@"; do
        value=${pair#*=}
        case $pair in
        current=*) _PROGRESS_CURRENT=$value ;;
        index=*) [[ $value =~ ^[0-9]+$ ]] && _PROGRESS_INDEX=$value ;;
        done=*) [[ $value =~ ^[0-9]+$ ]] && _PROGRESS_DONE=$value ;;
        total=*) [[ $value =~ ^[0-9]+$ ]] && _PROGRESS_TOTAL=$value ;;
        percent=*) [[ $value =~ ^[0-9]+$ ]] && _PROGRESS_PERCENT=$value ;;
        esac
    done
    _progress_show_running
    return 0
}

_progress_show_running() {
    local now elapsed bar=-1 count='' body signature
    now=$(_progress_now)
    elapsed=$((now - _PROGRESS_START))
    ((elapsed >= _PROGRESS_DELAY)) || return 0

    if ((_PROGRESS_PERCENT >= 0)); then
        bar=$_PROGRESS_PERCENT
    elif ((_PROGRESS_TOTAL > 0)); then
        bar=$((_PROGRESS_DONE * 100 / _PROGRESS_TOTAL))
    fi
    ((bar <= 100)) || bar=100

    signature="$bar|$_PROGRESS_CURRENT|$_PROGRESS_INDEX"
    [[ $signature != "$_PROGRESS_SENT_SIGNATURE" ]] || return 0
    # One update a second is plenty, and apt reports far more.
    if ((_PROGRESS_SENT_AT > 0 && now - _PROGRESS_SENT_AT < 1)); then
        return 0
    fi

    if ((_PROGRESS_INDEX > 0 && _PROGRESS_TOTAL > 0)); then
        count=" $_PROGRESS_INDEX of $_PROGRESS_TOTAL"
    elif ((_PROGRESS_INDEX > 0)) && [[ -n $_PROGRESS_UNIT ]]; then
        count=" $_PROGRESS_UNIT $_PROGRESS_INDEX"
    fi
    body="$_PROGRESS_VERB$count - $(_progress_duration "$elapsed")"
    [[ -z $_PROGRESS_CURRENT ]] || body="$_PROGRESS_CURRENT"$'\n'"$body"

    _PROGRESS_SENT_SIGNATURE=$signature
    _PROGRESS_SENT_AT=$now
    _progress_deliver "$_PROGRESS_TITLE" "$body" "$bar" view-refresh
}

progress_finish() {
    ((_PROGRESS_ACTIVE == 1 && _PROGRESS_FINISHED == 0)) || return 0
    _PROGRESS_FINISHED=1
    local outcome=${1:-success} quiet=0 message='' title icon body='' line
    shift || true
    while (($# > 0)); do
        case $1 in
        --quiet) quiet=1 ;;
        --message)
            message=${2:-}
            shift
            ;;
        *) break ;;
        esac
        shift
    done

    if [[ $outcome == success && $quiet == 1 ]]; then
        _progress_close
        _progress_cleanup
        return 0
    fi

    case $outcome in
    failure)
        title="$_PROGRESS_TITLE failed"
        icon=dialog-error
        ;;
    warning)
        title="$_PROGRESS_TITLE finished with problems"
        icon=dialog-warning
        ;;
    *)
        title="$_PROGRESS_TITLE finished"
        icon=dialog-information
        ;;
    esac
    [[ -z $message ]] || body=$message
    for line in "$@"; do
        [[ -n $line ]] || continue
        body+="${body:+$'\n'}$line"
    done
    body+="${body:+$'\n'}Took $(_progress_duration $(($(_progress_now) - _PROGRESS_START)))."
    _progress_deliver "$title" "$body" -1 "$icon"
    _progress_cleanup
    return 0
}

progress_abort() {
    ((_PROGRESS_ACTIVE == 1 && _PROGRESS_FINISHED == 0)) || return 0
    local status=${1:-0}
    if [[ $status =~ ^[0-9]+$ ]] && ((status != 0)); then
        progress_finish failure --message "Stopped unexpectedly (exit status $status)."
    else
        _PROGRESS_FINISHED=1
        _progress_close
        _progress_cleanup
    fi
    return 0
}
