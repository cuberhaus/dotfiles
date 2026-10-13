#!/bin/sh
# Copied from https://github.com/jaor/xmobar/issues/239#issuecomment-233206552
# Detects the width of running trayer-srg window (xprop name 'panel')
# and creates an XPM icon of that width, 1px height, and transparent.
# Outputs an <icon>-tag for use in xmobar to display the generated
# XPM icon.
#
# Run script from xmobar:
# `Run Com "/where/ever/trayer-padding-icon.sh" [] "trayerpad" 10`
# and use `%trayerpad%` in your template.


# Function to create a transparent Wx1 px XPM icon
create_xpm_icon () {
    timestamp=$(date)
    pixels=$(printf '%*s' "$1" '' | tr ' ' '.')

    cat << EOF > "$2"
/* XPM *
static char * trayer_pad_xpm[] = {
/* This XPM icon is used for padding in xmobar to */
/* leave room for trayer-srg. It is dynamically   */
/* updated by by trayer-padding-icon.sh which is run  */
/* by xmobar.                                     */
/* Created: ${timestamp} */
/* <w/cols>  <h/rows>  <colors>  <chars per pixel> */
"$1 1 1 1",
/* Colors (none: transparent) */
". c none",
/* Pixels */
"$pixels"
};
EOF
}

# Width of the trayer window
width=$(xprop -name panel 2>/dev/null | grep 'program specified minimum size' | cut -d ' ' -f 5)
case "${width:-}" in
    ''|*[!0-9]*|0) exit 0 ;;
esac

# Icon file name
iconfile="/tmp/trayer-padding-${width}px.xpm"

# If the desired icon does not exist create it
if [ ! -f "$iconfile" ]; then
    create_xpm_icon "$width" "$iconfile"
fi

# Output the icon tag for xmobar
echo "<icon=${iconfile}/>"
