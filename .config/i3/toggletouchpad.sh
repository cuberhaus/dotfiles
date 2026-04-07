#!/bin/bash
if ! command -v synclient &>/dev/null; then
    echo "error: synclient is not installed" >&2
    exit 1
fi
if synclient -l | grep "TouchpadOff .*=.*0" ; then
    synclient TouchpadOff=1 ;
else
    synclient TouchpadOff=0 ;
fi
