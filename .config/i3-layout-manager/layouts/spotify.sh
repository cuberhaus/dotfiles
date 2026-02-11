#!/bin/bash

i3-msg "workspace "3: 3"; append_layout $HOME/.config/i3-layout-manager/layouts/layout-SPOTI.json"

(termite -c ~/.config/termite/cava -e cava &)
(spotify &)
