#!/bin/bash

i3-msg "workspace "2: Work"; append_layout $HOME/.config/i3-layout-manager/layouts/layout-WORK.json"

(termite &)
(termite &)
(chromium &)
