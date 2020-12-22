#!/bin/bash

i3-msg "workspace "2: Work"; append_layout /home/pol/.config/i3-layout-manager/layouts/layout-WORK.json"

(termite &)
(termite &)
(chromium &)
