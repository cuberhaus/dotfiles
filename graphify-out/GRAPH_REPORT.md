# Graph Report - .  (2026-06-19)

## Corpus Check
- Corpus is ~25,596 words - fits in a single context window. You may not need a graph.

## Summary
- 230 nodes · 277 edges · 42 communities (19 shown, 23 thin omitted)
- Extraction: 81% EXTRACTED · 17% INFERRED · 1% AMBIGUOUS · INFERRED: 48 edges (avg confidence: 0.82)
- Token cost: 0 input · 0 output

## Community Hubs (Navigation)
- [[_COMMUNITY_Install Provisioning Functions|Install Provisioning Functions]]
- [[_COMMUNITY_Linux Uninstall Routines|Linux Uninstall Routines]]
- [[_COMMUNITY_MacWork Setup & Teardown|Mac/Work Setup & Teardown]]
- [[_COMMUNITY_Git Repo Helpers|Git Repo Helpers]]
- [[_COMMUNITY_Cinnamon Config & Theming|Cinnamon Config & Theming]]
- [[_COMMUNITY_Claude Cost Stats|Claude Cost Stats]]
- [[_COMMUNITY_Stow Management Helpers|Stow Management Helpers]]
- [[_COMMUNITY_System Fetch & Prompt|System Fetch & Prompt]]
- [[_COMMUNITY_YouCompleteMe Installer|YouCompleteMe Installer]]
- [[_COMMUNITY_BrightnessVolume Control|Brightness/Volume Control]]
- [[_COMMUNITY_PipeWire Audio Routing|PipeWire Audio Routing]]
- [[_COMMUNITY_Theme Toggling|Theme Toggling]]
- [[_COMMUNITY_Cinnamon AppletsDesklets|Cinnamon Applets/Desklets]]
- [[_COMMUNITY_Bloat Install (cross-OS)|Bloat Install (cross-OS)]]
- [[_COMMUNITY_Optional Install (cross-OS)|Optional Install (cross-OS)]]
- [[_COMMUNITY_Doctor Health Check|Doctor Health Check]]
- [[_COMMUNITY_Lint Helpers|Lint Helpers]]
- [[_COMMUNITY_Spotify Controls|Spotify Controls]]
- [[_COMMUNITY_Lint Runner|Lint Runner]]
- [[_COMMUNITY_Shutdown Fix|Shutdown Fix]]
- [[_COMMUNITY_Cinnamon Install|Cinnamon Install]]
- [[_COMMUNITY_Games Install|Games Install]]
- [[_COMMUNITY_GNOME Install|GNOME Install]]
- [[_COMMUNITY_Plasma Install|Plasma Install]]
- [[_COMMUNITY_VirtualBox Install|VirtualBox Install]]
- [[_COMMUNITY_Error Helper|Error Helper]]
- [[_COMMUNITY_Info Helper|Info Helper]]
- [[_COMMUNITY_Warn Helper|Warn Helper]]
- [[_COMMUNITY_App Store Install|App Store Install]]
- [[_COMMUNITY_macOS Defaults|macOS Defaults]]
- [[_COMMUNITY_Albert Install|Albert Install]]
- [[_COMMUNITY_Anaconda Install|Anaconda Install]]
- [[_COMMUNITY_High DPI Screen|High DPI Screen]]
- [[_COMMUNITY_Optional Snaps Install|Optional Snaps Install]]
- [[_COMMUNITY_Touchpad Speed|Touchpad Speed]]
- [[_COMMUNITY_Termite Update|Termite Update]]
- [[_COMMUNITY_Vim Update|Vim Update]]

## God Nodes (most connected - your core abstractions)
1. `main()` - 6 edges
2. `ycm.sh script` - 6 edges
3. `find_transcripts()` - 4 edges
4. `parse_since()` - 4 edges
5. `event_timestamp()` - 3 edges
6. `cost_for()` - 3 edges
7. `pipewire.sh script` - 3 edges
8. `set_default_playback_device_next()` - 3 edges
9. `print_block()` - 3 edges
10. `humanize()` - 2 edges

## Surprising Connections (you probably didn't know these)
- None detected - all connections are within the same source files.

## Import Cycles
- 1-file cycle: `.local/scripts/claude-cost-stats.py -> .local/scripts/claude-cost-stats.py`

## Hyperedges (group relationships)
- **Git Workflow Toolkit** — bin_clone_all, bin_git_ahead, bin_git_clean_branches, bin_git_recurse, bin_yolo [INFERRED 0.75]
- **Repo Scan And Base Branch Detection Pattern** — bin_git_ahead, bin_git_clean_branches, bin_git_ahead_detect_base, bin_git_clean_branches_detect_default [INFERRED 0.85]
- **Desktop Notification Hardware Controls** — bin_changevolume, bin_changebrightness, dunstify [INFERRED 0.75]
- **Shared bootstrap base (base_functions)** — bootstrap_arch, bootstrap_manjaro, bootstrap_mac, bootstrap_ubuntu, bootstrap_ubuntu_windows, bootstrap_base_functions [INFERRED 0.95]
- **Per-OS provisioning layer** — bootstrap_arch_functions, bootstrap_mac_functions, bootstrap_ubuntu_functions [INFERRED 0.85]
- **Cross-OS AI tools install step** — bootstrap_arch_functions_ai_tools_install, bootstrap_mac_functions_ai_tools_install, bootstrap_ubuntu_functions_ai_tools_install [INFERRED 0.80]
- **OS Uninstaller Teardown Suite** — bootstrap_uninstall_arch, bootstrap_uninstall_mac, bootstrap_uninstall_manjaro, bootstrap_uninstall_ubuntu, bootstrap_uninstall_work [INFERRED 0.75]
- **TUI Checklist Dispatch Flow** — bootstrap_uninstall_tui_run_selected_uninstalls, bootstrap_uninstall_tui_show_checklist, bootstrap_uninstall_tui__checklist_whiptail, bootstrap_uninstall_tui__checklist_dialog, bootstrap_uninstall_tui__checklist_text [INFERRED 0.75]
- **Work Machine Install Pipeline** — bootstrap_work, bootstrap_work_functions_shutdown_fix, bootstrap_work_functions_nvidia_install, bootstrap_work_functions_dev_tools_install, bootstrap_work_functions_node_install, bootstrap_work_functions_python_install, bootstrap_work_functions_docker_install, bootstrap_work_functions_gcloud_install, bootstrap_work_functions_gui_apps_install, bootstrap_work_functions_high_dpi_screen [INFERRED 0.75]
- **Cinnamon Theming Flow** — cinnamon_path_cinnamon_dump_config, cinnamon_path_cinnamon_load_config, cinnamon_path_dark_theme, cinnamon_path_light_theme, scripts_toggle_theme [INFERRED 0.85]
- **Stow Install/Uninstall Lifecycle** — scripts_stow_backup_conflicts, scripts_stow_uninstall, stow [INFERRED 0.85]
- **Spotify Now-Playing Flow** — scripts_spot, scripts_spoti, spotify [INFERRED 0.75]

## Communities (42 total, 23 thin omitted)

### Community 0 - "Install Provisioning Functions"
Cohesion: 0.08
Nodes (35): apps_AUR_install, cava_install, activate_reisub, ai_tools_install (arch), arch_and_manjaro, arch_install, base_install (arch), dwall (+27 more)

### Community 1 - "Linux Uninstall Routines"
Cohesion: 0.09
Nodes (24): arch_uninstall, base_uninstall, dwall_uninstall, emacs_uninstall, i3_uninstall, laptop_uninstall, papirus_uninstall, vim_uninstall (+16 more)

### Community 2 - "Mac/Work Setup & Teardown"
Cohesion: 0.09
Nodes (21): brew_cleanup, cli_uninstall, fonts_uninstall, gui_apps_uninstall, brew_uninstall, dev_tools_uninstall, docker_uninstall, gui_apps_uninstall (+13 more)

### Community 3 - "Git Repo Helpers"
Cohesion: 0.19
Nodes (10): detect_jobs, process_repo, detect_base, scan_repo, detect_default, scan_repo, exit_abnormal, mac_key (+2 more)

### Community 4 - "Cinnamon Config & Theming"
Cohesion: 0.21
Nodes (9): dark, exit_abnormal, light, usage, dark, exit_abnormal, light, usage (+1 more)

### Community 5 - "Claude Cost Stats"
Cohesion: 0.27
Nodes (11): datetime, Path, cost_for(), event_timestamp(), find_transcripts(), humanize(), main(), parse_since() (+3 more)

### Community 6 - "Stow Management Helpers"
Cohesion: 0.25
Nodes (7): info, ok, warn, info, ok, warn, stow

### Community 7 - "System Fetch & Prompt"
Cohesion: 0.32
Nodes (6): git_recursive, get_kernel, get_os, get_title, log, tput

### Community 8 - "YouCompleteMe Installer"
Cohesion: 0.43
Nodes (6): ycm.sh script, arch_based_install(), Help(), mac_install(), ubuntu_based_install(), ycm_install()

### Community 9 - "Brightness/Volume Control"
Cohesion: 0.33
Nodes (5): get_brightness, send_notification, Usage, Usage, dunstify

### Community 10 - "PipeWire Audio Routing"
Cohesion: 0.60
Nodes (5): pipewire.sh script, move_sinks_to_new_default(), print_block(), print_format(), set_default_playback_device_next()

### Community 11 - "Theme Toggling"
Cohesion: 0.40
Nodes (4): change_theme, update_gtk_theme, update_wallpaper, Usage

### Community 13 - "Bloat Install (cross-OS)"
Cohesion: 0.67
Nodes (3): bloat_install (arch), bloat_install (mac), bloat_install (ubuntu)

### Community 14 - "Optional Install (cross-OS)"
Cohesion: 0.67
Nodes (3): optional (arch), optional (mac), optional (ubuntu)

## Ambiguous Edges - Review These
- `arch` → `apps_AUR_install`  [AMBIGUOUS]
  bootstrap/arch · relation: calls
- `arch` → `cava_install`  [AMBIGUOUS]
  bootstrap/arch · relation: calls
- `ubuntu` → `brew_install (mac)`  [AMBIGUOUS]
  bootstrap/ubuntu · relation: calls
- `ubuntu_windows` → `brew_install (mac)`  [AMBIGUOUS]
  bootstrap/ubuntu_windows · relation: calls

## Knowledge Gaps
- **6 isolated node(s):** `Path`, `doctor.sh script`, `TOOLS`, `lint.sh script`, `lint_helpers.sh script` (+1 more)
  These have ≤1 connection - possible missing edges or undocumented components.
- **23 thin communities (<3 nodes) omitted from report** — run `graphify query` to explore isolated nodes.

## Suggested Questions
_Questions this graph is uniquely positioned to answer:_

- **What is the exact relationship between `arch` and `apps_AUR_install`?**
  _Edge tagged AMBIGUOUS (relation: calls) - confidence is low._
- **What is the exact relationship between `arch` and `cava_install`?**
  _Edge tagged AMBIGUOUS (relation: calls) - confidence is low._
- **What is the exact relationship between `ubuntu` and `brew_install (mac)`?**
  _Edge tagged AMBIGUOUS (relation: calls) - confidence is low._
- **What is the exact relationship between `ubuntu_windows` and `brew_install (mac)`?**
  _Edge tagged AMBIGUOUS (relation: calls) - confidence is low._
- **What connects `Path`, `Every *.jsonl under ~/.claude/projects/.`, `Parse 'NNd', 'NNh', or YYYY-MM-DD into a tz-aware datetime.` to the rest of the system?**
  _9 weakly-connected nodes found - possible documentation gaps or missing edges._
- **Should `Install Provisioning Functions` be split into smaller, more focused modules?**
  _Cohesion score 0.07928118393234672 - nodes in this community are weakly interconnected._
- **Should `Linux Uninstall Routines` be split into smaller, more focused modules?**
  _Cohesion score 0.09195402298850575 - nodes in this community are weakly interconnected._