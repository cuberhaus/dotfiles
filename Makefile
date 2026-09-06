# Dotfiles Makefile — common tasks in one place
# Usage: make <target>

STOW      := stow
STOW_DIR  := $(CURDIR)
TARGET    := $(HOME)
ifeq ($(OS),Windows_NT)
PYTHON ?= python
GITLEAKS ?= $(LOCALAPPDATA)/Microsoft/WinGet/Links/gitleaks.exe
else
PYTHON ?= python3
endif
BOOTSTRAP_ARGS ?= --unattended
PROFILE ?= auto
HIGH_DPI ?= no
REPAIR ?=
RESTORE_APPS ?=
RESTORE_APPLY ?= 0
CUBERHAUS_WORKSPACE_DIR ?= ../cuberhaus-workspace
CUBERHAUS_WORKSPACE_REPO ?= cuberhaus/cuberhaus-workspace
RESTORE_WORKSPACE_SKILLS ?= 1
POL_SERVER_HOST ?= home-nas

.PHONY: help check-stow install uninstall restow dry-run config-status config-diff config-import lint test check fix doctor audit-installation repair hooks test-shutdown-fix install-automations uninstall-automations uninstall-automations-dry-run maintenance-status maintenance-logs maintenance-digest restore-app restore-apps update submodules antigen-update skip-worktree workspace bootstrap-workspace dual-boot-utc bootstrap-unattended enroll-pol-server enroll-pol-server-maintenance revoke-pol-server-maintenance reboot-pol-server upgrade-pol-server bootstrap-pol-server audit-pol-server audit-pol-server-hardware audit-pol-server-storage prepare-pol-server-storage audit-pol-server-samba configure-pol-server-samba set-pol-server-samba-password generate-pol-server-samba-password test-pol-server-samba-access audit-pol-server-backup prepare-pol-server-backup generate-pol-server-restic-password run-pol-server-backup check-pol-server-backup test-pol-server-backup-restore install-pol-server-immich configure-pol-server-immich audit-pol-server-immich start-pol-server-immich stop-pol-server-immich backup-pol-server-immich upgrade-pol-server-immich restore-pol-server-immich restore-pol-server-immich-apply install-pol-server-monitoring audit-pol-server-monitoring bootstrap-pol-server-wireguard install-pol-server-wireguard audit-pol-server-wireguard generate-pol-server-wireguard-client show-pol-server-wireguard-qr revoke-pol-server-wireguard-client configure-pol-server-duckdns start-pol-server-smart-long-kingston start-pol-server-smart-long-micron test-pol-server-thermals configure-pol-server-github-mirrors sync-pol-server-github-mirrors audit-pol-server-github-mirrors bootstrap-arch bootstrap-manjaro bootstrap-ubuntu bootstrap-ubuntu-windows bootstrap-mac bootstrap-work uninstall-arch uninstall-manjaro uninstall-ubuntu uninstall-mac uninstall-work skills-list skills-update skills-restore

.DEFAULT_GOAL := help

##@ General

help: ## Show this help
	@awk ' \
		/^##@/      { printf "\n\033[1m%s\033[0m\n", substr($$0, 5); next } \
		/^[a-zA-Z_-]+:.*##/ { \
			split($$0, parts, ":.*## *"); \
			printf "  \033[36m%-18s\033[0m %s\n", parts[1], parts[2] \
		} \
	' $(MAKEFILE_LIST)

##@ Stow

check-stow:
	@if ! command -v "$(STOW)" >/dev/null 2>&1; then \
		echo "GNU Stow is required. Install it, then rerun your make command:"; \
		echo "  Ubuntu/Debian: sudo apt install stow"; \
		echo "  Arch/Manjaro:  sudo pacman -S stow"; \
		echo "  macOS:         brew install stow"; \
		exit 127; \
	fi

install: check-stow ## Symlink dotfiles into $HOME via stow (backs up conflicts first)
	@bash .local/scripts/stow-backup-conflicts
	$(STOW) -v -t $(TARGET) -d $(dir $(STOW_DIR)) $(notdir $(STOW_DIR))

uninstall: check-stow uninstall-automations ## Disable automations, remove symlinks, and restore backed-up files
	@bash .local/scripts/stow-uninstall

restow: check-stow ## Re-stow (uninstall then install — cleans stale links)
	$(STOW) -v -R -t $(TARGET) -d $(dir $(STOW_DIR)) $(notdir $(STOW_DIR))

dry-run: check-stow ## Simulate stow and report conflicts (no changes made)
	$(STOW) -v -n -t $(TARGET) -d $(dir $(STOW_DIR)) $(notdir $(STOW_DIR)) 2>&1

config-status: check-stow ## Show Stow deployment drift and source checkout changes
	$(PYTHON) .local/scripts/config_lifecycle.py status

config-diff: ## Diff tracked managed files against their current $$HOME targets
	$(PYTHON) .local/scripts/config_lifecycle.py diff

config-import: check-stow ## Preview home-to-repo import; pass APPLY=1 to adopt changes
	$(PYTHON) .local/scripts/config_lifecycle.py import $(if $(filter 1,$(APPLY)),--apply,)

##@ Quality

lint: ## Run shellcheck on all shell scripts
	bash .local/scripts/lint.sh

test: ## Run deterministic unit tests
	$(PYTHON) tests/test_installation_audit.py
	bash tests/test_brightness.sh
	bash tests/test_bootstrap_stow.sh
	bash tests/test_bootstrap_machine_state.sh
	bash tests/test_pol_server_bootstrap.sh
	bash tests/test_pol_server_storage.sh
	bash tests/test_pol_server_samba.sh
	bash tests/test_pol_server_backup.sh
	bash tests/test_pol_server_immich.sh
	bash tests/test_pol_server_monitoring.sh
	bash tests/test_pol_server_wireguard.sh
	bash tests/test_pol_server_github_mirror.sh
	bash tests/test_pol_server_hardware.sh
	bash tests/test_obsidian_bootstrap.sh
	bash tests/test_bootstrap_work.sh
	bash tests/test_shell_path.sh

check: lint test ## Run tests and all linters (shellcheck + markdownlint + vint). Fails if any tool is missing.
	@echo ""
	@echo "==> Running Gitleaks..."
ifeq ($(OS),Windows_NT)
	@"$(GITLEAKS)" git --redact --no-banner
else
	bash .local/scripts/gitleaks.sh
endif
	bash .local/scripts/check-formatting.sh
	@echo ""
	@echo "==> All checks complete."

test-shutdown-fix: ## Test the permanent shutdown-fix kernelstub path
	bash .local/scripts/test_permanent_shutdown_fix.sh

fix: ## Auto-fix markdown issues (markdownlint --fix)
	bash .local/scripts/check-formatting.sh --fix

doctor: ## Check tools, symlinks, configuration, packages, environment, and automations
	@bash .local/scripts/doctor.sh "$(PROFILE)"

audit-installation: ## Report drift between this repo and the installed machine (PROFILE=auto|arch|manjaro|ubuntu|ubuntu-windows|mac|work)
	$(PYTHON) .local/scripts/audit_installation.py --profile "$(PROFILE)"

repair: ## Re-run one idempotent setup step (REPAIR=config|aliases|environment|vim|automations|keyboard; PROFILE=auto|...)
	bash .local/scripts/repair-installation "$(REPAIR)" "$(PROFILE)"

##@ Setup

hooks: ## Install git pre-commit hook (runs shellcheck on staged files)
	cp .local/scripts/hooks/pre-commit .git/hooks/pre-commit
	chmod +x .git/hooks/pre-commit
	@echo "Pre-commit hook installed."

install-automations: ## Install native package-maintenance and workspace-pull schedules
	bash .local/scripts/automation/install

uninstall-automations: ## Disable and remove native automation schedules
	bash .local/scripts/automation/uninstall

uninstall-automations-dry-run: ## Preview removal of native automation schedules
	bash .local/scripts/automation/uninstall --dry-run

maintenance-status: ## Show native maintenance schedule status
	bash .local/scripts/automation/observe status

maintenance-logs: ## Show recent maintenance logs (LINES=100)
	bash .local/scripts/automation/observe logs "$(or $(LINES),100)"

maintenance-digest: ## Summarize last successful maintenance runs
	bash .local/scripts/automation/observe digest

restore-app: ## Preview app-data restore (APP=thunderbird|calibre|anki; APPLY=1 to copy)
	bash .local/scripts/restore-app-data "$(APP)" $(if $(filter 1,$(APPLY)),--apply,)

restore-apps: ## Restore selected apps after setup (RESTORE_APPS="..."; RESTORE_APPLY=1 to copy)
	@if [ -z "$(strip $(RESTORE_APPS))" ]; then echo "No app data requested; set RESTORE_APPS to thunderbird, calibre, and/or anki."; else for app in $(RESTORE_APPS); do bash .local/scripts/restore-app-data "$$app" $(if $(filter 1,$(RESTORE_APPLY)),--apply,); done; fi

# Files that are intentionally tracked (for the settings we care about) but
# change constantly at runtime — apps rewrite them on every launch.
# skip-worktree tells git to stop noticing local changes while keeping the
# committed version in the repo.  Run this once after cloning.
SKIP_WORKTREE_FILES := \
	.config/warp-terminal/user_preferences.json \
	.config/libreoffice/4/user/config/javasettings_Linux_X86_64.xml

skip-worktree: ## Ignore runtime changes to volatile config files (run once after cloning)
	git update-index --skip-worktree $(SKIP_WORKTREE_FILES)
	@echo "skip-worktree applied to:"
	@for f in $(SKIP_WORKTREE_FILES); do echo "  $$f"; done
	@echo "To commit a real settings change: git update-index --no-skip-worktree <file>"

##@ Skills

skills-list: ## List project skills installed via skills-lock.json
	@npx skills list -p

skills-update: ## Update project skills and show what changed
	@npx skills update -p -y
	@echo ""
	@echo "Changed skill files:"
	@git diff --name-only -- .agents/skills skills-lock.json || true

skills-restore: ## Download/restore pinned skills from skills-lock.json
	@npx skills experimental_install

##@ Workspace integration (cuberhaus multi-root)

bootstrap-workspace: ## Clone the private workspace repository when needed, then sync it
	@if [ ! -d "$(CUBERHAUS_WORKSPACE_DIR)/.git" ]; then \
		if [ -e "$(CUBERHAUS_WORKSPACE_DIR)" ]; then \
			echo "Workspace path exists but is not a Git checkout: $(CUBERHAUS_WORKSPACE_DIR)" >&2; \
			exit 2; \
		fi; \
		if ! command -v gh >/dev/null 2>&1; then \
			echo "GitHub CLI is required to clone $(CUBERHAUS_WORKSPACE_REPO)." >&2; \
			exit 127; \
		fi; \
		echo "Cloning $(CUBERHAUS_WORKSPACE_REPO) into $(CUBERHAUS_WORKSPACE_DIR)..."; \
		GH_PROMPT_DISABLED=1 gh repo clone "$(CUBERHAUS_WORKSPACE_REPO)" "$(CUBERHAUS_WORKSPACE_DIR)" || exit $$?; \
	fi; \
	if [ "$(RESTORE_WORKSPACE_SKILLS)" = 1 ] && \
		[ ! -e "$(CUBERHAUS_WORKSPACE_DIR)/.agents/skills/.restore-complete" ]; then \
		echo "Restoring approved pinned workspace skills..."; \
		npm_config_yes=true $(MAKE) --no-print-directory -C "$(CUBERHAUS_WORKSPACE_DIR)" skills-restore || exit $$?; \
		mkdir -p "$(CUBERHAUS_WORKSPACE_DIR)/.agents/skills"; \
		touch "$(CUBERHAUS_WORKSPACE_DIR)/.agents/skills/.restore-complete"; \
	fi
	@bash "$(CUBERHAUS_WORKSPACE_DIR)/sync.sh"

workspace: bootstrap-workspace ## Sync workspace files, refresh repos.json, then audit workspace policies
	$(PYTHON) "$(CUBERHAUS_WORKSPACE_DIR)/scripts/build-repos.py"
	$(PYTHON) "$(CUBERHAUS_WORKSPACE_DIR)/scripts/audit-policies.py"

##@ Submodules

submodules: ## Init and update all git submodules
	git submodule sync --recursive
	git submodule update --init --recursive

update: submodules ## Pull latest for every submodule
	git submodule foreach --recursive git pull origin HEAD

antigen-update: ## Fetch the latest antigen.zsh from GitHub
	curl -fsSL https://git.io/antigen > .config/antigen/antigen.zsh
	@echo "antigen.zsh updated. Restart your shell to pick up changes."

##@ Bootstrap (OS-specific)
# Each bootstrap target runs the OS-specific bash script, then deploys
# cuberhaus-workspace/ sources into ~/cuberhaus without API/policy maintenance
# without a separate manual step. Setup remains one command per OS.

audit-pol-server: ## Report drift from the tracked pol-server baseline
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --check

enroll-pol-server: ## Install/update pol-server's root-owned bootstrap (interactive sudo)
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --enroll

enroll-pol-server-maintenance: ## Enroll pol-server with eight-hour full sudo access
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --enroll-maintenance

revoke-pol-server-maintenance: ## Revoke temporary full sudo access immediately
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --revoke-maintenance

reboot-pol-server: ## Reboot pol-server during an active maintenance window
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --reboot

upgrade-pol-server: ## Apply current Debian upgrades during a maintenance window
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --upgrade

bootstrap-pol-server: ## Converge the enrolled pol-server baseline without a password
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --apply

audit-pol-server-hardware: ## Print a redacted hardware and SMART report from pol-server
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --hardware-report

audit-pol-server-storage: ## Verify the model-pinned Kingston storage preflight
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --check-storage

prepare-pol-server-storage: ## Erase the approved Kingston disk and create /srv/storage
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --prepare-storage

audit-pol-server-samba: ## Audit Samba configuration, account, and service state
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --check-samba

configure-pol-server-samba: ## Stage authenticated LAN-only Samba configuration
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --configure-samba

set-pol-server-samba-password: ## Enroll the Samba password and activate smbd
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --set-samba-password

generate-pol-server-samba-password: ## Generate a private local Samba password and activate smbd
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --generate-samba-password

test-pol-server-samba-access: ## Test authenticated SMB3 file operations and guest denial
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --test-samba-access

audit-pol-server-backup: ## Audit the WD mount, Restic repository, and timers
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --check-backup

prepare-pol-server-backup: ## Mount the accepted existing WD partition without formatting it
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --prepare-backup

generate-pol-server-restic-password: ## Generate a private Restic password and initialize the repository
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --generate-restic-password

run-pol-server-backup: ## Back up /srv/storage to the external WD now
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --run-backup

check-pol-server-backup: ## Check Restic metadata and a rotating repository data subset
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --check-backup-repository

test-pol-server-backup-restore: ## Restore and compare the stable backup acceptance probe
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --restore-test-backup

install-pol-server-immich: ## Install Docker Engine from Docker's official Debian repository
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --install-immich

configure-pol-server-immich: ## Configure and start the pinned Immich stack
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --configure-immich

audit-pol-server-immich: ## Audit Immich storage placement, LAN binding, and health
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --check-immich

start-pol-server-immich: ## Reconcile and start the configured Immich stack
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --start-immich

stop-pol-server-immich: ## Stop Immich without deleting its data
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --stop-immich

backup-pol-server-immich: ## Create a verified database dump before the next storage snapshot
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --backup-immich

upgrade-pol-server-immich: ## Run a full backup before pulling and reconciling Immich
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --upgrade-immich

restore-pol-server-immich: ## Validate the preserved WD recovery set without changing data
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --restore-immich

restore-pol-server-immich-apply: ## Restore the validated WD recovery set and retain rollback copies
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --restore-immich-apply

install-pol-server-monitoring: ## Install native LAN-only Netdata monitoring
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --install-monitoring

audit-pol-server-monitoring: ## Audit Netdata configuration, service, API, and listeners
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --check-monitoring

bootstrap-pol-server-wireguard: ## Run the complete interactive WireGuard rollout and acceptance
	bash server/pol-server/wireguard-bootstrap --host "$(POL_SERVER_HOST)"

install-pol-server-wireguard: ## Install the restricted WireGuard server and firewall
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --install-wireguard

audit-pol-server-wireguard: ## Audit WireGuard, fixed-peer, firewall, and DDNS state
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --check-wireguard

generate-pol-server-wireguard-client: ## Generate and enroll the private pol-iphone profile locally
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --generate-wireguard-client

show-pol-server-wireguard-qr: ## Show the private pol-iphone QR in this interactive terminal
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --show-wireguard-qr

revoke-pol-server-wireguard-client: ## Revoke the pol-iphone WireGuard peer
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --revoke-wireguard-client

configure-pol-server-duckdns: ## Store the DuckDNS token and enable its fallback timer
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --configure-duckdns

audit-pol-server-wd-backup: ## Print the external WD backup disk SMART report
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --audit-wd-backup

start-pol-server-smart-long-kingston: ## Start the Kingston data SSD long SMART test
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --start-smart-long-kingston

start-pol-server-smart-long-micron: ## Start the Micron system SSD long SMART test
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --start-smart-long-micron

start-pol-server-smart-long-wd-backup: ## Start the external WD backup disk SMART long test
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --start-smart-long-wd-backup

test-pol-server-thermals: ## Run a bounded two-minute CPU thermal test
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --thermal-load

configure-pol-server-github-mirrors: ## Store a read-only GitHub token on pol-server
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --configure-github-mirrors

sync-pol-server-github-mirrors: ## Mirror all cuberhaus GitHub repositories now
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --sync-github-mirrors

audit-pol-server-github-mirrors: ## Verify pol-server's GitHub mirrors and schedule
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --check-github-mirrors

configure-pol-server-rss-email: ## Store validated Gmail settings and enable RSS notifications
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --configure-rss-email

run-pol-server-rss-email: ## Check the configured RSS feed for new items now
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --run-rss-email

test-pol-server-rss-email: ## Send one delivery test to the configured RSS recipient
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --test-rss-email

email-pol-server-wd-report: ## Email the current redacted WD SMART report
	bash server/pol-server/deploy --host "$(POL_SERVER_HOST)" --email-wd-report

dual-boot-utc: ## Configure this physical Linux machine to use a UTC hardware clock
	bash -c 'source .local/scripts/bootstrap/base_functions; DUAL_BOOT_UTC=true; configure_dual_boot_utc_rtc'

bootstrap-unattended: ## Provision without prompts (PROFILE required; HIGH_DPI=no)
	@case "$(PROFILE)" in arch|manjaro|ubuntu|ubuntu-windows|mac|work) ;; *) echo "PROFILE must be arch, manjaro, ubuntu, ubuntu-windows, mac, or work" >&2; exit 2 ;; esac
	@$(MAKE) --no-print-directory bootstrap-$(PROFILE) BOOTSTRAP_ARGS="--unattended --high-dpi=$(HIGH_DPI)"

bootstrap-arch: ## Run Arch bootstrap (then deploy workspace files)
	bash .local/scripts/bootstrap/arch $(BOOTSTRAP_ARGS)
	@$(MAKE) --no-print-directory bootstrap-workspace
	@$(MAKE) --no-print-directory install-automations
	@$(MAKE) --no-print-directory restore-apps

bootstrap-manjaro: ## Run Manjaro bootstrap (then deploy workspace files)
	bash .local/scripts/bootstrap/manjaro $(BOOTSTRAP_ARGS)
	@$(MAKE) --no-print-directory bootstrap-workspace
	@$(MAKE) --no-print-directory install-automations
	@$(MAKE) --no-print-directory restore-apps

bootstrap-ubuntu: ## Run Ubuntu bootstrap (then deploy workspace files)
	bash .local/scripts/bootstrap/ubuntu $(BOOTSTRAP_ARGS)
	@$(MAKE) --no-print-directory bootstrap-workspace
	@$(MAKE) --no-print-directory install-automations
	@$(MAKE) --no-print-directory restore-apps

bootstrap-ubuntu-windows: ## Run Ubuntu-on-WSL bootstrap (no GUI apps, then deploy workspace files)
	bash .local/scripts/bootstrap/ubuntu_windows $(BOOTSTRAP_ARGS)
	@$(MAKE) --no-print-directory bootstrap-workspace
	@$(MAKE) --no-print-directory install-automations
	@$(MAKE) --no-print-directory restore-apps

bootstrap-mac: ## Run macOS bootstrap (then deploy workspace files)
	bash .local/scripts/bootstrap/mac $(BOOTSTRAP_ARGS)
	@$(MAKE) --no-print-directory bootstrap-workspace
	@$(MAKE) --no-print-directory install-automations
	@$(MAKE) --no-print-directory restore-apps

bootstrap-work: ## Run work machine bootstrap (Ubuntu + NVIDIA, then deploy workspace files)
	bash .local/scripts/bootstrap/work $(BOOTSTRAP_ARGS)
	@$(MAKE) --no-print-directory bootstrap-workspace
	@$(MAKE) --no-print-directory install-automations
	@$(MAKE) --no-print-directory restore-apps

##@ Uninstall (OS-specific)

uninstall-arch: ## Run Arch uninstaller
	bash .local/scripts/bootstrap/uninstall_arch

uninstall-manjaro: ## Run Manjaro uninstaller
	bash .local/scripts/bootstrap/uninstall_manjaro

uninstall-ubuntu: ## Run Ubuntu uninstaller
	bash .local/scripts/bootstrap/uninstall_ubuntu

uninstall-mac: ## Run macOS uninstaller
	bash .local/scripts/bootstrap/uninstall_mac

uninstall-work: ## Run work machine uninstaller
	bash .local/scripts/bootstrap/uninstall_work

##@ Understand (knowledge graph)

.PHONY: understand-dashboard
understand-dashboard: ## Launch the Understand Anything knowledge-graph dashboard (graph dir = repo root)
	@node -e "require(require('os').homedir()+'/.understand-anything/repo/understand-anything-plugin/packages/dashboard/launch.cjs')"
