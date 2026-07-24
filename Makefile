# Dotfiles Makefile — common tasks in one place
# Usage: make <target>

STOW      := stow
STOW_DIR  := $(shell pwd)
TARGET    := $(HOME)
BOOTSTRAP_ARGS ?=
PROFILE ?= auto

.PHONY: help install uninstall restow dry-run lint test check fix doctor audit-installation hooks test-shutdown-fix install-automations uninstall-automations uninstall-automations-dry-run update submodules antigen-update skip-worktree workspace dual-boot-utc bootstrap-arch bootstrap-manjaro bootstrap-ubuntu bootstrap-ubuntu-windows bootstrap-mac bootstrap-work uninstall-arch uninstall-manjaro uninstall-ubuntu uninstall-mac uninstall-work skills-list skills-update skills-restore

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

install: ## Symlink dotfiles into $HOME via stow (backs up conflicts first)
	@bash .local/scripts/stow-backup-conflicts
	$(STOW) -v -t $(TARGET) -d $(dir $(STOW_DIR)) $(notdir $(STOW_DIR))

uninstall: uninstall-automations ## Disable automations, remove symlinks, and restore backed-up files
	@bash .local/scripts/stow-uninstall

restow: ## Re-stow (uninstall then install — cleans stale links)
	$(STOW) -v -R -t $(TARGET) -d $(dir $(STOW_DIR)) $(notdir $(STOW_DIR))

dry-run: ## Simulate stow and report conflicts (no changes made)
	$(STOW) -v -n -t $(TARGET) -d $(dir $(STOW_DIR)) $(notdir $(STOW_DIR)) 2>&1

##@ Quality

lint: ## Run shellcheck on all shell scripts
	bash .local/scripts/lint.sh

test: ## Run deterministic unit tests
	python3 tests/test_installation_audit.py

check: lint test ## Run tests and all linters (shellcheck + markdownlint + vint). Fails if any tool is missing.
	@echo ""
	@echo "==> Running markdownlint..."
	@if command -v markdownlint-cli2 >/dev/null 2>&1; then \
		markdownlint-cli2 README.md .local/README.md .local/xdg/wallpapers/README.md; \
	elif command -v markdownlint >/dev/null 2>&1; then \
		markdownlint README.md .local/README.md .local/xdg/wallpapers/README.md; \
	else \
		echo "  markdownlint not found (npm install -g markdownlint-cli2 or run 'make doctor')"; \
		exit 1; \
	fi
	@echo ""
	@echo "==> Running vint (vimrc)..."
	@if command -v vint >/dev/null 2>&1; then \
		vint --style-problem .vim/vimrc || true; \
	else \
		echo "  vint not found (pip install vim-vint or run 'make doctor')"; \
		exit 1; \
	fi
	@echo ""
	@echo "==> All checks complete."

test-shutdown-fix: ## Test the permanent shutdown-fix kernelstub path
	bash .local/scripts/test_permanent_shutdown_fix.sh

fix: ## Auto-fix markdown issues (markdownlint --fix)
	@if command -v markdownlint-cli2 >/dev/null 2>&1; then \
		markdownlint-cli2 --fix README.md .local/README.md .local/xdg/wallpapers/README.md; \
	elif command -v markdownlint >/dev/null 2>&1; then \
		markdownlint --fix README.md .local/README.md .local/xdg/wallpapers/README.md; \
	else \
		echo "markdownlint not found (npm install -g markdownlint-cli2)"; \
		exit 1; \
	fi

doctor: ## Report missing lint tools and broken symlinks in $$HOME
	@bash .local/scripts/doctor.sh

audit-installation: ## Report drift between this repo and the installed machine (PROFILE=auto|arch|manjaro|ubuntu|ubuntu-windows|mac|work)
	python3 .local/scripts/audit_installation.py --profile "$(PROFILE)"

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

workspace: ## Sync workspace files, refresh repos.json, then audit workspace policies
	bash ../cuberhaus-workspace/sync.sh
	python3 ../cuberhaus-workspace/scripts/build-repos.py
	python3 ../cuberhaus-workspace/scripts/audit-policies.py

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

dual-boot-utc: ## Configure this physical Linux machine to use a UTC hardware clock
	bash -c 'source .local/scripts/bootstrap/base_functions; DUAL_BOOT_UTC=true; configure_dual_boot_utc_rtc'

bootstrap-arch: ## Run Arch bootstrap (then deploy workspace files)
	bash .local/scripts/bootstrap/arch $(BOOTSTRAP_ARGS)
	@bash ../cuberhaus-workspace/sync.sh
	@$(MAKE) --no-print-directory install-automations

bootstrap-manjaro: ## Run Manjaro bootstrap (then deploy workspace files)
	bash .local/scripts/bootstrap/manjaro $(BOOTSTRAP_ARGS)
	@bash ../cuberhaus-workspace/sync.sh
	@$(MAKE) --no-print-directory install-automations

bootstrap-ubuntu: ## Run Ubuntu bootstrap (then deploy workspace files)
	bash .local/scripts/bootstrap/ubuntu $(BOOTSTRAP_ARGS)
	@bash ../cuberhaus-workspace/sync.sh
	@$(MAKE) --no-print-directory install-automations

bootstrap-ubuntu-windows: ## Run Ubuntu-on-WSL bootstrap (no GUI apps, then deploy workspace files)
	bash .local/scripts/bootstrap/ubuntu_windows
	@bash ../cuberhaus-workspace/sync.sh
	@$(MAKE) --no-print-directory install-automations

bootstrap-mac: ## Run macOS bootstrap (then deploy workspace files)
	bash .local/scripts/bootstrap/mac
	@bash ../cuberhaus-workspace/sync.sh
	@$(MAKE) --no-print-directory install-automations

bootstrap-work: ## Run work machine bootstrap (Ubuntu + NVIDIA, then deploy workspace files)
	bash .local/scripts/bootstrap/work $(BOOTSTRAP_ARGS)
	@bash ../cuberhaus-workspace/sync.sh
	@$(MAKE) --no-print-directory install-automations

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
