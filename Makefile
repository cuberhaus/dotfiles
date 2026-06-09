# Dotfiles Makefile — common tasks in one place
# Usage: make <target>

STOW      := stow
STOW_DIR  := $(shell pwd)
TARGET    := $(HOME)

.PHONY: help install uninstall restow dry-run lint check fix doctor hooks update submodules antigen-update skip-worktree sync-workspace sync-workspace-dry-run check-parity update-repos audit-policies bootstrap-arch bootstrap-manjaro bootstrap-ubuntu bootstrap-ubuntu-windows bootstrap-mac bootstrap-work uninstall-arch uninstall-manjaro uninstall-ubuntu uninstall-mac uninstall-work skills-list skills-update skills-restore

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

uninstall: ## Remove symlinks from $HOME and restore backed-up files
	@bash .local/scripts/stow-uninstall

restow: ## Re-stow (uninstall then install — cleans stale links)
	$(STOW) -v -R -t $(TARGET) -d $(dir $(STOW_DIR)) $(notdir $(STOW_DIR))

dry-run: ## Simulate stow and report conflicts (no changes made)
	$(STOW) -v -n -t $(TARGET) -d $(dir $(STOW_DIR)) $(notdir $(STOW_DIR)) 2>&1

##@ Quality

lint: ## Run shellcheck on all shell scripts
	bash .local/scripts/lint.sh

check: lint ## Run all linters (shellcheck + markdownlint + vint). Fails if any tool is missing.
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

##@ Setup

hooks: ## Install git pre-commit hook (runs shellcheck on staged files)
	cp .local/scripts/hooks/pre-commit .git/hooks/pre-commit
	chmod +x .git/hooks/pre-commit
	@echo "Pre-commit hook installed."

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

sync-workspace: ## Copy cuberhaus-workspace/ sources into $$HOME/cuberhaus (workspace root)
	bash cuberhaus-workspace/sync.sh

sync-workspace-dry-run: ## Show what sync-workspace would change without writing
	bash cuberhaus-workspace/sync.sh -n

check-parity: ## Verify cuberhaus-workspace/ is byte-identical with the WinDotfiles peer
	@if [ ! -d "$$HOME/cuberhaus/WinDotfiles/cuberhaus-workspace" ]; then \
		echo "WinDotfiles peer not found at $$HOME/cuberhaus/WinDotfiles/cuberhaus-workspace; skipping."; exit 0; \
	fi; \
	diff_out=$$(diff -r --brief \
		--exclude=sync.sh --exclude=sync.ps1 --exclude=README.md --exclude=repos.json \
		cuberhaus-workspace/ "$$HOME/cuberhaus/WinDotfiles/cuberhaus-workspace/" || true); \
	if [ -n "$$diff_out" ]; then \
		echo "Drift detected between dotfiles and WinDotfiles cuberhaus-workspace/:"; \
		echo "$$diff_out"; exit 1; \
	else \
		echo "OK: cuberhaus-workspace/ is byte-identical with WinDotfiles peer (sync.sh/sync.ps1/README.md/repos.json excluded)."; \
	fi

update-repos: ## Refresh repos.json in $$HOME/cuberhaus (GitHub API + local enrichment; needs gh auth + python3)
	python3 cuberhaus-workspace/scripts/build-repos.py

audit-policies: ## Audit cuberhaus repo settings + files against cuberhaus-workspace/policies.json (read-only, exits 1 on drift)
	python3 cuberhaus-workspace/scripts/audit-policies.py

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

bootstrap-arch: ## Run Arch bootstrap
	bash .local/scripts/bootstrap/arch

bootstrap-manjaro: ## Run Manjaro bootstrap
	bash .local/scripts/bootstrap/manjaro

bootstrap-ubuntu: ## Run Ubuntu bootstrap
	bash .local/scripts/bootstrap/ubuntu

bootstrap-ubuntu-windows: ## Run Ubuntu-on-WSL bootstrap (no GUI apps)
	bash .local/scripts/bootstrap/ubuntu_windows

bootstrap-mac: ## Run macOS bootstrap
	bash .local/scripts/bootstrap/mac

bootstrap-work: ## Run work machine bootstrap (Ubuntu + NVIDIA)
	bash .local/scripts/bootstrap/work

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
