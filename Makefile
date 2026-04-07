# Dotfiles Makefile — common tasks in one place
# Usage: make <target>

STOW      := stow
STOW_DIR  := $(shell pwd)
TARGET    := $(HOME)

.PHONY: help install uninstall restow dry-run lint check hooks update submodules antigen-update skip-worktree bootstrap-arch bootstrap-manjaro bootstrap-ubuntu bootstrap-mac bootstrap-work

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*##' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*##"}; {printf "  \033[36m%-20s\033[0m %s\n", $$1, $$2}'

# ---------------------------------------------------------------------------
# Stow
# ---------------------------------------------------------------------------

install: ## Symlink dotfiles into $HOME via stow (backs up conflicts first)
	@bash .local/scripts/stow-backup-conflicts
	$(STOW) -v -t $(TARGET) -d $(dir $(STOW_DIR)) $(notdir $(STOW_DIR))

uninstall: ## Remove symlinks from $HOME and restore backed-up files
	@bash .local/scripts/stow-uninstall

restow: ## Re-stow (uninstall then install — cleans stale links)
	$(STOW) -v -R -t $(TARGET) -d $(dir $(STOW_DIR)) $(notdir $(STOW_DIR))

dry-run: ## Simulate stow and report conflicts (no changes made)
	$(STOW) -v -n -t $(TARGET) -d $(dir $(STOW_DIR)) $(notdir $(STOW_DIR)) 2>&1

# ---------------------------------------------------------------------------
# Quality
# ---------------------------------------------------------------------------

lint: ## Run shellcheck on all shell scripts
	bash .local/scripts/lint.sh

check: lint ## Run all linters (shellcheck + markdownlint + vint)
	@echo ""
	@echo "==> Running markdownlint..."
	@if command -v markdownlint-cli2 >/dev/null 2>&1; then \
		markdownlint-cli2 README.md .local/README.md .local/xdg/wallpapers/README.md; \
	elif command -v markdownlint >/dev/null 2>&1; then \
		markdownlint README.md .local/README.md .local/xdg/wallpapers/README.md; \
	else \
		echo "  markdownlint not found, skipping (npm install -g markdownlint-cli2)"; \
	fi
	@echo ""
	@echo "==> Running vint (vimrc)..."
	@if command -v vint >/dev/null 2>&1; then \
		vint --style-problem .vim/vimrc || true; \
	else \
		echo "  vint not found, skipping (pip install vim-vint)"; \
	fi
	@echo ""
	@echo "==> All checks complete."

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

# ---------------------------------------------------------------------------
# Submodules
# ---------------------------------------------------------------------------

submodules: ## Init and update all git submodules
	git submodule sync --recursive
	git submodule update --init --recursive

update: submodules ## Pull latest for every submodule
	git submodule foreach --recursive git pull origin HEAD

antigen-update: ## Fetch the latest antigen.zsh from GitHub
	curl -fsSL https://git.io/antigen > .config/antigen/antigen.zsh
	@echo "antigen.zsh updated. Restart your shell to pick up changes."

# ---------------------------------------------------------------------------
# Bootstrap (OS-specific)
# ---------------------------------------------------------------------------

bootstrap-arch: ## Run Arch bootstrap
	bash .local/scripts/bootstrap/arch

bootstrap-manjaro: ## Run Manjaro bootstrap
	bash .local/scripts/bootstrap/manjaro

bootstrap-ubuntu: ## Run Ubuntu bootstrap
	bash .local/scripts/bootstrap/ubuntu

bootstrap-mac: ## Run macOS bootstrap
	bash .local/scripts/bootstrap/mac

bootstrap-work: ## Run work machine bootstrap (Ubuntu + NVIDIA)
	bash .local/scripts/bootstrap/work
