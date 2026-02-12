# Dotfiles Makefile — common tasks in one place
# Usage: make <target>

STOW      := stow
STOW_DIR  := $(shell pwd)
TARGET    := $(HOME)

.PHONY: help install uninstall restow lint update submodules bootstrap-arch bootstrap-manjaro bootstrap-ubuntu bootstrap-mac

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*##' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*##"}; {printf "  \033[36m%-20s\033[0m %s\n", $$1, $$2}'

# ---------------------------------------------------------------------------
# Stow
# ---------------------------------------------------------------------------

install: ## Symlink dotfiles into $HOME via stow
	$(STOW) -v -t $(TARGET) -d $(dir $(STOW_DIR)) $(notdir $(STOW_DIR))

uninstall: ## Remove symlinks from $HOME
	$(STOW) -v -D -t $(TARGET) -d $(dir $(STOW_DIR)) $(notdir $(STOW_DIR))

restow: ## Re-stow (uninstall then install — cleans stale links)
	$(STOW) -v -R -t $(TARGET) -d $(dir $(STOW_DIR)) $(notdir $(STOW_DIR))

# ---------------------------------------------------------------------------
# Quality
# ---------------------------------------------------------------------------

lint: ## Run shellcheck on all shell scripts
	bash .local/scripts/lint.sh

# ---------------------------------------------------------------------------
# Submodules
# ---------------------------------------------------------------------------

submodules: ## Init and update all git submodules
	git submodule sync --recursive
	git submodule update --init --recursive

update: submodules ## Pull latest for every submodule
	git submodule foreach --recursive git pull origin HEAD

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
