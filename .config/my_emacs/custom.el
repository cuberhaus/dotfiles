;;---------------------
;; AUTOMATIC CONFIG    
;;---------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" default))
 '(flycheck-checker-error-threshold 400)
 '(package-selected-packages
   '(matlab-mode autopair markdown-mode systemd systemd-mode reveal-in-osx-finder reveal-in-folder grip-mode markdown-toc auto-yasnippet yasnippets-latex cdlatex PDDL-mode minesweeper mines asm-blox treemacs pomodoro multiple-cursors evil-numbers org-appear auto-complete-auctex auto-complete-config ac-math auto-complete yasnippet predictive org-roam darkroom minions diminish openwith evil-surround evil-smartparens ccls lsp-java lsp-haskell treemacs-tab-bar treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil smartparens vterm-toggle format-all clips-mode eshell-git-prompt vterm eterm-256color auctex latex-preview-pane org-fragtog dired-hide-dotfiles dired-open all-the-icons-dired dired-single latex-mode python-mode dap-mode company-box company lsp-ivy lsp-treemacs lsp-ui lsp-mode haskell-mode git-gutter fzf org-inline-pdf diff-hl diff-hl-mode visual-fill-column org-bullets forge evil-magit magit counsel-projectile projectile evil-commentary evil-commentary-mode hydra evil-collection evil general doom-themes which-key use-package rainbow-delimiters ivy-rich helpful doom-modeline counsel command-log-mode))
 '(warning-suppress-log-types '((comp) (use-package)))
 '(warning-suppress-types '((comp) (comp) (comp) (comp) (comp) (use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))
