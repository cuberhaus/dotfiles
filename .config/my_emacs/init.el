;; -*- lexical-binding: t; -*-
 ;; The default is 800 kilobytes.  Measured in bytes.
 ;; (defvar last-file-name-handler-alist file-name-handler-alist)
 ;; (setq gc-cons-threshold 402653184
 ;;     gc-cons-percentage 0.6
 ;;     file-name-handler-alist nil)

(setq use-package-verbose t) ;; debug to see which packages load, and maybe shouldn't, should be off

(defun my/org-tangle-to-multiple-targets ()
  "Tangle the source file to two target files."
  (interactive)
  (let ((source-file  "~/dotfiles/dotfiles/.config/emacs.org")
        (target2-file "~/repos/WinDotfiles/home/dot_emacs.d/init.el")
        (target1-file "~/dotfiles/dotfiles/.config/my_emacs/init.el"))
 (org-babel-tangle-file source-file target1-file "emacs-lisp")
(copy-file target1-file target2-file t)   
    ))

;; Define the target file list
 (setq my-target-files '("~/dotfiles/dotfiles/.config/my_emacs/init.el" "~/repos/WinDotfiles/home/dot_emacs.d/init.el"))

 ;; Intermediate paths
 (setq home-dir (getenv "HOME"))
 (setq docs-dir (concat home-dir "/repos/docs"))
 (setq config-dir (concat  home-dir "/.config"))
 (setq cache-dir (concat home-dir "/.cache"))
 (setq org-dir-string "/org")

 ;; Paths that are used in the code
 (setq user-emacs-dir (concat cache-dir "/emacs/"))
 (setq desktop-dir (concat home-dir "/.emacs.d/"))
 (setq org-roam-dir (concat docs-dir org-dir-string "/roam"))
 (setq journal-dir (concat docs-dir org-dir-string "/Journal.org"))
 (setq tasks-dir (concat docs-dir org-dir-string "/Tasks.org"))
 (setq metrics-dir (concat docs-dir org-dir-string "/Metrics.org"))
 (setq habits-dir (concat docs-dir org-dir-string "/Habits.org"))
 (setq birthday-dir (concat docs-dir org-dir-string "/birthday.org"))
 (setq custom-file-unix (concat config-dir "/my_emacs/custom.el"))
 (setq custom-file-windows (concat home-dir "/.emacs.d/custom.el"))
 (setq languagetool-server-dir (concat cache-dir "/texstudio/dictionaries/LanguageTool-5.7/languagetool-server.jar"))
 (setq spell-fu-dir (concat config-dir "/spell_fu"))
 (setq ispell-personal-dir (concat config-dir "/spell_fu/.pws"))
 (setq url-history-dir (expand-file-name "url/history" user-emacs-dir))
 (setq emacs-babel-config-file (concat home-dir "/dotfiles/dotfiles/.config" "/emacs.org")) ;; this has to be with /dotfiles/dotfiles

 ;; (setq emacs-babel-config-file (concat config-dir "/emacs.org"))
 (setq doom-snippets-dir (concat config-dir "/snippets"))

(cond ((eq system-type 'windows-nt)
       ;; Windows-specific code goes here.
      (setq custom-file custom-file-windows)
       )
       ;; Mac-specific code goes here.
      ((eq system-type 'darwin)
      (setq custom-file custom-file-unix)
       )
       ;; Linux-specific code goes here.
      ((eq system-type 'gnu/linux)
      (setq custom-file custom-file-unix)
       )
       )
    (load custom-file)

;; Initialize package sources
(require 'package) ; bring in package module
; package repositories
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize) ; Initializes package system
(unless package-archive-contents ; unless package exists we refresh package list
 (package-refresh-contents)) 

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package) ; is this package installed, unless its installed install it
   (package-install 'use-package))
(require 'use-package)

(setq use-package-always-ensure t) ;; equivalent to writing :ensure t in all packages
;; makes sure that package is downloaded before use

;; ;; Bootstrap straight.el
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;         'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; ;; Always use straight to install on systems other than Linux
;; (setq straight-use-package-by-default (not (eq system-type 'gnu/linux)))

;; ;; Use straight.el for use-package expressions
;; (straight-use-package 'use-package)

;; Clean up unused repos with `straight-remove-unused-repos'

;; (setq inhibit-startup-message t) ; Disable startup menu
;; (scroll-bar-mode -1) ; Disable the scrollbar
;; (tool-bar-mode -1)
;; ;(tooltip-mode -1) disable tooltips ;; (text displayed when hovering over an element)
;; (set-fringe-mode 10) ; Make some space
;; (menu-bar-mode -1) ;; remove top bar

;; (setq vc-follow-symlinks nil) ;; or never follow them

;; You will most likely need to adjust this font size for your system!
(defvar runemacs/default-font-size 110)

(cond ((eq system-type 'windows-nt)
       ;; Windows-specific code goes here.
       )
      ((eq system-type 'darwin)
       (setq ring-bell-function ;; subtle mode line flash
             (lambda ()
               (let ((orig-fg (face-foreground 'mode-line)))
                 (set-face-foreground 'mode-line "#F2804F")
                 (run-with-idle-timer 0.1 nil
                                      (lambda (fg) (set-face-foreground 'mode-line fg))
                                      orig-fg))))
       )
      ((eq system-type 'gnu/linux)
       (setq visible-bell t)
       ))

;;; Scrolling

(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2)


(column-number-mode)
(global-display-line-numbers-mode t) ;; display line numbers everywhere

    (defun efs/display-startup-time ()
      (message "Emacs loaded in %s with %d garbage collections."
               (format "%.2f seconds"
                       (float-time
                       (time-subtract after-init-time before-init-time)))
               gcs-done))

    (add-hook 'emacs-startup-hook #'efs/display-startup-time)

(global-hl-line-mode t)

;; (require 'sublimity)
;; (require 'sublimity-scroll)
;; (sublimity-mode 1)

;; (use-package dashboard ;; for some reason activating this fixes python bug LOL have to debug that another time
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook))

(use-package page-break-lines
  :config (global-page-break-lines-mode))

;; (setq ad-redefinition-action 'accept)

(setq vc-follow-symlinks t) ;; always follow symlinks

(setq large-file-warning-threshold nil)

(use-package diminish)

;; Font Configuration -----------------------
  ;; (set-face-attribute 'default nil :font "SauceCodePro Nerd Font 11")
  ;; IF FONT LOOKS WEIRD (TOO SLIM) then it means the font is not working properly, CHANGE IT

       (defun aard-set-face (frame)
         "Configure faces on frame creation"
         (select-frame frame)
         (if (display-graphic-p)
             (progn
            (if (member "FuraCode Nerd Font" (font-family-list))
            (set-frame-font "FuraCode Nerd Font-12"))

            (if (member "Terminus" (font-family-list))
            (set-frame-font "Terminus-12"))
         )))

(cond ((eq system-type 'windows-nt)
    ;; Windows-specific code goes here.
    )
      ((eq system-type 'darwin)
      (set-face-attribute 'default nil :font "FiraCode Nerd Font" :height 170)


      ;; Set the fixed pitch face
      (set-face-attribute 'fixed-pitch nil :font "FiraCode Nerd Font" :height 180)

      ;; Set the variable pitch face
      (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 180 :weight 'regular)
    )
    ((eq system-type 'gnu/linux)
      ;; (add-hook 'after-make-frame-functions 'aard-set-face)
      (set-face-attribute 'default nil :font "FuraCode Nerd Font" :height runemacs/default-font-size)

      ;; Set the fixed pitch face
      (set-face-attribute 'fixed-pitch nil :font "FuraCode Nerd Font" :height 120)

      ;; Set the variable pitch face
      (set-face-attribute 'variable-pitch nil :font "DejaVu Sans" :height 120 :weight 'regular)
    ))
  ;; -------------------------------------------------------

(use-package auto-package-update
  :custom
  (auto-package-update-interval 90)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00")
  (setq auto-package-update-delete-old-versions t)
  )

;; (setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)
;; (setq indent-line-function 'insert-tab)

;(use-package command-log-mode)

(setq x-select-enable-clipboard-manager nil); weird emacs bug where it won't close

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(global-auto-revert-mode 1) ;;
;; Revert Dired and other buffers
(setq global-auto-revert-non-file-buffers t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0 ))))

;; has to install pdf2svg on pc first
;; (use-package org-inline-pdf
;;   :init
;;   (add-hook 'org-mode-hook #'org-inline-pdf-mode))

;; (use-package bufler
;;   ;; :commands (bufler-switch-buffer bufler-workspace-frame-set bufler-list)
;;   ;; :disabled
;;   :bind (("C-M-j" . bufler-switch-buffer)
;;          ("C-M-k" . bufler-workspace-frame-set))
;;   :config
;;   ;; (evil-collection-define-key 'normal 'bufler-list-mode-map
;;   ;;   (kbd "RET")   'bufler-list-buffer-switch
;;   ;;   (kbd "M-RET") 'bufler-list-buffer-peek
;;   ;;   "D"           'bufler-list-buffer-kill)

;;   (bufler-defgroups
;;    (group
;;     ;; Subgroup collecting all named workspaces.
;;     (auto-workspace))
;;    (group
;;     ;; Subgroup collecting all `help-mode' and `info-mode' buffers.
;;     (group-or "*Help/Info*"
;;               (mode-match "*Help*" (rx bos "help-"))
;;               (mode-match "*Info*" (rx bos "info-"))))
;;    (group
;;     ;; Subgroup collecting all special buffers (i.e. ones that are not
;;     ;; file-backed), except `magit-status-mode' buffers (which are allowed to fall
;;     ;; through to other groups, so they end up grouped with their project buffers).
;;     (group-and "*Special*"
;;                (lambda (buffer)
;;                  (unless (or (funcall (mode-match "Magit" (rx bos "magit-status"))
;;                                       buffer)
;;                              (funcall (mode-match "Dired" (rx bos "dired"))
;;                                       buffer)
;;                              (funcall (auto-file) buffer))
;;                    "*Special*")))
;;     (group
;;      ;; Subgroup collecting these "special special" buffers
;;      ;; separately for convenience.
;;      (name-match "**Special**"
;;                  (rx bos "*" (or "Messages" "Warnings" "scratch" "Backtrace") "*")))
;;     (group
;;      ;; Subgroup collecting all other Magit buffers, grouped by directory.
;;      (mode-match "*Magit* (non-status)" (rx bos (or "magit" "forge") "-"))
;;      (auto-directory))
;;     ;; Subgroup for Helm buffers.
;;     (mode-match "*Helm*" (rx bos "helm-"))
;;     ;; Remaining special buffers are grouped automatically by mode.
;;     (auto-mode))
;;    ;; All buffers under "~/.emacs.d" (or wherever it is).
;;    (dir user-emacs-directory)
;;    (group
;;     ;; Subgroup collecting buffers in `org-directory' (or "~/org" if
;;     ;; `org-directory' is not yet defined).
;;     (dir (if (bound-and-true-p org-directory)
;;              org-directory
;;            "~/org"))
;;     (group
;;      ;; Subgroup collecting indirect Org buffers, grouping them by file.
;;      ;; This is very useful when used with `org-tree-to-indirect-buffer'.
;;      (auto-indirect)
;;      (auto-file))
;;     ;; Group remaining buffers by whether they're file backed, then by mode.
;;     (group-not "*special*" (auto-file))
;;     (auto-mode))
;;    (group
;;     ;; Subgroup collecting buffers in a projectile project.
;;     (auto-projectile))
;;    (group
;;     ;; Subgroup collecting buffers in a version-control project,
;;     ;; grouping them by directory.
;;     (auto-project))
;;    ;; Group remaining buffers by directory, then major mode.
;;    (auto-directory)
;;    (auto-mode)))

;; (use-package default-text-scale
;;   :defer 1
;;   :config
;;   (default-text-scale-mode))

;; (use-package languagetool
;;   :ensure t
;;   :defer t
;;   :commands (languagetool-check
;;              languagetool-clear-suggestions
;;              languagetool-correct-at-point
;;              languagetool-correct-buffer
;;              languagetool-set-language
;;              languagetool-server-mode
;;              languagetool-server-start
;;              languagetool-server-stop)
;;   :config
;;   (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8")
;;         languagetool-console-command (concat (getenv "HOME") "/.config/texstudio/dictionaries/LanguageTool-5.7/languagetool-commandline.jar")
;;         languagetool-server-command (concat (getenv "HOME") "/.config/texstudio/dictionaries/LanguageTool-5.7/languagetool-server.jar")))

(use-package flycheck-languagetool
      :ensure t
      :hook (text-mode . flycheck-languagetool-setup)
      :init
      (setq flycheck-languagetool-server-jar languagetool-server-dir))
(setq flycheck-languagetool-language "es")

;; in arch linux use languagetool path
                ;; (setq langtool-java-classpath
                ;;       "/usr/share/languagetool:/usr/share/java/languagetool/*")
(setq langtool-server-user-arguments '("-p" "8085")) ;; this makes it possible to run two servers, or rather two connections to the server from flycheck-languagetool for on the fly highlight and langtool for correction suggestions (GODLIKE)
                    (use-package langtool
                      :commands (langtool-check langtool-check-done))

;; execute spanish spell-checking on buffer
              (defun flyspell-spanish ()
                (interactive)
                (ispell-change-dictionary "castellano")
                (flyspell-buffer))

              (defun flyspell-english ()
                (interactive)
                (ispell-change-dictionary "default")
                (flyspell-buffer))
                                                      ; if: Warning (emacs): Unable to activate package `elpy'.
                                                      ;Required package `highlight-indentation-0.5.0' is unavailable then install package
              (use-package pkg-info)
              (use-package spell-fu
                :commands spell-fu-mode
                ) ;; this underlines mistakes
          (add-hook 'spell-fu-mode-hook ;;this is what really makes it work
                    (lambda ()
              (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "es")) ;;if functions are correct this works
                      (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "ca"))
              (spell-fu-dictionary-add (spell-fu-get-ispell-dictionary "en"))
                ))
    (setq ispell-dictionary "es") ;; sets spanish as default
      (setq ispell-program-name "aspell") ;; already points to aspell
      (setq ispell-extra-args '("--sug-mode=ultra" "--lang=es"))
              (setq spell-fu-directory spell-fu-dir) ;; Please create this directory manually. where spell_fu stores stuff
              (setq ispell-personal-dictionary ispell-personal-dir) ;;spell_fu stores stuff here
              ;; (spell-fu-dictionary-add (spell-fu-get-ispell "es"))
              ;; (spell-fu-dictionary-add (spell-fu-get-ispell "en"))
              ;; (spell-fu-dictionary-add (spell-fu-get-ispell "ca"))

              ;; (global-spell-fu-mode)
              (use-package flycheck
                :commands (flycheck-mode global-flycheck-mode)
                :ensure t
                ;; :init (global-flycheck-mode)
                )
              (use-package flycheck-popup-tip
                :after flycheck)
              (with-eval-after-load 'flycheck
                '(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode))
(use-package flyspell-lazy
  :commands (flycheck-mode global-flycheck-mode)
  )
(flyspell-lazy-mode 1)

;; Change the user-emacs-directory to keep unwanted things out of ~/.emacs.d
(setq user-emacs-directory user-emacs-dir
      url-history-file url-history-dir)

;; NOTE: If you want to move everything out of the ~/.emacs.d folder
;; reliably, set `user-emacs-directory` before loading no-littering!
;(setq user-emacs-directory "~/.cache/emacs")

(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; ;; Keep customization settings in a temporary file (thanks Ambrevar!)
;; (setq custom-file
;;       (if (boundp 'server-socket-dir)
;;           (expand-file-name "custom.el" server-socket-dir)
;;         (expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
;; (load custom-file t)

(use-package restart-emacs)

(use-package eyebrowse
  :ensure t
  :init
  (setq eyebrowse-keymap-prefix (kbd "")) ;; this seems to work to unbind keybindings :D
  (global-unset-key (kbd "C-c C-w"))
  ;; we have to set this before the package is initialized  https://github.com/wasamasa/eyebrowse/issues/49
  :config
  (eyebrowse-mode t)
  (setq eyebrowse-new-workspace t) ; by default nil, clones last workspace, set to true shows scratch
  )

(winner-mode 1)

;; (desktop-save-mode 1)

;; use only one desktop

(setq desktop-path '(desktop-dir))
(setq desktop-dirname desktop-dir)
(setq desktop-base-file-name "emacs-desktop")

;; remove desktop after it's been read
;; (add-hook 'desktop-after-read-hook
;;           '(lambda ()
;;              ;; desktop-remove clears desktop-dirname
;;              (setq desktop-dirname-tmp desktop-dirname)
;;              (desktop-remove)
;;              (setq desktop-dirname desktop-dirname-tmp)))

(defun saved-session ()
  (file-exists-p (concat desktop-dirname "/" desktop-base-file-name)))

;; use session-restore to restore the desktop manually
(defun session-restore ()
  "Restore a saved emacs session."
  (interactive)
  (if (saved-session)
      (desktop-read)
    (message "No desktop found.")))

;; use session-save to save the desktop manually
(defun session-save ()
  "Save an emacs session."
  (interactive)
  (if (saved-session)
      (if (y-or-n-p "Overwrite existing desktop? ")
          (desktop-save-in-desktop-dir)
        (message "Session not saved."))
    (desktop-save-in-desktop-dir)))

;; ask user whether to restore desktop at start-up
;; (add-hook 'after-init-hook
;;           '(lambda ()
;;              (if (saved-session)
;;                  (if (y-or-n-p "Restore desktop? ")
;;                      (session-restore)))))

;; (add-hook 'kill-emacs-hook '(lambda ()
;;                              (if (y-or-n-p "Save desktop? ")
;;                               (desktop-save-in-desktop-dir))
;;                              ))

(use-package ivy ; makes navigation between stuff easier
  :diminish ; do not show stuff on bar or something
  :bind (("C-s" . swiper) ;;like / but with context
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill) ;; delete ivy buffer
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))
;; eval last sexp is better cause inconsistencies from hooks when running evalbuffer
;; and show keybindings

(use-package ivy-rich ;; shows better explanations
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package ivy-prescient
  :after counsel ;; must have this
  ;; :custom
  ;; (ivy-prescient-enable-filtering nil) ;; keep ivy filtering style
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1)
  )
;; (setq prescient-filter-method '(fuzzy regexp))
;; (setq prescient-sort-length-enable nil) ;; do not sort by length

(use-package company-prescient
:after company
:config
(company-prescient-mode 1))

;; With ivy-rich shows descriptions for commands 
(use-package counsel
:bind (("M-x" . counsel-M-x)
        ("C-x b" . counsel-ibuffer)
        ("C-x C-f" . counsel-find-file)
        :map minibuffer-local-map
        ("C-r" . 'counsel-minibuffer-history))
        :config
        (setq ivy-initial-inputs-alist nil))

(use-package all-the-icons)
;; custom command line
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))
(use-package doom-themes) ;; counsel-load-theme to load a theme from the list
(load-theme 'doom-one t) ;; if not using t will prompt if its safe to https://github.com/Malabarba/smart-mode-line/issues/100

;; (use-package minions
;;   :hook (doom-modeline-mode . minions-mode))

;; key bindings
(when (eq system-type 'darwin) ;; mac specific settings
  (setq mac-option-modifier 'alt)
  (setq mac-command-modifier 'meta)
  (global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
  (setq mac-right-option-modifier 'none) ;; so that you can write backslash and basically use alt gr (right option on mac)
  )
    ;; (setq mac-command-modifier 'meta)
    ;; (defun show-in-finder ()
    ;;   (interactive)
    ;;   (shell-command (concat "open -R "  buffer-file-name))
    ;;   )
    (use-package reveal-in-osx-finder) ;; works well

(global-set-key (kbd "C-M-j") 'counsel-switch-buffer) ;; easier command to switch buffers
    ;; example (define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme) define keybinding only in emacs-lisp-mode

    (use-package general ;; set personal bindings for leader key for example
     ; (general-define-key "C-M-j" 'counsel-switch-buffer) ;; allows to define multiple global keybindings
      ;; :after evil
      :config
      (general-evil-setup t)
      (general-create-definer pol/leader-key
        :keymaps '(normal insert visual emacs)
        :prefix "SPC" 
        :global-prefix "C-SPC") ;; leader
      (general-create-definer pol/ctrl-c-keys
        :prefix "C-c"))

        ;;,** Mode Keybindings
        ;; (general-define-key
        ;; :keymaps 'eyebrowse-mode-map
        ;; :prefix "SPC a"
        ;; ;; bind "C-c C-l"
        ;; ;; "C-z" 'cider-switch-to-repl-buffer
        ;; )

(cond ((eq system-type 'windows-nt)
       ;; Windows-specific code goes here.
       )
      ((eq system-type 'darwin)
       (pol/leader-key
         "oo" '(reveal-in-osx-finder :which-key "Open file in finder")
         )
       )
      ((eq system-type 'gnu/linux)
       ))
;; same as :bind-keymap
    ;; (general-define-key
    ;;  :prefix "SPC"
    ;;   )

    ;; define workspace keys
    (pol/leader-key
      "TAB" '(:ignore s :which-key "workspace")
      "TAB <" '(eyebrowse-prev-window-config :which-key "Previous window") 
      "TAB >" '(eyebrowse-next-window-config :which-key "Next window")
      "TAB '" '(eyebrowse-last-window-config :which-key "Last window")
      "TAB k" '(eyebrowse-close-window-config :which-key "Close window")
      "TAB ," '(eyebrowse-rename-window-config :which-key "Rename window")
      "TAB ." '(eyebrowse-switch-to-window-config :which-key "Switch to window")
      "TAB c" '(eyebrowse-create-window-config :which-key "Create window config")
      ;; "0" '(eyebrowse-switch-to-window-config-0 :which-key "Switch to final workspace")
      ;; "1" '(eyebrowse-switch-to-window-config-1 :which-key "Switch to 1st workspace")
      ;; "2" '(eyebrowse-switch-to-window-config-2 :which-key "Switch to 2nd workspace")
      ;; "3" '(eyebrowse-switch-to-window-config-3 :which-key "Switch to 3rd workspace")
      ;; "4" '(eyebrowse-switch-to-window-config-4 :which-key "Switch to 4th workspace")
      ;; "5" '(eyebrowse-switch-to-window-config-5 :which-key "Switch to 5th workspace")
      ;; "6" '(eyebrowse-switch-to-window-config-6 :which-key "Switch to 6th workspace")
      ;; "7" '(eyebrowse-switch-to-window-config-7 :which-key "Switch to 7th workspace")
      ;; "8" '(eyebrowse-switch-to-window-config-8 :which-key "Switch to 8th workspace")
      ;; "9" '(eyebrowse-switch-to-window-config-9 :which-key "Switch to 9th workspace")
      "0" '(eyebrowse-switch-to-window-config-0 :which-key "ws 0")
      "1" '(eyebrowse-switch-to-window-config-1 :which-key "ws 0")
      "2" '(eyebrowse-switch-to-window-config-2 :which-key "ws 0")
      "3" '(eyebrowse-switch-to-window-config-3 :which-key "ws 0")
      "4" '(eyebrowse-switch-to-window-config-4 :which-key "ws 0")
      "5" '(eyebrowse-switch-to-window-config-5 :which-key "ws 0")
      "6" '(eyebrowse-switch-to-window-config-6 :which-key "ws 0")
      "7" '(eyebrowse-switch-to-window-config-7 :which-key "ws 0")
      "8" '(eyebrowse-switch-to-window-config-8 :which-key "ws 0")
      "9" '(eyebrowse-switch-to-window-config-9 :which-key "ws 0")
      )

    (pol/leader-key
      "n" '(:ignore n :which-key "notes")
      "nrf" '(org-roam-node-find :which-key "Find node")
      "nri" '(org-roam-node-insert :which-key "Inset node")
      "nrI" '(org-roam-node-insert-immediate :which-key "Insert Node quick")
      "nrd" '(:keymap org-roam-dailies-map :package org-roam :which-key "dailies")
      ;; "nrdY" '( :keymap org-roam-dailies-map :package org-roam :which-key "dailies")
      ;; "nrdT" '( :keymap org-roam-dailies-map :package org-roam :which-key "dailies")
      )
    (pol/leader-key ;; try to have similar keybindings in vim as well
      "<RET>" '(bookmark-jump :which-key "Jump to bookmark")
      "." '(counsel-find-file :which-key "Find file")
      "s" '(:ignore s :which-key "session")
      "ss" '(session-save :which-key "Session save")
      "sr" '(session-restore :which-key "Session restore")
      "o" '(:ignore o :which-key "open") 
      "ot" '(vterm-toggle :which-key "Toggle vterm")
      "od" '(vterm-toggle-cd :which-key "Toggle vterm on current folder")
      "o-" '(dired-jump :which-key "Toggle vterm")
      "X" '(org-capture :which-key "Org-capture") ;; this is F*** awesome
      "c" '(:ignore c :which-key "code") 
      "cf" '(format-all-buffer :which-key "code") 
      "h" '(:ignore h :which-key "git-gutter") 
      "hn" '(git-gutter:next-hunk :which-key "Next hunk") 
      "hp" '(git-gutter:previous-hunk :which-key "Previous hunk") 
      "hv" '(git-gutter:popup-hunk :which-key "Preview hunk") 
      "hs" '(git-gutter:stage-hunk :which-key "Stage hunk") 
      "hu" '(git-gutter:revert-hunk :which-key "Undo hunk") ;; take back changes
      "hg" '(git-gutter :which-key "Update changes") 
      "b" '(:ignore b :which-key "buffers") 
      "bn" '(evil-next-buffer :which-key "Next buffer") 
      "bp" '(evil-prev-buffer :which-key "Previous buffer")
      "bk" '(evil-delete-buffer :which-key "Kill buffer")
      "bd" '(evil-delete-buffer :which-key "Kill buffer")
      "br" '(revert-buffer-quick :which-key "Revert buffer")
      "bR" '(rename-buffer :which-key "Rename buffer")
      "bs" '(basic-save-buffer :which-key "Save the current buffer in its visited file")
      "bS" '(basic-save-buffer :which-key "Save all buffers visiting a file")
      "<" '(counsel-switch-buffer :which-key "Switch buffer") ;; similarity with doom
      "u" '(universal-argument :which-key "Universal argument") ;; similarity with doom
      "-" '(evil-switch-to-windows-last-buffer :which-key "Switch to last buffer") ;; similarity with doom
      "w" '(:ignore w :which-key "windows")
      "wr" '(winner-redo :which-key "Redo window layout")
      "wu" '(winner-undo :which-key "Undo window layout")
      "p" '(:ignore s :which-key "project")
      "pr" '(projectile-recentf :which-key "Recent file")
      "pp" '(projectile-switch-project :which-key "Switch project")
      "pb" '(projectile-switch-to-buffer :which-key "Switch buffer")
      "f" '(:ignore s :which-key "file")
      "fr" '(counsel-recentf :which-key "Recent file")
      "fs" '(save-buffer :which-key "Save buffer") ;; classic vim save
      "fS" '(write-file :which-key "Write current buffer into file FILENAME")
      "fD" '(delete-file-and-buffer :which-key "Delete file")
      "t" '(:ignore t :which-key "toggles") ;; "folder" for toggles
      "to" '(openwith-mode :which-key "Open with external app")
      "tt" '(counsel-load-theme :which-key "Choose theme")
      "ts" '(spell-fu-mode :which-key "Spell checker")
      "tf" '(flycheck-mode :which-key "Flycheck")
      "tg" '(git-gutter-mode :which-key "Git-gutter toggle") 
      "tp" '(:ignore tp :which-key "pomodoro") 
      "tp C-s" '(pomodoro-start :which-key "Pomodoro start") 
      "tpp" '(pomodoro-pause :which-key "Pomodoro pause") 
      "tpr" '(pomodoro-resume :which-key "Pomodoro resume") 
      "m" '(:ignore m :which-key "markdown") 
      "mp" '(grip-mode :which-key "Live preview")
      "mt" '(markdown-toc-generate-or-refresh-toc :which-key "Generate or refresh toc")
      "l" '(:ignore l :which-key "language tool") 
      "ll" '(langtool-check :which-key "Check buffer") 
      "ld" '(langtool-check-done :which-key "Check-done, remove markers") 
      "lc" '(langtool-correct-buffer :which-key "Correct buffer") 
      ;; "ln" '(flycheck-next-error :which-key "Go to next error") 
      ;; "lp" '(flycheck-previous-error :which-key "Go to previous error") 
      ;; "le" '(flycheck-display-error-at-point :which-key "Display error") 
      "ln" '(langtool-goto-next-error :which-key "Go to next error") 
      "lp" '(langtool-goto-previous-error :which-key "Go to previous error") 
      )

      ;; (global-unset-key (kbd "C-c C-w"))
      ;; (global-unset-key (kbd "SPC a"))

(global-set-key (kbd "C-M-u") 'universal-argument)

(use-package hydra
    :defer t) ;; emacs bindings that stick around like mode for i3

  (defhydra hydra-text-scale (:timeout 4)
    "scale text"
    ("j" text-scale-increase "in")
    ("k" text-scale-decrease "out")
    ("q" nil "finished" :exit t))
  (pol/leader-key
    "t+" '(hydra-text-scale/body :which-key "Scale text"))

  (pol/leader-key
    "tr" '(window-resize-hydra/body :which-key "Resize windows"))

  (defhydra window-resize-hydra (:hint nil)
  "
             _k_ increase height
_h_ decrease width    _l_ increase width
             _j_ decrease height
"
  ("h" evil-window-decrease-width)
  ("j" evil-window-increase-height)
  ("k" evil-window-decrease-height)
  ("l" evil-window-increase-width)

  ("q" nil))

;; vim keybindings for easier on the fingers typing :D
                    (use-package evil
                      :init
                      (setq evil-want-integration t) ;; must have
                      (setq evil-want-keybinding nil)
                      (setq evil-want-C-u-scroll t)
                      (setq evil-want-C-i-jump nil)
                      ;;(setq evil-respect-visual-line-mode t) idk
                      ;;(setq evil-undo-system 'undo-tree) idk
                      :config
                      (evil-mode 1)
                      (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
                      ;(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

                      ;; Use visual line motions even outside of visual-line-mode buffers
                      (evil-global-set-key 'motion "j" 'evil-next-visual-line) ;; both of these
                      (evil-global-set-key 'motion "k" 'evil-previous-visual-line) ;; are needed for org mode where g-j doesn't work properly

                      (evil-set-initial-state 'messages-buffer-mode 'normal)
                      (evil-set-initial-state 'dashboard-mode 'normal))
                    ;; to center screen on cursor, zz or emacs-style C-l

                    ;; https://github.com/linktohack/evil-commentary
                    ;; use-package makes it so that it installs it from config and config section
                    ;; activates the mode
                    (use-package evil-commentary
                      :after evil
                      :config
                      (evil-commentary-mode))

                    (use-package evil-collection
                      :after evil ;; load after evil, must have
                      :config
                      (evil-collection-init))

    ;; glorious increment like in vim :D
    (use-package evil-numbers
      :after evil)
;; (define-key evil-visual-state-map (kbd "C-a") 'evil-numbers/inc-at-pt) ;; vim classic
;; (define-key evil-visual-state-map (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental) ;; vim classic
;; (define-key evil-visual-state-map (kbd "C-x") 'evil-numbers/dec-at-pt) ;; vim classic
;; (define-key evil-visual-state-map (kbd "g C-x") 'evil-numbers/dec-at-pt-incremental) ;; vim classic

;; (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
;; (define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)

        ;; only in normal and insert vim classic bindings
        (evil-define-key '(normal visual) 'global (kbd "C-a") 'evil-numbers/inc-at-pt)
        (evil-define-key '(normal visual) 'global (kbd "C-x") 'evil-numbers/dec-at-pt)
        (evil-define-key '(normal visual) 'global (kbd "g C-a") 'evil-numbers/inc-at-pt-incremental)
        (evil-define-key '(normal visual) 'global (kbd "g C-x") 'evil-numbers/dec-at-pt-incremental)

    ;; without shadowing regular + -
    ;;     (evil-define-key '(normal visual) 'global (kbd "<kp-add>") 'evil-numbers/inc-at-pt)
    ;; (evil-define-key '(normal visual) 'global (kbd "<kp-subtract>") 'evil-numbers/dec-at-pt)
    ;; (evil-define-key '(normal visual) 'global (kbd "C-<kp-add>") 'evil-numbers/inc-at-pt-incremental)
    ;; (evil-define-key '(normal visual) 'global (kbd "C-<kp-subtract>") 'evil-numbers/dec-at-pt-incremental)

                    ; C-z go back to EMACS MODE

;; (use-package evil-goggles
;;   :ensure t
;;   :after evil
;;   :config
;;   (evil-goggles-mode)

;;   ;; optionally use diff-mode's faces; as a result, deleted text
;;   ;; will be highlighed with `diff-removed` face which is typically
;;   ;; some red color (as defined by the color theme)
;;   ;; other faces such as `diff-added` will be used for other actions
;;   (evil-goggles-use-diff-faces))

;; (darkroom-mode 0) this makes keybinding work automatically but also runs on startup
(use-package darkroom
  :commands darkroom-mode
  :config
  (setq darkroom-text-scale-increase 0)
  )

(defun dw/enter-focus-mode ()
  (interactive)
  (darkroom-mode 1)
  (display-line-numbers-mode 0))

(defun dw/leave-focus-mode ()
  (interactive)
  (darkroom-mode 0)
  (display-line-numbers-mode 1))

(defun dw/toggle-focus-mode ()
  (interactive)
  (if (symbol-value darkroom-mode)
    (dw/leave-focus-mode)
    (dw/enter-focus-mode)))

(pol/leader-key
  "tz" '(dw/toggle-focus-mode :which-key "Focus mode")
  ;; "te" '(dw/enter-focus-mode :which-key "focus mode")
  ;; "ta" '(dw/leave-focus-mode :which-key "focus mode")
  )

(use-package which-key ;; This shows which commands are available for current keypresses
  :commands(helpful-callable helpfull-variable helpful-command helpful-key)
  :defer 0
  ;; runs before package is loaded automatically whether package is loaded or not we can also invoke the mode
  :diminish which-key-mode
  :config ;; this is run after the package is loaded
 (which-key-mode)
  (setq which-key-idle-delay 0.3)) ;; delay on keybindings 

(use-package helpful ;; better function descriptions
  :custom ;; custom variables
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function) ;; remap keybinding to something different
  ([remap describe-command] . helpful-command) 
  ([remap describe-variable] . counsel-describe-variable))

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (if (y-or-n-p (concat "Do you really want to delete file " filename " ?"))
            (progn
              (delete-file filename)
              (message "Deleted file %s." filename)
              (kill-buffer)))
      (message "Not a file visiting buffer!"))))

(add-to-list 'dnd-protocol-alist
             '("^file:///.*\\.png" . org-insert-link))

(with-eval-after-load 'org
    (require 'org-tempo)
    (add-to-list 'org-structure-template-alist '("py" . "src python"))
    (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
    (add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
    (add-to-list 'org-structure-template-alist '("cpp" . "src C++"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    )

(use-package haskell-mode
  :after (org lsp) ) ;; needed for haskell snippets

(with-eval-after-load 'org
    (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (java . t)
        (python . t)))
    (push '("conf-unix" . conf-unix) org-src-lang-modes)
    )

(defun efs/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))) ;; replace - in lists for a dot

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2) ;; variable sizes for headers
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "DejaVu Sans" :weight 'regular :height(cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch)) ;; fixed pitch on some stuff so that it lines up correctly, and variable on others so that it looks better
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1) ;; allows text to be of variable size
  (visual-line-mode 1) ;; makes emacs editing commands act on visual lines not logical ones, also word-wrapping, idk if i want this
  )

(use-package org  ;; org is already installed though
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0)) ;; doesnt'work
  (message "Org mode loaded")
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t ;; this hides emphasis markers like bold or itallics
        org-src-fontify-natively t
        org-fontify-quote-and-verse-blocks t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 4
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2
        ) ;; change ... to another symbol that is less confusing
  (efs/org-font-setup) ;; setup font
   ;; hides *bold* and __underlined__ and linked words [name][link]
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time) ;; logs when a task goes to done C-h-v (describe variable)
  (setq org-log-into-drawer t) ;; collapse logs into a drawer
  (setq org-agenda-files
        (list birthday-dir
          tasks-dir
          habits-dir
          ))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit) ;;  add org-habit to org-modules
  (setq org-habit-graph-column 60) ;; what column the habit tracker shows

  (setq org-todo-keywords
    '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
      ;; (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")
  ))

  (setq org-refile-targets ;; move TODO tasks to a different file
    '(("Archive.org" :maxlevel . 1)
      ("Tasks.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("@errand" . ?E)
       ("@home" . ?H)
       ("@work" . ?W)
       ("agenda" . ?a)
       ("planning" . ?p)
       ("publish" . ?P)
       ("batch" . ?b)
       ("note" . ?n)
       ("idea" . ?i)))

;; Configure custom agenda views
  (setq org-agenda-custom-commands
   '(("d" "Dashboard"
     ((agenda "" ((org-deadline-warning-days 7)))
      (todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))
      (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

    ("n" "Next Tasks"
     ((todo "NEXT"
        ((org-agenda-overriding-header "Next Tasks")))))

    ("W" "Work Tasks" tags-todo "+work-email")

    ;; Low-effort next actions
    ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
     ((org-agenda-overriding-header "Low Effort Tasks")
      (org-agenda-max-todos 20)
      (org-agenda-files org-agenda-files)))

    ("w" "Workflow Status"
     ((todo "WAIT"
            ((org-agenda-overriding-header "Waiting on External")
             (org-agenda-files org-agenda-files)))
      (todo "REVIEW"
            ((org-agenda-overriding-header "In Review")
             (org-agenda-files org-agenda-files)))
      (todo "PLAN"
            ((org-agenda-overriding-header "In Planning")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "BACKLOG"
            ((org-agenda-overriding-header "Project Backlog")
             (org-agenda-todo-list-sublevels nil)
             (org-agenda-files org-agenda-files)))
      (todo "READY"
            ((org-agenda-overriding-header "Ready for Work")
             (org-agenda-files org-agenda-files)))
      (todo "ACTIVE"
            ((org-agenda-overriding-header "Active Projects")
             (org-agenda-files org-agenda-files)))
      (todo "COMPLETED"
            ((org-agenda-overriding-header "Completed Projects")
             (org-agenda-files org-agenda-files)))
      (todo "CANC"
            ((org-agenda-overriding-header "Cancelled Projects")
             (org-agenda-files org-agenda-files)))))))

 (setq org-capture-templates
    `(("t" "Tasks / Projects")
      ("tt" "Task" entry (file+olp tasks-dir "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

      ("j" "Journal Entries")
      ("jj" "Journal" entry
           (file+olp+datetree journal-dir)
           "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
           ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
           :clock-in :clock-resume
           :empty-lines 1)
      ("jm" "Meeting" entry
           (file+olp+datetree journal-dir)
           "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
           :clock-in :clock-resume
           :empty-lines 1)

      ("w" "Workflows")
      ("we" "Checking Email" entry (file+olp+datetree journal-dir)
           "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

      ("m" "Metrics Capture")
      ("mw" "Weight" table-line (file+headline metrics-dir "Weight")
       "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

  )

(setq org-highlight-latex-and-related '(native))  ;; has to be set to native otherwise see some strange beheaviour, this way its colored green as in doom emacs



(use-package org-appear
  :hook (org-mode . org-appear-mode))
(setq org-appear-inside-latex t)

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100 ;; set column width (character width?)
        visual-fill-column-center-text t) ;; center text on middle of screen
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package org-bullets ;; changes headers so that it doesn't show all of the stars
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))) ;; default symbols get weird

(setq org-startup-latex-with-latex-preview t) ;; doesn't work
(use-package org-fragtog)
(add-hook 'org-mode-hook 'org-fragtog-mode) ;; This should enable org-fragtog when entering org-mode

;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (buffer-file-name)
                      (expand-file-name emacs-babel-config-file))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (my/org-tangle-to-multiple-targets) )))
(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config))) ;; add hook to org mode

;; (org-babel-tangle)  instead of my/org-tangle, before

(use-package org-roam
      ;; :ensure t
      ;; :demand t
      :init
      (setq org-roam-v2-ack t)
      :custom
      (org-roam-directory org-roam-dir)
      (org-roam-completion-everywhere t)
      (org-roam-capture-templates
       '(("d" "default" plain ;; first template should be default one cause keybindings ahead will use that for fast typing
          "%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
          :unnarrowed t)

         ("l" "programming language" plain
          "* Characteristics\n\n- Family: %?\n- Inspired by: \n\n* Reference:\n\n"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
          :unnarrowed t)

         ("b" "book notes" plain
          "\n* Source\n\nAuthor: %^{Author}\nTitle: ${title}\nYear: %^{Year}\n\n* Summary\n\n%?"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
          :unnarrowed t)

         ("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
          :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: Project")
          :unnarrowed t)

         ))
      (org-roam-dailies-capture-templates
      '(("d" "default" entry "* %<%I:%M %p>: %?"
         :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

      :bind (
             ;; ("C-c n l" . org-roam-buffer-toggle)
             ;; ("C-c n f" . org-roam-node-find)
             ;; ("C-c n i" . org-roam-node-insert)
             ;; ("C-c n I" . org-roam-node-insert-immediate)

             ;; ("C-c n p" . my/org-roam-find-project)
             ;; ("C-c n t" . my/org-roam-capture-task)
             ;; ("C-c n b" . my/org-roam-capture-inbox)
             :map org-mode-map
             ("C-M-i"    . completion-at-point)
             :map org-roam-dailies-map
             ("Y" . org-roam-dailies-capture-yesterday)
             ("T" . org-roam-dailies-capture-tomorrow))
      :bind-keymap
      ("C-c n d" . org-roam-dailies-map)
      :config
      (org-roam-setup)
      (require 'org-roam-dailies) ;; Ensure the keymap is available
      (org-roam-db-autosync-mode)
      )

    ;; Bind this to C-c n I
  (defun org-roam-node-insert-immediate (arg &rest args)
    (interactive "P")
    (let ((args (cons arg args))
          (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                    '(:immediate-finish t)))))
      (apply #'org-roam-node-insert args)))

;; (defun my/org-roam-filter-by-tag (tag-name)
;;   (lambda (node)
;;     (member tag-name (org-roam-node-tags node))))

;; (defun my/org-roam-list-notes-by-tag (tag-name)
;;   (mapcar #'org-roam-node-file
;;           (seq-filter
;;            (my/org-roam-filter-by-tag tag-name)
;;            (org-roam-node-list))))

;; (defun my/org-roam-refresh-agenda-list ()
;;   (interactive)
;;   (setq org-agenda-files (my/org-roam-list-notes-by-tag "Project")))

;; ;; Build the agenda list the first time for the session
;; (my/org-roam-refresh-agenda-list)

;; (defun my/org-roam-project-finalize-hook ()
;;   "Adds the captured project file to `org-agenda-files' if the
;; capture was not aborted."
;;   ;; Remove the hook since it was added temporarily
;;   (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

;;   ;; Add project file to the agenda list if the capture was confirmed
;;   (unless org-note-abort
;;     (with-current-buffer (org-capture-get :buffer)
;;       (add-to-list 'org-agenda-files (buffer-file-name)))))

;; (defun my/org-roam-find-project ()
;;   (interactive)
;;   ;; Add the project file to the agenda after capture is finished
;;   (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

;;   ;; Select a project file to open, creating it if necessary
;;   (org-roam-node-find
;;    nil
;;    nil
;;    (my/org-roam-filter-by-tag "Project")
;;    :templates
;;    '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
;;       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
;;       :unnarrowed t))))

;; (defun my/org-roam-capture-inbox ()
;;   (interactive)
;;   (org-roam-capture- :node (org-roam-node-create)
;;                      :templates '(("i" "inbox" plain "* %?"
;;                                   :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

;; (defun my/org-roam-capture-task ()
;;   (interactive)
;;   ;; Add the project file to the agenda after capture is finished
;;   (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

;;   ;; Capture the new task, creating the project file if necessary
;;   (org-roam-capture- :node (org-roam-node-read
;;                             nil
;;                             (my/org-roam-filter-by-tag "Project"))
;;                      :templates '(("p" "project" plain "** TODO %?"
;;                                    :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
;;                                                           "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
;;                                                           ("Tasks"))))))

;; (defun my/org-roam-copy-todo-to-today ()
;;   (interactive)
;;   (let ((org-refile-keep t) ;; Set this to nil to delete the original!
;;         (org-roam-dailies-capture-templates
;;           '(("t" "tasks" entry "%?"
;;              :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
;;         (org-after-refile-insert-hook #'save-buffer)
;;         today-file
;;         pos)
;;     (save-window-excursion
;;       (org-roam-dailies--capture (current-time) t)
;;       (setq today-file (buffer-file-name))
;;       (setq pos (point)))

;;     ;; Only refile if the target file is different than the current file
;;     (unless (equal (file-truename today-file)
;;                    (file-truename (buffer-file-name)))
;;       (org-refile nil nil (list "Tasks" today-file nil pos)))))

;; (add-to-list 'org-after-todo-state-change-hook
;;              (lambda ()
;;                (when (equal org-state "DONE")
;;                  (my/org-roam-copy-todo-to-today))))

;; Use keybindings
(use-package grip-mode
  :ensure t
  :commands grip-mode
  )

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package markdown-toc
  :after markdown-mode)

(global-set-key (kbd "M-f") #'ian/format-code)
  (defun ian/format-code ()
    "Auto-format whole buffer."
    (interactive)
    (if (derived-mode-p 'prolog-mode)
        (prolog-indent-buffer)
      (format-all-buffer)))
(use-package format-all
  :commands (format-all-buffer)
  :config
  (add-hook 'prog-mode-hook #'format-all-ensure-formatter))

  ;; (setq format-all-formatters (("LaTeX" latexindent)))

(use-package clips-mode
  :mode "\\.clp\\'"
  )

(use-package rainbow-delimiters
      :hook (prog-mode . rainbow-delimiters-mode)) ;; prog-mode is based mode for any programming language
(add-hook 'clips-mode-hook 'rainbow-delimiters-mode) ;; activate rainbow-mode

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;; (use-package lsp-modSmartparens
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)) ;; give description for keys with wichkey

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-show-code-actions t)
  (lsp-ui-doc-position 'bottom))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")'
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))
(use-package lsp-treemacs
      :after lsp)

(use-package lsp-ivy
  :after (lsp-mode lsp))

(setq dap-auto-configure-features '(sessions locals controls tooltip))
    (use-package dap-mode
      ;; Uncomment the config below if you want all UI panes to be hidden by default!
      ;; :custom
      ;; (lsp-enable-dap-auto-configure nil)
      ;; :config
      ;; (dap-ui-mode 1)
      :after lsp
      :config
      ;; Set up Node debugging
      (require 'dap-node)
      (dap-node-setup) ;; Automatically installs Node debug adapter if needed

      ;; Bind `C-c l d` to `dap-hydra` for easy access
      (general-define-key
        :keymaps 'lsp-mode-map
        :prefix lsp-keymap-prefix
        "d" '(dap-hydra t :wk "debugger")))

(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  ;; NOTE: Set these if Python 3 is called "python3" on your system!
  (python-shell-interpreter "python3")
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)
  :config
  (require 'dap-python))

(use-package lsp-haskell
  :hook (haskell-mode . lsp-deferred)
  )

(use-package ccls
  :hook (c-mode c++-mode objc-mode))

(use-package matlab-mode
  :ensure t
  :mode "\\.m\\'"
  :init
  (setq matlab-indent-function t) ; if you want function bodies indented
  (setq matlab-shell-command "matlab")
  :config
  (setq matlab-indent-level 4)) ; set indentation level to 2 spaces

(use-package lsp-java
  :hook (java-mode . lsp-deferred))

;; ;; yasnippet code 'optional', before auto-complete
(use-package yasnippet)
(use-package doom-snippets ;; this gets you nice snippets to use just tab and they will complete for you
  :load-path doom-snippets-dir
  :after yasnippet)
;; (use-package yasnippets-latex)
(yas-global-mode 1)

;; (use-package auto-complete)
;;   (use-package auto-complete-auctex) 
;;    (global-auto-complete-mode t) 
;;        (use-package latex-preview-pane
;;          :hook (latex-mode . latex-preview-pane-mode)
;;        )

(use-package tex
  :mode "\\.tex\\'"
  :ensure auctex
  :config 
  (latex-mode)
  )
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-PDF-mode t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(use-package cdlatex
  :hook latex-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-cdlatex)   ; with Emacs latex mode

(use-package auto-yasnippet)
;; (global-set-key (kbd "H-w") #'aya-create)
;; (global-set-key (kbd "H-y") #'aya-expand)
(global-set-key (kbd "C-o") #'aya-open-line) ;;change TAB to C-o to expand yasnippets

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))
;; (use-package company-dict)
(use-package company-box
  :hook (company-mode . company-box-mode))

;; bring in the GIT
;; use C-x g to open magit status
;; type ? to know what can you do with magit
(use-package magit ;; use tab to open instead of za in vim
  :commands magit-status
  ;; :custom
  ;;   (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )

;; emacs variables local to projects
(use-package projectile ;; git projects management
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy)) ;; use ivy for completion can also use helm
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/")
    (setq projectile-project-search-path '("~/")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile ;; more commands with M-o in projectile (ivy allows that)
  :after projectile
  :config(counsel-projectile-mode))

(use-package git-gutter ;; works just like in vim :D
  :commands (git-gutter-mode git-gutter)
  :config
  ;; If you enable global minor mode
  ;; (global-git-gutter-mode t)
  ;; If you enable git-gutter-mode for some modes
  (add-hook 'ruby-mode-hook 'git-gutter-mode)
  )

;; (use-package diff-hl
;;   :init
;;   (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
;;   (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
;;   :config
;;   (global-diff-hl-mode)
;;   (diff-hl-margin-mode)
;;   )
;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
;; (use-package forge) ;; more git functionality

(use-package
  systemd)

(use-package term
  :commands term
  :config
  (setq explicit-shell-file-name "zsh") ;; Change this to zsh, etc
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(setq vterm-always-compile-module t)
(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  ;;(setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(use-package vterm-toggle
  ;; :commands (vterm-toggle vterm-toggle-cd) ;; trying to improve performance for this breaks it, you could also take out bellow function but vterm opens weirdly without it
  )
;; (global-set-key [M-t] 'vterm-toggle-cd)
;; (global-set-key [C-f2] 'vterm-toggle)
(setq vterm-toggle-fullscreen-p nil)
(add-to-list 'display-buffer-alist
             '((lambda(bufname _) (with-current-buffer bufname
                                    (or (equal major-mode 'vterm-mode)
                                        (string-prefix-p vterm-buffer-name bufname))))
               (display-buffer-reuse-window display-buffer-at-bottom)
               ;;(display-buffer-reuse-window display-buffer-in-direction)
               ;;display-buffer-in-direction/direction/dedicated is added in emacs27
               ;;(direction . bottom)
               ;;(dedicated . t) ;dedicated is supported in emacs27
               (reusable-frames . visible)
               (window-height . 0.3)))

(when (eq system-type 'windows-nt)
  (setq explicit-shell-file-name "powershell.exe")
  (setq explicit-powershell.exe-args '()))

(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "C-r") 'counsel-esh-history)
  (evil-define-key '(normal insert visual) eshell-mode-map (kbd "<home>") 'eshell-bol)
  (evil-normalize-keymaps)

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))
  ;; (eshell-git-prompt-use-theme 'powerline)
  )

(use-package dired
  :ensure nil ;; make sure package manager doesn't try to install
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single
  :commands (dired dired-jump));; doesn't open new buffers like classic jump

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :commands (dired dired-jump)
  :config
  ;; Doesn't work as expected!
  ;;(add-to-list 'dired-open-functions #'dired-open-xdg t)
  (setq dired-open-extensions '(("png" . "feh") ;; use programs for file extensions
                                ("mkv" . "mpv"))))
(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(setq tex-fontify-script nil)

(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions)) ;; do not query to kill the buffer

(use-package smartparens
    ;; :hook (prog-mode . smartparens-mode)
    :config
    (require 'smartparens-config)
    (smartparens-global-mode t)
    ;; (sp-pair "$" "$") 
    (sp-local-pair '(org-mode LaTeX-mode) "$" "$") ;; only use this in org-mode
    ;; (smartparens-global-strict-mode t)
    )
;; (add-hook 'js-mode-hook #'smartparens-mode)
;; (add-hook 'c++-mode-hook #'smartparens-mode)

(use-package evil-smartparens
    :after (smartparens)
    )
(add-hook 'smartparens-enabled-hook #'evil-smartparens-mode) ;; enable evil smartparens when smartparents is up
(add-hook 'smartparens-enabled-hook #'sp-use-smartparens-bindings) ;; enable smartparens keybindings

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package pomodoro
  ;; :commands pomodoro-start
  ;; :config
  ;; (pomodoro-add-to-mode-line)
  ;; :init
  ;; (pomodoro-add-to-mode-line)
  )
  (pomodoro-add-to-mode-line)
  (setq pomodoro-inhibit-prompting-messages nil)
  (setq pomodoro-desktop-notification nil)

;; (use-package tomatinho)

;; (use-package openwith
;;   :commands (openwith-mode)
;;   :config
;;   (setq openwith-associations
;;         (list
;;           (list (openwith-make-extension-regexp
;;                 '("mpg" "mpeg" "mp3" "mp4"
;;                   "avi" "wmv" "wav" "mov" "flv"
;;                   "ogm" "ogg" "mkv"))
;;                 "vlc"
;;                 '(file))
;;           (list (openwith-make-extension-regexp
;;                 '("xbm" "pbm" "pgm" "ppm" "pnm"
;;                   "png" "gif" "bmp" "tif" "jpeg")) ;; Removed jpg because Telega was
;;                   ;; causing feh to be opened...
;;                   "feh"
;;                   '(file))
;;           (list (openwith-make-extension-regexp
;;                 '("pdf"))
;;                 "zathura"
;;                 '(file))))
;;   )

;; after startup, it is important you reset this to some reasonable default. A large 
;; gc-cons-threshold will cause freezing and stuttering during long-term 
;; interactive use. I find these are nice defaults:

  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1
        file-name-handler-alist last-file-name-handler-alist)
