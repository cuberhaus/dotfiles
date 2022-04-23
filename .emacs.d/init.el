;; prefixes: C-x, C-h, M-x
;; emacs swap window C-x o
(setq inhibit-startup-message t) ; Disable startup menu

(scroll-bar-mode -1) ; Disable the scrollbar
(tool-bar-mode -1)
;(tooltip-mode -1) disable tooltips
(set-fringe-mode 10) ; Make some space
(menu-bar-mode -1) ;; remove top bar
(set-face-attribute 'default nil :font "SauceCodePro Nerd Font 11")
;(setq visible-bell t)
(load-theme 'doom-one t) ;; if not using t will prompt if its safe to https://github.com/Malabarba/smart-mode-line/issues/100

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
(setq x-select-enable-clipboard-manager nil); weird emacs bug where it won't close
;(use-package command-log-mode)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(global-set-key (kbd "C-M-j") 'counsel-switch-buffer) ;; easier command to switch buffers
;; example (define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme) define keybinding only in emacs-lisp-mode

(use-package all-the-icons)
;; custom command line
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes) ;; counsel-load-theme to load a theme from the list

(column-number-mode)
(global-display-line-numbers-mode t) ;; display line numbers everywhere

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0 ))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)) ;; prog-mode is based mode for any programming language

(use-package which-key ;; This shows which commands are available for current keypresses
  :init (which-key-mode) ;; runs before package is loaded automatically whether package is loaded or not we can also invoke the mode
  :diminish which-key-mode
  :config ;; this is run after the package is loaded
  (setq which-key-idle-delay 0.15)) ;; delay on keybindings 

;; eval last sexp is better cause inconsistencies from hooks when running evalbuffer
;; and show keybindings
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; With ivy-rich shows descriptions for commands 
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	  ("C-x b" . counsel-ibuffer)
	  ("C-x C-f" . counsel-find-file)
	  :map minibuffer-local-map
	  ("C-r" . 'counsel-minibuffer-history))
	 :config
	 (setq ivy-initial-inputs-alist nil))

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

(use-package general ;; set personal bindings for leader key for example
 ; (general-define-key "C-M-j" 'counsel-switch-buffer) ;; allows to define multiple global keybindings
  :config
  (general-create-definer rune/leader-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC" 
  :global-prefix "C-SPC") ;; leader
  (rune/leader-keys
   "t" '(:ignore t :which-key "toggles") ;; "folder" for toggles
   "b" '(:ignore b :which-key "buffers") ;; "folder" for toggles
   "bn" '(evil-next-buffer :which-key "next buffer") ;; "folder" for toggles
   "bp" '(evil-prev-buffer :which-key "previous buffer") ;; "folder" for toggles
   "bd" '(delete-file-and-buffer :which-key "delete file") ;; classic vim save
   "w" '(save-buffer :which-key "save buffer") ;; classic vim save
   "tt" '(counsel-load-theme :which-key "choose theme")))

;; vim keybindings for easier on the fingers typing :D
;; C-r WONT WORK, cause we are not using a redo we have a undo stack
;; undo is treated like a normal command, therefore you can undo the undo.
;; undo's will go to the stack, if a move like "k" is done then you can undo again, this makes it possible to undo every single edit.
;; in typicall (global-undo-tree-mode) (evil-set-undo-system 'undo-tree)
;; if you are doing several undos and miss the "correct spot" and do anything at all which is not an undo command, you are stuck: you broke the chain of undos https://www.emacswiki.org/emacs/RedoMode

(use-package evil
  :init
  (setq evil-want-integration t) ;; must have
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;(define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  ;(evil-global-set-key 'motion "j" 'evil-next-visual-line)
  ;(evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))
;; to center screen on cursor, zz or emacs-style C-l

;; https://github.com/linktohack/evil-commentary
;; use-package makes it so that it installs it from config and config section
;; activates the mode
(use-package evil-commentary
  :config
  (evil-commentary-mode))

(use-package evil-collection
  :after evil ;; load after evil, must have
  :config
  (evil-collection-init))

; C-z go back to EMACS MODE
;;  if package give error try:type list-packages command to update packages
 
(use-package hydra) ;; emacs bindings that stick around like mode for i3

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; C-c are user defined commands
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
  :config(counsel-projectile-mode)) 

;; bring in the GIT
(use-package magit ;; use tab to open instead of za in vim
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )

;; C-n C-p in normal mode to go back and forth the clipboard

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
 '(package-selected-packages
   '(evil-magit magit counsel-projectile projectile evil-commentary evil-commentary-mode hydra evil-collection evil general doom-themes which-key use-package rainbow-delimiters ivy-rich helpful doom-modeline counsel command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
