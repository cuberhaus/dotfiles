;; prefixes: C-x, C-h, M-x
(setq inhibit-startup-message t) ; Disable startup menu

(scroll-bar-mode -1) ; Disable the scrollbar
(tool-bar-mode -1)
;(tooltip-mode -1) disable tooltips
(set-fringe-mode 10) ; Make some space
;(menu-bar-mode -1)
(set-face-attribute 'default nil :font "SauceCodePro Nerd Font 11")
;(setq visible-bell t)
(load-theme 'doom-one)

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
  :bind (("C-s" . swiper)
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" default))
 '(package-selected-packages
   '(doom-themes which-key use-package rainbow-delimiters ivy-rich helpful doom-modeline counsel command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )