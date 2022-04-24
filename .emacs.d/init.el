;; The default is 800 kilobytes.  Measured in bytes.
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      file-name-handler-alist nil)

  (setq use-package-verbose t) ;; debug to see which packages load, and maybe shouldn't
    (setq custom-file "~/.emacs.d/custom.el")
    (load custom-file)

;; You will most likely need to adjust this font size for your system!
  (defvar runemacs/default-font-size 110)

  (setq inhibit-startup-message t) ; Disable startup menu
  (scroll-bar-mode -1) ; Disable the scrollbar
  (tool-bar-mode -1)
  ;(tooltip-mode -1) disable tooltips ;; (text displayed when hovering over an element)
  (set-fringe-mode 10) ; Make some space
  (menu-bar-mode -1) ;; remove top bar
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

  (setq scroll-step            1
      scroll-conservatively  10000) ;; scroll line by line not like a fucking degenerate
  (setq smooth-scroll-margin 4) ;; margin like in vim

  (setq vc-follow-symlinks t) ;; always follow symlinks
  (column-number-mode)
  (global-display-line-numbers-mode t) ;; display line numbers everywhere
  ;; (setq vc-follow-symlinks nil) ;; or never follow them

(defun efs/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                   (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'efs/display-startup-time)

;; Font Configuration -----------------------
  ;; (set-face-attribute 'default nil :font "SauceCodePro Nerd Font 11")
  ;; IF FONT LOOKS WEIRD (TOO SLIM) then it means the font is not working properly, CHANGE IT

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
      (set-face-attribute 'default nil :font "FuraCode Nerd Font" :height runemacs/default-font-size)

      ;; Set the fixed pitch face
      (set-face-attribute 'fixed-pitch nil :font "FuraCode Nerd Font" :height 120)

      ;; Set the variable pitch face
      (set-face-attribute 'variable-pitch nil :font "DejaVu Sans" :height 120 :weight 'regular)
    ))
  ;; -------------------------------------------------------

;; execute spanish spell-checking on buffer
(defun flyspell-spanish ()
  (interactive)
  (ispell-change-dictionary "castellano")
  (flyspell-buffer))

(defun flyspell-english ()
  (interactive)
  (ispell-change-dictionary "default")
  (flyspell-buffer))

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
(global-auto-revert-mode) ;;

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                shell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0 ))))

;; (use-package langtool)

;; has to install pdf2svg on pc first
;; (use-package org-inline-pdf
;;   :init
;;   (add-hook 'org-mode-hook #'org-inline-pdf-mode))

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

;; eval last sexp is better cause inconsistencies from hooks when running evalbuffer
;; and show keybindings
(use-package ivy-rich ;; shows better explanations
  :after ivy
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

(use-package all-the-icons)
;; custom command line
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))
(use-package doom-themes) ;; counsel-load-theme to load a theme from the list
(load-theme 'doom-one t) ;; if not using t will prompt if its safe to https://github.com/Malabarba/smart-mode-line/issues/100

(global-set-key (kbd "C-M-j") 'counsel-switch-buffer) ;; easier command to switch buffers
  ;; example (define-key emacs-lisp-mode-map (kbd "C-x M-t") 'counsel-load-theme) define keybinding only in emacs-lisp-mode

(use-package general ;; set personal bindings for leader key for example
 ; (general-define-key "C-M-j" 'counsel-switch-buffer) ;; allows to define multiple global keybindings
  ;; :after evil
  :config
  (general-create-definer rune/leader-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC" 
  :global-prefix "C-SPC") ;; leader
  (rune/leader-keys ;; try to have similar keybindings in vim as well
   "t" '(:ignore t :which-key "toggles") ;; "folder" for toggles
   "b" '(:ignore b :which-key "buffers") 
   "h" '(:ignore h :which-key "git-gutter") 
   "c" '(org-capture :which-key "org-capture") ;; this is F*** awesome
   "g" '(git-gutter-mode :which-key "git-gutter toggle") 
   "hn" '(git-gutter:next-hunk :which-key "next hunk") 
   "hp" '(git-gutter:previous-hunk :which-key "previous hunk") 
   "hv" '(git-gutter:popup-hunk :which-key "preview hunk") 
   "hs" '(git-gutter:stage-hunk :which-key "stage hunk") 
   "hu" '(git-gutter:revert-hunk :which-key "undo hunk") ;; take back changes
   "hg" '(git-gutter :which-key "update changes") 
   "o" '(buffer-menu :which-key "buffer menu") 
   "bn" '(evil-next-buffer :which-key "next buffer") 
   "bp" '(evil-prev-buffer :which-key "previous buffer")
   "bc" '(evil-delete-buffer :which-key "close buffer")
   "bd" '(delete-file-and-buffer :which-key "delete file")
   "w" '(save-buffer :which-key "save buffer") ;; classic vim save
   "tt" '(counsel-load-theme :which-key "choose theme")))

(use-package hydra
  :defer t) ;; emacs bindings that stick around like mode for i3

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))
(rune/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; vim keybindings for easier on the fingers typing :D
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

; C-z go back to EMACS MODE

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)) ;; prog-mode is based mode for any programming language

(use-package which-key ;; This shows which commands are available for current keypresses
  :commands(helpful-callable helpfull-variable helpful-command helpful-key)
  :defer 0
  ;; runs before package is loaded automatically whether package is loaded or not we can also invoke the mode
  :diminish which-key-mode
  :config ;; this is run after the package is loaded
 (which-key-mode)
  (setq which-key-idle-delay 0.15)) ;; delay on keybindings 

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

(defun efs/org-mode-setup ()
        (org-indent-mode)
        (variable-pitch-mode 1) ;; allows text to be of variable size
        (visual-line-mode 1) ;; makes emacs editing commands act on visual lines not logical ones, also word-wrapping, idk if i want this
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
  (with-eval-after-load 'org
      (org-babel-do-load-languages
        'org-babel-load-languages
        '((emacs-lisp . t)
          (python . t)))
      (push '("conf-unix" . conf-unix) org-src-lang-modes)
      )
  (with-eval-after-load 'org
      (require 'org-tempo)
      (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
      (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
      )

        ;; Ensure that anything that should be fixed-pitch in Org files appears that way
        (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
        (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch)) ;; fixed pitch on some stuff so that it lines up correctly, and variable on others so that it looks better
        (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
        (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
        (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
        (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
        (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

      (use-package org  ;; org is already installed though
        :commands (org-capture org-agenda)
        :hook (org-mode . efs/org-mode-setup)
        :config
        (message "Org mode loaded")
        (setq org-ellipsis " ▾") ;; change ... to another symbol that is less confusing
        (efs/org-font-setup) ;; setup font
         ;; hides *bold* and __underlined__ and linked words [name][link]
        (setq org-agenda-start-with-log-mode t)
        (setq org-log-done 'time) ;; logs when a task goes to done C-h-v (describe variable)
        (setq org-log-into-drawer t) ;; collapse logs into a drawer
        (setq org-agenda-files
              '("~/fib/org/birthday.org"
                "~/fib/org/Tasks.org"
                "~/fib/org/Habits.org"
                ))

        (require 'org-habit)
        (add-to-list 'org-modules 'org-habit) ;;  add org-habit to org-modules
        (setq org-habit-graph-column 60) ;; what column the habit tracker shows

        (setq org-todo-keywords
          '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
            (sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

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
            ("tt" "Task" entry (file+olp "~/fib/org/Tasks.org" "Inbox")
                 "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

            ("j" "Journal Entries")
            ("jj" "Journal" entry
                 (file+olp+datetree "~/fib/org/Journal.org")
                 "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
                 ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
                 :clock-in :clock-resume
                 :empty-lines 1)
            ("jm" "Meeting" entry
                 (file+olp+datetree "~/fib/org/Journal.org")
                 "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
                 :clock-in :clock-resume
                 :empty-lines 1)

            ("w" "Workflows")
            ("we" "Checking Email" entry (file+olp+datetree "~/fib/org/Journal.org")
                 "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

            ("m" "Metrics Capture")
            ("mw" "Weight" table-line (file+headline "~/fib/org/Metrics.org" "Weight")
             "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

        )

      (use-package org-bullets ;; changes headers so that it doesn't show all of the stars
        :hook (org-mode . org-bullets-mode)
        :custom
        (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))) ;; default symbols get weird

      (defun efs/org-mode-visual-fill ()
        (setq visual-fill-column-width 100 ;; set column width (character width?)
              visual-fill-column-center-text t) ;; center text on middle of screen
        (visual-fill-column-mode 1))

      (use-package visual-fill-column
        :hook (org-mode . efs/org-mode-visual-fill))

      ;; Automatically tangle our Emacs.org config file when we save it
      (defun efs/org-babel-tangle-config ()
        (when (string-equal (buffer-file-name)
                            (expand-file-name "~/dotfiles/dotfiles/.org/babel.org"))
          ;; Dynamic scoping to the rescue
          (let ((org-confirm-babel-evaluate nil))
            (org-babel-tangle))))

      (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config))) ;; add hook to org mode

;; after startup, it is important you reset this to some reasonable default. A large 
;; gc-cons-threshold will cause freezing and stuttering during long-term 
;; interactive use. I find these are nice defaults:
  (setq gc-cons-threshold 16777216
        gc-cons-percentage 0.1
        file-name-handler-alist last-file-name-handler-alist)
