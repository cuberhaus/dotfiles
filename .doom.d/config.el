;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Pol Casacuberta"
      user-mail-address "polcg@hotmail.es")

(setq ispell-aspell-data-dir "~/Dictionary")
(setq ispell-aspell-dict-dir ispell-aspell-data-dir)
;;(setq ispell-aspell-dictionary-alist '())
(add-to-list 'ispell-aspell-dictionary-alist (ispell-aspell-find-dictionary "es_ES"))
(add-to-list 'ispell-aspell-dictionary-alist (ispell-aspell-find-dictionary "ca"))

;; flyspell to activate spell checking
(after! company
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2)
  (setq company-show-numbers t)
  (add-hook 'evil-normal-state-entry-hook #'company-abort)) ;; make aborting less annoying.


;; (after! org-appear )
(setq org-hide-emphasis-markers t)
(add-hook 'org-mode-hook 'org-appear-mode)
(add-hook 'org-mode-hook 'org-fragtog-mode) ;; This should enable org-fragtog when entering org-mode
(global-page-break-lines-mode)
;;;;
;;(setq company-idle-delay 0.2
;;      company-minimum-prefix-length 2)

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/.org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(setq confirm-kill-emacs nil)

(sp-local-pair '(org-mode LaTeX-mode) "$" "$") ;; only use this in org-mode
;; (sp-local-pair '(org-mode LaTeX-mode) "=" "=") ;; only use this in org-mode
(sp-local-pair '(org-mode LaTeX-mode) "{" "}") ;; only use this in org-mode
;; (sp-local-pair '(org-mode LaTeX-mode) "~" "~") ;; only use this in org-mode
;; Custom capture templates
(after! org
   (setq org-capture-templates
      `(("t" "Tasks / Projects")
        ("tt" "Task" entry (file+olp "~/docs/org/Tasks.org" "Inbox")
             "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

        ("j" "Journal Entries")
        ("jj" "Journal" entry
             (file+olp+datetree "~/docs/org/Journal.org")
             "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
             ;; ,(dw/read-file-as-string "~/Notes/Templates/Daily.org")
             :clock-in :clock-resume
             :empty-lines 1)
        ("jm" "Meeting" entry
             (file+olp+datetree "~/docs/org/Journal.org")
             "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
             :clock-in :clock-resume
             :empty-lines 1)

        ("w" "Workflows")
        ("we" "Checking Email" entry (file+olp+datetree "~/docs/org/Journal.org")
             "* Checking Email :email:\n\n%?" :clock-in :clock-resume :empty-lines 1)

        ("m" "Metrics Capture")
        ("mw" "Weight" table-line (file+headline "~/docs/org/Metrics.org" "Weight")
         "| %U | %^{Weight} | %^{Notes} |" :kill-buffer t)))

    )


(setq org-directory "~/docs/org/")

(setq prescient-sort-length-enable nil) ;; do not sort by length, not working in doom i dont think

(with-eval-after-load 'org
    (require 'org-tempo)
    (add-to-list 'org-structure-template-alist '("py" . "src python"))
    (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
    (add-to-list 'org-structure-template-alist '("hs" . "src haskell"))
    (add-to-list 'org-structure-template-alist '("cpp" . "src C++"))
    (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
    )

(setq x-select-enable-clipboard-manager nil); weird emacs bug where it won't close


(use-package! eyebrowse
    :ensure t
    :init
    (setq eyebrowse-keymap-prefix (kbd "")) ;; this seems to work to unbind keybindings :D
    (global-unset-key (kbd "C-c C-w"))
    ;; we have to set this before the package is initialized  https://github.com/wasamasa/eyebrowse/issues/49
    :config
    (eyebrowse-mode t)
    (setq eyebrowse-new-workspace t) ; by default nil, clones last workspace, set to true shows scratch
)
;; https://www.youtube.com/watch?v=QRmKpqDP5yE&ab_channel=ZaisteProgramming
;; lookup function !map within emacs
;; with :desc you can add a description string immediately after

;; (define-key (kbd "SPC m b") nil)

;; to view in which keymap a key is defined check C-h k in keybindings section
(map!
 (:after evil
  :map evil-normal-state-map
 "C-=" nil ;; to set a key to nothing
 "C-+" nil
 ;; "C--" #'text-scale-decrease
 "C-=" #'doom/reset-font-size ;; this was increase for some reason
 "C--" #'text-scale-decrease
 "C-+" #'text-scale-increase ;; this was reset font size lol
  )

 (:after evil
  :map evil-org-mode-map
   :m "g j" nil
   :m "g k" nil
;; (evil-global-set-key 'motion "j" 'evil-next-visual-line) ;; both of these
;; (evil-global-set-key 'motion "k" 'evil-previous-visual-line) ;; are needed for org mode where g-j doesn't work properly

 ;; :desc "go down visual line"  "g j" nil
 ;; :desc "go down visual line"  "g k" nil
 )
 (:leader
      :desc "ws 0" "0" #'eyebrowse-switch-to-window-config-0
      :desc "ws 1" "1" #'eyebrowse-switch-to-window-config-1
      :desc "ws 2" "2" #'eyebrowse-switch-to-window-config-2
      :desc "ws 3" "3" #'eyebrowse-switch-to-window-config-3
      :desc "ws 4" "4" #'eyebrowse-switch-to-window-config-4
      :desc "ws 5" "5" #'eyebrowse-switch-to-window-config-5
      :desc "ws 6" "6" #'eyebrowse-switch-to-window-config-6
      :desc "ws 7" "7" #'eyebrowse-switch-to-window-config-7
      :desc "ws 8" "8" #'eyebrowse-switch-to-window-config-8
      :desc "ws 9" "9" #'eyebrowse-switch-to-window-config-9
      )
      )

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
