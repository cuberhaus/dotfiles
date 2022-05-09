;; -*- lexical-binding: t; -*-
;; The default is 800 kilobytes.  Measured in bytes.
(defvar last-file-name-handler-alist file-name-handler-alist)
(setq gc-cons-threshold 402653184
       gc-cons-percentage 0.6
       file-name-handler-alist nil)

(setq inhibit-startup-message t) ; Disable startup menu
(scroll-bar-mode -1) ; Disable the scrollbar
(tool-bar-mode -1)
;(tooltip-mode -1) disable tooltips ;; (text displayed when hovering over an element)
(set-fringe-mode 10) ; Make some space
(menu-bar-mode -1) ;; remove top bar
