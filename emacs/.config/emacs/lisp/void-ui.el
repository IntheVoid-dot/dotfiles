;;; lisp/void-ui.el --- Void UI: silence & lisibilité  -*- lexical-binding: t; -*-
;;; Commentary:
;; Minimal UI defaults. No theme. No decoration.
;; Everything here serves focus and readability.
;;; Code:

(require 'void-core)

;; --- Frame / window basics -------------------------------------------------

(setq-default
 inhibit-startup-screen t
 inhibit-startup-message t
 inhibit-startup-echo-area-message user-login-name
 frame-title-format nil)

;; No blinking, no noise
(setq visible-bell nil
      ring-bell-function #'ignore)

;; Cursor: steady, readable
(setq-default
 cursor-type 'bar
 blink-cursor-mode nil)

;; --- Fringes & margins -----------------------------------------------------

;; Keep fringes minimal but present (visual breathing room)
(set-fringe-mode '(6 . 6))

;; Avoid surprise horizontal scrolling
(setq-default truncate-lines nil
              word-wrap t)

;; --- Line numbers ----------------------------------------------------------

;; Relative line numbers, only where they make sense
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)

;; Disable where it hurts readability
(dolist (hook '(org-mode-hook
                term-mode-hook
                eshell-mode-hook
                shell-mode-hook))
  (add-hook hook (lambda () (display-line-numbers-mode -1))))

;; --- Scrolling -------------------------------------------------------------

(setq scroll-margin 5
      scroll-conservatively 101
      scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(1 ((shift) . 1))
      mouse-wheel-progressive-speed nil)

;; --- Minibuffer ------------------------------------------------------------

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;; --- Mode line -------------------------------------------------------------

;; Keep mode-line simple for now (theme will refine later)
(setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   " "
   mode-line-buffer-identification
   "  "
   mode-line-position
   "  "
   mode-line-modes))

;; --- Window management -----------------------------------------------------

(setq window-divider-default-right-width 1
      window-divider-default-bottom-width 1
      window-divider-default-places 'right-only)

(window-divider-mode 1)

;; Prefer vertical splits (more readable on wide screens)
(setq split-width-threshold 120
      split-height-threshold nil)

;; --- Misc visual sanity ----------------------------------------------------

;; Highlight current line, but softly (theme will tune faces)
(global-hl-line-mode 1)

;; Matching parens: subtle, no jumping
(setq show-paren-delay 0.1)
(show-paren-mode 1)

;; No visual distractions
(setq-default
 indicate-empty-lines nil
 indicate-buffer-boundaries nil)

(provide 'void-ui)
;;; void-ui.el ends here
