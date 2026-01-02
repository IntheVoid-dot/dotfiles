;;; lisp/void-sanity.el --- Void sanity defaults + hygiene  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;;; Code:

(require 'void-core)

;; --- Create required directories -------------------------------------------

(void--ensure-dir void-var-dir)
(void--ensure-dir void-cache-dir)
(void--ensure-dir (expand-file-name "backups/" void-var-dir))
(void--ensure-dir (expand-file-name "auto-save/" void-var-dir))
(void--ensure-dir (expand-file-name "auto-save-list/" void-var-dir))
(void--ensure-dir (expand-file-name "eln-cache/" void-cache-dir))

;; --- Custom file goes to var/ (keeps root clean) ---------------------------

(setq custom-file (expand-file-name "custom.el" void-var-dir))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;; --- Backups & autosaves: never in projects --------------------------------

(setq backup-directory-alist `(("." . ,(expand-file-name "backups/" void-var-dir)))
      backup-by-copying t
      delete-old-versions t
      kept-new-versions 10
      kept-old-versions 3
      version-control t)

(setq auto-save-default t
      auto-save-file-name-transforms `((".*" ,(expand-file-name "auto-save/" void-var-dir) t)))

;; Avoid creating *auto-save-list/* at root (early-init set prefix already)
(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" void-var-dir))

;; --- History/state files in var/ -------------------------------------------

(setq savehist-file (expand-file-name "savehist.el" void-var-dir))
(savehist-mode 1)

(setq recentf-save-file (expand-file-name "recentf" void-var-dir))
(recentf-mode 1)

(setq bookmark-default-file (expand-file-name "bookmarks" void-var-dir))

;; --- Startup: restore sane GC ----------------------------------------------

(add-hook
 'emacs-startup-hook
 (lambda ()
   (setq gc-cons-threshold (* 16 1024 1024)
         gc-cons-percentage 0.1)))

(provide 'void-sanity)
;;; void-sanity.el ends here
