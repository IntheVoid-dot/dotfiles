;;; void-lang.el --- Void language configs  -*- lexical-binding: t; -*-
;;; Commentary:
;; Per-language configuration for Signal's stack.
;;; Code:

(require 'void-core)

;; ---------------------------------------------------------------------------
;; Shared dev defaults
;; ---------------------------------------------------------------------------

(defun void-dev--common ()
  "Common settings for programming buffers."
  ;; Indentation: default to spaces, sane tab width
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)

  ;; Whitespace is real, but keep it calm
  (setq-local show-trailing-whitespace t)

  ;; Prefer deleting by hunks
  (setq-local backward-delete-char-untabify-method 'hungry)

  ;; Auto-fill is off in code (we wrap via editor, not text)
  (auto-fill-mode -1))

(add-hook 'prog-mode-hook #'void-dev--common)

;; Display compile buffer without stealing focus too aggressively
(setq compilation-scroll-output 'first-error)

;; ---------------------------------------------------------------------------
;; Python (IA)
;; ---------------------------------------------------------------------------

(defun void-python-setup ()
  (setq-local tab-width 4)
  (setq-local python-indent-offset 4)
  ;; Useful when mixing docs + code
  (setq-local fill-column 88)
  ;; Optional: start eglot automatically for python buffers
  ;; (eglot-ensure)
  )

(add-hook 'python-mode-hook #'void-python-setup)
(add-hook 'python-ts-mode-hook #'void-python-setup)

;; ---------------------------------------------------------------------------
;; Rust
;; ---------------------------------------------------------------------------

(defun void-rust-setup ()
  (setq-local tab-width 4)
  (setq-local fill-column 100)
  ;; rust-analyzer formatting uses rustfmt via eglot-format usually
  ;; (eglot-ensure)
  )

(add-hook 'rust-mode-hook #'void-rust-setup)
(add-hook 'rust-ts-mode-hook #'void-rust-setup)

;; ---------------------------------------------------------------------------
;; C / C++
;; ---------------------------------------------------------------------------

(defun void-c-cpp-setup ()
  (setq-local tab-width 4)
  (setq-local c-basic-offset 4)
  (setq-local fill-column 100)
  ;; (eglot-ensure)
  )

(add-hook 'c-mode-hook #'void-c-cpp-setup)
(add-hook 'c-ts-mode-hook #'void-c-cpp-setup)
(add-hook 'c++-mode-hook #'void-c-cpp-setup)
(add-hook 'c++-ts-mode-hook #'void-c-cpp-setup)

;; ---------------------------------------------------------------------------
;; TypeScript / JS / Web
;; ---------------------------------------------------------------------------

(defun void-ts-js-setup ()
  (setq-local tab-width 2)
  (setq-local fill-column 100)
  ;; (eglot-ensure)
  )

(add-hook 'typescript-ts-mode-hook #'void-ts-js-setup)
(add-hook 'tsx-ts-mode-hook #'void-ts-js-setup)
(add-hook 'js-ts-mode-hook #'void-ts-js-setup)

(defun void-web-setup ()
  (setq-local tab-width 2)
  (setq-local fill-column 100))

(add-hook 'css-ts-mode-hook #'void-web-setup)
(add-hook 'html-mode-hook #'void-web-setup)
(add-hook 'html-ts-mode-hook #'void-web-setup)
(add-hook 'mhtml-mode-hook #'void-web-setup)
(add-hook 'json-ts-mode-hook #'void-web-setup)

;; ---------------------------------------------------------------------------
;; TOML / YAML / JSON (data)
;; ---------------------------------------------------------------------------

(defun void-data-setup ()
  (setq-local tab-width 2)
  (setq-local fill-column 100))

(add-hook 'toml-ts-mode-hook #'void-data-setup)
(add-hook 'yaml-mode-hook #'void-data-setup)

;; ---------------------------------------------------------------------------
;; Shell
;; ---------------------------------------------------------------------------

(defun void-shell-setup ()
  (setq-local tab-width 2)
  (setq-local sh-basic-offset 2)
  (setq-local sh-indentation 2))

(add-hook 'sh-mode-hook #'void-shell-setup)
(add-hook 'bash-ts-mode-hook #'void-shell-setup)

(provide 'void-lang)
;;; void-lang.el ends here
