;;; lisp/void-dev.el --- Void dev stack (native-first)  -*- lexical-binding: t; -*-
;;; Commentary:
;; project.el + eglot + flymake + treesit (Emacs 31 native-first).
;;; Code:

(require 'void-core)

;; --- project.el ------------------------------------------------------------

(require 'project)

;; Keep projects calm: use VC roots and remember recent projects
(setq project-vc-extra-root-markers
      '(".project" "Makefile" "justfile" "compile_commands.json" "Cargo.toml" "go.mod" "pyproject.toml"))

;; A couple of sane bindings (minimal)
(global-set-key (kbd "C-c p p") #'project-switch-project)
(global-set-key (kbd "C-c p f") #'project-find-file)
(global-set-key (kbd "C-c p g") #'project-find-regexp)
(global-set-key (kbd "C-c p b") #'project-switch-to-buffer)
(global-set-key (kbd "C-c p k") #'project-kill-buffers)

;; --- treesit (tree-sitter) -------------------------------------------------
;; Emacs 31 includes treesit; grammar installation is external.
(when (and (fboundp 'treesit-available-p) (treesit-available-p))
  ;; Prefer tree-sitter modes when available.
  (setq major-mode-remap-alist
        (append
         '((python-mode . python-ts-mode)
           (js-mode     . js-ts-mode)
           (js2-mode    . js-ts-mode)
           (typescript-mode . typescript-ts-mode)
           (json-mode   . json-ts-mode)
           (css-mode    . css-ts-mode)
           (bash-mode   . bash-ts-mode)
           (c-mode      . c-ts-mode)
           (c++-mode    . c++-ts-mode))
         major-mode-remap-alist)))

;; --- eglot (LSP) -----------------------------------------------------------

(require 'eglot)

;; --- eglot: server programs (Signal's stack) --------------------------------

(defun void--exe (name)
  "Return NAME if it exists in PATH, else nil."
  (when (executable-find name) name))

(defun void--eglot-set-server (modes program)
  "Set eglot server PROGRAM (a list) for MODES if all executables exist."
  (let ((exe (car program)))
    (when (and exe (void--exe exe))
      (dolist (mode modes)
        (setf (alist-get mode eglot-server-programs nil nil #'eq) program)))))

;; Python (IA)
;; Prefer pyright for speed and type info.
(void--eglot-set-server '(python-mode python-ts-mode)
                        '("pyright-langserver" "--stdio"))

;; Rust
(void--eglot-set-server '(rust-mode rust-ts-mode)
                        '("rust-analyzer"))

;; C / C++
(void--eglot-set-server '(c-mode c-ts-mode c++-mode c++-ts-mode)
                        '("clangd" "--background-index" "--clang-tidy"))

;; TypeScript / JavaScript
;; typescript-language-server needs node + typescript in some setups.
(void--eglot-set-server '(typescript-ts-mode tsx-ts-mode
                          js-ts-mode)
                        '("typescript-language-server" "--stdio"))

;; Web: HTML/CSS/JSON
;; Typically provided by vscode-langservers-extracted.
(void--eglot-set-server '(html-mode html-ts-mode mhtml-mode)
                        '("vscode-html-language-server" "--stdio"))
(void--eglot-set-server '(css-mode css-ts-mode)
                        '("vscode-css-language-server" "--stdio"))
(void--eglot-set-server '(json-mode json-ts-mode)
                        '("vscode-json-language-server" "--stdio"))

;; YAML
(void--eglot-set-server '(yaml-mode)
                        '("yaml-language-server" "--stdio"))

;; TOML (optional LSP; nice but not mandatory)
(void--eglot-set-server '(toml-mode toml-ts-mode)
                        '("taplo" "lsp" "stdio"))

;; Don’t spam the echo area; keep it calm.
(setq eglot-events-buffer-size 0
      eglot-autoshutdown t
      eglot-report-progress nil)

;; Start eglot in prog buffers on demand (explicit key)
(global-set-key (kbd "C-c l") #'eglot)

;; Quality-of-life bindings inside eglot-managed buffers
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c f") #'eglot-format)
  (define-key eglot-mode-map (kbd "C-c h") #'eldoc))

;; --- flymake ---------------------------------------------------------------

(require 'flymake)

;; Flymake UX: quiet + useful.
(setq flymake-no-changes-timeout 0.5
      flymake-start-on-save-buffer t
      flymake-start-on-flymake-mode t)

(add-hook 'prog-mode-hook #'flymake-mode)

;; Easy navigation
(global-set-key (kbd "M-n") #'flymake-goto-next-error)
(global-set-key (kbd "M-p") #'flymake-goto-prev-error)

(provide 'void-dev)
;;; void-dev.el ends here
