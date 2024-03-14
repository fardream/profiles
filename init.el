;;; package --- Summary
;;; Commentary:
;;; personal init
(require 'package)

;;; Code:
(setq package-archives
      (list
       '("melpa" . "https://melpa.org/packages/")
       '("gnu" . "https://elpa.gnu.org/packages/")
       '("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

;; disable lockfiles
(setq create-lockfiles nil)

;; Change the location of the saves
(setq backup-directory-alist `(("." . "~/.saves")))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f"
     default))
 '(indent-tabs-mode nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(ace-window
     apheleia
     auctex
     bazel
     bind-key
     clang-format
     company
     consult
     cython-mode
     dockerfile-mode
     eat
     eldoc
     elisp-autofmt
     ess
     faceup
     flycheck
     go-mode
     js2-mode
     json-mode
     julia-mode
     julia-repl
     lsp-julia
     lsp-mode
     lsp-treemacs
     lua-mode
     magit
     markdown-mode
     org
     pinentry
     protobuf-mode
     rustic
     tide
     typescript-mode
     undo-tree
     use-package
     verilog-mode
     vertico
     web-mode
     which-key
     yaml-mode
     yasnippet
     zenburn-theme))
 '(split-height-threshold 200)
 '(tab-width 4)
 '(vc-follow-symlinks t)
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; below is from magit warning
(setq package-install-upgrade-built-in t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; all the interfacing stuff that has nothing to do with programming languages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; vertico
(use-package vertico :ensure t :init (vertico-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; consult
(use-package
 consult
 :ensure t
 :hook (completion-list-mode . consult-preview-at-point-mode)
 ;; The :init configuration is always executed (Not lazy)
 :init

 ;; Optionally configure the register formatting. This improves the register
 ;; preview for `consult-register', `consult-register-load',
 ;; `consult-register-store' and the Emacs built-ins.
 (setq
  register-preview-delay 0.5
  register-preview-function #'consult-register-format)

 ;; Optionally tweak the register preview window.
 ;; This adds thin lines, sorting and hides the mode line of the window.
 (advice-add #'register-preview :override #'consult-register-window)

 ;; Use Consult to select xref locations with preview
 (setq
  xref-show-xrefs-function #'consult-xref
  xref-show-definitions-function #'consult-xref)
 :config

 ;; Optionally configure preview. The default value
 ;; is 'any, such that any key triggers the preview.
 ;; (setq consult-preview-key 'any)
 ;; (setq consult-preview-key "M-.")
 ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
 ;; For some commands and buffer sources it is useful to configure the
 ;; :preview-key on a per-command basis using the `consult-customize' macro.
 (consult-customize
  consult-theme
  :preview-key
  '(:debounce 0.2 any)
  consult-ripgrep
  consult-git-grep
  consult-grep
  consult-bookmark
  consult-recent-file
  consult-xref
  consult--source-bookmark
  consult--source-file-register
  consult--source-recent-file
  consult--source-project-recent-file
  ;; :preview-key "M-."
  :preview-key '(:debounce 0.4 any))

 ;; Optionally configure the narrowing key.
 ;; Both < and C-+ work reasonably well.
 (setq consult-narrow-key "<") ;; "C-+"
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
(use-package magit :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GPG
;; don't do this on mac
(unless (eq system-type 'darwin)
  (require 'epg)
  (setq epg-pinentry-mode 'loopback)
  (use-package pinentry :ensure t :config (pinentry-start)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zenburn
(use-package zenburn-theme :ensure t :config (load-theme 'zenburn t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global auto revert
(global-auto-revert-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido
(require 'ido)
(ido-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo-tree
(use-package
 undo-tree
 :ensure t
 :config
 (setq undo-tree-history-directory-alist '(("." . "~/.saves")))
 (global-undo-tree-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ace-window
(use-package
 ace-window
 :ensure t
 :config (global-set-key (kbd "M-o") 'ace-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting up languages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp
(use-package
 lsp-mode
 :ensure t
 :init
 ;; disable the headerline
 (setq lsp-headerline-breadcrumb-enable nil)
 ;; remap the keys
 (setq lsp-keymap-prefix "C-c l")
 ;; eldoc render all
 (setq lsp-eldoc-render-all t)
 (defun lsp-python-install-save-hooks ()
   (setq lsp-pylsp-plugins-black-enabled t)
   (add-hook 'before-save-hook #'lsp-format-buffer t t))
 (add-hook 'python-mode-hook #'lsp-python-install-save-hooks)
 :config
 (dolist (e
          '("[/\\\\]build\\'"
            "[/\\\\]external\\'"
            "[/\\\\]bazel-.*\\'"))
   (add-to-list 'lsp-file-watch-ignored-directories e))
 :hook
 ( ;; c
  (c-mode . lsp)
  ;; c++
  (c++-mode . lsp)
  ;; python
  (python-mode . lsp)
  ;; rust turn this off
  ;; (rustic-mode . lsp)
  ;; R
  (ess-r-mode . lsp)
  ;; julia
  (julia-mode . lsp)
  ;; lsp
  (lsp-mode . lsp-enable-which-key-integration)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; whichkey
(use-package which-key :ensure t :config (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eldoc
(use-package
 eldoc
 :config
 ;; make it show up everywhere
 (global-eldoc-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company
(use-package
 company
 :ensure t
 :init (global-set-key (kbd "C-c c") 'company-complete-common)
 :config (add-hook 'after-init-hook 'global-company-mode)
 (setq
  company-idle-delay 0.0
  company-minimum-prefix-length 1
  lsp-idle-delay 0.1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet
(use-package yasnippet :ensure t :config (yas-global-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck
(use-package flycheck :ensure t :init (global-flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang-format
(use-package
 clang-format
 :ensure t
 :init (setq clang-format-fallback-style "Google")
 :config
 (defun my-format-before-save ()
   (add-hook 'before-save-hook 'clang-format-buffer nil 'local))
 (add-hook 'c++-mode-hook 'my-format-before-save)
 (add-hook 'c-mode-hook 'my-format-before-save)
 (add-hook 'protobuf-mode-hook 'my-format-before-save))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; apheleia prettier
(use-package
 apheleia
 :ensure t
 :init
 (defun prettier-apheleia-mode ()
   "Setup apheleia mode."
   (interactive)
   (apheleia-mode)
   (setq apheleia-formatter 'prettier))
 (add-hook 'markdown-mode-hook 'prettier-apheleia-mode)
 (add-hook 'conf-toml-mode-hook 'prettier-apheleia-mode)
 :hook
 (json-mode . apheleia-mode)
 (yaml-mode . apheleia-mode)
 (conf-toml-mode . apheleia-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go
(use-package
 go-mode
 :ensure t
 :mode "\\.go\\'"
 :config
 (defun go-hooks ()
   (add-hook 'before-save-hook #'lsp-format-buffer t t)
   (add-hook 'before-save-hook #'lsp-organize-imports t t))
 (add-hook 'go-mode-hook #'go-hooks)
 :custom (lsp-go-use-gofumpt t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protobuf
(use-package
 protobuf-mode
 :ensure t
 :mode ("\\.proto\\'" . protobuf-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bazel
(use-package
 bazel
 :load-path "~/github.com/phst/emacs-bazel-mode"
 :custom (bazel-buildifier-before-save t)
 :config
 (if (file-exists-p "~/bin/buildifier-with-lint")
     (setq bazel-buildifier-command "buildifier-with-lint"))
 :mode
 ("\\BUILD\\'" . bazel-build-mode)
 ("\\BUILD.bazel\\'" . bazel-build-mode)
 ("\\MODULE.bazel\\'" . bazel-module-mode)
 ("\\WORKSPACE\\'" . bazel-workspace-mode)
 ("\\WORKSPACE.bazel\\'" . bazel-workspace-mode)
 ("\\.bzl\\'" . bazel-starlark-mode)
 ("\\.bazelrc\\'" . bazelrc-mode)
 ("\\.bazelignore\\'" . bazelignore-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typescript
(defun setup-tide-mode ()
  "Setup tide mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1)
  (apheleia-mode +1))

(use-package
 tide
 :ensure t
 :after (typescript-mode company flycheck)
 :hook
 ((typescript-mode . tide-setup)
  (typescript-mode . tide-hl-identifier-mode))
 :config
 (flycheck-add-mode 'javascript-eslint 'web-mode)
 (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append))

(use-package
 js2-mode
 :after (tide)
 :ensure t
 :mode "\\.js\\'"
 :config (add-hook 'js2-mode-hook 'setup-tide-mode))

(use-package
 web-mode
 :ensure t
 :mode "\\.jsx\\'" "\\.tsx\\'"
 :config
 (add-hook
  'web-mode-hook
  (lambda ()
    (when (string-equal "jsx" (file-name-extension buffer-file-name))
      (setup-tide-mode))))
 (add-hook
  'web-mode-hook
  (lambda ()
    (when (string-equal "tsx" (file-name-extension buffer-file-name))
      (setup-tide-mode)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTex
(use-package
 tex
 :ensure auctex
 :mode ("\\.ltx\\'" . LaTeX-mode)
 :config
 ;; local configuration for TeX modes
 (defun my-latex-mode-setup ()
   (setq-local company-backends
               (append
                '((company-math-symbols-latex company-latex-commands)) company-backends)))
 (add-hook 'LaTeX-mode-hook 'my-latex-mode-setup)
 (add-hook 'LaTex-mode-hook 'flyspell-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rust
(use-package
 rustic
 :ensure t
 ;; :hook (rustic-mode . lsp-inlay-hints-mode)
 :init
 ;; (with-eval-after-load 'lsp-rust
 ;;   (setq lsp-rust-analyzer-cargo-watch-command "clippy")
 ;;   (setq lsp-rust-analyzer-server-display-inlay-hints t)
 ;;   (setq lsp-rust-analyzer-use-rustc-wrapper-for-build-scripts t))
 :config (setq rustic-format-trigger 'on-save))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; markdown
(use-package markdown-mode :ensure t :mode ("\\.MD\\'" "\\.md\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; json
(use-package json-mode :ensure t :mode "\\.json\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yaml
(use-package yaml-mode :ensure t :mode "\\.yml")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; R
(use-package
 ess-r-mode
 :ensure ess
 :mode ("\\.R\\'" "\\.r\\'")
 :init (setq ess-use-flymake nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; julia
(use-package
 julia-mode
 :ensure t
 :after lsp-mode
 :config
 (defun lsp-julia-install-save-hooks ()
   (add-hook 'before-save-hook #'lsp-format-buffer t t))
 (add-hook 'lsp-mode-hook #'lsp-julia-install-save-hooks))

(use-package lsp-julia :ensure t :after (lsp-mode julia-mode))

(use-package
 julia-repl
 :ensure t
 :after julia-mode
 :hook (julia-mode . julia-repl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elisp
(use-package elisp-autofmt :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eat
(use-package
 eat
 :ensure t
 :config (add-to-list 'eat-semi-char-non-bound-keys '[M-o]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dockerfile-mode
(use-package dockerfile-mode :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lua-mode
(use-package lua-mode :ensure t :mode ("\\.lua\\'"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; make some variables safe
(put 'lsp-go-env 'safe-local-variable #'listp)
(put 'lsp-go-goimports-local 'safe-local-variable #'stringp)
(put 'lsp-go-directory-filters 'safe-local-variable #'listp)
(put 'lsp-go-codelenses 'safe-local-variable #'listp)
(put 'lsp-go-gopls-server-args 'safe-local-variable #'listp)

(add-hook
 'hack-local-variables-hook
 (lambda ()
   (when (derived-mode-p 'go-mode)
     (lsp))))
;;; init.el ends here
