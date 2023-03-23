(require 'package)

;; Add necessary
(setq package-archives (list '("melpa" . "https://melpa.org/packages/")
                             '("gnu" . "https://elpa.gnu.org/packages/")
                             '("org" . "https://orgmode.org/elpa/")))

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
 '(apheleia-mode-alist
   '((php-mode . phpcs)
     (json-mode . prettier-json)
     (json-ts-mode . prettier-json)
     (bash-ts-mode . shfmt)
     (beancount-mode . bean-format)
     (c++-ts-mode . clang-format)
     (caddyfile-mode . caddyfmt)
     (cc-mode . clang-format)
     (c-mode . clang-format)
     (c-ts-mode . clang-format)
     (c++-mode . clang-format)
     (caml-mode . ocamlformat)
     (common-lisp-mode . lisp-indent)
     (crystal-mode . crystal-tool-format)
     (css-mode . prettier-css)
     (css-ts-mode . prettier-css)
     (dart-mode . dart-format)
     (elixir-mode . mix-format)
     (elixir-ts-mode . mix-format)
     (elm-mode . elm-format)
     (fish-mode . fish-indent)
     (go-mode . gofmt)
     (go-mod-ts-mode . gofmt)
     (go-ts-mode . gofmt)
     (graphql-mode . prettier-graphql)
     (haskell-mode . brittany)
     (html-mode . prettier-html)
     (java-mode . google-java-format)
     (java-ts-mode . google-java-format)
     (js3-mode . prettier-javascript)
     (js-mode . prettier-javascript)
     (js-ts-mode . prettier-javascript)
     (kotlin-mode . ktlint)
     (latex-mode . latexindent)
     (LaTeX-mode . latexindent)
     (lua-mode . stylua)
     (lisp-mode . lisp-indent)
     (nix-mode . nixfmt)
     (python-mode . black)
     (python-ts-mode . black)
     (ruby-mode . prettier-ruby)
     (ruby-ts-mode . prettier-ruby)
     (rustic-mode . rustfmt)
     (rust-mode . rustfmt)
     (rust-ts-mode . rustfmt)
     (scss-mode . prettier-scss)
     (terraform-mode . terraform)
     (TeX-latex-mode . latexindent)
     (TeX-mode . latexindent)
     (tsx-ts-mode . prettier-typescript)
     (tuareg-mode . ocamlformat)
     (typescript-mode . prettier-typescript)
     (typescript-ts-mode . prettier-typescript)
     (web-mode . prettier)
     (yaml-mode . prettier-yaml)
     (yaml-ts-mode . prettier-yaml)
     (markdown-mode . prettier)))
 '(bazel-buildifier-before-save t)
 '(custom-safe-themes
   '("2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f" default))
 '(indent-tabs-mode nil)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(apheleia auctex magit markdown-mode js2-mode web-mode typescript-mode tide rustic pinentry bazel yasnippet clang-format protobuf-mode lsp-treemacs lsp-mode zenburn-theme helm-xref which-key use-package helm eldoc ace-window company flycheck go-mode undo-tree))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; all the interfacing stuff that has nothing to do with programming languages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
(use-package magit
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GPG
;; don't do this on mac
(unless (eq system-type 'darwin)
  (require 'epg)
  (setq epg-pinentry-mode 'loopback)
  (use-package pinentry
    :ensure t
    :config
    (pinentry-start)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zenburn
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global auto revert
(global-auto-revert-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido
(require 'ido)
(ido-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo-tree
(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.saves")))
  (global-undo-tree-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ace-window
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting up languages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp
(use-package lsp-mode
  :ensure t
  :init
  ;; disable the headerline
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; remap the keys
  (setq lsp-keymap-prefix "C-c l")
  ;; eldoc render all
  (setq lsp-eldoc-render-all t)
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build\\'")
  (lsp-register-custom-settings
   '(("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)))
  :hook (;; c
         (c-mode . lsp)
         ;; c++
         (c++-mode . lsp)
         ;; go
         (go-mode . lsp)
         ;; lsp
         (lsp-mode . lsp-enable-which-key-integration)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; whichkey
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; eldoc
(use-package
  eldoc
  :config (global-eldoc-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company
(use-package
  company
  :ensure t
  :init (global-set-key (kbd "C-c c") 'company-complete-common)
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-idle-delay 0.0 company-minimum-prefix-length 1 lsp-idle-delay 0.1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet
(use-package
  yasnippet
  :ensure t
  :config (yas-global-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flycheck
(use-package
  flycheck
  :ensure t
  :init (global-flycheck-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helm
(use-package
  helm
  :ensure t
  :config
  ;; helm
  ;; global key map
  (define-key global-map [remap find-file] #'helm-find-files)
  (define-key global-map [remap execute-extended-command] #'helm-M-x)
  (define-key global-map [remap switch-to-buffer] #'helm-mini))

(use-package helm-xref
  :after (helm)
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang-format
(use-package clang-format
  :ensure t
  :config
  (defun my-format-before-save ()
    (add-hook 'before-save-hook 'clang-format-buffer nil 'local))
  (add-hook 'c++-mode-hook  'my-format-before-save)
  (add-hook 'c-mode-hook  'my-format-before-save)
  (add-hook 'protobuf-mode 'my-format-before-save)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; apheleia prettier
(use-package apheleia
  :ensure t
  :hook ((markdown-mode . apheleia-mode))
  :config
  (with-eval-after-load 'markdown-mode
    (add-to-list 'apheleia-mode-alist '(markdown-mode . prettier))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go
(use-package
  go-mode
  :ensure t
  :mode "\\.go\\'"
  :config
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (setq lsp-go-use-gofumpt t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protobuf
(use-package protobuf-mode
  :ensure t
  :mode ("\\.proto\\'" . protobuf-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bazel
(use-package
  bazel
  :ensure t
  :mode
  ("\\BUILD\\'" . bazel-build-mode)
  ("\\BUILD.bazel\\'" . bazel-build-mode)
  ("\\WORKSPACE\\'" . bazel-workspace-mode)
  ("\\WORKSPACE.bazel\\'" . bazel-workspace-mode)
  ("\\.bzl\\'" . bazel-starlark-mode)
  ("\\.bazelrc\\'" . bazelrc-mode)
  ("\\.bazelignore\\'" . bazelignore-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Typescript
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode))
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append))

(use-package js2-mode
  :after (tide)
  :ensure t
  :mode "\\.js\\'"
  :config
  (add-hook 'js2-mode-hook 'setup-tide-mode))

(use-package web-mode
  :ensure t
  :mode "\\.jsx\\'" "\\.tsx\\'"
  :config
  (add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
  (add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTex
(require 'tex-site)
(use-package tex
  :ensure auctex
  :mode ("\\.ltx\\'" . LaTeX-mode)
  :config
  ;; local configuration for TeX modes
  (defun my-latex-mode-setup ()
  (setq-local company-backends (append '((company-math-symbols-latex company-latex-commands))
                                       company-backends)))
  (add-hook 'LaTeX-mode-hook 'my-latex-mode-setup)
  (add-hook 'LaTex-mode-hook 'flyspell-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; rust
(use-package rustic
  :ensure t
  :hook
  (rustic-mode . lsp-rust-analyzer-inlay-hints-mode)
  :init
  (with-eval-after-load 'lsp-rust
    (setq lsp-rust-analyzer-cargo-watch-command "clippy")
    (setq lsp-rust-analyzer-server-display-inlay-hints t))
  :config
  (setq rustic-format-trigger 'on-save))

;;; init.el ends here
