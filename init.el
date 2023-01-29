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
 '(bazel-buildifier-before-save t)
 '(custom-safe-themes
   '("2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f" default))
 '(indent-tabs-mode nil)
 '(lsp-file-watch-ignored-directories
   '("[/\\\\]\\.git\\'" "[/\\\\]\\.github\\'" "[/\\\\]\\.circleci\\'" "[/\\\\]\\.hg\\'" "[/\\\\]\\.bzr\\'" "[/\\\\]_darcs\\'" "[/\\\\]\\.svn\\'" "[/\\\\]_FOSSIL_\\'" "[/\\\\]\\.idea\\'" "[/\\\\]\\.ensime_cache\\'" "[/\\\\]\\.eunit\\'" "[/\\\\]node_modules" "[/\\\\]\\.yarn\\'" "[/\\\\]\\.fslckout\\'" "[/\\\\]\\.tox\\'" "[/\\\\]dist\\'" "[/\\\\]dist-newstyle\\'" "[/\\\\]\\.stack-work\\'" "[/\\\\]\\.bloop\\'" "[/\\\\]\\.metals\\'" "[/\\\\]target\\'" "[/\\\\]\\.ccls-cache\\'" "[/\\\\]\\.vscode\\'" "[/\\\\]\\.venv\\'" "[/\\\\]\\.mypy_cache\\'" "[/\\\\]\\.deps\\'" "[/\\\\]build-aux\\'" "[/\\\\]autom4te.cache\\'" "[/\\\\]\\.reference\\'" "bazel-[^/\\\\]+\\'" "[/\\\\]\\.meta\\'" "[/\\\\]\\.lsp\\'" "[/\\\\]\\.clj-kondo\\'" "[/\\\\]\\.shadow-cljs\\'" "[/\\\\]\\.babel_cache\\'" "[/\\\\]\\.cpcache\\'" "[/\\\\]\\checkouts\\'" "[/\\\\]\\.gradle\\'" "[/\\\\]\\.m2\\'" "[/\\\\]bin/Debug\\'" "[/\\\\]obj\\'" "[/\\\\]_opam\\'" "[/\\\\]_build\\'" "[/\\\\]\\.elixir_ls\\'" "[/\\\\]\\.direnv\\'" "[/\\\\]build\\'"))
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(pinentry bazel yasnippet clang-format protobuf-mode lsp-treemacs lsp-mode zenburn-theme helm-xref which-key use-package magit helm eldoc ace-window company flycheck go-mode undo-tree))
 '(split-height-threshold 200)
 '(tab-width 4)
 '(undo-tree-history-directory-alist '(("." . "~/.undo-tree")))
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
;; GPG
(require 'epg)
(setq epg-pinentry-mode 'loopback)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zenburn
(load-theme 'zenburn t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; global auto revert
(global-auto-revert-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ido
(require 'ido)
(ido-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ace-window
(require 'ace-window)
(global-set-key (kbd "M-o") 'ace-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setting up languages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp
(use-package 
  lsp 
  :init
  ;; disable the headerline
  (setq lsp-headerline-breadcrumb-enable nil)
  ;; remap the keys
  (setq lsp-keymap-prefix "C-c l")
  :config
  ;; (lsp-treemacs-sync-mode 1)
  (setq lsp-eldoc-render-all t)  
  ;; (lsp-rust-analyzer-cargo-watch-command "clippy") 
  ;; (lsp-rust-analyzer-server-display-inlay-hints t)
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
;; eldoc
(use-package 
  eldoc 
  :config (global-eldoc-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company
(use-package 
  company 
  :init (global-set-key (kbd "C-c c") 'company-complete-common) 
  :config (add-hook 'after-init-hook 'global-company-mode) 
  (setq company-idle-delay 0.0 company-minimum-prefix-length 1 lsp-idle-delay 0.1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; yasnippet
(use-package 
  yasnippet 
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
  :config
  ;; helm
  (use-package 
    helm-xref)
  ;; global key map
  (define-key global-map [remap find-file] #'helm-find-files) 
  (define-key global-map [remap execute-extended-command] #'helm-M-x) 
  (define-key global-map [remap switch-to-buffer] #'helm-mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; clang-format
(require 'clang-format)
(defun my-format-before-save () 
  (add-hook 'before-save-hook 'clang-format-buffer nil 'local))
(add-hook 'c++-mode-hook  'my-format-before-save)
(add-hook 'c-mode-hook  'my-format-before-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go
(use-package
  go-mode
  :config
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (setq lsp-go-use-gofumpt t)
  :init
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move-lang
;; (add-to-list 'load-path (concat user-emacs-directory "lisp/"))
;; (load "move-mode")
;; (add-to-list 'auto-mode-alist '("\\.move\\'" . move-mode))
;; (with-eval-after-load 'lsp-mode
;;    (add-to-list 'lsp-language-id-configuration '(move-mode . "move"))
;;    (lsp-register-client
;;     (make-lsp-client
;;      :new-connection (lsp-stdio-connection "move-analyzer")
;;      :activation-fn (lsp-activate-on "move")
;;      :priority -1
;;      :server-id 'move-analyzer)))
;; (add-hook 'move-mode-hook 'lsp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protobuf
(require 'protobuf-mode)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))
(add-hook 'protobuf-mode-hook  'my-format-before-save)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bazel
(use-package 
  bazel 
  :config (add-to-list 'auto-mode-alist '("\\BUILD\\'" . bazel-build-mode)) 
  (add-to-list 'auto-mode-alist '("\\BUILD.bazel\\'" . bazel-build-mode)) 
  (add-to-list 'auto-mode-alist '("\\WORKSPACE\\'" . bazel-workspace-mode)) 
  (add-to-list 'auto-mode-alist '("\\WORKSPACE.bazel\\'" . bazel-workspace-mode)) 
  (add-to-list 'auto-mode-alist '("\\.bzl\\'" . bazel-starlark-mode)) 
  (add-to-list 'auto-mode-alist '("\\.bazelrc\\'" . bazelrc-mode)) 
  (add-to-list 'auto-mode-alist '("\\.bazelignore\\'" . bazelignore-mode)))

;;; init.el ends here
