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
 '(custom-safe-themes
   '("2dc03dfb67fbcb7d9c487522c29b7582da20766c9998aaad5e5b63b5c27eec3f" default))
 '(epg-pinentry-mode 'loopback)
 '(lsp-go-use-gofumpt t)
 '(menu-bar-mode nil)
 '(package-selected-packages
   '(lsp-mode zenburn-theme helm-xref which-key use-package magit helm eldoc ace-window company flycheck go-mode undo-tree)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'zenburn t)

(global-auto-revert-mode)

(require 'ido)
(ido-mode 1)

(require 'undo-tree)
(global-undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ace-window
(global-set-key (kbd "M-o") 'ace-window)

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
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build\\'")  
  ;;
  ;; (lsp-rust-analyzer-cargo-watch-command "clippy") 
  ;; (lsp-rust-analyzer-server-display-inlay-hints t)  
  :hook (;; c
         (c-mode . lsp)
         ;; c++
         (c++-mode . lsp)
         ;; go
         (go-mode . lsp)
         ;; lsp
         (lsp-mode . lsp-enable-which-key-integration)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company
(use-package 
  company 
  :init (global-set-key (kbd "C-c c") 'company-complete-common) 
  :config (add-hook 'after-init-hook 'global-company-mode) 
  (setq company-idle-delay 0.0 company-minimum-prefix-length 1 lsp-idle-delay 0.1))

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
    helm-config) 
  (use-package 
    helm-xref)
  ;; global key map
  (define-key global-map [remap find-file] #'helm-find-files) 
  (define-key global-map [remap execute-extended-command] #'helm-M-x) 
  (define-key global-map [remap switch-to-buffer] #'helm-mini))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Go
(require 'go-mode)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(defun lsp-go-install-save-hooks () 
  (add-hook 'before-save-hook #'lsp-format-buffer t t) 
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
