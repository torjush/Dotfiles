(require 'package)
;;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t))

(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))
(setq load-prefer-newer t)

;; Startup
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Global
(delete-selection-mode 1)
(tool-bar-mode -1)
(setq column-number-mode t)
(setq show-trailing-whitespace t)
(electric-pair-mode t)
(setq require-final-newline t)
(setq gc-cons-threshold (* 100 1024 1024))
(setq read-process-output-max (* 1024 1024))

;; genmake
(add-to-list 'auto-mode-alist '("\\.def\\'" . python-mode))

;; Meta key
(setq mac-left-option-modifier 'meta)
(setq mac-right-option-modifier 'none)

;; Terminal emacs
(unless (display-graphic-p)
  (setq xterm-extra-capabilities "check")
  (xterm-mouse-mode))

;; Redirect backups
(setq backup-directory-alist `(("." . "~/.backups_emacs")))

;;  PACKAGES  ;;
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

(use-package move-text
  :ensure t
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)))

;; Appearance
(use-package tree-sitter
  :ensure t
  :init
  (global-tree-sitter-mode)
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode))
(use-package tree-sitter-langs
  :ensure t)

(use-package nord-theme
  :ensure t
  :init (load-theme 'nord t))

(use-package display-line-numbers
  :ensure t
  :init
  (setq-default display-line-numbers 'relative) ;; or 'visual or 'absolute
  :hook
  (prog-mode . display-line-numbers-mode))

;; Org
;; (use-package org
;;   :ensure t
;;   :init
;;   (global-set-key (kbd "C-c a") 'org-agenda)
;;   (global-set-key (kbd "C-c l") 'org-store-link)
;;   (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE"))
;;         org-agenda-files (directory-files-recursively "~/Documents/org/todos/by_theme" "\\.org$")
;;         org-log-done 'time))

;; Magit
(use-package magit
  :ensure t
  :init (setq-default with-editor-emacsclient-executable "emacsclient"))

;; Helm
(use-package helm
  :ensure t
  :bind
  (("C-x b" . 'helm-mini)
   ("C-x r b" . 'helm-bookmarks)
   ("M-x" . 'helm-M-x)
   ("M-y" . 'helm-show-kill-ring)
   ("C-x C-f" . 'helm-find-files)
   ("C-c g" . (lambda ()
                (interactive)
                (let ((current-prefix-arg 1))
                  (call-interactively 'helm-grep-do-git-grep))))))
;; Projectile
(use-package projectile
  :ensure t
  :config
  (setq projectile-enable-caching t)
  :init
  (projectile-mode))

(use-package helm-projectile
  :ensure t
  :init (global-set-key (kbd "C-x C-x") 'helm-projectile-find-file))

(use-package helm-xref
  :ensure t)

;; Company mode
(use-package cc-mode
  :ensure t)
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))
(use-package company-quickhelp
  :ensure t
  :init
  (company-quickhelp-mode))

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind
  (("C-c C-l" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-a" . mc/mark-all-like-this)
   ("C-c C-d" . mc/skip-to-next-like-this)
   ("s-<mouse-1>" . mc/add-cursor-on-click)))

(use-package transpose-frame
  :ensure t)

;; dired-sidebar
(use-package dired-sidebar
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands))

  ;; (setq dired-sidebar-subtree-line-prefix "__")
  ;; (setq dired-sidebar-theme 'vscode)
  ;; (setq dired-sidebar-use-term-integration t)
  ;; (setq dired-sidebar-use-custom-font t))


;; lsp
(use-package lsp-mode
  :commands lsp
  :ensure t
  :config
  (setq lsp-clangd-binary-path "/usr/bin/clangd")
  (setq lsp-clients-clangd-args '("--header-insertion=never"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                    :major-modes '(c-mode c++-mode)
                    :remote? t
                    :server-id 'clangd-remote))
  :hook ((c-mode c++-mode objc-mode) .
         (lambda () (lsp))))

(use-package helm-lsp
  :ensure t)

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode))

;; elpy
(use-package elpy
  :ensure t
  :config
  (when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package cc-mode
  :config
  (setq c-default-style "stroustrup"
        c-basic-offset 4))

;; nxml
(use-package nxml-mode
  ;; :ensure t
  :config
  (setq nxml-slash-auto-complete-flag t))

;; Edit functions
(defun select-line (arg)
  (interactive "p")
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil))
  (forward-line arg))
(global-set-key "\C-l" 'select-line)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(lsp-mode move-text tree-sitter-langs tree-sitter helm-xref dumb-jump buffer-move transpose-frame org-bullets flycheck company-rtags flycheck-rtags helm-rtags projectile helm-gtags magit helm arjen-grey-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
