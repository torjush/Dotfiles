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
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
;;(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
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
(global-linum-mode 1)
(tool-bar-mode -1)
(setq column-number-mode t)
(setq show-trailing-whitespace t)
(electric-pair-mode t)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . fullheight))
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))
;; genmake
(add-to-list 'auto-mode-alist '("\\.def\\'" . python-mode))

;; Meta key
(setq mac-left-option-modifier 'meta)
(setq mac-right-option-modifier 'none)

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

;; Theme
(use-package tree-sitter
  :ensure t
  :init
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  )
(use-package tree-sitter-langs
  :ensure t)
;; (use-package arjen-grey-theme
;;   :ensure t
;;   :init (load-theme 'arjen-grey t))
(use-package nord-theme
  :ensure t
  :init (load-theme 'nord t))
;; Org
(use-package org
  :ensure t
  :init
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c l") 'org-store-link)
  (setq org-todo-keywords '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
(setq org-agenda-files (directory-files-recursively "~/Documents/org/" "\\.org$")))

;; Magit
(use-package magit
  :ensure t
  :init (setq-default with-editor-emacsclient-executable "emacsclient"))

;; Helm
(use-package helm
  :ensure t
  :init
  ;; (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-x r b") 'helm-bookmarks)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (global-set-key (kbd "C-c g") (lambda ()
                                  (interactive)
                                  (let ((current-prefix-arg 1))
                                    (call-interactively 'helm-grep-do-git-grep)))))

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
  :config
  (setq company-backends (delete 'company-clang company-backends))
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
   ("C-c DEL" . mc/skip-to-next-like-this)
   ("s-<mouse-1>" . mc/add-cursor-on-click)))

(use-package dumb-jump
  :ensure t
  :init
  (add-hook 'xref-backend-functions 'dumb-jump-xref-activate))

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


;; ;;lsp
;; (use-package lsp-mode
;;   :commands lsp
;;   :ensure t
;;   :config
;;   (setq lsp-diagnostic-package nil)
;;   (setq lsp-enable-file-watchers nil))

;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :ensure t
;;   :config
;;   (setq lsp-lens-enable nil)
;;   (setq lsp-headerline-breadcrumb-enable nil))
;; ;; (setq company-transformers nil company-lsp-async t company-lsp-cache-candidates nil)

;; (use-package ccls
;;   :ensure t
;;   :config
;;   (setq ccls-executable "ccls")
;;   (setq ccls-args '("--log-file=/tmp/ccls-log.txt"))
;;   ;; (setq lsp-prefer-flymake nil)
;;   ;; (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
;;   :hook ((c-mode c++-mode objc-mode) .
;;          (lambda () (require 'ccls) (lsp))))


;; elpy
(use-package elpy
  :ensure t
  :config
  (when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
  (elpy-enable))

(use-package cc-mode
  :config
  (setq c-default-style "stroustrup"
        c-basic-offset 4))

;; nxml
(use-package nxml-mode
  ;; :ensure t
  :config
  (setq nxml-slash-auto-complete-flag t))

;; (defun nxml-where ()
;;   "Display the hierarchy of XML elements the point is on as a path."
;;   (interactive)
;;   (let ((path nil))
;;     (save-excursion
;;       (save-restriction
;;         (widen)
;;         (while (and (< (point-min) (point)) ;; Doesn't error if point is at beginning of buffer
;;                     (condition-case nil
;;                         (progn
;;                           (nxml-backward-up-element) ; always returns nil
;;                           t)
;;                       (error nil)))
;;           (setq path (cons (xmltok-tag-local-name) path)))
;;         (if (called-interactively-p t)
;;             (message "/%s" (mapconcat 'identity path "/"))
;;           (format "/%s" (mapconcat 'identity path "/")))))))

;; Edit functions
(defun select-line (arg)
  (interactive "p")
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil))
  (forward-line arg))
(global-set-key "\C-l" 'select-line)

;; Function list
(let ((primitives-list '()))
     (mapatoms
      (lambda (sym)
        (if (subrp (symbol-function sym))
           (push sym primitives-list))))
     primitives-list)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-export-backends '(ascii html icalendar latex md odt))
 '(package-selected-packages
   '(tree-sitter-langs tree-sitter helm-xref dumb-jump buffer-move transpose-frame org-bullets flycheck company-rtags flycheck-rtags helm-rtags projectile helm-gtags magit helm arjen-grey-theme)))
