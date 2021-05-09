(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(setq package-enable-at-startup nil)
(setq straight-use-package-by-default 1)
(straight-use-package 'use-package)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(delete-selection-mode 1)

(require 'display-line-numbers)
(defcustom display-line-numbers-exempt-modes '(treemacs-mode)
  "Major modes on which to disable the linum mode, exempts them from global requirement"
  :group 'display-line-numbers
  :type 'list
  :version "green")

(defun display-line-numbers--turn-on ()
  "turn on line numbers but excempting certain majore modes defined in `display-line-numbers-exempt-modes'"
  (if (and
       (not (member major-mode display-line-numbers-exempt-modes))
       (not (minibufferp)))
      (display-line-numbers-mode)))

(global-display-line-numbers-mode)

(set-frame-font (font-spec :family "Cascadia Mono PL" :size 15 :weight 'SemiBold) nil t)

(setq-default indent-tabs-mode nil)
(electric-pair-mode)

(use-package paradox
  :config
  (setq paradox-github-token ""))

(use-package mwim
  :config
  (autoload 'mwim "mwim" nil t)
  (autoload 'mwim-beginning "mwim" nil t)
  (autoload 'mwim-end "mwim" nil t)
  (autoload 'mwim-beginning-of-code-or-line "mwim" nil t)
  (autoload 'mwim-beginning-of-line-or-code "mwim" nil t)
  (autoload 'mwim-beginning-of-code-or-line-or-comment "mwim" nil t)
  (autoload 'mwim-end-of-code-or-line "mwim" nil t)
  (autoload 'mwim-end-of-line-or-code "mwim" nil t)
  :bind
  ("C-a" . mwim-beginning)
  ("C-e" . mwim-end))

(use-package which-key
  :config
  (which-key-mode))

(use-package htmlize
  :defer t)

(use-package magit
  :defer t)

(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

(use-package elcord
  :config
  (elcord-mode)
  (setq elcord--editor-name "Emacs")
  (setq elcord--editor-icon "spacemacs_icon"))

(use-package try
  :defer t)

(use-package nyan-mode
  :config
  (nyan-mode))

(use-package company
  :config
  (global-company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :config
  (lsp-mode))

(use-package lsp-java
  :hook
  (java-mode . lsp-deferred))

(use-package lsp-ui)

(use-package flycheck
  :config
  (global-flycheck-mode))

(use-package swiper
  :bind
  ("C-s" . swiper))

(use-package ivy
  :config
  (ivy-mode))

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package counsel
  :config
  (counsel-mode))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner "~/.emacs.d/doom.png")
  (setq dashboard-center-content t)

  ;; To disable shortcut "jump" indicators for each section, set
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-items '((recents  . 5)
                        (bookmarks . 5)
                        (agenda . 5)
                        (registers . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-navigator t)
  (setq dashboard-set-init-info t))

(use-package all-the-icons)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
	doom-themes-treemacs-theme "doom-colors")
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;;(use-package doom-modeline
;;  :init (doom-modeline-mode 1))

(use-package spaceline)

(use-package spaceline-all-the-icons
  :config (spaceline-all-the-icons-theme))

(use-package treemacs
  :bind ("<f9>" . treemacs))

(use-package go-mode
  :hook
  (before-save . gofmt-before-save)
  (go-mode . lsp-deferred))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil)
 '(paradox-automatically-star nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
