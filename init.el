
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

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
  :ensure t
  :config
  (setq paradox-github-token "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"))

(use-package mwim
  :ensure t
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
  :ensure t
  :config
  (which-key-mode))

(use-package htmlize
  :ensure t
  :defer t)

(use-package magit
  :ensure t
  :defer t)

(use-package solaire-mode
  :ensure t
  :hook (after-init . solaire-global-mode))

(use-package elcord
  :ensure t
  :config
  (elcord-mode)
  (setq elcord--editor-name "Emacs")
  (setq elcord--editor-icon "spacemacs_icon"))

(use-package try
  :ensure t
  :defer t)

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode))

(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :ensure t
  :config
  (lsp-mode))

(use-package lsp-java
  :ensure t
  :hook
  (java-mode . lsp-deferred))

(use-package lsp-ui
  :ensure t)

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode))

(use-package swiper
  :ensure t
  :bind
  ("C-s" . swiper))

(use-package ivy
  :ensure t
  :config
  (ivy-mode))

(use-package all-the-icons-ivy
  :ensure t
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package counsel
  :ensure t
  :config
  (counsel-mode))

(use-package dashboard
  :ensure t
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

(use-package all-the-icons
  :ensure t)

(use-package doom-themes
 :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
	doom-themes-treemacs-theme "doom-colors")
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config))

;;(use-package doom-modeline
;;  :ensure t
;;  :init (doom-modeline-mode 1))

(use-package spaceline
  :ensure t)

(use-package spaceline-all-the-icons
  :ensure t
  :config (spaceline-all-the-icons-theme))

(use-package treemacs
  :ensure t
  :bind ("<f9>" . treemacs))

(use-package go-mode
  :ensure t
  :hook
  (before-save . gofmt-before-save)
  (go-mode . lsp-deferred))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil)
 '(package-selected-packages '(use-package))
 '(paradox-automatically-star nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
