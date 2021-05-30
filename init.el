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
(defalias 'yes-or-no-p 'y-or-n-p)
(setq backup-directory-alist '(("." . "~/.emacs.d/emacs-saves")))
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

(use-package magit
  :defer t)

(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

(use-package elcord
  :straight (elcord :fork "2000Slash/elcord")
  :config
  (elcord-mode))

(use-package nyan-mode
  :config
  (nyan-mode))

(use-package company
  :config
  (global-company-mode))

(use-package tide
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1)
  (setq company-tooltip-align-annotations t)
  (add-hook 'before-save-hook 'tide-format-before-save)
  :hook
  (typescript-mode . (lambda ()
                       (interactive)
                       (tide-setup))))

(use-package rust-mode
  :hook (rust-mode . (lambda ()
                       (lsp))))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-treemacs)

(use-package lsp-mode
  :config
  (setq lsp-keymap-prefix "C-c l")
  (lsp-mode))

(use-package lsp-java
  :hook
  (java-mode . lsp-deferred))

(use-package lsp-python-ms
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))  ; or lsp-deferred)

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

;; (use-package doom-modeline
;;   :init (doom-modeline-mode 1))

(use-package spaceline
    :config
    (spaceline-spacemacs-theme))

;;(use-package spaceline-all-the-icons
;;  :config (spaceline-all-the-icons-theme))

(use-package treemacs
  :bind ("<f9>" . treemacs))

(use-package go-mode
  :hook
  (before-save . gofmt-before-save)
  (go-mode . lsp-deferred))




(setq exit-messages '(
	"Please don't leave, there's more demons to toast!"
	"Let's beat it -- This is turning into a bloodbath!"
	"I wouldn't leave if I were you. Vim is much worse."
	"Don't leave yet -- There's a demon around that corner!"
	"Ya know, next time you come in here I'm gonna toast ya."
	"Go ahead and leave. See if I care."
	"Are you sure you want to quit this great editor?"
	"Emacs will remember that."
	"Emacs, Emacs never changes."
	"Okay, look. We've both said a lot of things you're going to regret..."
	"You are *not* prepared!"
	"Look, bud. You leave now and you forfeit your body count!"
	"Get outta here and go back to your boring editors."
	"You're lucky I don't smack you for thinking about leaving."
	"Don't go now, there's a dimensional shambler waiting at the prompt!"
	"Just leave. When you come back I'll be waiting with a bat."
	"Are you a bad enough dude to stay?"
	"It was worth the risk... I assure you."
	"I'm willing to take full responsibility for the horrible events of the last 24 hours."
	))

(defun random-choice (items)
  (let* ((size (length items))
	 (index (random size)))
	(nth index items)))

(defun save-buffers-kill-emacs-with-confirm ()
  (interactive)
  (if (null current-prefix-arg)
	  (if (y-or-n-p (format "%s Quit? " (random-choice exit-messages)))
	(save-buffers-kill-emacs))
	(save-buffers-kill-emacs)))

(global-set-key "\C-x\C-c" 'save-buffers-kill-emacs-with-confirm)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
