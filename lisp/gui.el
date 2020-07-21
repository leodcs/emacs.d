;; Favorites Themes => '(doom-molokai distinguished junio granger spolsky)
(load-theme 'distinguished t)

;; Themes
(use-package sublime-themes)
(use-package doom-themes)
(use-package distinguished-theme)

(use-package auto-dim-other-buffers
  :config
  (auto-dim-other-buffers-mode))

(use-package which-key
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
    which-key-side-window-max-width 0.33
    which-key-idle-delay 0.7
    which-key-idle-secondary-delay 0.35)
  (push '((nil . "projectile-rails-\\(.+\\)") . (nil . "\\1"))
        which-key-replacement-alist)
  :diminish which-key-mode)

(use-package dashboard
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)

  (setq dashboard-items '((recents  . 10)
			  (bookmarks . 10)
			  (projects . 5)))

  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  (setq dashboard-set-footer nil)
  (setq dashboard-startup-banner 2))

;; Cool Icons
(use-package all-the-icons)
(use-package all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup))

(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))
