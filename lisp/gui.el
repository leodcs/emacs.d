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
  (setq dashboard-startup-banner 2)
)

;; Cool Icons
(use-package all-the-icons)
(use-package all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup)
  )
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  )

;; Theme
(use-package doom-themes)
(use-package kaolin-themes)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))
