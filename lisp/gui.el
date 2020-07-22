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
  :config
  (use-package dashboard-hackernews)
  (dashboard-setup-startup-hook)
  (add-hook 'dashboard-mode-hook 'visual-line-mode)
  (setq dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-center-content t
        dashboard-set-init-info t
        dashboard-show-shortcuts nil
        dashboard-footer-messages (leo/file-to-list "~/.emacs.d/verses.txt")
        initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-startup-banner 2
        dashboard-items '((recents  . 10)
                          (projects . 5)
                          (hackernews . 6))
        dashboard-footer-icon (all-the-icons-faicon "book"
                                                    :height 1.1
                                                    :v-adjust -0.05
                                                    :face 'font-lock-keyword-face)))
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
