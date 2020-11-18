(use-package sublime-themes
  :config
  (use-package distinguished-theme)
  (use-package doom-themes)
  (use-package doom-modeline :config (doom-modeline-mode))
  (use-package solarized-theme)
  (use-package color-theme-sanityinc-tomorrow)
  (use-package base16-theme)

  ;; List of favorites => '(doom-molokai
  ;;                        distinguished
  ;;                        junio
  ;;                        granger
  ;;                        spolsky
  ;;                        solarized-dark
  ;;                        sanityinc-tomorrow-night
  ;;                        base16-default-dark)

  (load-theme 'base16-default-dark t)
)

(use-package auto-dim-other-buffers
  :config
  (auto-dim-other-buffers-mode -1))

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
  (dashboard-setup-startup-hook)
  (add-hook 'dashboard-mode-hook 'visual-line-mode)
  (add-hook 'dashboard-mode-hook 'redisplay)

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
                          (bookmarks . 5))
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

(defvar leo/window-configuration nil)
(define-minor-mode leo/single-window-toggle
  "Toggle between multiple windows and single window.
This is the equivalent of maximising a window."
  :global nil
  (if (one-window-p)
      (when leo/window-configuration
        (setq global-mode-string nil)
        (set-window-configuration leo/window-configuration))
    (setq leo/window-configuration (current-window-configuration))
    (setq global-mode-string "< Maximized >")
    (delete-other-windows)))
