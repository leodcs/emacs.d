(use-package treemacs)

(add-hook 'lisp-mode-hook 'leo/close-treemacs)
(add-hook 'prog-mode-hook 'leo/close-treemacs)
(add-hook 'org-mode-hook 'leo/close-treemacs)
(add-hook 'web-mode-hook 'leo/close-treemacs)

(defun leo/close-treemacs ()
  (interactive)
  (if (eq (treemacs-current-visibility) 'visible)
      (delete-window (treemacs-get-local-window))))

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)
