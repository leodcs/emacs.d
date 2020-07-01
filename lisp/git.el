(use-package magit
  :config
  (use-package evil-magit)
  (use-package with-editor)
  (setq git-commit-summary-max-length 50)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package git-link
  :config
  (setq git-link-open-in-browser t)

  (defun leo/git-open-branch-in-repo ()
    (interactive)
    (let* ((branch (magit-read-branch "Base branch"))
           (git-link-default-branch branch))
      (call-interactively 'git-link))))
