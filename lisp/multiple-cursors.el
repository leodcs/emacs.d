(use-package multiple-cursors
  :after evil
  :config
  (use-package expand-region :commands (er/expand-region er/mark-word er/mark-symbol))
  (use-package phi-search)

  (add-hook 'multiple-cursors-mode-enabled-hook 'evil-emacs-state)
  (add-hook 'multiple-cursors-mode-disabled-hook 'leo/multiple-cursors-mode-exit)

  (setq mc/always-run-for-all t
        mc/insert-numbers-default 1
        mc/unsupported-minor-modes '(company-mode
                                     auto-complete-mode
                                     linum-mode
                                     ivy-posframe-mode
                                     yasnippet-mode
                                     evil-mode
                                     jedi-mode))

  (defun leo/multiple-cursors-expand-or-mark-next-word ()
    (interactive)
    (if (not (region-active-p))
        (leo/expand-region)
      (call-interactively #'mc/mark-next-like-this)))

  (defun leo/expand-region ()
    (interactive)
    (if (evil-emacs-state-p)
        (call-interactively #'er/expand-region)
      (evil-emacs-state)
      (call-interactively #'er/expand-region)))

  (defun leo/multiple-cursors-mode-exit()
    (interactive)
    (evil-force-normal-state)
    (ivy-posframe-mode t)))
