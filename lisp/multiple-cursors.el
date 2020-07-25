(use-package expand-region
  :commands (er/expand-region er/mark-word er/mark-symbol)
  :config
  (defun leo/expand-region ()
    (interactive)
    (if (evil-emacs-state-p)
        (call-interactively #'er/expand-region)
      (evil-emacs-state)
      (call-interactively #'er/expand-region))))

(use-package evil-mc
  :after evil
  :config
  (global-evil-mc-mode t)
  (setq evil-mc-undo-cursors-on-keyboard-quit t))

(use-package multiple-cursors
  :config
  (setq mc/unsupported-minor-modes '(company-mode auto-complete-mode flyspell-mode jedi-mode))
  (add-hook 'multiple-cursors-mode-enabled-hook 'evil-emacs-state)
  (add-hook 'multiple-cursors-mode-disabled-hook 'evil-normal-state)
  (defun leo/multiple-cursors-expand-or-mark-next-word ()
    (interactive)
    (if (not (region-active-p))
        (leo/expand-region)
      (call-interactively #'mc/mark-next-like-this))))
