(use-package expand-region
  :commands (er/mark-word er/mark-symbol))

(use-package evil-mc
  :after evil
  :config
  (global-evil-mc-mode t)
  (setq evil-mc-undo-cursors-on-keyboard-quit t))

(use-package multiple-cursors
  :config
  (setq mc/unsupported-minor-modes '(company-mode auto-complete-mode flyspell-mode jedi-mode))
  (add-hook 'multiple-cursors-mode-enabled-hook 'evil-emacs-state)
  (add-hook 'multiple-cursors-mode-disabled-hook 'evil-normal-state))
