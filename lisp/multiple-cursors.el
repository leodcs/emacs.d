(use-package multiple-cursors
  :config
  (setq mc/unsupported-minor-modes '(company-mode auto-complete-mode flyspell-mode jedi-mode))
  (add-hook 'multiple-cursors-mode-enabled-hook 'evil-emacs-state)
  (add-hook 'multiple-cursors-mode-disabled-hook 'evil-normal-state))
