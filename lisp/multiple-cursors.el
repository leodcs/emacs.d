(use-package expand-region
    :commands (er/mark-word er/mark-symbol))

(use-package evil-mc
  :after evil
  :config
  (global-evil-mc-mode t)
  (setq evil-mc-undo-cursors-on-keyboard-quit t))

(use-package evil-multiedit)
