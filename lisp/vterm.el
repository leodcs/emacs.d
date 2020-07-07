(use-package vterm
  :config
  (setq vterm-max-scrollback 100000))

(defun leo/run-vterm-console ()
  "Opens a new instance of vterm everytime it is called."
  (interactive)
  (split-window-horizontally)
  (other-window 1)
  (vterm)
  (evil-insert-state))

(defun leo/projectile-run-vterm ()
  "Opens one single instance of vterm inside current projectile project."
  (interactive)
  (split-window-vertically)
  (other-window 1)
  (evil-window-move-very-bottom)
  (projectile-run-vterm)
  (evil-insert-state))
