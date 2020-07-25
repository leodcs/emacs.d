(use-package vterm
  :load-path  "~/.emacs.d/vendor/emacs-libvterm/"
  :config
  (setq vterm-max-scrollback 20000000)

  (add-hook 'vterm-mode-hook 'leo/vterm-mode-enter)

  (defun leo/run-vterm-console ()
    "Opens a new instance of vterm everytime it is called."
    (interactive)
    (setq-local default-directory (projectile-project-root))
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

  (defun leo/vterm-mode-enter ()
    (interactive)
    (setq-local evil-move-cursor-back nil)
    (rvm-activate-corresponding-ruby))

  (defun leo/vterm-evil-append-line ()
    (interactive)
    (execute-kbd-macro (kbd "<end>"))
    (evil-insert-state))

  (defun leo/vterm-evil-insert-line ()
    (interactive)
    (execute-kbd-macro (kbd "<home>"))
    (evil-insert-state))

  (defun leo/vterm-mode-enter ()
    (interactive)
    (setq-local evil-move-cursor-back nil)
    (rvm-activate-corresponding-ruby))

  (defun leo/vterm-send-C-l ()
    (interactive)
    (vterm-send-C-l)
    (evil-insert-state))

  (defun leo/vterm-copy-mode-done ()
    (interactive)
    (call-interactively #'vterm-copy-mode-done)
    (evil-insert-state)))
