(use-package vterm
  :quelpa (vterm
           :fetcher github
           :repo "leodcs/emacs-libvterm")
  :config
  (use-package multi-vterm)

  (setq vterm-max-scrollback 20000000)

  (add-hook 'vterm-mode-hook 'leo/vterm-mode-enter)

  (defun leo/find-file ()
    (list "find-file"
          (lambda (path)
            (if-let* ((buf (find-file-noselect path))
                      (window (display-buffer-below-selected buf nil)))
                (select-window window)
              (message "Failed to open file: %s" path)))))
  (push (leo/find-file) vterm-eval-cmds)

  (defun leo/run-vterm-console ()
    "Opens a instance of vterm on projectile root."
    (interactive)
    (multi-vterm-projectile)
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
    (setq-local evil-move-cursor-back nil
                evil-insert-state-cursor '(box "#00FF00"))
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

  (defun leo/vterm-clear ()
    (interactive)
    (vterm-clear)
    (evil-insert-state))

  (defun leo/vterm-copy-mode-done ()
    (interactive)
    (call-interactively #'vterm-copy-mode-done)
    (evil-insert-state)))
