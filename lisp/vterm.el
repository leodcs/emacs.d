(use-package vterm
  :load-path  "~/.emacs.d/vendor/emacs-libvterm/"
  :config

  (setq vterm-max-scrollback 20000000
        vterm-kill-buffer-on-exit nil)

  (add-hook 'vterm-mode-hook 'leo/vterm-mode-enter)
  (add-hook 'vterm-copy-mode-hook 'evil-normal-state)

  (evil-define-key 'insert 'vterm-mode-map
    (kbd "M-1") 'leo/vterm-select-current-line)

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
    (evil-normal-state))

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

  (defun leo/vterm-send-C-u ()
    (interactive)
    (vterm-send-C-e)
    (vterm-send-C-u))

  (defun leo/vterm-select-current-line ()
    (interactive)
    (execute-kbd-macro (kbd "\C-gvg_")))

  (defun leo/vterm-copy-mode-done ()
    (interactive)
    (call-interactively #'vterm-copy-mode-done)
    (evil-insert-state))

  (defun leo/vterm-copy-current-line()
    (interactive)
    (let* ((region-begin (vterm--get-prompt-point))
           (region-end (vterm--get-end-of-line))
           (command (buffer-substring-no-properties region-begin region-end)))
      (kill-new command)
      (pulse-momentary-highlight-region region-begin region-end))))

(use-package multi-vterm
  :after vterm)

(use-package vterm-extra
  :after vterm
  :load-path  "~/.emacs.d/vendor/vterm-extra"
  :config
  (add-hook 'vterm-extra-edit-mode-hook (lambda () (setq-local require-final-newline nil)))

  (defun vterm-extra--kill-and-return-current-command ()
    "Return the command in the current line after killing it.
This is used to prepare the populate the buffer to edit commands."
    (interactive)
    (let ((command
           (buffer-substring-no-properties
            (vterm--get-prompt-point) (vterm--get-end-of-line))))
      (vterm-send-C-a)
      (vterm-send-C-k)
      command)))
