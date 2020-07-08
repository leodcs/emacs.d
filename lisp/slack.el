(add-hook 'slack-message-buffer-mode-hook 'leo/slack-message-mode-enter)
(add-hook 'slack-thread-message-buffer-mode-hook 'leo/slack-message-mode-enter)

(use-package slack
  :commands (slack-start)
  :config
  (setq slack-prefer-current-team t
        slack-thread-also-send-to-room nil)

  (slack-register-team
   :name "interage"
   :default t
   :token (exec-path-from-shell-getenv "SLACK_TOKEN")
   :full-and-display-names t))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'osx-notifier))

(use-package emojify)

(defun leo/slack-message-mode-enter ()
  (interactive)
  (evil-insert-state)
  (emojify-mode))
