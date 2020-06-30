(use-package slack
  :commands (slack-start)
  :config
  (setq slack-prefer-current-team t)

  (slack-register-team
   :name "interage"
   :default t
   :token (exec-path-from-shell-getenv "SLACK_TOKEN")
   :full-and-display-names t))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'osx-notifier))

(use-package emojify
  :config
  (global-emojify-mode))
