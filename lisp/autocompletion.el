(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (eval-after-load 'rspec-mode
    '(rspec-install-snippets)))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package yasnippet-classic-snippets
  :config
  (yas-reload-all)
  :after yasnippet)

(use-package company
  :init
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0)
  :config
  (global-company-mode)
  (eval-after-load 'company '(push 'company-robe company-backends)))
