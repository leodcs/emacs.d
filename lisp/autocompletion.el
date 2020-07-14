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
  (setq company-dabbrev-downcase 0
        company-show-numbers t
        company-idle-delay 0)
  :config
  (global-company-mode)
  (with-eval-after-load 'company
    '(push 'company-robe company-backends)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "C-<return>") 'newline)
    (define-key company-active-map (kbd "<tab>") 'yas-expand)))

(use-package company-tabnine
  :config
  (add-to-list 'company-backends #'company-tabnine))
