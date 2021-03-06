(use-package projectile
  :init
  (setq projectile-completion-system 'ivy
        projectile-git-command "fd . -0 --type f --hidden --color=never")
  (projectile-mode +1)
  :bind (("C-c p" . projectile-command-map)
         ("C-," . counsel-projectile-switch-to-buffer)
         ("C-." . projectile-find-file)))

(use-package counsel-projectile
  :init
  (counsel-projectile-mode)
  (setq projectile-globally-ignored-directories
        (append '(".bundle" ".vendor" "public" "node-modules")
                projectile-globally-ignored-directories)))

(use-package projectile-rails
  :config
  (setq projectile-switch-project-action 'leo/projectile-switch-project-action)
  (projectile-rails-global-mode)
  (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map)
  (setq inf-ruby-console-environment "development")
  (add-hook 'projectile-after-switch-project-hook #'leo/projectile-switch-project-action)
  (global-set-key (kbd "C-c r 3 T") (kbd "C-c , 4 t")))

(defun leo/projectile-switch-project-action ()
  "Set correct ruby version on project change."
  (interactive)
  (rbenv-use-corresponding))

(defun leo/projectile-find-file ()
  (interactive)
  (ido-find-file-in-dir (projectile-project-root)))
