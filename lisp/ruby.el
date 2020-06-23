; -------------------------- Keybindings --------------------------------

(define-key ruby-mode-map (kbd "C-c w") #'leo/copy-ruby-class-name)
(define-key ruby-mode-map (kbd "C-)") #'leo/rubocop-current)

; -------------------------- Hooks --------------------------------

(add-hook 'ruby-mode-hook 'evil-ruby-text-objects-mode)
(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(add-to-list 'auto-mode-alist '("\\.arb\\'"          . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.axlsx\\'"          . ruby-mode))

; -------------------------- Variables --------------------------------

(setq ruby-insert-encoding-magic-comment nil)
(setq enh-ruby-add-encoding-comment-on-save nil)
(defvar projectile-rails-vanilla-command "bin/rails")

; -------------------------- Packages --------------------------------
(use-package rubocop)
(use-package yaml-mode)
(use-package hungry-delete)

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(use-package rvm
  :init
  (rvm-use-default))


; -------------------------- Functions --------------------------------

(defun leo/rubocop-current ()
  "RUBOCOP ON CURRENT FILE."
  (interactive)
  (save-buffer)
  (message "%s" (shell-command-to-string
                 (concat "bundle exec rubocop -a "
                         (shell-quote-argument (buffer-file-name)))))
  (leo/revert-buffer-no-confirm))

(defun leo/ruby-symbol-at-point ()
  (let ((l (point)))
    (save-excursion
      (forward-sexp 1)
      (buffer-substring l (point)))))

(defun leo/copy-ruby-class-name ()
  (interactive)
  (save-excursion
    (let ((name nil)
          (case-fold-search nil))
      (skip-chars-backward (rx (syntax symbol)))
      (when (looking-at-p "\\_<[A-Z]")
        (setq name (leo/ruby-symbol-at-point)))
      (while (ignore-errors (backward-up-list) t)
        (when (looking-at-p "class\\|module")
          (save-excursion
            (forward-word 1)
            (skip-chars-forward "\r\n[:blank:]")
            (setq name (if name
                           (concat (leo/ruby-symbol-at-point) "::" name)
                         (leo/ruby-symbol-at-point))))))
      (kill-new name)
      (message "Copied %s" name))))

(defun leo/save-and-run-rubocop ()
  "Save buffer and run rubocop autocorrect."
  (interactive)
  (save-buffer)
  (rubocop-autocorrect-current-file))

(defun leo/save-and-run-erblint-autocorrect ()
  "Save buffer and run erblint autocorrect."
  (interactive)
  (save-buffer)
  (erblint-autocorrect-current-file)
  (web-mode-reload)
  (leo/revert-buffer-no-confirm))
