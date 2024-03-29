; -------------------------- Hooks --------------------------------

(add-hook 'ruby-mode-hook 'leo/ruby-mode-enter)
(add-hook 'web-mode-hook 'leo/web-mode-enter)
(add-to-list 'auto-mode-alist '("\\.arb\\'"          . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.axlsx\\'"          . ruby-mode))

; -------------------------- Variables --------------------------------

(setq ruby-insert-encoding-magic-comment nil
      rubocop-autocorrect-command "rubocop -A --format emacs --config ~/Projects/personal/dotfiles/rubocop/rubocop.yml"
      enh-ruby-add-encoding-comment-on-save nil)

(defvar projectile-rails-vanilla-command "bin/rails")

; -------------------------- Macros --------------------------------

(fset 'open-html-tag-macro "ysit\C-j=it")
(fset 'close-html-tag-macro "JxJx")
(fset 'select-current-line "vg_")

; -------------------------- Packages --------------------------------
(require 'plural)

(use-package dart-mode)
(use-package rubocop)
(use-package yaml-mode)
(use-package hungry-delete)
(use-package erblint
  :config
  (setq erblint-project-root-function 'projectile-rails-root
        erblint-check-command "~/.rvm/gems/ruby-2.4.2/bin/erblint"
        erblint-autocorrect-command (concat erblint-check-command " -a")))
(use-package rbenv)
(use-package evil-ruby-text-objects)
(use-package dumb-jump
  :load-path  "~/.emacs.d/vendor/dumb-jump"
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

; -------------------------- Functions --------------------------------

(defun leo/ruby-mode-enter ()
  (interactive)
  (robe-mode)
  (whitespace-mode)
  (display-fill-column-indicator-mode)
  (evil-ruby-text-objects-mode))

(defun leo/web-mode-enter ()
  (interactive)
  (whitespace-mode)
  (display-fill-column-indicator-mode))

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
  (leo/indent-whole-buffer)
  (save-buffer)
  (rbenv-use-corresponding)
  (rubocop-autocorrect-current-file))

(defun leo/save-and-run-erblint-autocorrect ()
  "Save buffer and run erblint autocorrect."
  (interactive)
  (leo/indent-whole-buffer)
  (save-buffer)
  (erblint-autocorrect-current-file)
  (web-mode-reload))

(defun leo/open-html-tag ()
  (interactive)
  (execute-kbd-macro 'open-html-tag-macro))

(defun leo/close-html-tag ()
  (interactive)
  (execute-kbd-macro 'close-html-tag-macro))

(defun leo/select-current-line ()
  (interactive)
  (evil-normal-state)
  (execute-kbd-macro 'select-current-line))

(defun leo/select-current-line-from-beginning ()
  (interactive)
  (back-to-indentation)
  (leo/select-current-line))

(defun leo/goto-current-model-on-schema()
  (interactive)
  (let* ((model-name (projectile-rails-current-resource-name))
         (pluralized-string (pluralize-string model-name)))
    (projectile-rails-goto-schema)
    (evil-goto-first-line)
    (search-forward-regexp (concat "create_table \".*" pluralized-string "\""))
    (pulse-momentary-highlight-one-line (point))))

(defun leo/browse-rubygem()
  (interactive)
  (let ((rubygems-url "https://rubygems.org/gems/"))
    (execute-kbd-macro (kbd "yi'"))
    ;; Bundler.definition.dependencies.find { |d| d.name == <GEM_NAME> }&.to_spec&.homepage
    (browse-url (concat rubygems-url (current-kill 0)))))
