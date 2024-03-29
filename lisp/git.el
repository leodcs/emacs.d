(add-hook 'after-save-hook 'magit-after-save-refresh-status t)
(add-hook 'with-editor-mode-hook 'leo/with-editor-mode-enter)
(add-hook 'magit-mode-hook 'leo/magit-mode-enter)
(add-hook 'git-commit-setup-hook 'leo/git-commit-insert-branch)

(use-package magit
  :config
  (use-package with-editor)
  (setq git-commit-summary-max-length 50
        magit-blame-time-format "%d/%m/%Y %H:%M")
  (evil-define-key 'normal magit-blame-mode-map (kbd "<escape>") 'magit-blame-quit)
  (evil-define-key 'normal magit-process-mode-map (kbd "C-<return>") 'browse-url-at-point)

  (defun leo/magit-process-kill-no-confirm ()
    "Kill the process at point instantly."
    (interactive)
    (let ((process (magit-section-value-if 'process)))
      (kill-process process))))

(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package git-link
  :config
  (setq git-link-open-in-browser t)

  (defun leo/git-open-buffer-file-in-repo ()
    (interactive)
    (let* ((branch (magit-read-branch "Base branch"))
           (git-link-default-branch branch))
      (call-interactively 'git-link))))

(use-package git-timemachine)

(defun leo/with-editor-mode-enter ()
  (interactive)
  (evil-insert-state)
  (setq-local fill-column 57)
  (display-fill-column-indicator-mode))

(defun parse-git-url (url)
  "Convert a git remote location as a HTTP URL"
  (if (string-match "^http" url)
      url
    (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
                              "https://\\2/\\3"
                              url)))

(defun leo/open-gh-pull-request ()
  (interactive)
  (shell-command "gh pr create --web"))

(defun leo/view-gh-pull-request ()
  (interactive)
  (shell-command "gh pr view --web"))

(defun leo/magit-mode-enter ()
  (interactive))

(defun leo/magit-push ()
  (interactive)
  (magit-push)
  (magit-process-buffer))

(defun leo/extract-branch-tag (branch-name)
  (let ((TICKET-PATTERN "\\(?:[[:alpha:]]+-\\)?\\([[:alpha:]]+-[[:digit:]]+\\)-.*"))
    (when (string-match-p TICKET-PATTERN branch-name)
       (s-upcase (replace-regexp-in-string TICKET-PATTERN "\\1 " branch-name)))))

(defun leo/git-commit-insert-branch ()
  (if (leo/current-line-empty-p)
      (insert (leo/extract-branch-tag (magit-get-current-branch)))))
