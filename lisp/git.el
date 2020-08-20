(add-hook 'after-save-hook 'magit-after-save-refresh-status t)
(add-hook 'with-editor-mode-hook 'leo/with-editor-mode-enter)

(use-package magit
  :config
  (use-package evil-magit)
  (use-package with-editor)
  (setq git-commit-summary-max-length 50
        magit-blame-time-format "%d/%m/%Y %H:%M")
  (evil-define-key 'normal magit-blame-mode-map (kbd "<escape>") 'magit-blame-quit)
  (evil-define-key 'normal magit-process-mode-map (kbd "C-<return>") 'browse-url-at-point))

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
  (setq-local fill-column 50)
  (display-fill-column-indicator-mode))

(defun parse-git-url (url)
  "Convert a git remote location as a HTTP URL"
  (if (string-match "^http" url)
      url
    (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
                              "https://\\2/\\3"
                              url)))
