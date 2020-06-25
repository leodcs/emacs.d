(use-package magit
  :config
  (use-package evil-magit)
  (use-package with-editor)
  (setq git-commit-summary-max-length 50)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  (add-hook 'with-editor-mode-hook 'evil-insert-state))

(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(defun leo/parse-git-url (url)
  "convert a git remote location as a HTTP URL"
  (if (string-match "^http" url)
      url
    (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
                              "https://\\2/\\3"
                              url)))

(defun leo/magit-open-file-in-repo ()
  "open remote repo URL"
  (interactive)
  ;; TODO: add Github link structure
  ;; https://github.com/arthurcgusmao/acg-emacs/commit/2257058e424b82e1d670cadf2e870f0a6a37d669#diff-8b418bbba22fdd016dd96726e07256f0R82-R89
  (let ((url
         (concat (magit-get "remote" "origin" "url")
                 "/-/tree/"
                 (magit-get-current-branch)
                 "/"
                 (leo/get-current-file-relative-path))))
    (progn
      (browse-url (leo/parse-git-url url))
      (message "opening repo %s" url))))
