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

(use-package git-link
  :config
  (setq git-link-open-in-browser t)

  (defun leo/git-open-branch-in-repo ()
    (interactive)
    (let* ((branch (magit-read-branch "Base branch"))
           (git-link-default-branch branch))
      (call-interactively 'git-link))))

(use-package git-timemachine)

(defun leo/get-current-branch ()
  (interactive)
  (message (magit-get-current-branch)))

(defun leo/git-timemachine-show-selected-revision ()
  "Show last (current) revision of file."
  (interactive)
  (let* ((collection (mapcar (lambda (rev)
                               ;; re-shape list for the ivy-read
                               (cons (concat (substring-no-properties (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                             (git-timemachine--revisions))))
    (ivy-read "commits:"
              collection
              :action (lambda (rev)
                        ;; compatible with ivy 9+ and ivy 8
                        (unless (string-match-p "^[a-z0-9]*$" (car rev))
                          (setq rev (cdr rev)))
                        (git-timemachine-show-revision rev)))))

(defun leo/git-timemachine ()
  "Open git snapshot with the selected version.  Based on ivy-mode."
  (interactive)
  (unless (featurep 'git-timemachine)
    (require 'git-timemachine))
  (git-timemachine--start #'leo/git-timemachine-show-selected-revision))
