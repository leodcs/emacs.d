; -------------------------- Hooks --------------------------------

(add-hook 'org-mode-hook 'leo/org-mode-hook)

; -------------------------- Variables --------------------------------

(setq org-startup-indented t
      org-ellipsis "  " ;; folding symbol
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-html-postamble nil
      org-superstar-headline-bullets-list '())

; -------------------------- Packages --------------------------------
(require 'org-pretty-table)
(require 'livedown)
(require 'ox-md)
(require 'ox-beamer)

(use-package ox-twbs)
(use-package org-superstar)

(use-package deft
  :commands (deft)
  :config
  (setq deft-directory "~/Dropbox/Notes")
  (setq deft-recursive t)
  (setq deft-auto-save-interval 0)
  (setq deft-default-extension "org")
  (setq deft-extensions '("txt" "md" "org"))
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-text-mode 'org-mode)
  (evil-set-initial-state 'deft-mode 'emacs))

(use-package evil-org
  :ensure t
  :after (evil org)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme))))

; -------------------------- Functions --------------------------------

(defun leo/org-toggle-emphasis-markers ()
  "Toggle between showing and hidding the emphasis markers on org-mode"
  (interactive)
  (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
  (save-buffer)
  (leo/revert-buffer-no-confirm)
    (message (or (and org-hide-emphasis-markers "Hiding emphasis markers")
             "Showing emphasis markers")))

(defun leo/org-mode-hook ()
  (interactive)
  (visual-line-mode)
  (org-pretty-table-mode)
  (org-superstar-mode))
