; -------------------------- Hooks --------------------------------

(add-hook 'org-mode-hook 'leo/org-mode-enter)

; -------------------------- Variables --------------------------------

(setq org-startup-indented t
      org-ellipsis "  " ;; folding symbol
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-html-postamble nil
      org-superstar-headline-bullets-list '("➣" "◉" "○" "➝" "»" "➥"))

; -------------------------- Packages --------------------------------
(require 'livedown)
(require 'ox-md)
(require 'ox-beamer)

(use-package ox-twbs)
(use-package org-superstar)

(use-package deft
  :commands (deft)
  :config
  (setq deft-directory "~/Dropbox/Notes"
        deft-recursive t
        deft-auto-save-interval 0
        deft-default-extension "org"
        deft-extensions '("txt" "md" "org")
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t
        deft-text-mode 'org-mode)
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

(defun leo/org-mode-enter ()
  (interactive)
  (visual-line-mode)
  (org-superstar-mode))
