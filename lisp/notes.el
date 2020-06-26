; -------------------------- Variables --------------------------------
(setq org-hide-emphasis-markers t)
(setq org-ellipsis "⤵")
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
; Don’t include a footer with my contact and publishing information at the bottom of every exported HTML document.
(setq org-html-postamble nil)

; -------------------------- Packages --------------------------------
(require 'livedown)

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode))

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
(require 'ox-md)
(require 'ox-beamer)

(use-package ox-twbs)

(defun leo/org-toggle-emphasis-markers ()
  "Toggle between showing and hidding the emphasis markers on org-mode"
  (interactive)
  (setq org-hide-emphasis-markers (not org-hide-emphasis-markers))
    (message (or (and org-hide-emphasis-markers "Hiding emphasis markers")
             "Showing emphasis markers"))
)
