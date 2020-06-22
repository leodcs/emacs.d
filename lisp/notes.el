; -------------------------- Variables --------------------------------
(setq org-hide-emphasis-markers t)
(setq org-ellipsis "⤵")
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

; -------------------------- Packages --------------------------------
(require 'livedown)

(use-package org-bullets
  :init
  (add-hook 'org-mode-hook 'org-bullets-mode))

(use-package deft
  :commands (deft)
  :config
  (setq deft-directory "~/notes")
  (setq deft-recursive t)
  (setq deft-use-filename-as-title t)
  (setq deft-auto-save-interval 0)

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
    (message (or (and org-hide-emphasis-markers "Hiding emphasis markers")
             "Showing emphasis markers"))
)
