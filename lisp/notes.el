;; (use-package org-bullets
;;   :init
;;   (add-hook 'org-mode-hook 'org-bullets-mode))

(setq org-ellipsis "⤵")
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)

(use-package deft
  :commands (deft)
  :config

  (setq deft-directory "~/notes"
        deft-recursive t
        deft-use-filename-as-title t)

  (evil-set-initial-state 'deft-mode 'emacs))
