; -------------------------- Variables --------------------------------
(setq js-indent-level 2)

; -------------------------- Hooks --------------------------------

(add-to-list 'auto-mode-alist '("\\.js\\..*\\'" . javascript-mode))
(add-hook 'js2-mode-hook 'leo/js2-mode-enter)

; -------------------------- Packages --------------------------------

(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode)))

(use-package rjsx-mode
  :config
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("src\\/.*\\.js\\'" . rjsx-mode)))

(use-package js2-refactor
  :defer 3
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c j")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill))

(use-package xref-js2
  :config
  (add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(use-package tide
  :defer 2
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(defun leo/js2-mode-enter ()
  (interactive)
  (whitespace-mode)
  (display-fill-column-indicator-mode))
