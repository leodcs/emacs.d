(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  :config
  (defun leo/web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2
          web-mode-auto-close-style nil))
  (add-hook 'web-mode-hook  'leo/web-mode-hook))

(use-package restclient)
