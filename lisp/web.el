(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

  (defun leo/web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 2))
  (add-hook 'web-mode-hook  'leo/web-mode-hook))
