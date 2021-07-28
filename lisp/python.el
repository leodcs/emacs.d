(use-package py-autopep8)

(add-hook 'python-mode-hook 'leo/python-mode-enter)

(use-package flymake-python-pyflakes
  :after flycheck)

(defun leo/python-mode-enter ()
  (interactive)
  (flycheck-mode 1)
  (flymake-python-pyflakes-load)
  (setq evil-shift-width 4))
