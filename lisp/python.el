(use-package py-autopep8)

(add-hook 'python-mode-hook 'leo/python-mode-enter)

(use-package flymake-python-pyflakes)

(defun leo/python-mode-enter ()
  (interactive)
  (flymake-python-pyflakes-load)
  (setq evil-shift-width 4))
