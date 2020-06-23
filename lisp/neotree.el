(use-package neotree
  :after evil
  :config
  (setq-default neo-show-hidden-files t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t)
  (setq neo-confirm-create-file 'off-p)
  (setq neo-window-width 35)
  (setq neo-default-system-application "open")
  (setq neo-confirm-create-directory 'off-p)

  (defun leo/neotree-toggle ()
    (interactive)
    (neotree-toggle)
    (evil-forward-WORD-begin))

  (defun leo/neo-open-file-hide (full-path &optional arg)
    "Open a file node and hides tree."
    (neo-global--select-mru-window arg)
    (find-file full-path)
    (neotree-hide))

  (defun leo/neotree-enter (&optional arg)
    "Enters file and hides neotree directly"
    (interactive "P")
    (neo-buffer--execute arg 'leo/neo-open-file-hide 'neo-open-dir))

  (defun leo/neotree-select-next-sibling-node ()
    (interactive)
    (neotree-select-next-sibling-node)
    (evil-forward-WORD-begin))

  (defun leo/neotree-select-previous-sibling-node ()
    (interactive)
    (neotree-select-previous-sibling-node)
    (evil-forward-WORD-begin))

  (defun leo/neotree-search-inside-current-node-directory ()
    "Calls counsel-ag passing the directory at point as parameter"
    (interactive)
    (counsel-ag nil (neo-buffer--get-filename-current-line))))
