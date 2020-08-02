(use-package neotree
  :after evil
  :config
  (setq-default neo-show-hidden-files t)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)
        neo-smart-open t
        neo-confirm-create-file 'off-p
        neo-window-width 35
        neo-confirm-create-directory 'off-p
        neo-default-system-application "open -R")

  (defun leo/neotree-toggle ()
    (interactive)
    (neotree-toggle)
    (evil-forward-WORD-begin)
    (evil-scroll-line-to-center (line-number-at-pos)))

  (defun leo/neo-open-file-hide (full-path &optional arg)
    "Open a file node and hides tree."
    (neo-global--select-mru-window arg)
    (find-file full-path)
    (neotree-hide))

  (defun leo/reveal-neotree-file-in-system-application ()
    "Open a file under point in finder."
    (interactive)
    neo-default-system-application
    (shell-command
     (concat neo-default-system-application
             " "
             (neo-buffer--get-filename-current-line))))

  (defun leo/neotree-enter (&optional arg)
    "Enters file and hides neotree directly"
    (interactive "P")
    (neo-buffer--execute arg 'leo/neo-open-file-hide 'neo-open-dir))

  (defun leo/neotree-search-inside-current-node-directory ()
    "Calls counsel-ag passing the directory at point as parameter"
    (interactive)
    (pulse-momentary-highlight-one-line (point))
    (counsel-ag nil (neo-buffer--get-filename-current-line))))
