(use-package json-mode
  :config
  (defun leo/beautify-json ()
    (interactive)
    (let ((b (if mark-active (min (point) (mark)) (point-min)))
          (e (if mark-active (max (point) (mark)) (point-max))))
      (shell-command-on-region b e
                               "python -m json.tool" (current-buffer) t)))
  (define-key json-mode-map (kbd "C-c C-b") 'leo/beautify-json)
  )
