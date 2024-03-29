(use-package evil
  :ensure t
  :config
  (evil-mode t)

  (add-hook 'after-save-hook 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "#") 'evil-search-word-forward)
  (define-key evil-normal-state-map (kbd "*") 'evil-search-word-backward)
  (define-key evil-normal-state-map (kbd "C-e") 'end-of-line)

  (define-key evil-insert-state-map (kbd "C-y") 'yank)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "M-q") 'yas-expand)
  (define-key evil-insert-state-map (kbd "<ESCAPE>") 'keyboard-quit)

  (setq evil-insert-state-cursor '(bar "#00FF00")
        evil-visual-state-cursor '(box "#FF00FF")
        evil-normal-state-cursor '(box "#E2E8EF")
        evil-emacs-state-cursor '(bar "#FF2500"))

  (defun leo/evil-paste ()
    (interactive)
    (evil-visual-paste 1)
    (right-char))

  (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t))

  (defgroup evil-textobj-entire nil
    "Text object entire buffer for Evil"
    :prefix "evil-textobj-entire-"
    :group 'evil)

  (defcustom evil-textobj-entire-key "e"
    "Key for evil-inner-entire"
    :type 'string
    :group 'evil-textobj-entire)

  (evil-define-text-object evil-entire-entire-buffer (count &optional beg end type)
    "Select entire buffer"
    (evil-range (point-min) (point-max)))

  (define-key evil-outer-text-objects-map evil-textobj-entire-key 'evil-entire-entire-buffer)
  (define-key evil-inner-text-objects-map evil-textobj-entire-key 'evil-entire-entire-buffer)

  (provide 'evil-textobj-entire)

  (evil-define-operator leo/evil-indent (beg end)
    "Indent text."
    :move-point nil
    :type line
    (if (and (= beg (line-beginning-position))
             (= end (line-beginning-position 2)))
        ;; since some Emacs modes can only indent one line at a time,
        ;; implement "==" as a call to `indent-according-to-mode'
        (leo/indent-according-to-mode) ;; I Changed this line.
      (goto-char beg)
      (indent-region beg end))
    ;; We also need to tabify or untabify the leading white characters
    (when evil-indent-convert-tabs
      (let* ((beg-line (line-number-at-pos beg))
             (end-line (line-number-at-pos end))
             (ln beg-line)
             (convert-white (if indent-tabs-mode 'tabify 'untabify)))
        (save-excursion
          (while (<= ln end-line)
            (goto-char (point-min))
            (forward-line (- ln 1))
            (back-to-indentation)
            ;; Whether tab or space should be used is determined by indent-tabs-mode
            (funcall convert-white (line-beginning-position) (point))
            (setq ln (1+ ln)))))
      (back-to-indentation)))

  (defun leo/indent-according-to-mode ()
    "Move down after indenting"
    (interactive)
    (indent-according-to-mode)
    (evil-next-line))

  (defun leo/evil-shift-right ()
    (interactive)
    (evil-shift-right evil-visual-beginning evil-visual-end)
    (evil-normal-state)
    (evil-visual-restore))

  (defun leo/evil-shift-left ()
    (interactive)
    (evil-shift-left evil-visual-beginning evil-visual-end)
    (evil-normal-state)
    (evil-visual-restore))
  )

(fset 'evil-redirect-digit-argument 'ignore)

(evil-define-key 'motion 'evil-org-mode
  (kbd "0") 'evil-org-beginning-of-line)

(use-package evil-org
  :after '(org evil)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme))))

(use-package evil-string-inflection
  :after evil)

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))
