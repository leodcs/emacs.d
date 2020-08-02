(use-package ivy
  :init (ivy-mode t)
  :config
  (use-package ivy-hydra)
  (use-package wgrep)
  (use-package wgrep-ag)
  (use-package counsel)
  (use-package ivy-posframe
    :config
    (setq ivy-posframe-parameters '((left-fringe . 8) (right-fringe . 8))
          ivy-posframe-display-functions-alist
          '((t . ivy-posframe-display-at-frame-center)))
    (ivy-posframe-mode 1))

  (add-hook 'ivy-occur-grep-mode-hook 'leo/ivy-occur-enter)

  (setq enable-recursive-minibuffers t
        counsel-ag-base-command "ag --hidden --ignore .git --ignore vendor --vimgrep %s"
        ivy-use-virtual-buffers t
        ivy-height 20
        ivy-count-format "%d/%d "
        wgrep-auto-save-buffer t
        ivy-initial-inputs-alist nil)

    (defun leo/ivy-occur-enter ()
      (interactive)
      (evil-normal-state)))
