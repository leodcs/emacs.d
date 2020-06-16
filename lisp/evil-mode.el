(use-package evil-leader
  :init (global-evil-leader-mode)
  :config
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key "<SPC>" 'ivy-switch-buffer)
  (evil-leader/set-key "<down>" 'open-html-tag)
  (evil-leader/set-key "<up>" 'close-html-tag)
  (evil-leader/set-key "RET" 'evil-switch-to-windows-last-buffer)
  (evil-leader/set-key "d" 'delete-other-windows)
  (evil-leader/set-key "fm" 'projectile-rails-find-model)
  (evil-leader/set-key "TAB" 'other-window)
  (evil-leader/set-key "v" 'evil-window-vsplit)

  (use-package evil
    :init (evil-mode)
    :config
    (add-hook 'after-save-hook 'evil-normal-state)
    (define-key evil-normal-state-map (kbd "C-e") 'end-of-line)
    (define-key evil-normal-state-map (kbd "C-t") 'projectile-run-shell)
    (define-key evil-insert-state-map (kbd "C-y") 'yank)
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
    (define-key evil-insert-state-map (kbd "M-q") 'yas-expand)
    (define-key evil-insert-state-map (kbd "<ESCAPE>") 'keyboard-quit)

    (global-display-line-numbers-mode 0)
    (global-linum-mode 0)

    (with-eval-after-load 'evil
      (defalias #'forward-evil-word #'forward-evil-symbol)
      ;; make evil-search-word look for symbol rather than word boundaries
      (setq-default evil-symbol-word-search t))
    (use-package neotree
      :config
      (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
      (setq neo-smart-open t)
      (setq neo-confirm-create-file 'off-p)
      (setq-default neo-show-hidden-files t)
      (defun neo-open-file-hide (full-path &optional arg)
        "Open a file node and hides tree."
        (neo-global--select-mru-window arg)
        (find-file full-path)
        (neotree-hide))

      (defun neotree-enter-hide (&optional arg)
        "Enters file and hides neotree directly"
        (interactive "P")
        (neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir))

      (evil-define-key 'normal neotree-mode-map (kbd "s-r") 'neotree-rename-node)
      (evil-define-key 'normal neotree-mode-map (kbd "s-c") 'neotree-copy-node)
      (evil-define-key 'normal neotree-mode-map (kbd "s-n") 'neotree-create-node)
      (evil-define-key 'normal neotree-mode-map (kbd "<s-backspace>") 'neotree-delete-node)
      (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter-hide)
      (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
      (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
      (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-stretch-toggle)
      (evil-define-key 'normal neotree-mode-map (kbd "L") 'neotree-stretch-toggle)
      (evil-define-key 'normal neotree-mode-map (kbd "P") 'neotree-select-up-node)
      (evil-define-key 'normal neotree-mode-map (kbd "R") 'neotree-refresh)
      )
    (use-package nlinum-relative
      :config
      ;; something else you want
      (nlinum-relative-setup-evil)
      (global-nlinum-relative-mode)
      (setq nlinum-relative-redisplay-delay 0)
      (setq nlinum-relative-current-symbol "")
    )
    (use-package evil-commentary
      :config
      (evil-commentary-mode)
    )
    (use-package evil-surround
      :config
      (global-evil-surround-mode 1)
    )
    (defgroup evil-textobj-entire nil
      "Text object entire buffer for Evil"
      :prefix "evil-textobj-entire-"
      :group 'evil
    )
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

    (add-hook 'ruby-mode-hook 'evil-ruby-text-objects-mode)

    (use-package evil-mc
      :config
      (global-evil-mc-mode  1)
      (setq evil-mc-undo-cursors-on-keyboard-quit t)
      (evil-define-key 'visual evil-mc-key-map
        "A" #'evil-mc-make-cursor-in-visual-selection-end
        "I" #'evil-mc-make-cursor-in-visual-selection-beg)
      (progn
        (evil-define-key 'normal evil-mc-key-map (kbd "<escape>") 'evil-mc-undo-all-cursors)
      )
    )
    ;; MACROS
    ;;
    ;; Evil-mode macros are not special, they are just ordinary Emacs macros and you save them the same way,
    ;; but you'll need to do some special work to get them into the evil registers.

    ;; Let's walk through an example:

    ;; In a buffer, do qfifoobarESCq. This will save a macro into the f register that inserts foobar into the buffer.

    ;; Now run M-x name-last-kbd-macro <RET> mymacro <RET>

    ;; Go to your init.el file and do M-x insert-kbd-macro <RET> mymacro <RET>

    ;; This will dump your macro out into an fset call.

    ;; (fset 'mymacro [?i ?f ?o ?o ?b ?a ?r escape])
    ;;
    ;;
    (fset 'open-html-tag "ysit\C-j=it")
    (fset 'close-html-tag "JxJx")
  )
)
