(use-package evil
  :config
  (evil-mode t)

  (evil-set-initial-state 'ivy-occur-grep-mode 'normal)
  (add-hook 'after-save-hook 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-normal-state-map (kbd "C-t") 'projectile-run-shell)

  (define-key evil-insert-state-map (kbd "C-y") 'yank)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "M-q") 'yas-expand)
  (define-key evil-insert-state-map (kbd "<ESCAPE>") 'keyboard-quit)

  (global-display-line-numbers-mode 0)

  (with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t))

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

(use-package evil-org
  :after '(org evil)
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme))))

(use-package evil-mc
  :config
  (global-evil-mc-mode  1)
  (setq evil-mc-undo-cursors-on-keyboard-quit t)
  (evil-define-key 'visual evil-mc-key-map
    "A" #'evil-mc-make-cursor-in-visual-selection-end
    "I" #'evil-mc-make-cursor-in-visual-selection-beg)
  (progn
    (evil-define-key 'normal evil-mc-key-map (kbd "<escape>") 'evil-mc-undo-all-cursors)))

(use-package evil-string-inflection
  :after evil)

(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)
  (setq evil-goggles-duration 0.100)
  (evil-goggles-use-diff-faces))
