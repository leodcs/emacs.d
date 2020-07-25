(use-package general
  :config
  (general-evil-setup)

  (general-define-key
   :keymaps 'override
   "s-<return>" 'ivy-switch-buffer
   "s->" 'evil-mc-make-and-goto-next-match
   "s-<" 'evil-mc-undo-last-added-cursor
   "s-r" 'leo/revert-buffer-no-confirm
   "s-n" 'leo/generate-new-untitled-buffer
   "s-N" 'deft
   "s-d" 'leo/multiple-cursors-expand-or-mark-next-word
   "s-C" 'leo/copy-relative-file-path
   "s-w" 'delete-window
   "s-W" 'delete-other-windows
   "s-L" 'leo/split-window-vertically
   "s-J" 'leo/split-window-horizontally
   "s-e" 'leo/neotree-toggle
   "s-o" 'evil-switch-to-windows-last-buffer
   "s-p" 'counsel-projectile-find-file
   "s-P" 'counsel-projectile-switch-project
   "s-f" 'evil-search-forward
   "s-F" 'leo/counsel-ag-search-all-project
   "s-k" 'kill-this-buffer
   "s-t" 'leo/run-vterm-console
   "s-j" 'leo/projectile-run-vterm
   "s-g" 'magit-status
   "M-2" 'leo/expand-region
   "M-E" 'leo/eval-buffer
   "M-x" 'counsel-M-x
   "C->" 'evil-mc-skip-and-goto-next-match
   "C-u" 'leo/send-C-u
   "C-s" 'evil-search-next
   "C-c nk" 'leo/nuke-all-buffers
   "C-c rm" 'projectile-rails-find-model
   "C-c C-b" 'ivy-switch-buffer
   "C-c cp" 'leo/copy-full-file-path
   "C-x C-r" 'rename-buffer
   "C-x tt" 'leo/set-current-trello-card-url
   "C-x tg" 'leo/open-current-trello-card
   "C-=" 'leo/indent-whole-buffer)

  (general-define-key
   :states '(normal insert visual emacs)
   "C-e" 'move-end-of-line
   "C-a" 'move-beginning-of-line
   "s-<right>" 'move-end-of-line
   "s-<left>" 'move-beginning-of-line)

  (general-iemap
    "C-g" 'evil-normal-state
    "<return>" 'newline)

  (general-emap
    "M-a" 'mc/edit-beginnings-of-lines
    "M-e" 'mc/edit-ends-of-lines
    "s-<mouse-1>" 'mc/add-cursor-on-click
    "s-d" 'mc/mark-next-like-this)

  ;; multiple-cursors-mode
  (general-emap 'mc/keymap
    "s-f" 'phi-search
    "s-F" 'phi-search-backward
    "M-1" 'mc/insert-numbers
    "M-A" 'mc/mark-all-dwim
    "M-h" 'mc-hide-unmatched-lines-mode
    "M-l" 'mc/insert-letters
    "M-s" 'mc/sort-regions
    "s-D" 'mc/skip-to-next-like-this)

  (general-define-key
   :keymaps '(browse-kill-ring-mode-map)
   "<escape>" 'browse-kill-ring-quit)

  (general-define-key
   :states '(visual)
   :keymaps 'override
   "s-v" 'leo/evil-paste)

  (general-define-key
   :keymaps 'isearch-mode-map
   "s-v" 'isearch-yank-kill)

  ;; General use keys that shouldn't be overridden
  (general-define-key
   :prefix "C-C"
   :keymaps 'override
   "y" 'browse-kill-ring
   "C-f" 'find-file)

  (general-define-key
   :prefix "C-c g"
   :keymaps 'override
   "d" 'magit-diff-buffer-file
   "s" 'magit-status
   "l" 'magit-log
   "t" 'git-timemachine
   "b" 'magit-blame-addition
   "o" 'leo/git-open-buffer-file-in-repo
   "r" 'leo/open-git-repository-in-browser)

  (general-define-key
   "C-c e" 'leo/eval-and-replace
   "C-q" 'yas-expand
   "s-E" 'eval-last-sexp)

  (general-define-key
   :keymaps 'ruby-mode-map
   :states '(normal insert visual)
   "s-S" 'leo/save-and-run-rubocop
   "s-]" 'robe-jump
   "s-[" 'pop-tag-mark
   "C-c w" 'leo/copy-ruby-class-name)

  (general-define-key
   :keymaps 'web-mode-map
   :states '(normal insert visual)
   "C-c j" 'leo/open-html-tag
   "C-c k" 'leo/close-html-tag
   "s-S" 'leo/save-and-run-erblint-autocorrect)

  ;; Magit
  (general-define-key
   :keymaps 'transient-base-map
   "<escape>" 'transient-quit-all)
  (general-define-key
   :keymaps 'magit-mode-map
   "s-r" 'magit-refresh-all)

  (general-define-key
   :states '(normal visual)
   "=" 'leo/evil-indent)

  (general-define-key
   :states '(normal visual)
   :keymaps 'org-mode-map
   "C-c tm" 'leo/org-toggle-emphasis-markers)

  (general-define-key
   :states '(insert)
   :keymaps 'org-mode-map
   "M-<left>" 'left-word
   "M-<right>" 'right-word)

  (general-define-key
   :keymaps 'org-mode-map
   "M-<up>" 'org-up-element
   "M-<down>" 'org-down-element
   "M-<left>" 'org-shifttab
   "M-<right>" 'org-shifttab
   "s-<up>" 'org-metaup
   "s-<down>" 'org-metadown
   "s-<left>" 'org-metaleft
   "s-<right>" 'org-metaright)

  (general-define-key
   :keymaps 'org-mode-map
   :states '(insert)
   "<return>" 'org-insert-heading-respect-content
   "S-<tab>" 'org-metaleft
   "<tab>" 'org-metaright
   "C-<return>" 'org-return)

  (general-define-key
   :states '(insert)
   :keymaps 'vterm-mode-map
   "<escape>" 'vterm--self-insert
   "C-c" 'vterm-send-C-c
   "C-d" 'vterm-send-C-d
   "C-z" 'vterm-send-C-z
   "C-r" 'vterm-send-C-r
   "C-u" 'vterm-send-C-u
   "C-a" 'vterm-send-C-a
   "C-e" 'vterm-send-C-e
   "C-k" 'vterm-send-C-k
   "M-<left>" 'vterm-send-M-b
   "M-<right>" 'vterm-send-M-f)

  (general-define-key
   :states '(normal insert visual emacs)
   :keymaps 'vterm-mode-map
   "s-s" 'leo/export-buffer-contents
   "s-f" 'swiper-isearch)

  (general-define-key
   :states '(normal visual)
   :keymaps 'vterm-mode-map
   "A" 'leo/vterm-evil-append-line
   "I" 'leo/vterm-evil-insert-line
   "M-<left>" 'vterm-send-M-b
   "M-<right>" 'vterm-send-M-f
   "b" 'vterm-send-M-b
   "e" 'vterm-send-M-f
   "w" 'vterm-send-M-f)

  ;; Window numbers
  (general-define-key
   :states '(normal insert visual emacs)
   :keymaps '(override)
   "s-1" 'winum-select-window-1
   "s-2" 'winum-select-window-2
   "s-3" 'winum-select-window-3
   "s-4" 'winum-select-window-4
   "s-5" 'winum-select-window-5
   "s-6" 'winum-select-window-6
   "s-7" 'winum-select-window-7
   "s-8" 'winum-select-window-8)

  (general-define-key
   :states '(normal insert visual)
   :keymaps '(git-timemachine-mode-map)
   "C-j" 'git-timemachine-show-next-revision
   "C-k" 'git-timemachine-show-previous-revision
   "C-g" 'git-timemachine-quit)

  (general-define-key
   :states 'visual
   :keymaps 'evil-mc-key-map
   "A" #'evil-mc-make-cursor-in-visual-selection-end
   "I" #'evil-mc-make-cursor-in-visual-selection-beg)

  ;; Help
  (general-nvmap
    emacs-lisp-mode-map
    "Kf" 'describe-function
    "Kv" 'describe-variable
    "Kk" 'describe-key)

  ;; Neotree
  (general-define-key
   :keymaps 'neotree-mode-map
   :states '(normal visual)
   "A" 'neotree-stretch-toggle
   "P" 'neotree-select-up-node
   "H" 'neotree-hidden-file-toggle
   "y" 'neotree-copy-node
   "c" 'neotree-create-node
   "r" 'neotree-rename-node
   "s-r" 'neotree-refresh
   "s-F" 'leo/neotree-search-inside-current-node-directory
   "o" 'leo/reveal-neotree-file-in-system-application
   "<tab>" 'leo/neotree-enter
   "<return>" 'leo/neotree-enter
   "d" 'neotree-delete-node))
