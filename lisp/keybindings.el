(use-package general
  :config
  (general-evil-setup)

  (general-define-key
   :keymaps 'override
   "s-a" 'mark-whole-buffer
   "s-z" 'undo-tree-undo
   "s-Z" 'undo-tree-redo
   "s-q" 'save-buffers-kill-emacs
   "s-s" 'save-buffer
   "s-v" 'yank
   "s-<return>" 'ivy-switch-buffer
   "s-<mouse-1>" 'evil-goto-definition
   "s-." 'evil-goto-definition
   "s->" 'evil-jump-backward
   "s-/" 'leo/counsel-ag-search-all-project
   "s-u" 'universal-argument
   "s-C" 'leo/copy-relative-file-path
   "s-J" 'leo/split-window-horizontally
   "s-L" 'leo/split-window-vertically
   "s-N" 'deft
   "s-P" 'counsel-projectile-switch-project
   "s-d" 'leo/multiple-cursors-expand-or-mark-next-word
   "s-D" 'leo/duplicate-line
   "s-E" 'treemacs-select-window
   "s-f" 'evil-search-forward
   "s-F" 'evil-search-backward
   "s-g" 'magit-status
   "s-k" 'kill-this-buffer
   "s-K" 'leo/purge-this-buffer
   "s-n" 'leo/generate-new-scratch-buffer
   "s-o" 'leo/projectile-find-file
   "s-p" 'counsel-projectile-find-file
   "s-r" 'leo/revert-buffer-no-confirm
   "s-W" 'delete-other-windows
   "s-w" 'leo/delete-window
   "s-A" 'winner-undo
   "s-l" 'goto-line
   "s-x" 'kill-region
   "M-e" 'treemacs
   "M-<up>" 'drag-stuff-up
   "M-<down>" 'drag-stuff-down
   "M-A" 'winner-redo
   "M-1" 'leo/select-current-line
   "M-2" 'leo/expand-region
   "M-3" 'leo/select-current-line-from-beginning
   "M-x" 'counsel-M-x
   "M-#" 'evil-commentary-line
   "M-D" 'leo/delete-line
   "M-E" 'eval-last-sexp
   "C-a" 'back-to-indentation
   "C-s-c" 'leo/copy-full-file-path
   "C-u" 'leo/undo-last-text
   "C-s" 'evil-search-next
   "C-M-e" 'leo/eval-buffer
   "C-c nk" 'leo/nuke-all-buffers
   "C-c C-b" 'ivy-switch-buffer
   "C-x C-c" 'leo/string-inflection-cycle
   "C-x C-r" 'rename-buffer
   "C-x C-t" 'leo/insert-current-timestamp
   "C-x ts" 'leo/set-current-trello-card-url
   "C-x tt" 'leo/open-current-trello-card
   "C-=" 'leo/indent-whole-buffer
   "<f11>" 'toggle-frame-maximized)

  (general-define-key
   :states '(normal insert visual emacs)
   "S-<return>" 'newline
   "C-e" 'move-end-of-line
   "C-a" 'back-to-indentation
   "C-s" 'ace-jump-mode
   "s-<right>" 'move-end-of-line
   "C-k" 'kill-line
   "s-<left>" 'move-beginning-of-line)

  (general-iemap
    "C-o" 'evil-open-above
    "C-j" 'evil-open-below
    "s-c" 'kill-ring-save
    "C-d" 'delete-char
    "s-]" 'evil-shift-right-line
    "s-[" 'evil-shift-left-line
    "C-g" 'leo/ie-keyboard-quit
    "<S-mouse-1>" 'mouse-save-then-kill)

  (general-nmap
    "C-<return>" 'browse-url-at-point
    "<tab>" 'vimish-fold-toggle
    ">" 'evil-shift-right-line
    "s-]" 'evil-shift-right-line
    "<" 'evil-shift-left-line
    "s-[" 'evil-shift-left-line
    "C-C d" 'vimish-fold-delete
    "s-c" 'leo/copy-current-line)

  (general-vmap
    "<backspace>" 'evil-delete
    "M-D" 'delete-region
    "<tab>" 'vimish-fold
    ">" 'leo/evil-shift-right
    "s-]" 'leo/evil-shift-right
    "<" 'leo/evil-shift-left
    "s-[" 'leo/evil-shift-left
    "s-c" 'kill-ring-save)

  (general-emap
    "C-u" 'undo
    "M-a" 'mc/edit-beginnings-of-lines
    "M-e" 'mc/edit-ends-of-lines
    "M-r" 'mc/mark-all-dwim
    "s-<mouse-1>" 'mc/add-cursor-on-click
    "s-d" 'mc/mark-next-like-this)

  ;; multiple-cursors-mode
  (general-emap 'mc/keymap
    "<return>" 'newline
    "s-f" 'phi-search
    "s-F" 'phi-search-backward
    "M-1" 'mc/insert-numbers
    "M-h" 'mc-hide-unmatched-lines-mode
    "M-l" 'mc/insert-letters
    "M-s" 'mc/sort-regions
    "s-D" 'mc/skip-to-next-like-this)

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
   "y" 'browse-kill-ring)

  (general-define-key
   :states '(normal)
   :keymaps 'magit-process-mode-map
   "C-c" 'leo/magit-process-kill-no-confirm)

  (general-define-key
   :states '(normal)
   :keymaps 'magit-mode-map
   "p" 'leo/magit-push)

  (general-define-key
   :prefix "M-g"
   :keymaps 'override
   "d" 'magit-diff-buffer-file
   "s" 'magit-status
   "l" 'magit-log
   "t" 'git-timemachine
   "b" 'magit-blame-addition
   "o" 'leo/git-open-buffer-file-in-repo
   "r" 'git-link-homepage
   "p" 'leo/view-gh-pull-request
   "c" 'leo/open-gh-pull-request)

  (general-define-key
   "C-c e" 'leo/eval-and-replace
   "C-q" 'yas-expand)

  (general-define-key
   :keymaps 'ruby-mode-map
   :states '(normal insert visual)
   "s-S" 'leo/save-and-run-rubocop
   "C-c w" 'leo/copy-ruby-class-name)

  (general-define-key
   :keymaps '(web-mode-map rjsx-mode-map)
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
   "s-r" 'magit-refresh-all
   "<tab>" 'magit-section-toggle)

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
   "s-<up>" 'org-metaup
   "s-<down>" 'org-metadown
   "C-x <left>" 'org-metaleft
   "C-x <right>" 'org-metaright
   "C-x p" 'leo/org-screenshot
   "M-<return>" 'org-open-at-point)

  (general-define-key
   :keymaps 'org-mode-map
   :states '(insert)
   "M-S-<tab>" 'org-metaleft
   "M-<tab>" 'org-metaright)

  ;; vterm
  (general-define-key
   :keymaps 'vterm-mode-map
   :states '(normal insert visual emacs)
   "s-t" 'leo/delete-window
   "s-f" 'leo/vterm-search-forward
   "s-F" 'leo/vterm-search-backward
   "s-d" 'vterm-other-window
   "C-a" 'vterm-send-C-a
   "C-e" 'vterm-send-C-e
   "s-r" 'leo/vterm-clear
   "M-m" 'evil-first-non-blank
   "s-s" 'leo/export-buffer-contents)

  (general-define-key
   :keymaps 'override
   "s-t" 'leo/toggle-vterm
   "s-j" 'leo/run-vterm-console)

  (general-vmap 'vterm-mode-map
    "s-c" 'leo/vterm-visual-copy)

  (general-imap 'vterm-mode-map
    "<escape>" 'vterm--self-insert
    "<return>" 'vterm-send-return
    "s-e" 'vterm-extra-edit-command-in-new-buffer
    "C-c" 'vterm-send-C-c
    "C-d" 'vterm-send-C-d
    "C-z" 'vterm-send-C-z
    "C-r" 'vterm-send-C-r
    "C-u" 'leo/vterm-send-C-u
    "C-k" 'vterm-send-C-k
    "M-p" 'vterm-send-up
    "M-n" 'vterm-send-down
    "M-<left>" 'vterm-send-M-b
    "M-<right>" 'vterm-send-M-f)

  (general-define-key
   :keymaps 'vterm-mode-map
   :states '(normal insert visual)
   "M-k" 'vterm-previous-prompt
   "M-j" 'vterm-next-prompt)

  (general-define-key
   :keymaps 'vterm-extra-edit-mode-map
   :states '(normal insert visual)
   "s-w" 'leo/vterm-extra-edit-cancel)

  (general-nvmap 'vterm-mode-map
    "<return>" 'evil-insert-resume
    "A" 'leo/vterm-evil-append-line
    "I" 'leo/vterm-evil-insert-line
    "M-<left>" 'vterm-send-M-b
    "M-<right>" 'vterm-send-M-f)

  ;; Window numbers
  (general-define-key
   :states general-describe-evil-states
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

  ;; Help
  (general-nvmap
    "Kf" 'describe-function
    "Kv" 'describe-variable)

  (general-nvmap 'browse-kill-ring-mode-map
    "<escape>" 'browse-kill-ring-quit)

  (general-nmap 'ivy-occur-grep-mode-map
    "C-c C-c" 'wgrep-finish-edit)

  ;; Neotree
  (general-define-key
   :keymaps 'neotree-mode-map
   :states '(normal visual)
   "s-A" 'neotree-stretch-toggle
   "P" 'neotree-select-up-node
   "H" 'neotree-hidden-file-toggle
   "a" 'neotree-create-node
   "s-c" 'neotree-copy-node
   "r" 'neotree-rename-node
   "s-r" 'neotree-refresh
   "s-/" 'leo/neotree-search-inside-current-node-directory
   "o" 'leo/reveal-neotree-file-in-system-application
   "<tab>" 'leo/neotree-enter
   "<return>" 'leo/neotree-enter
   "d" 'neotree-delete-node)

  ;; Ruby on Rails
  (general-define-key
   :keymaps 'override
   :prefix "M-r"
   "m" 'projectile-rails-find-model
   "s" 'leo/goto-current-model-on-schema
   "c" 'projectile-rails-find-controller
   "v" 'projectile-rails-find-view)

  (general-define-key
   :keymaps 'isearch-mode-map
   "<return>" 'leo/isearch-exit)

  ;; Python
  (general-define-key
   :keymaps 'python-mode-map
   "s-S" 'py-autopep8)

  (general-define-key
   :keymaps 'ivy-switch-buffer-mode-map
   "C-k" 'ivy-switch-buffer-kill)

  (general-define-key
   :keymaps '(evil-motion-state-map evil-operator-state-map)
   "SPC" 'evil-ace-jump-char-mode
   "C-SPC" 'evil-ace-jump-word-mode)

  (general-define-key
   :keymaps 'xref--xref-buffer-mode-map
   "<return>" 'xref-show-location-at-point)
)
