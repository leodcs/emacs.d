(use-package general)

;; Leader Keybindings
(defconst leo/leader "<SPC>")
(general-define-key
 :prefix leo/leader
 :states '(normal visual)
 :keymaps 'override
 "<SPC>" 'ivy-switch-buffer
 "<down>" 'open-html-tag
 "<up>" 'close-html-tag
 "RET" 'evil-switch-to-windows-last-buffer
 "d" 'delete-other-windows
 "fm" 'projectile-rails-find-model)

;; General use keys that shouldn't be overridden
(general-define-key
 :keymaps 'override
 "C-c n" 'deft
 "C-c e" 'eval-and-replace
 "s-r" 'rename-buffer
 "s-n" 'leo/generate-new-untitled-buffer
 "s-C" 'copy-relative-file-path
 "s-w" 'delete-window
 "s-d" 'evil-mc-make-and-goto-next-match
 "s-J" 'leo/duplicate-line
 "s-e" 'leo/neotree-toggle
 "s-p" 'counsel-projectile-find-file
 "s-F" 'counsel-ag-search-all-project
 "s-k" 'kill-this-buffer
 "C-c s d" 'magit-diff-buffer-file
 "C-c g s" 'magit-status
 "C-=" 'leo/indent-whole-buffer)

(general-define-key
 "C-q" 'yas-expand
 "C-(" 'rubocop-check-current-file
 "C-x C-f" 'counsel-find-file
 "s-E" 'eval-last-sexp)

;; Neotree
(general-define-key
 :states '(normal visual)
 :keymaps 'neotree-mode-map
 "n" 'leo/neotree-select-next-sibling-node
 "p" 'leo/neotree-select-previous-sibling-node
 "P" 'neotree-select-up-node
 "H" 'neotree-hidden-file-toggle
 "y" 'neotree-copy-node
 "c" 'neotree-create-node
 "r" 'neotree-rename-node
 "s-r" 'neotree-refresh
 "s-F" 'leo/neotree-search-inside-current-node-directory
 "o" 'neotree-open-file-in-system-application
 "<tab>" 'neotree-stretch-toggle
 "<return>" 'leo/neotree-enter
 "d" 'neotree-delete-node)

;; use E to go to EOL
(general-define-key
 :states '(motion)
 "E" '(lambda ()
        (interactive)
        (evil-end-of-line)))
