(use-package general)

;; Leader Keybindings
(defconst leo/leader "<SPC>")
(general-define-key
 :keymaps 'override
 :prefix leo/leader
 :states '(normal visual)
 "<SPC>" 'ivy-switch-buffer
 "<down>" 'open-html-tag
 "<up>" 'close-html-tag
 "RET" 'evil-switch-to-windows-last-buffer
 "d" 'delete-other-windows
 "fm" 'projectile-rails-find-model)

;; General use keys that shouldn't be overridden
(general-define-key
 :prefix "C-C"
 :keymaps 'override
 "y" 'browse-kill-ring
 "C-r" 'rename-buffer
 "ff" 'find-file)

(general-define-key
 :prefix "C-C g"
 :keymaps 'override
 "d" 'magit-diff-buffer-file
 "s" 'magit-status
 "o" 'leo/magit-open-file-in-repo)

(general-define-key
 :keymaps 'override
 "s-x" 'counsel-M-x
 "M-x" 'counsel-M-x
 "s-r" 'leo/revert-buffer-no-confirm
 "s-n" 'leo/generate-new-untitled-buffer
 "s-N" 'deft
 "s-C" 'leo/copy-relative-file-path
 "s-w" 'delete-window
 "s-d" 'evil-mc-make-and-goto-next-match
 "s-J" 'leo/duplicate-line
 "s-e" 'treemacs
 "s-p" 'counsel-projectile-find-file
 "s-F" 'leo/counsel-ag-search-all-project
 "s-k" 'kill-this-buffer
 "C-=" 'leo/indent-whole-buffer)

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
 :states '(normal visual)
 "s-S" 'leo/save-and-run-erblint-autocorrect)

;; Magit
(general-define-key
   :keymaps 'transient-base-map
   "<escape>" 'transient-quit-all)

;; use E to go to EOL
(general-define-key
 :states '(motion)
 "E" 'evil-end-of-line)

(general-define-key
 :states '(normal visual)
 "=" 'leo/evil-indent)
