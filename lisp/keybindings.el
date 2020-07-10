(use-package general)

(general-define-key
 :keymaps 'override
 "s-<return>" 'ivy-switch-buffer
 "s-x" 'counsel-M-x
 "s-r" 'leo/revert-buffer-no-confirm
 "s-n" 'leo/generate-new-untitled-buffer
 "s-N" 'deft
 "s-C" 'leo/copy-relative-file-path
 "s-w" 'delete-window
 "s-W" 'delete-other-windows
 "s-d" 'leo/multiple-cursor-on-next-match
 "s-J" 'leo/duplicate-line
 "s-e" 'treemacs
 "s-o" 'evil-switch-to-windows-last-buffer
 "s-p" 'counsel-projectile-find-file
 "s-F" 'leo/counsel-ag-search-all-project
 "s-k" 'kill-this-buffer
 "s-t" 'leo/projectile-run-vterm
 "C-c fp" 'leo/copy-full-file-path
 "C-c rm" 'projectile-rails-find-model
 "C-c vt" 'leo/run-vterm-console
 "C-x C-r" 'rename-buffer
 "C-x C-b" 'ivy-switch-buffer
 "C-=" 'leo/indent-whole-buffer)

(general-define-key
 :keymaps '(browse-kill-ring-mode-map)
 "<escape>" 'browse-kill-ring-quit)

(general-define-key
 :states '(visual)
 :keymaps 'override
 "s-v" 'leo/evil-paste)

;; General use keys that shouldn't be overridden
(general-define-key
 :prefix "C-C"
 :keymaps 'override
 "y" 'browse-kill-ring
 "ff" 'find-file)

(general-define-key
 :prefix "C-C g"
 :keymaps 'override
 "d" 'magit-diff-buffer-file
 "s" 'magit-status
 "l" 'magit-log-buffer-file
 "o" 'leo/git-open-branch-in-repo)

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

;; use E to go to EOL
(general-define-key
 :states '(motion)
 "E" 'evil-end-of-line)

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
 "C-d" 'vterm-send-C-d
 "C-c" 'vterm-send-C-c
 "C-r" 'vterm-send-C-r
 "C-u" 'vterm-send-C-u
 "C-a" 'vterm-send-C-a
 "C-e" 'vterm-send-C-e
 "C-k" 'vterm-send-C-k
 "M-<left>" 'vterm-send-M-b
 "M-<right>" 'vterm-send-M-f)

;; Window numbers
(general-define-key
 :states '(normal insert visual)
 :keymaps '(override)
 "s-1" 'winum-select-window-1
 "s-2" 'winum-select-window-2
 "s-3" 'winum-select-window-3
 "s-4" 'winum-select-window-4
 "s-5" 'winum-select-window-5
 "s-6" 'winum-select-window-6
 "s-7" 'winum-select-window-7
 "s-8" 'winum-select-window-8)
