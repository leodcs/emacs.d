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
 "s-d" 'evil-mc-make-and-goto-next-match
 "s-J" 'leo/duplicate-line
 "s-e" 'treemacs
 "s-p" 'counsel-projectile-find-file
 "s-F" 'leo/counsel-ag-search-all-project
 "s-k" 'kill-this-buffer
 "s-t" 'leo/projectile-run-vterm
 "C-c fp" 'leo/copy-full-file-path
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

;; Leader Keybindings
(defconst leo/leader "<SPC>")
(general-define-key
 :keymaps 'override
 :prefix leo/leader
 :states '(normal visual)
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
 "ff" 'find-file)

(general-define-key
 :prefix "C-C g"
 :keymaps 'override
 "d" 'magit-diff-buffer-file
 "s" 'magit-status
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
  "C-<return>" 'org-return)

(general-define-key
 :states '(insert)
 :keymaps 'vterm-mode-map
 "C-d" 'vterm-send-C-d
 "C-c" 'vterm-send-C-c
 "C-u" 'vterm-send-C-u)

;; Slack
(general-define-key
 :prefix "C-c"
 :states '(normal)
 :keymaps 'override
 "sQ" 'slack-ws-close
 "sS" 'slack-start
 "ss" 'slack-select-rooms
 "su" 'slack-select-unread-rooms
 "sma" 'slack-message-run-action
 "sme" 'slack-message-edit
 "smt" 'slack-thread-show-or-create)

(general-define-key
 :states '(insert)
 :keymaps '(slack-mode-map slack-message-edit-buffer-mode-map slack-thread-message-buffer-mode-map)
 "@" 'slack-message-embed-mention
 "#" 'slack-message-embed-channel)
