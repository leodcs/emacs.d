(global-set-key [remap move-beginning-of-line] 'prelude-move-beginning-of-line)
(global-set-key [escape] 'keyboard-escape-quit)
(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-c C-y") 'copy-line)
(global-set-key (kbd "C-,") 'ivy-switch-buffer)
(global-set-key (kbd "C-(") 'rubocop-check-current-file)
(global-set-key (kbd "C-)") 'rubocop-autocorrect-current-file)
(global-set-key (kbd "C-q") (kbd "C-x 0"))
(global-set-key (kbd "C-c C-d") 'duplicate-line)
(global-set-key (kbd "C-u") 'undo)
(global-set-key (kbd "C-<return>") 'counsel-bookmark)
(global-set-key (kbd "C-q") 'yas-expand)
(global-set-key (kbd "s-k") 'kill-this-buffer)
(global-set-key (kbd "s-e") 'neotree-toggle)
(global-set-key (kbd "s-r") 'rename-buffer)
(global-set-key (kbd "s-n") 'create-untitled-buffer)
(global-set-key (kbd "s-p") 'counsel-projectile-find-file)
(global-set-key (kbd "s-p") 'counsel-projectile-find-file)
(global-set-key (kbd "s-F") 'counsel-ag-search-all-project)
(global-set-key (kbd "s-C") 'copy-relative-file-path)

(defun counsel-ag-search-all-project ()
  (interactive)
  (if (eq evil-state 'visual)
      (let ((search-string (buffer-substring
                            (evil-range-beginning (evil-visual-range))
                            (evil-range-end (evil-visual-range)))))
        (evil-normal-state)
        (counsel-ag search-string))
    (counsel-ag))
)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


(use-package auto-dim-other-buffers
  :config
  (auto-dim-other-buffers-mode)
  )


(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  )

(use-package fill-column-indicator
  :defer t
  :init
  (progn
    (setq fci-rule-width 1)
    (setq fci-rule-color "#D0BF8F")
    ;; manually register the minor mode since it does not define any
    ;; lighter
    (push '(fci-mode "") minor-mode-alist)
    (setq whitespace-line-column 79)
    (setq whitespace-style '(face lines-tail))
    (custom-set-faces
     '(whitespace-line ((t (:foreground "black" :background "red" :underline t))))
     )

    (add-hook 'ruby-mode-hook 'turn-on-fci-mode)
    (add-hook 'ruby-mode-hook 'whitespace-mode)
    (add-hook 'web-mode-hook 'turn-on-fci-mode)
    (add-hook 'web-mode-hook 'whitespace-mode)
    )
  )

(setq frame-resize-pixelwise t)

(use-package json-mode
  :config
  (defun beautify-json ()
    (interactive)
    (let ((b (if mark-active (min (point) (mark)) (point-min)))
          (e (if mark-active (max (point) (mark)) (point-max))))
      (shell-command-on-region b e
                               "python -m json.tool" (current-buffer) t)))
  (define-key json-mode-map (kbd "C-c C-b") 'beautify-json)
  )
                                        ; Map escape to cancel (like C-g)...
(define-key isearch-mode-map [escape] 'isearch-abort)   ;; isearch
(define-key isearch-mode-map "\e" 'isearch-abort)   ;; \e seems to work better for terminals


(add-to-list 'auto-mode-alist '("\\.js\\..*\\'" . javascript-mode))

(column-number-mode)
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

(defun my-switch-project-hook ()
  "Perform some action after switching Projectile projects."
  ;; Do something interesting here...
  ;;
  ;; `projectile-current-project-files', and `projectile-current-project-dirs' can be used
  ;; to get access to the new project's files, and directories.
  (rvm-activate-corresponding-ruby)
  )

(add-hook 'projectile-after-switch-project-hook #'my-switch-project-hook)

(evil-set-initial-state 'ivy-occur-grep-mode 'normal)
;; set default tab char's display width to 4 spaces
(setq-default tab-width 2) ; emacs 23.1 to 26 default to 8

;; set current buffer's tab char's display width to 4 spaces
(setq tab-width 2)
(progn
  ;; make indent commands use space only (never tab character)
  (setq-default indent-tabs-mode nil)
  ;; emacs 23.1 to 26, default to t
  ;; if indent-tabs-mode is t, it means it may use tab, resulting mixed space and tab
  )

(defun create-untitled-buffer ()
  "Create a new buffer with name untitled."
  (interactive)
  (switch-to-buffer (generate-new-buffer "untitled"))
  (nlinum-relative-mode)
  (evil-insert-state)
)

(defun copy-line ()
  (interactive)
  (save-excursion
    (back-to-indentation)
    (kill-ring-save
     (point)
     (line-end-position)))
  (message "line sent to clipboard"))

(setq kill-buffer-query-functions nil)

(defun copy-relative-file-path ()
  "Copy the current buffer's relative file path to `kill-ring'."
  (interactive)
  (kill-new
   (file-relative-name buffer-file-name (projectile-project-root)))
  (evil-echo "File path copied: \"%s\"" (car kill-ring))
)

(defun copy-full-file-path (&optional @dir-path-only-p)
  "Copy the current buffer's file path or dired path to `kill-ring'.
   Result is full path.
   If `universal-argument' is called first, copy only the dir path.

   If in dired, copy the file/dir cursor is on, or marked files.

   If a buffer is not file and not dired, copy value of `default-directory' (which is usually the “current” dir when that buffer was created)

   URL `http://ergoemacs.org/emacs/emacs_copy_file_path.html'
   Version 2017-09-01"

  (interactive "P")
  (let (($fpath
         (if (string-equal major-mode 'dired-mode)
             (progn
               (let (($result (mapconcat 'identity (dired-get-marked-files) "\n")))
                 (if (equal (length $result) 0)
                     (progn default-directory )
                   (progn $result))))
           (if (buffer-file-name)
               (buffer-file-name)
             (expand-file-name default-directory)))))
    (kill-new
     (if @dir-path-only-p
         (progn
           (message "Directory path copied: 「%s」" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: 「%s」" $fpath)
         $fpath )))))

(modify-syntax-entry ?_ "w" (standard-syntax-table))

(setq counsel-ag-base-command "ag --hidden --ignore .git --ignore vendor --vimgrep %s")
(use-package undo-tree)

(setq x-select-enable-clipboard t)

(defun save-and-run-rubocop ()
  "Saves buffer and runs rubocop autocorrect"
  (interactive)
  (save-buffer)
  ;; (call-process-shell-command
  ;;  (format "rubocop -a --config ~/projects/personal/dotfiles/rubocop/rubocop.yml %s"
  ;;          (shell-quote-argument (buffer-file-name))))
  (rubocop-autocorrect-current-file)
  )

(add-hook 'ruby-mode-hook
          (lambda () (local-set-key (kbd "C-x C-x") 'save-and-run-rubocop)))

(defun save-and-run-erblint ()
  "Saves buffer and runs erblint autocorrect"
  (interactive)
  (save-buffer)
  (shell-command
   (format "erblint -a --config ~/projects/personal/dotfiles/erb-lint/erb-lint.yml %s"
           (shell-quote-argument (buffer-file-name))))
  (web-mode-reload)
  )

(add-hook 'web-mode-hook
          (lambda () (local-set-key (kbd "C-x C-x") 'save-and-run-erblint)))


(use-package rubocop
  :config
  )

(transient-mark-mode 1)

(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line) ; move to end of line
  (set-mark (line-beginning-position)))

(setq-default
 whitespace-line-column 80
 whitespace-style       '(face lines-tail))
(whitespace-mode)

(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))


;;; dired START
(require 'dired-x)

;; Nice listing
(setq find-ls-option '("-print0 | xargs -0 ls -alhd" . ""))

;; Always copy/delete recursively
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))

;; Auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Hide some files
(setq dired-omit-files "^\\..*$\\|^\\.\\.$")
(setq dired-omit-mode t)

;; List directories first
(defun sof/dired-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))

(add-hook 'dired-after-readin-hook 'sof/dired-sort)

;; Automatically create missing directories when creating new files
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'my-create-non-existent-directory)

;; Use ls from emacs
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

;; Changing the way M-<and M-> work in dired
;; Instead of taking me to the very beginning or very end, they now take me to the first or last file.
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (next-line 2))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

;; C-a is nicer in dired if it moves back to start of files
(defun dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))

(define-key dired-mode-map (kbd "C-a") 'dired-back-to-start-of-files)

;;; dired END
(use-package saveplace
  :init (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

(setq-default frame-title-format '((:eval (if (buffer-file-name)
                                              (abbreviate-file-name (buffer-file-name)) "%f"))))
(use-package anzu
  :init (global-anzu-mode +1))

(setq system-uses-terminfo nil)
(global-auto-revert-mode 1)
(show-paren-mode 1)

(use-package flx)
(setq ivy-re-builders-alist
      '((ivy-switch-buffer . ivy--regex-plus)))
(setq ivy-initial-inputs-alist nil)


(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

(global-auto-revert-mode 1) ;; Always reload the file if it changed on disk
(setq-default line-spacing 1) ;; A nice line height
(show-paren-mode 1) ;; Highlight matching parens

;; Saves the cursor position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; New undo alternatives

;; Bookmark

;; Expand Region
(use-package expand-region
  :bind ("M-2" . 'er/expand-region))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (flycheck-pos-tip-mode 0)
  )

;; Company - Auto complation

(use-package company
  :init
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0)
  :config
  (eval-after-load 'company
    '(push 'company-robe company-backends))

  (global-company-mode)
  )

(use-package yasnippet
  :defer 5
  :init
  (yas-global-mode 1)
  :config
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (eval-after-load 'rspec-mode
    '(rspec-install-snippets))
  )

(use-package recentf
  :defer 1)

(use-package yasnippet-snippets
  :after (yasnippet)
  )
(use-package yasnippet-classic-snippets
  :config
  (yas-reload-all)
  :after yasnippet
  )

(use-package dotenv-mode
  :config (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)))
