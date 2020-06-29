; -------------------------- Modes --------------------------------
(show-paren-mode 1) ;; Highlight matching parens
(column-number-mode)
(transient-mark-mode 1)
(global-auto-revert-mode 1) ;; Always reload the file if it changed on disk

; -------------------------- Variables --------------------------------

(setq delete-by-moving-to-trash t)
(setq confirm-kill-emacs 'yes-or-no-p)
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Monaco")
  (set-face-attribute 'default nil :height 172)
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding")))
(modify-syntax-entry ?_ "w" (standard-syntax-table))
(setq x-select-enable-clipboard t)
(setq tab-width 2)
(progn (setq-default indent-tabs-mode nil))
(setq system-uses-terminfo nil)
(setq ivy-re-builders-alist '((ivy-switch-buffer . ivy--regex-plus)))
(setq ivy-initial-inputs-alist nil)
(setq find-ls-option '("-print0 | xargs -0 ls -alhd" . ""))
(setq counsel-ag-base-command "ag --hidden --ignore .git --ignore vendor --vimgrep %s")
(setq kill-buffer-query-functions nil)
(setq-default line-spacing 1) ;; A nice line height
(setq linum-relative-current-symbol "")
(setq-default frame-title-format '((:eval (if (buffer-file-name)
                                              (abbreviate-file-name (buffer-file-name)) "%f"))))

; -------------------------- Hooks --------------------------------

(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))
(add-hook 'ruby-mode-hook (lambda () (local-set-key (kbd "C-x C-x") 'save-and-run-rubocop)))
(add-hook 'compilation-filter-hook 'leo/fix-colors-on-compilation-mode)

; -------------------------- Packages --------------------------------
(linum-relative-global-mode t)

(require 'ansi-color)

(defun leo/fix-colors-on-compilation-mode()
  "Fixes the output colors on compilation mode buffers."
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

(use-package helm
  :init
  (global-set-key (kbd "M-x") 'helm-M-x))

(use-package drag-stuff
  :init
  (drag-stuff-global-mode)
  (global-set-key (kbd "<s-up>") 'drag-stuff-up)
  (global-set-key (kbd "<s-down>") 'drag-stuff-down))

(use-package fill-column-indicator
  :init
  (progn
    (setq-default fill-column 80)
    (setq fci-rule-width 1)
    (setq fci-rule-color "#D0BF8F")
    ;; manually register the minor mode since it does not define any
    ;; lighter
    (push '(fci-mode "") minor-mode-alist)
    (setq whitespace-line-column 80)
    (setq whitespace-style '(face lines-tail))
    (custom-set-faces
     '(whitespace-line ((t (:foreground "black" :background "red" :underline t))))
     )

    (add-hook 'ruby-mode-hook 'turn-on-fci-mode)
    (add-hook 'ruby-mode-hook 'whitespace-mode)
    (add-hook 'web-mode-hook 'turn-on-fci-mode)
    (add-hook 'web-mode-hook 'whitespace-mode)))

(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package saveplace
  :init (setq-default save-place t)
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

(use-package anzu :config (global-anzu-mode t))

;; Expand Region
(use-package expand-region
  :bind ("M-2" . 'er/expand-region))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (flycheck-pos-tip-mode 0))

(use-package dotenv-mode
  :config (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)))

; -------------------------- Functions --------------------------------

(defun system-move-file-to-trash (file)
  "Use \"trash\" to move FILE to the system trash.
When using Homebrew, install it using \"brew install trash\"."
  (call-process (executable-find "trash")
		nil 0 nil
		file))

(defun leo/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t)
  (message "Buffer reloaded."))

(defun leo/counsel-ag-search-all-project ()
  (interactive)
  (if (eq evil-state 'visual)
      (let ((search-string (buffer-substring
                            (evil-range-beginning (evil-visual-range))
                            (evil-range-end (evil-visual-range)))))
        (evil-normal-state)
        (counsel-ag search-string))
    (counsel-ag))
)

(defun leo/eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun leo/generate-new-untitled-buffer ()
  "Create a new buffer with name untitled."
  (interactive)
  (switch-to-buffer (generate-new-buffer "untitled"))
  (linum-relative-mode)
  (evil-mc-mode)
  (evil-insert-state))

(defun leo/get-current-file-relative-path ()
  (interactive)
  (file-relative-name buffer-file-name (projectile-project-root)))

(defun leo/copy-relative-file-path ()
  "Copy the current buffer's relative file path to `kill-ring'."
  (interactive)
  (kill-new (leo/get-current-file-relative-path))
  (evil-echo "File path copied: \"%s\"" (car kill-ring)))

(defun leo/copy-full-file-path (&optional @dir-path-only-p)
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

(defun leo/duplicate-line (arg)
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

(defun leo/indent-whole-buffer ()
  "INDENT WHOLE BUFFER."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))
