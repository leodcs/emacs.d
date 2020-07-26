; -------------------------- Modes --------------------------------
(show-paren-mode t) ;; Highlight matching parens
(column-number-mode)
(transient-mark-mode t)
(global-auto-revert-mode t) ;; Always reload the file if it changed on disk
(savehist-mode t)

; -------------------------- Variables --------------------------------

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Monaco")
  (set-face-attribute 'default nil :height 172)
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding")))
(progn (setq-default indent-tabs-mode nil))
(setq-default line-spacing 1)
(setq-default fill-column 80)
(setq-default frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%f"))))
(modify-syntax-entry ?_ "w" (standard-syntax-table))
(setq whitespace-line-column 80
      whitespace-style '(face lines-tail)
      delete-by-moving-to-trash t
      confirm-kill-emacs 'yes-or-no-p
      select-enable-clipboard t
      tab-width 2
      system-uses-terminfo nil
      find-ls-option '("-print0 | xargs -0 ls -alhd" . "")
      kill-buffer-query-functions nil)

; -------------------------- Hooks --------------------------------

(add-hook 'compilation-filter-hook 'leo/fix-colors-on-compilation-mode)

; -------------------------- Packages --------------------------------

(require 'ansi-color)

(use-package winum
  :config
  (winum-mode))

(use-package drag-stuff
  :init
  (drag-stuff-global-mode)
  (global-set-key (kbd "<s-up>") 'drag-stuff-up)
  (global-set-key (kbd "<s-down>") 'drag-stuff-down))

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

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (use-package flycheck-posframe
    :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
    (setq flycheck-posframe-warning-prefix "\u26a0 ")))

(use-package dotenv-mode
  :config (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)))

; -------------------------- Functions --------------------------------

(defun leo/ie-keyboard-quit ()
  "Bind C-g to first deactivate-mark an only then goto normal state."
  (interactive)
  (if (region-active-p)
      (deactivate-mark)
    (evil-normal-state)))

(defun leo/move-beginning-of-line-or-indentation ()
  "Toggle between the beginning of the line and the beginning of the code."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

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
  (message "Buffer reloaded"))

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
           (message "Directory path copied: \"%s\"" (file-name-directory $fpath))
           (file-name-directory $fpath))
       (progn
         (message "File path copied: \"%s\"" $fpath)
         $fpath )))))

(defun leo/indent-whole-buffer ()
  "INDENT WHOLE BUFFER."
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(defun leo/fix-colors-on-compilation-mode()
  "Fixes the output colors on compilation mode buffers."
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

(defun leo/display-buffer-at-bottom (buffer alist)
  (select-window (display-buffer-at-bottom buffer alist)))

(defun leo/nuke-all-buffers ()
  (interactive)
  (let (dashboard (get-buffer "*dashboard*"))
    (delete-other-windows)
    (mapc 'kill-buffer (delq (get-buffer "*dashboard*") (buffer-list)))
    (setq recentf-list dashboard)
    (message "Boom!")))

(defun leo/set-current-trello-card-url ()
  (interactive)
  (setq current-trello-card-url (read-string "URL: ")))

(defun leo/open-current-trello-card ()
  (interactive)
  (browse-url current-trello-card-url))

(defun leo/send-C-u ()
  (interactive)
  (delete-region (line-beginning-position) (line-end-position)))

(defun leo/file-to-list (filePath)
  "Return the filePath as a list of strings"
  (interactive)
  (split-string
   (with-temp-buffer
     (insert-file-contents filePath)
     (buffer-substring-no-properties
      (point-min)
      (point-max))) "\n" t))

(defun leo/split-window-vertically ()
  (interactive)
  (evil-window-vsplit)
  (other-window 1)
  (counsel-projectile-find-file))

(defun leo/split-window-horizontally ()
  (interactive)
  (evil-window-split)
  (other-window 1)
  (counsel-projectile-find-file))

(defun leo/export-buffer-contents ()
  "Print file in the current buffer as pdf. Requires `ps2pdf'."
  (interactive)
  (let* ((folder "~/.emacs.d/exports/")
         (current-mode (replace-regexp-in-string "-mode" "" (symbol-name major-mode)))
         (timestamp (format-time-string "%Y%m%d@%H%M%S" (current-time)))
         (default-filename (concat folder current-mode "_" timestamp))
         (ps-file (read-string "Filepath (without extension): " default-filename))
         (pdf-file (concat (file-name-sans-extension ps-file) ".pdf")))
    (ps-print-buffer ps-file)
    (print pdf-file)
    (shell-command (concat "ps2pdf " ps-file " " pdf-file))
    (delete-file ps-file)
    (message "Wrote %s" pdf-file)))

(defvar current-date-format "%d/%m/%Y"
  "Format of date to insert with `insert-current-date' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun leo/insert-current-timestamp ()
  "insert the current date and time into current buffer."
  (interactive)
  (let* ((date (format-time-string current-date-format (current-time)))
         (time (format-time-string current-time-format (current-time))))
  (insert (concat date " " time))))

(defun leo/insert-current-date ()
  "insert the current date into the current buffer."
  (interactive)
  (insert (format-time-string current-date-format (current-time))))

(defun leo/insert-current-time ()
  "insert the current time into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time))))

(defun leo/eval-buffer ()
  (interactive)
  (eval-buffer)
  (message "buffer evaluated"))
