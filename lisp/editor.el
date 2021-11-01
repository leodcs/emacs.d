; -------------------------- Modes --------------------------------
(show-paren-mode t) ;; Highlight matching parens
(column-number-mode)
(transient-mark-mode t)
(global-auto-revert-mode t) ;; Always reload the file if it changed on disk
(savehist-mode t)
(global-hl-line-mode t)

; -------------------------- Variables --------------------------------

(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :family "Monaco")
  (set-face-attribute 'default nil :height 150)
  (set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding")))
(progn (setq-default indent-tabs-mode nil))
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default evil-shift-width 2)
(setq-default line-spacing 1)
(setq-default fill-column 80)
(setq-default truncate-lines t)
(setq-default frame-title-format '((:eval (if (buffer-file-name) (abbreviate-file-name (buffer-file-name)) "%f"))))
(modify-syntax-entry ?_ "w" (standard-syntax-table))
(setq whitespace-line-column 80
      bidi-inhibit-bpa t
      css-indent-offset 2
      column-number-indicator-zero-based nil
      whitespace-style '(face lines-tail)
      delete-by-moving-to-trash t
      confirm-kill-emacs 'yes-or-no-p
      select-enable-clipboard t
      tab-width 2
      system-uses-terminfo nil
      highlight-indent-guides-method 'bitmap
      highlight-indent-guides-responsive 'top
      find-ls-option '("-print0 | xargs -0 ls -alhd" . "")
      hl-line-sticky-flag nil
      mac-option-modifier 'meta
      mac-command-modifier 'super
      mac-pass-command-to-system nil
      kill-buffer-query-functions nil
      evil-want-keybinding nil)

; -------------------------- Hooks --------------------------------

(add-hook 'compilation-filter-hook 'leo/fix-colors-on-compilation-mode)
(add-hook 'text-mode-hook 'leo/text-mode-with-hash-comments)
(add-hook 'prog-mode-hook 'leo/prog-mode-enter)

; -------------------------- Packages --------------------------------

(use-package ace-jump-mode)

(use-package symbol-overlay)

(use-package google-c-style
  :config
  (add-hook 'java-mode-hook
            (lambda()
              (subword-mode)
              (setq c-basic-offset 2))))

(use-package vimish-fold
  :ensure t
  :config
  (vimish-fold-global-mode t)
  (use-package evil-vimish-fold
    :ensure
    :after vimish-fold
    :config
    (global-evil-vimish-fold-mode)))

(require 'ansi-color)

(use-package browse-kill-ring
  :config
  (setq browse-kill-ring-highlight-current-entry t)
  (setq browse-kill-ring-highlight-inserted-item "pulse"))

(use-package string-inflection
  :config
  (defun leo/string-inflection-cycle ()
    "switching by major-mode"
    (interactive)
    (cond
     ;; for emacs-lisp-mode
     ((eq major-mode 'emacs-lisp-mode)
      (string-inflection-all-cycle))
     (t
      ;; default
      (string-inflection-ruby-style-cycle)))))

(use-package scratch)

(use-package winum
  :config
  (winum-mode))

(use-package drag-stuff
  :init
  (drag-stuff-global-mode))

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package saveplace
  :init (setq-default save-place t)
  :config
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

(use-package anzu :config (global-anzu-mode t))

(use-package dotenv-mode
  :config (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)))

(use-package highlight-indent-guides)

; -------------------------- Functions --------------------------------

(defun leo/current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

(defun leo/prog-mode-enter ()
  (interactive)
  (highlight-indent-guides-mode)
  (symbol-overlay-mode 1)
  (display-line-numbers-mode t))

(defun leo/load-editor-config ()
  (interactive)
  (load (concat (projectile-project-root) "./.editorconfig")))

(defun leo/disable-whitespace-mode ()
  (interactive)
  (whitespace-mode -1)
  (setq whitespace-style '())
  (message "whitespace-mode disabled"))

(defun leo/text-mode-with-hash-comments ()
  "text-mode with # comments"
  (setq comment-start "#")
  (font-lock-add-keywords nil '(("#.+" . font-lock-comment-face))))

(defun leo/copy-current-line()
  (interactive)
  (let* ((region-begin (current-indentation))
         (region-end -1)
         (line (substring (thing-at-point 'line) region-begin region-end)))
    (kill-new line)
    (pulse-momentary-highlight-one-line (point))))

(defun leo/delete-window ()
  (interactive)
  (delete-window)
  (balance-windows)
  (evil-scroll-line-to-center (line-number-at-pos)))

(defun leo/purge-this-buffer ()
  (interactive)
  (kill-this-buffer)
  (leo/delete-window))

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

(defun leo/generate-new-scratch-buffer ()
  (interactive)
  (call-interactively 'scratch)
  (let* ((mode (format "%s" major-mode))
         (string (concat "Scratch buffer for: " mode "\n\n")))
    (when scratch-buffer
      (save-excursion
        (insert string)
        (goto-char (point-min))
        (comment-region (point-at-bol) (point-at-eol)))
      (forward-line 2))
    (rename-buffer (concat "*Scratch for " mode "*") t))
  (display-fill-column-indicator-mode -1)
  (whitespace-mode -1)
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

(defun leo/undo-last-text ()
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
  (other-window 1))

(defun leo/split-window-horizontally ()
  (interactive)
  (evil-window-split)
  (other-window 1))

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
  (let ((timestamp (format-time-string "%Y%m%d_%H%M%S" (current-time))))
    (insert timestamp)))

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

(defun leo/org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (org-display-inline-images)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
  ; take screenshot
  (if (eq system-type 'darwin)
      (call-process "screencapture" nil nil nil "-i" filename))
  (if (eq system-type 'gnu/linux)
      (call-process "import" nil nil nil filename))
  ; insert into file if correctly taken
  (if (file-exists-p filename)
    (insert (concat "[[file:" filename "][" (read-string "Link text: ") "]]"))))

(defun leo/delete-line ()
  "Delete (not kill) the current line."
  (interactive)
  (save-excursion
    (delete-region
     (progn (forward-visible-line 0) (point))
     (progn (forward-visible-line 1) (point)))))
