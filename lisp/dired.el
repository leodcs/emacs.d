(require 'dired-x)

;; Use ls from emacs
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

(defun leo/dired-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))


;; Automatically create missing directories when creating new files
(defun leo/create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
(add-to-list 'find-file-not-found-functions #'leo/create-non-existent-directory)

;; Changing the way M-<and M-> work in dired
;; Instead of taking me to the very beginning or very end, they now take me to the first or last file.
(defun leo/dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (next-line 2))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'leo/dired-back-to-top)

(defun leo/dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'leo/dired-jump-to-bottom)

;; C-a is nicer in dired if it moves back to start of files
(defun leo/dired-back-to-start-of-files ()
  (interactive)
  (backward-char (- (current-column) 2)))
