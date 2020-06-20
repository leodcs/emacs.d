(setq-default fill-column 80) ; toggle wrapping text at this column
(setq gc-cons-threshold (* 50 1000 1000))
(setq delete-old-versions -1 ) ; delete excess backups silently
(setq version-control t )
(setq vc-make-backup-files t )
(setq vc-follow-symlinks t )
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) )
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) )
(setq inhibit-startup-screen t )
(setq ring-bell-function 'ignore ) ; silent bell on mistakes
(setq coding-system-for-read 'utf-8 )
(setq coding-system-for-write 'utf-8)
(setq sentence-end-double-space nil)
(global-display-line-numbers-mode t )
(menu-bar-mode -1) ; no need for the menu bars - we've got key combos for that!
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(defalias 'yes-or-no-p 'y-or-n-p)
(winner-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(save-place-mode 1)
(delete-selection-mode 1)
(setq recentf-max-saved-items 120)
(global-set-key [escape] 'keyboard-escape-quit)
(global-set-key (kbd "M-o") 'other-window)

;; UTF-8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(when (eq system-type 'windows-nt)
  (set-clipboard-coding-system 'utf-16le-dos))

;; use-package setup

(require 'package)
(setq package-enable-at-startup nil) ; dont do it immediately
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
             ("gnu"       . "http://elpa.gnu.org/packages/")
             ("melpa"     . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents) ; update archives
  (package-install 'use-package)) ; grab the newest use-package

;; Define packages
(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/vendor/")

(load "~/.emacs.d/lisp/autocompletion.el")
(load "~/.emacs.d/lisp/dired.el")
(load "~/.emacs.d/lisp/editor.el")
(load "~/.emacs.d/lisp/evil.el")
(load "~/.emacs.d/lisp/git.el")
(load "~/.emacs.d/lisp/gui.el")
(load "~/.emacs.d/lisp/javascript.el")
(load "~/.emacs.d/lisp/json.el")
(load "~/.emacs.d/lisp/markdown.el")
(load "~/.emacs.d/lisp/notes.el")
(load "~/.emacs.d/lisp/project_management.el")
(load "~/.emacs.d/lisp/ruby.el")
(load "~/.emacs.d/lisp/search.el")
(load "~/.emacs.d/lisp/web.el")
(load "~/.emacs.d/lisp/neotree.el")
(load "~/.emacs.d/lisp/keybindings.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-org evil-collection evil-goggles general dracula-theme distinguished-theme dinstinguished-theme cyberpunk-theme browse-kill-ring deft evil-magit auto-dim-other-buffers golden-ratio zoom-frm json-mode evil-ruby-text-objects evil-ruby-text-objects-mode evil-leader evil-multiedit evil-mc fill-column-indicator eglot lsp-mode dotenv-mode markdown-mode auto-answer evil-surround evil-commentary nlinum-relative linum-relative evil rubocop anzu flx goto-chg dot-mode helm helm-config rvm highlight-indent-guides which-key rbenv yasnippet-snippets yasnippet smartparens company ace-window flycheck-popup-tip flycheck-pos-tip flycheck expand-region zencoding-mode web-mode hungry-delete exec-path-from-shell rspec-mode robe projectile-rails counsel-projectile projectile git-gutter magit wgrep-ag wgrep ivy-hydra counsel dashboard neotree kaolin-themes doom-themes all-the-icons-dired all-the-icons-ivy all-the-icons drag-stuff use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-dim-other-buffers-face ((t (:background "gray22"))))
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed))))
 '(whitespace-line ((t (:foreground "black" :background "red" :underline t)))))

(setq gc-cons-threshold (* 2 1000 1000))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
