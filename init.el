(setq gc-cons-threshold (* 50 1000 1000)
      delete-old-versions -1  ; delete excess backups silently
      version-control t
      vc-make-backup-files t
      vc-follow-symlinks t
      backup-directory-alist `(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t))
      inhibit-startup-screen t
      ring-bell-function 'ignore  ; silent bell on mistakes
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8
      sentence-end-double-space nil
      recentf-max-saved-items 120
      frame-resize-pixelwise t
      linum-relative-current-symbol "")
(defalias 'yes-or-no-p 'y-or-n-p)
(global-display-line-numbers-mode t )
(menu-bar-mode -1) ; no need for the menu bars - we've got key combos for that!
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(winner-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(save-place-mode 1)
(delete-selection-mode 1)
(linum-relative-global-mode t)
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
(use-package quelpa-use-package)

;; Set PATH
(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(add-to-list 'load-path "~/.emacs.d/vendor/")
(load "~/.emacs.d/lisp/autocompletion.el")
(load "~/.emacs.d/lisp/dired.el")
(load "~/.emacs.d/lisp/editor.el")
(load "~/.emacs.d/lisp/evil.el")
(load "~/.emacs.d/lisp/git.el")
(load "~/.emacs.d/lisp/gui.el")
(load "~/.emacs.d/lisp/javascript.el")
(load "~/.emacs.d/lisp/json.el")
(load "~/.emacs.d/lisp/org.el")
(load "~/.emacs.d/lisp/project_management.el")
(load "~/.emacs.d/lisp/ruby.el")
(load "~/.emacs.d/lisp/search.el")
(load "~/.emacs.d/lisp/web.el")
(load "~/.emacs.d/lisp/keybindings.el")
(load "~/.emacs.d/lisp/neotree.el")
(load "~/.emacs.d/lisp/vterm.el")
(load "~/.emacs.d/lisp/multiple-cursors.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-dabbrev-downcase 0)
 '(company-idle-delay 0)
 '(display-buffer-alist
   '(("*Help*"
      (leo/display-buffer-at-bottom))
     ("RuboCop"
      (leo/display-buffer-at-bottom))
     ("magit: "
      (display-buffer-reuse-window display-buffer-same-window))
     ("magit"
      (display-buffer-reuse-window display-buffer-pop-up-window))))
 '(flycheck-posframe-warning-prefix "⚠ ")
 '(global-company-mode t)
 '(global-flycheck-mode t)
 '(package-selected-packages
   '(multi-vterm phi-search dashboard-hackernews emacs-nav sr-speedbar project-explorer dired-sidebar git-timemachine winum company-tabnine git-link vterm sublime-themes soothe-theme spacemacs-theme org-superstar flycheck-posframe ivy-posframe erblint neotree ox-twbs evil-org evil-goggles general distinguished-theme browse-kill-ring deft evil-magit auto-dim-other-buffers json-mode evil-ruby-text-objects evil-ruby-text-objects-mode evil-surround evil-mc evil-commentary linum-relative evil rubocop anzu rvm which-key yasnippet-snippets yasnippet company flycheck-popup-tip flycheck-pos-tip flycheck web-mode hungry-delete exec-path-from-shell robe projectile-rails counsel-projectile projectile magit wgrep-ag wgrep ivy-hydra counsel dashboard doom-themes all-the-icons-dired all-the-icons-ivy all-the-icons drag-stuff use-package))
 '(safe-local-variable-values
   '((projectile-project-run-cmd . "mkdir -p build; cd build; cmake ..; make run")
     (projectile-project-compilation-cmd . "mkdir -p build; cd build; cmake ..; make"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(auto-dim-other-buffers-face ((t (:background "gray20" :foreground "gray30" :slant italic))))
 '(company-preview ((t (:background "blue4" :foreground "gray30"))))
 '(company-preview-common ((t nil)))
 '(company-tooltip-mouse ((t (:background "orange1"))))
 '(cursor ((t (:background "SpringGreen3"))))
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed))))
 '(fill-column-indicator ((t (:inherit shadow :inverse-video nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1.0 :width condensed))))
 '(flycheck-info ((t (:underline (:color "gold1" :style wave)))))
 '(flycheck-posframe-face ((t (:inherit nil :background "goldenrod1" :foreground "black"))))
 '(flycheck-posframe-warning-face ((t (:inherit flycheck-posframe-face :foreground "red2"))))
 '(flycheck-warning ((t (:background "#2d2e2e" :underline (:color "red2" :style wave)))))
 '(isearch ((t (:background "DarkGoldenrod1" :foreground "Black"))))
 '(isearch-fail ((t (:background "firebrick1" :foreground "black"))))
 '(mc/cursor-bar-face ((t (:background "#FF2500" :height 0.1))))
 '(org-ellipsis ((t (:foreground "LightGoldenrod"))))
 '(org-table ((t (:foreground "gray70"))))
 '(query-replace ((t (:inherit isearch))))
 '(whitespace-line ((t (:foreground "black" :background "red" :underline t)))))

(setq gc-cons-threshold (* 2 1000 1000))
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
