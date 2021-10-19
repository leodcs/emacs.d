(setq-default gc-cons-threshold 200000000)

(setq delete-old-versions -1  ; delete excess backups silently
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
      frame-resize-pixelwise t)
(defalias 'yes-or-no-p 'y-or-n-p)
(menu-bar-mode -1) ; no need for the menu bars - we've got key combos for that!
(toggle-scroll-bar t)
(tool-bar-mode -1)
(winner-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(save-place-mode 1)
(delete-selection-mode 1)
(global-set-key [escape] 'keyboard-escape-quit)
(global-set-key (kbd "M-o") 'other-window)
(toggle-frame-maximized)

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

;; Use use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents) ; update archives
  (package-install 'use-package)) ; grab the newest use-package
(require 'use-package)
(setq use-package-always-ensure t)

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
(load "~/.emacs.d/lisp/treemacs.el")
(load "~/.emacs.d/lisp/vterm.el")
(load "~/.emacs.d/lisp/multiple-cursors.el")
(load "~/.emacs.d/lisp/python.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-dabbrev-downcase 0)
 '(company-idle-delay 0)
 '(custom-safe-themes
   '("7922b14d8971cce37ddb5e487dbc18da5444c47f766178e5a4e72f90437c0711" "9efb2d10bfb38fe7cd4586afb3e644d082cbcdb7435f3d1e8dd9413cbe5e61fc" "196df8815910c1a3422b5f7c1f45a72edfa851f6a1d672b7b727d9551bb7c7ba" "043c8375cad0cf1d5c42f5d85cbed601075caf09594da04a74712510e9437d2b" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "5036346b7b232c57f76e8fb72a9c0558174f87760113546d3a9838130f1cdb74" "57bd93e7dc5fbb5d8d27697185b753f8563fe0db5db245592bab55a8680fdd8c" "8d7684de9abb5a770fbfd72a14506d6b4add9a7d30942c6285f020d41d76e0fa" "a3b6a3708c6692674196266aad1cb19188a6da7b4f961e1369a68f06577afa16" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "41098e2f8fa67dc51bbe89cce4fb7109f53a164e3a92356964c72f76d068587e" "17d158ec27961156ee222055f1089364b7fd38cf838304a1f9fe8b1cd561c188" "dd4628d6c2d1f84ad7908c859797b24cc6239dfe7d71b3363ccdd2b88963f336" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" default))
 '(display-buffer-alist
   '(("*Help*"
      (leo/display-buffer-at-bottom))
     ("RuboCop"
      (leo/display-buffer-at-bottom))
     ("*vterm -"
      (leo/display-buffer-at-bottom))
     ("magit: "
      (display-buffer-reuse-window display-buffer-same-window))
     ("magit"
      (display-buffer-reuse-window display-buffer-pop-up-window))))
 '(ivy-ignore-buffers '("\\` " "\\`\\*tramp/" "*RuboCop"))
 '(neo-hidden-regexp-list '("\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.o$" "\\.DS_Store"))
 '(package-selected-packages
   '(auto-dictionary flyspell-correct-helm flyspell-correct-avy-menu ace-jump-mode flymake-python-pyflakes symbol-overlay auto-highlight-symbol idle-highlight-mode py-autopep8 zerodark-theme melancholy-theme spaceline ewal-evil-cursors treemacs-persp treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil treemacs elpy simpleclip google-c-style evil-vimish-fold highlight-indent-guides flycheck-flow prettier-js vimish-fold vs-dark-theme afternoon-theme gotham-theme alect-themes alect-theme material-theme monokai-theme gruvbox-theme discreet-theme ewal-spacemacs-themes rjsx-mode zoom rbenv dumb-jump evil-escape base16-theme color-theme-sanityinc-tomorrow solarized-theme zenburn-theme zenburn-emacs vscode-dark-plus-theme vscdark-theme vsdark-theme objed scratch esup discover-my-major makey multi-vterm phi-search dashboard-hackernews emacs-nav sr-speedbar project-explorer dired-sidebar git-timemachine winum company-tabnine git-link vterm sublime-themes soothe-theme spacemacs-theme org-superstar flycheck-posframe ivy-posframe erblint neotree ox-twbs evil-org evil-goggles general distinguished-theme browse-kill-ring deft evil-magit auto-dim-other-buffers json-mode evil-ruby-text-objects evil-ruby-text-objects-mode evil-surround evil-commentary evil rubocop anzu rvm which-key yasnippet-snippets yasnippet company flycheck-popup-tip flycheck-pos-tip flycheck web-mode hungry-delete exec-path-from-shell robe projectile-rails counsel-projectile projectile magit wgrep-ag wgrep ivy-hydra counsel dashboard doom-themes all-the-icons-dired all-the-icons-ivy all-the-icons drag-stuff use-package))
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
 '(query-replace ((t (:inherit isearch))))
 '(symbol-overlay-default-face ((t (:background "blue" :foreground "gray100")))))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
