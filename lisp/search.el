; -------------------------- Variables --------------------------------
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)

; -------------------------- Packages --------------------------------
(use-package ivy-hydra)
(use-package wgrep)
(use-package wgrep-ag)

(use-package ivy
  :diminish (ivy-mode . "")
  :init (ivy-mode 1) ; globally at startup
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 20)
  (setq ivy-count-format "%d/%d "))
(provide 'init-ivy)

(use-package counsel)
