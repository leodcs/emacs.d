; -------------------------- Variables --------------------------------
(setq enable-recursive-minibuffers t
      ivy-use-virtual-buffers t
      counsel-ag-base-command "ag --hidden -ignore .git --ignore vendor --vimgrep %s")

; -------------------------- Packages --------------------------------

(use-package ivy
  :init (ivy-mode 1) ; globally at startup
  :config
  (setq ivy-use-virtual-buffers t
        ivy-height 20
        ivy-count-format "%d/%d "
        ivy-initial-inputs-alist nil)

  (use-package ivy-hydra)
  (use-package wgrep)
  (use-package wgrep-ag)
  (use-package counsel)
  (use-package ivy-posframe
    :config
    (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center))
          ivy-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))
    (ivy-posframe-mode 1)))
