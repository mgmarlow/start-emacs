(setq tab-always-indent 'complete
      inhibit-startup-screen t
      dired-auto-revert-buffer t
      eshell-scroll-to-bottom-on-input 'this
      mouse-yank-at-point t
      require-final-newline t
      load-prefer-newer t
      backup-by-copying t
      frame-inhibit-implied-resize t
      ring-bell-function #'ignore
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq-default indent-tabs-mode nil)

(savehist-mode t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(use-package ef-themes
  :ensure t
  :config
  (ef-themes-select 'ef-trio-dark))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Respect color escape sequences
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
