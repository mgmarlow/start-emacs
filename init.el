;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun se--configure-fonts ()
  "Configure your default font family and size."
  (let ((base "Hack 11"))
    (custom-set-faces
     `(default ((t :font ,base)))
     `(fixed-pitch ((t :inherit (default))))
     `(default ((t :inherit (default)))))))

(add-hook 'emacs-startup-hook #'se--configure-fonts)

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

;; Turn off the default Emacs UI elements. Drill those keybindings
;; instead! You can use "C-h C-q" to pull up a quick-reference sheet.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq-default indent-tabs-mode nil)

;; Display line numbers in programming language modes.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(savehist-mode t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Initialize package.el for loading third-party packages.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

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


;;; init.el ends here
