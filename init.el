;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(defun my--configure-fonts ()
  "Configure your default font family and size."
  (let ((base "Hack 11"))
    (custom-set-faces
     `(default ((t :font ,base)))
     `(fixed-pitch ((t :inherit (default))))
     `(default ((t :inherit (default)))))))

(add-hook 'emacs-startup-hook #'my--configure-fonts)

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

;; Respect color escape sequences
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;; Initialize package.el for loading third-party packages.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;;; Packages:

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

(use-package ef-themes
  :ensure t
  :config
  (ef-themes-select 'ef-trio-dark))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package corfu
  :ensure t
  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package eglot
  :hook ((rust-ts-mode . eglot-ensure)))

;;; Custom lisp:

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(use-package my-package-refresh
  :config
  (setq my-package-automatic-refresh-threshold (* 7 24)))

;;; init.el ends here
