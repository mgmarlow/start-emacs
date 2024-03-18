;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:

;; A starting point for your Emacs configuration.
;;
;; Start Emacs is not a distribution, but a set of defaults and
;; recommended packages to help get you started with your own
;; configuration.  The goal is a readable, discoverable configuration
;; that you can learn to modify to suit your needs.
;;
;; I recommend starting with the Emacs tutorial: "C-h t".  After
;; completing it, return to this file and read through the various
;; comments and customizations to learn more about customizing your
;; Emacs experience.  You can use "C-h o" to read the built-in
;; documentation on any symbol that you don't recognize.

;;; Code:

;; TODO: Un-comment the following code and swap "Hack 12" with your
;; preferred font family and size.

;; (defun my--configure-fonts ()
;;   "Configure your default font family and size."
;;   (let ((base "Hack 12"))
;;     (custom-set-faces
;;      `(default ((t :font ,base)))
;;      `(fixed-pitch ((t :inherit (default))))
;;      `(default ((t :inherit (default)))))))
;;
;; (add-hook 'emacs-startup-hook #'my--configure-fonts)

;;; Setting tweaks:

;; No auto-resizing of frames.
(setq frame-inhibit-implied-resize t)

;; Bell sounds from my text editor are no fun.
(setq ring-bell-function #'ignore)

;; Auto-revert dired (the directory editor) when revisiting
;; directories, since they may have changed underneath.
(setq dired-auto-revert-buffer t)

;; Tab does quite a bit of stuff in Emacs, so it's helpful to have it
;; attempt completions when it's not doing something else.
(setq tab-always-indent 'complete)

;; Scroll Eshell to the bottom when new output is added.
(setq eshell-scroll-to-bottom-on-input 'this)

;; Make certain mouse commands more intuitive.
(setq mouse-yank-at-point t)

;; The safest, but slowest method for creating backups.
(setq backup-by-copying t)

;; Avoid cluttering up project directories with backup files by saving
;; them to the same place.
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;; "M-x customize" can tweak Emacs Lisp variables via a graphical
;; interface, but those tweaks are normally saved directly to your
;; hand-edited `init.el'.  I like a clean `init.el', so I write those
;; customizations to a different file instead.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Emacs Lisp files can be byte-compiled into `.elc' files, which run
;; faster.  By default Emacs prefers `.elc' to `.el' in all cases,
;; causing occasional annoyances if you make a change to an Emacs Lisp
;; file but forget to byte-compile it.  `load-prefer-newer' always
;; prefers the last-edited file, preventing this problem.
(setq load-prefer-newer t)

;; Automatically retain a final newline when saving a file.
(setq require-final-newline t)

;; Avoid tabs when possible.
(setq-default indent-tabs-mode nil)

;; Turn off the default Emacs UI elements, drill those keybindings
;; instead!  You can use "C-h C-q" to pull up a quick-reference sheet
;; that will help you remember the basics.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Display line numbers in programming language modes.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Respect color escape sequences.  Particularly useful for "M-x
;; compile" with modern programming languages that use colors to
;; convey information.
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;; Initialize package.el for loading third-party packages.  Also set
;; up package.el to accept packages from the MELPA package archives,
;; the largest package repository for Emacs.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

;;; Packages:

;; All of the package configuration in this file uses `use-package', a
;; macro that makes it easier to load and configure packages, both
;; third-party and built-in.  To learn more, access the `use-package'
;; manual via "C-h i m use-package".

;; `repeat-mode' allows you to trigger certain repeat commands by
;; typing the final character without the control modifier.  For
;; example, if you're switching between multiple buffers, you can use
;; "C-x o o o" to swap three times, instead of repeating the full
;; sequence: "C-x o", "C-x o", "C-x o".
(use-package repeat
  :config
  (repeat-mode))

;; When there are conflicting names in your buffer-selector ("C-x b"),
;; `uniquify' disambiguates them by prepending the directory.
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

;; A beautiful set of themes by Protesilaos.  You might try calling
;; `ef-themes-select' to find one that speaks to you.
(use-package ef-themes
  :ensure t
  :config
  (ef-themes-select 'ef-trio-dark))

;; When using Mac OSX or Linux, you likely want your shell environment
;; path available to Emacs so that Emacs can locate your custom
;; utilities.  This is helpful if you use your PATH variable for LSP
;; servers, CLI tools, language environments etc.  Windows users can
;; effectively ignore this package.
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Emacs ships with several completion engines, but none are as
;; flexible as Vertico.  This is the secret sauce that powers the
;; Emacs "Command Palette", enabling tab-completion when using "M-x
;; command", `project-find-file', and other minibuffer commands.
(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

;; Persist minibuffer history over Emacs restarts.  Vertico uses this
;; to sort based on history.
(use-package savehist
  :init
  (savehist-mode))

;; Marginalia adds, well, marginalia to the Emacs minibuffer,
;; extending Vertico with a ton of rich information.
(use-package marginalia
  :ensure t
  :after vertico
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the
  ;; binding available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  ;; Marginalia must be activated in the :init section of use-package
  ;; such that the mode gets enabled right away.  Note that this
  ;; forces loading the package.
  (marginalia-mode))

;; When the minibuffer is open and you're searching for some text,
;; Emacs can be very persnickety about the order in which you type.
;; Orderless laxes this behavior so the search is "fuzzier"; you'll
;; see results more often even if you type things in the wrong order.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Where Vertico is a completion engine for your Emacs minibuffer,
;; Corfu is a completion engine for your source code.  This package
;; takes the data from things like LSP or Dabbrev and puts those
;; results in a convenient autocomplete.
(use-package corfu
  :ensure t
  ;; Recommended: Enable Corfu globally.  This is recommended since
  ;; Dabbrev can be used globally (M-/).  See also the customization
  ;; variable `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  ;; You might want to configure these variables to suit your
  ;; preferences.  It's recommended to have some amount of delay if
  ;; you use Corfu with Eglot, otherwise editing performance could
  ;; suffer.
  (corfu-auto-delay 0.25)
  (corfu-auto-prefix 2))

;; Emacs includes Tree-sitter support as of version 29, but does not
;; bundle Tree-sitter grammars via the usual installation methods.
;; That means that if you want to use a Tree-sitter major mode, you
;; must first install the respective language grammar.  `treesit-auto'
;; is a handy package that manages this extra step for you, prompting
;; the installation of Tree-sitter grammars when necessary.
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; If LSP is your jam, Eglot is your fix. Use the `eglot' command in a
;; programming major mode to boot up a language server and connect it
;; to Emacs.  Eglot does not install language servers for you, so you
;; must have the language server installed for a particular language
;; (e.g. rust-analyzer for Rust) before `eglot' will work its magic.
(use-package eglot
  ;; Uncomment these dotted pairs to automatically activate Eglot when
  ;; that major mode is active.
  ;;
  ;; :hook ((rust-ts-mode . eglot-ensure)
  ;;        (go-ts-mode . eglot-ensure))
  :bind (("C-c ." . eglot-code-action-quickfix)))

;; Add breadcrumbs to the top of buffers.
(use-package breadcrumb
  :ensure t
  :config
  (breadcrumb-mode))

;;; Custom lisp modules:

;; Add `./lisp/' to the Emacs `load-path' so Emacs can find your
;; custom Emacs Lisp code.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Automatically refresh `package-archive-contents' at most once a
;; week when calling `package-install'.
(use-package my-package-refresh
  :config
  (setq my-package-automatic-refresh-threshold (* 7 24)))

;;; init.el ends here
