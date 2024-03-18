# Extending Start Emacs

Some suggested recipes for extending Start Emacs.

## Git UI

[Magit](https://magit.vc/) is an absolute legend when it comes to Git
user interfaces. It's also incredibly easy to setup:

```elisp
(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))
```

You can start it by navigating to a buffer inside of a Git repository
and using `C-c g`.

## Vim emulation

[Evil mode](https://github.com/emacs-evil/evil) is the de facto Vim
emulation layer for Emacs.

```elisp
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))
```

Those who prefer a key other than Esc for returning to normal mode can
add [evil-escape](https://github.com/syl20bnr/evil-escape):

```elisp
(use-package evil-escape
  :ensure t
  :after evil
  :config
  (setq evil-escape-key-sequence "jj")
  (setq evil-escape-delay 0.2)
  ;; Prevent "jj" from escaping any mode other than insert-mode.
  (setq evil-escape-inhibit-functions
        (list (lambda () (not (evil-insert-state-p)))))
  (evil-escape-mode))
```

## Org Mode note-taking

[Denote](https://protesilaos.com/emacs/denote) is a feature-rich, yet
simple package for note-taking that supports [Org
Mode](https://orgmode.org/).

```elisp
(use-package denote
  :ensure t
  :config
  (setq denote-directory "~/path/to/notes/"))
```
