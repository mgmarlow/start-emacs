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

## Writing prose

### Markdown

Look no further than
[markdown-mode](https://jblevins.org/projects/markdown-mode/):

```elisp
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))
```

### Note-taking and Org Mode

[Org Mode](https://orgmode.org/) is one of the classic killer-features
of Emacs, offering a markup format that is like Markdown on
steroids. It's also built into Emacs so you don't need an extra
package to start using it. You may, however, enjoy extending Org Mode
with a few packages to make the experience a little nicer.

One package that I recommend everyone to try out is [Org
Modern](https://github.com/minad/org-modern), which cleans up the Org
Mode UI with a fresher interface.

```elisp
(use-package org-modern
  :ensure t
  :after org
  :config
  (global-org-modern-mode))
```

For those who want a more structure to their note-taking experience,
[Denote](https://protesilaos.com/emacs/denote) is one such
Org-compatible package that handles your Org file naming schemes,
organization, and linking.

```elisp
(use-package denote
  :ensure t
  :config
  (setq denote-directory "~/path/to/notes/"))
```


## Common Lisp editing

Many people start using Emacs because they're interested in learning
Common Lisp. I recommend installing [SBCL](https://www.sbcl.org/) and
working through Peter Siebel's _[Practical Common
Lisp](https://gigamonkeys.com/book/)_.

You can configure a Common Lisp REPL with
[Sly](https://github.com/joaotavora/sly). The only configuration
necessary is telling Sly where to find your Lisp interpreter:

```elisp
(use-package sly
  :ensure t
  :custom
  (inferior-lisp-program "/path/to/sbcl"))
```

You might also take a look at [Paredit](https://paredit.org/) to help
make working with parentheses more intuitive.
