# Start Emacs

A starter project for your Emacs configuration in a single command.

## Install

First, download the [latest version of
Emacs](https://www.gnu.org/software/emacs/) (version 29+ required).

Then, run one of the following installation commands to clone Start
Emacs into your Emacs configuration directory.

> Note: the Start Emacs installation directory depends on your Emacs
> value of `user-emacs-directory`, which may be different than the one
> below. If you open up Emacs and don't see any changes after
> following the install step, try invoking `M-: user-emacs-directory`
> to discover where Start Emacs should be cloned.

**Mac/Linux**:

```sh
git clone https://github.com/mgmarlow/start-emacs.git "${XDG_CONFIG_HOME:-$HOME/.config}"/emacs
```

**Windows (Powershell)**:

```
git clone https://github.com/mgmarlow/start-emacs.git $env:USERPROFILE\AppData\Roaming\.emacs.d\
```

**Post installation**:

You're all set! Open up Emacs and you should see a bunch of stuff
initializing, which might take a few seconds. First-time startup
always takes a little longer since Emacs must refresh it's package
archive, download packages used in Start Emacs, and get everything
initialized.

## Next steps

After cloning the project, booting up Emacs, and working through the
[Emacs tutorial](https://www.gnu.org/software/emacs/tour/), read
through the code in [init.el](./init.el) and learn what everything
does. You can use `C-h o` to open up the built-in help docs to learn
more about specific variables, functions, or macros.

[Extending Start Emacs](./extending.md) offers a handful of recipes
that explore other useful features, like Vim emulation or Common
Lisp editing.

## Why Start Emacs?

Start Emacs is a set of default settings and recommended packages
paired with some extensive documentation to help new Emacs users get
started with their own configuration. The goal is simplicity and
readability while providing a strong first-time user experience.

In contrast to distributions like
[Doom](https://github.com/doomemacs/doomemacs), Start Emacs is
primarily motivated with helping new users learn how to configure
Emacs themselves.

### Tree-sitter

[Tree-sitter](https://tree-sitter.github.io/tree-sitter/) is a fancy
tool that parses source code into syntax trees that are used for
syntax highlighting (as opposed to using regular expressions, like
classic Emacs modes). Start Emacs prefers tree-sitter major modes
to their non-tree-sitter counterparts.

Although Emacs 29 ships with the tools necessary for enabling
tree-sitter, it does not bundle the programming language grammars
actually used for parsing. Those grammars must be installed
separately. To work around this, Start Emacs uses a great little
library, [treesit-auto](https://github.com/renzmann/treesit-auto),
that prompts for grammar installation if one is available for the
current buffer. This will kick in automatically for any programming
language that treesit-auto recognizes.

## Kudos

- [kickstart.nvim](https://github.com/nvim-lua/kickstart.nvim)
- [Crafted Emacs](https://github.com/SystemCrafters/crafted-emacs)
- [Technomancy's Better Defaults](https://git.sr.ht/~technomancy/better-defaults)
- [Andrey Listopadov's Blog](https://andreyor.st/posts/2022-07-15-refresh-package-contents-automatically/)
