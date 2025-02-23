# Start Emacs

A single-file starting point for your Emacs configuration.

## Install

**Step one: install Emacs**:

* Download the [latest version of
Emacs](https://www.gnu.org/software/emacs/) (version >=29 required).

**Step two: install external dependencies**:

* `git`
* `gcc` (a C compiler is required for compiling  tree-sitter grammars)

**Step three: clone Start Emacs**:

> [!Note]
> The proper installation directory for Start Emacs depends on your
> Emacs settings, if you have previously launched Emacs (see: [How
> Emacs Finds Your Init
> File](https://www.gnu.org/software/emacs/manual/html_node/emacs/Find-Init.html)).

**Mac/Linux**:

```sh
git clone https://github.com/mgmarlow/start-emacs.git ~/.emacs.d/
```

**Windows (Powershell)**:

> [!Warning]
> Windows users may prefer running Emacs via WSL, as outlined in
> [WINDOWS.md](./WINDOWS.md).

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

With Emacs running, the next step is to work your way through the
tutorial. You can open it via the keybinding: `C-h t` (ctrl + h,
followed by t). The tutorial will teach you all of the basic
navigation tools that will help you when reading the Start Emacs
configuration source in [init.el](./init.el).

> [!Note]
> While you're reading Emacs Lisp, don't forget that you can highlight
> any symbol and use `C-h o` to pull up the built-in help docs.

When you're comfortable with the basic Start Emacs configuration,
check out [EXTENDING.md](./EXTENDING.md) for a bunch of recipes that
implement other useful Emacs features.

If you want to version control your Emacs configuration ([like I
do](https://github.com/mgmarlow/dotemacs)), you will want to replace
the git artifacts from Start Emacs with your own. The simple way to do
this is to blow away the `.git` folder and re-initialize:

```sh
cd my-emacs-directory/
rm -rf .git/
git init
git commit -m "My new Emacs configuration"
```

You may also be interested in using a tool like [GNU
Stow](https://www.gnu.org/software/stow/) if your dotfile needs are
more complicated.

## Why Start Emacs?

Rolling your own Emacs configuration is a ton of fun, if a bit
daunting. Along the way you encounter several difficulty spikes:
learning the Emacs keybindings, navigating the configuration file and
help system, and finally, learning Emacs Lisp. This project is meant
to soften those difficulty spikes by bringing the default Emacs
experience a little closer to other modern editors.

Start Emacs is intentionally light. It tries not to assume too much
about your preferred working environment, or deviate too far from the
baseline Emacs experience. You'll need to learn how to hack your
configuration yourself to finely tailor Emacs to your preferences.

With that goal in mind, the configuration file provided by Start Emacs
is heavily annotated with comments. These comments explain the
reasoning behind a particular setting change and where you can learn
more about that change if you want to tweak it yourself.

Emacs improves with every release and Start Emacs is the product of
all of the hard work of the Emacs maintainers and community. The 29
release in particular added a ton of great features to Emacs that this
project takes advantage of, like tree-sitter and built-in LSP. Now is
a great time to get started with Emacs.

## Features

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

### LSP

[Eglot](https://github.com/joaotavora/eglot) is a LSP client that
ships with Emacs 29 and maintains a high-level of compatibility with
other built-in Emacs packages. The configuration from Start Emacs
plays nicely with the completion framework
[Corfu](https://elpa.gnu.org/packages/corfu.html), which provides LSP
suggestions directly in your Emacs buffer.

### Searching and navigation improvements

Start Emacs replaces the default Emacs navigation with a few tools
that do the job (arguably)
better. [Vertico](https://elpa.gnu.org/packages/vertico.html) and
[Consult](https://elpa.gnu.org/packages/consult.html) do most of the
heavy-lifting here, providing interactive completing-read suggestions
when working in the minibuffer or searching
buffers. [Orderless](https://elpa.gnu.org/packages/orderless.html)
makes both of these tools better with space-delimited matching,
meaning your query does not need to exactly match the buffer text to
find results.

## Kudos

- [kickstart.nvim](https://github.com/nvim-lua/kickstart.nvim)
- [Crafted Emacs](https://github.com/SystemCrafters/crafted-emacs)
- [Technomancy's Better Defaults](https://git.sr.ht/~technomancy/better-defaults)
