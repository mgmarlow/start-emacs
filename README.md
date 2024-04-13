# Start Emacs

A starter project for your Emacs configuration in a single command.

## Install

**Install Emacs 29+**:

* Download the [latest version of
Emacs](https://www.gnu.org/software/emacs/) (version 29+ required).

**Install external dependencies**:

* `git`
* `gcc` (a C compiler is required for compiling  tree-sitter grammars)

**Install Start Emacs**:

> [!Note]
> Your installation directory is the `user-emacs-directory` variable
> in Emacs. If you open up Emacs and don't see any changes after
> cloning, try invoking `M-: user-emacs-directory` in Emacs to
> discover the correct location.

**Mac/Linux**:

```sh
git clone https://github.com/mgmarlow/start-emacs.git "${XDG_CONFIG_HOME:-$HOME/.config}"/emacs
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

There's a lot of joy to be had from throwing together your own Emacs
configuration, but it can be hard to get started. A lot of that
initial difficulty comes from the obtuseness of Emacs when viewed as a
modern text editor; it just doesn't pack the same default experience
that you may be used to.

The goal of Start Emacs is to set up some of those modern defaults
with comments that explain what is being changed. It intentionally
doesn't configure everything for you out-of-the-box, just the baseline
features that might be expected from other editors like VS Code or
Helix. Ultimately, your configuration should be your own and the
comments are there to help you learn to be self-sufficient.

With Emacs 29 there's actually not that much configuration that you
need (in my opinion) to have a great text editor, thanks in large part
to the Emacs maintainers and ecosystem changes around LSP and
tree-sitter. However, knowing what to change and how to do it comes
with experience. Start Emacs can help bridge that gap.

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
- [Andrey Listopadov's Blog](https://andreyor.st/posts/2022-07-15-refresh-package-contents-automatically/)
