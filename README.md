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

```ps1
git clone https://github.com/mgmarlow/start-emacs.git $env:USERPROFILE\AppData\Roaming\.emacs.d\
```

**Post installation**:

You're all set! Open up Emacs and you should see a bunch of stuff
initializing, which might take a few seconds. First-time startup
always takes a little longer since Emacs must refresh it's package
archive, download packages used in Start Emacs, and get everything
initialized.

## Why Start Emacs?

Start Emacs is a set of default settings and recommended packages
paired with some extensive documentation to help new Emacs users get
started with their own configuration. The goal is simplicity and
readability while providing a strong first-time user experience.

In contrast to distributions like
[Doom](https://github.com/doomemacs/doomemacs), Start Emacs is
primarily motivated to help new users learn how to configure Emacs
themselves.

## Next steps

Check out the [extending](./extending.md) guide for some suggested
configurations, like Vim emulation.

## Kudos

- [kickstart.nvim](https://github.com/nvim-lua/kickstart.nvim)
- [Crafted Emacs](https://github.com/SystemCrafters/crafted-emacs)
- [Technomancy's Better Defaults](https://git.sr.ht/~technomancy/better-defaults)
