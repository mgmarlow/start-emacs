# Windows guide

Emacs on Windows can be a finicky beast, no thanks in part to the
necessity of a C compiler to install tree-sitter grammars. There are a
few different schools of thought for setting up a Windows dev
environment that has the necessary tools. You can set up Visual Studio
(though you may still be missing the occasional program), install a
Linux-compatibility package like [Cygwin](https://cygwin.com/) or
[w64devkit](https://github.com/skeeto/w64devkit), or you can use
[Windows Subsystem for Linux
(WSL)](https://learn.microsoft.com/en-us/windows/wsl/install).

In my opinion, your best bet on using Windows with Start Emacs is to
use WSL. Follow the steps in this guide [How to install Linux on
Windows with
WSL](https://learn.microsoft.com/en-us/windows/wsl/install) and pick a
Linux distribution that has Emacs 29 readily available.

You may alternatively compile Emacs from source using WSL, which is
outlined below.

## Example building from source on WSL

Install the Debian subsystem:

```
wsl --install Debian
```

Open up your Debian terminal and install required dependencies (git,
gcc, make, gccjit, etc.):

```
apt-get update
apt-get install git build-essentials texinfo autoconf \
  libx11-dev libxpm-dev libjpeg-dev libpng-dev \
  libgif-dev libtiff-dev libncurses-dev gnutls-dev \
  libgtk-3-dev libgccjit-12-dev libtree-sitter-dev
```

Clone Emacs and begin the installation process (this could take some
time):

```
git clone -b master git://git.sv.gnu.org/emacs.git
cd emacs

# The steps that follow are from emacs/INSTALL

./autogen.sh

./configure --with-native-compilation --with-tree-sitter

make

# Verify that Emacs built properly
src/emacs -Q

# Only run this if the previous command launches Emacs correctly
make install
```

If Emacs does not build/start properly, you may be missing a
dependency. You can try again after `make clean`. The command `make
bootstrap` may also help iron out some issues.

