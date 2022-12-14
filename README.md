# Emacs config

This config has the following dependencies:

For C/C++ buffers:
- [clangd](https://emacs-lsp.github.io/lsp-mode/page/lsp-clangd/) for LSP mode (code completion, ...).
- [CMake](https://cmake.org/download/) for CMake projects. Required to auto-build CMake projects. On Windows, it doesn't need to be in the `PATH`; it can also be installed via MSYS2 (go into the MSYS2 shell, then run `pacman -Syu cmake`).

For CMake buffers (e.g. `CMakeLists.txt` or `*.cmake` files):
- [cmake-language-server](https://emacs-lsp.github.io/lsp-mode/page/lsp-cmake/) for LSP mode (code completion, ...).

## Building Emacs with modules support for VTerm

[VTerm](https://github.com/akermu/emacs-libvterm) requires Emacs to be compiled with dynamic modules support. Without dynamic modules support, this config using the Term package instead.

## Installing all dependencies and building Emacs

### Ubuntu

```bash
## Dependencies for LSP mode (language-aware completion and help functions)
# For C++
sudo apt-get install clangd
# For CMake
sudo pip3 install cmake-language-server
# Other LSP servers
sudo npm install -g yaml-language-server vscode-langservers-extracted
# Dependencies to allow Emacs and its VTerm package to build
sudo apt-get install build-essential texinfo libx11-dev libxpm-dev libjpeg-dev libpng-dev libgif-dev libtiff-dev libncurses-dev automake autoconf libgtk2.0-dev libtool-bin

## Other dependencies
sudo apt install python # Required by visual-regexp-steroids

# Build Emacs with modules support
emacs_tar_basename=emacs-28.2
tmpdir="$(mktemp -d)"
echo "Building Emacs in ${tmpdir}"
cd "${tmpdir}" && wget https://ftp.gnu.org/gnu/emacs/${emacs_tar_basename}.tar.xz && tar -xJf ${emacs_tar_basename}.tar.xz && cd ${emacs_tar_basename}
./configure --with-modules --without-pop --with-x-toolkit=gtk2 && make -j10
```

If happy with `./src/emacs`, then install it with:

```bash
sudo make install
```

