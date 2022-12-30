# Emacs config

This config has some dependencies; see the script below. I try to go to Windows and do whatever tweaks are necessary to keep it working once in a while.

## Building Emacs with modules support for VTerm

[VTerm](https://github.com/akermu/emacs-libvterm) requires Emacs to be compiled with dynamic modules support. Without dynamic modules support, this config using the Term package instead.

## Installing all dependencies and building Emacs

### Ubuntu

```bash
## Dependencies for LSP mode (language-aware completion and help functions)
# For C++
sudo apt-get install clangd-12
# Other LSP servers
sudo npm install -g yaml-language-server vscode-langservers-extracted
# hdl-checker: For VHDL/Verilog
sudo pip3 install hdl-checker --upgrade
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

