# Emacs config

This config has the following dependencies:

For C/C++ buffers:
- [clangd](https://emacs-lsp.github.io/lsp-mode/page/lsp-clangd/) for LSP mode (code completion, ...).
- [CMake](https://cmake.org/download/) for CMake projects. Required to auto-build CMake projects. On Windows, it doesn't need to be in the `PATH`; it can also be installed via MSYS2 (go into the MSYS2 shell, then run `pacman -Syu cmake`).

For CMake buffers (e.g. `CMakeLists.txt` or `*.cmake` files):
- [cmake-language-server](https://emacs-lsp.github.io/lsp-mode/page/lsp-cmake/) for LSP mode (code completion, ...).

