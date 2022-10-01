; Much of the config taken from
; https://github.com/daviwil/emacs-from-scratch/blob/master/init.el

(setq inhibit-startup-message t)

; Disable visible scrollbar
(scroll-bar-mode -1)
; Disable the toolbar
(tool-bar-mode -1)
; Disable tooltips
(tooltip-mode -1)
; Give some breathing room
(set-fringe-mode 10)
; Disable menu bar
(menu-bar-mode -1)

(setq visible-bell t)

(load-theme 'wombat)

; Need package stuff for use-package!
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
; Only do that on initial package setupl
(unless package-archive-contents (package-refresh-contents))
; For non-Linux platforms apparently...
(unless (package-installed-p 'use-package) (package-install 'use-package))

(require 'use-package)
; Always ensure packages loaded by use-package is downloaded
(setq use-package-always-ensure t)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
    :map ivy-minibuffer-map
    ("TAB" . ivy-alt-done)
    ("C-l" . ivy-alt-done)
    ("C-j" . ivy-next-line)
    ("C-k" . ivy-previous-line)
    :map ivy-switch-buffer-map
    ("C-k" . ivy-previous-line)
    ("C-l" . ivy-done)
    ("C-d" . ivy-switch-buffer-kill)
    :map ivy-reverse-i-search-map
    ("C-k" . ivy-previous-line)
    ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))
