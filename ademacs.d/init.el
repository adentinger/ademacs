;;; Much of the config taken from
;;; https://github.com/daviwil/emacs-from-scratch/blob/master/init.el

;; Create dir specific to flag files that indicate whether some action has been
;; performed already
(setq ade/flag-dir (concat user-emacs-directory "/ade-flags"))
(unless (file-directory-p ade/flag-dir) (mkdir ade/flag-dir t))

(setq inhibit-startup-message t)

;; Disable visible scrollbar
(scroll-bar-mode -1)
;; Disable the toolbar
(tool-bar-mode -1)
;; Disable tooltips
(tooltip-mode -1)
;; Give some breathing room
(set-fringe-mode 10)
;; Disable menu bar
(menu-bar-mode -1)

(setq visible-bell t)

;; Load temporary theme while the real theme is being downloaded
;; or loaded. I don't like to be blinded!
(load-theme 'wombat)

;; Start emacs maximized
(add-hook 'emacs-startup-hook 'toggle-frame-fullscreen)

;; Font size
(set-face-attribute 'default nil :height 190 :font "Consolas" :weight 'normal)

;; Don't add the annoying Custom line things in this file; add
;; them to a separate file
(setq custom-file (concat user-emacs-directory "/custom.el"))

(column-number-mode)
(global-display-line-numbers-mode t)
(set-default-coding-systems 'utf-8)

;; Don't show line number in certain modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Need package stuff for use-package!
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
;; Put the packages inside the current emacs dir, not the default one.
(setq package-user-dir (concat user-emacs-directory "/packages"))
(package-initialize)
;; Only do that on initial package setup
(unless package-archive-contents (package-refresh-contents))
;; For non-Linux platforms apparently...
(unless (package-installed-p 'use-package) (package-install 'use-package))

(require 'use-package)
;; Always ensure packages loaded by use-package is downloaded
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

;; Counsel package is used by ivy-rich; ivy-rich doesn't seem to work without it
(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 ("C-M-j" . counsel-switch-buffer)
	 :map minibuffer-local-map ("C-r" . 'counsel-minibuffer-history))
  :config (setq ivy-initial-inputs-alist nil) ; Don't start searches with "^"
)

(use-package ivy-rich
  :init (ivy-rich-mode 1))

(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 2)))

(use-package doom-themes
  :init (load-theme 'doom-dark+ t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; To show available keybindings when using a hierarchical keybinding
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))

(use-package helpful
  ;; :custom section is to avoid adding variables in the annoying custom.el file
  ;; that we setup earlier
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ;; remap thing apparently returns the string representing the keybinding that
  ;; binds given function
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))
