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

;; Font && font size
(if (eq system-type 'windows-nt)
  (progn (set-face-attribute 'default nil :height 190 :weight 'normal :font "Consolas"))
  (progn (set-face-attribute 'default nil :height 190 :weight 'normal)))

;; Don't add the annoying Custom line things in this file; add
;; them to a separate file
(setq custom-file (concat user-emacs-directory "/custom.el"))

(column-number-mode)
(global-display-line-numbers-mode t)
(set-default-coding-systems 'utf-8)
(setq-default tab-width 4)

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
  ;; 'remap' thing apparently returns the string representing the keybinding that
  ;; binds given function
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config (evil-mode 1)

  ;; Hitting "." shouldn't move the cursor (normally repeats last command)
  (setq evil-repeat-move-cursor nil)
  ;; Exiting insert mode shouldn't move the cursor one char back
  (setq evil-move-cursor-back nil)
  ;; evil-forward/backward-char can move to the EOL character. Like, seriously.
  (setq evil-move-beyond-eol t)
  ;; evil-forward/backward-char can move accross lines
  (setq evil-cross-lines t)

  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-f") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "M-x") (lambda () (interactive) (scroll-down 1)))
  (define-key evil-insert-state-map (kbd "M-c") (lambda () (interactive) (scroll-up 1)))

  (define-key evil-normal-state-map (kbd "C-f") 'evil-normal-state)
  (define-key evil-normal-state-map (kbd "M-x") (lambda () (interactive) (scroll-down 1)))
  (define-key evil-normal-state-map (kbd "M-c") (lambda () (interactive) (scroll-up 1)))
  (define-key evil-normal-state-map (kbd "a") 'evil-backward-char)
  (define-key evil-normal-state-map (kbd "f") 'evil-forward-char)
  (define-key evil-normal-state-map (kbd "s") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd "d") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-insert)
  (define-key evil-normal-state-map (kbd "K") 'evil-insert-line)
  (define-key evil-normal-state-map (kbd "l") 'evil-append)
  (define-key evil-normal-state-map (kbd "L") 'evil-append-line)
  (define-key evil-normal-state-map (kbd "w") 'evil-backward-word-begin)
  (define-key evil-normal-state-map (kbd "e") 'evil-forward-word-end)
  (define-key evil-normal-state-map (kbd "W") 'evil-backward-WORD-begin)
  (define-key evil-normal-state-map (kbd "E") 'evil-forward-WORD-end)
  (define-key evil-normal-state-map (kbd "q") 'evil-beginning-of-visual-line)
  (define-key evil-normal-state-map (kbd "r") 'evil-end-of-visual-line)
  (define-key evil-normal-state-map (kbd "Q") 'beginning-of-buffer)
  (define-key evil-normal-state-map (kbd "R") 'end-of-buffer)
  (define-key evil-normal-state-map (kbd "x") 'scroll-down)
  (define-key evil-normal-state-map (kbd "c") 'scroll-up)
  (define-key evil-normal-state-map (kbd "h") 'evil-visual-char)
  (define-key evil-normal-state-map (kbd "H") 'evil-visual-line)
  (define-key evil-normal-state-map (kbd "M-h") 'evil-visual-block)

  (define-key evil-visual-state-map (kbd "C-f") 'evil-normal-state)
  (define-key evil-visual-state-map (kbd "M-x") (lambda () (interactive) (scroll-down 1)))
  (define-key evil-visual-state-map (kbd "M-c") (lambda () (interactive) (scroll-up 1)))
  (define-key evil-visual-state-map (kbd "a") 'evil-backward-char)
  (define-key evil-visual-state-map (kbd "f") 'evil-forward-char)
  (define-key evil-visual-state-map (kbd "s") 'evil-previous-visual-line)
  (define-key evil-visual-state-map (kbd "d") 'evil-next-visual-line)
  (define-key evil-visual-state-map (kbd "k") 'evil-insert)
  (define-key evil-visual-state-map (kbd "K") 'evil-insert-line)
  (define-key evil-visual-state-map (kbd "l") 'evil-append)
  (define-key evil-visual-state-map (kbd "L") 'evil-append-line)
  (define-key evil-visual-state-map (kbd "w") 'evil-backward-word-begin)
  (define-key evil-visual-state-map (kbd "e") 'evil-forward-word-end)
  (define-key evil-visual-state-map (kbd "W") 'evil-backward-WORD-begin)
  (define-key evil-visual-state-map (kbd "E") 'evil-forward-WORD-end)
  (define-key evil-visual-state-map (kbd "q") 'evil-beginning-of-visual-line)
  (define-key evil-visual-state-map (kbd "r") 'evil-end-of-visual-line)
  (define-key evil-visual-state-map (kbd "Q") 'beginning-of-buffer)
  (define-key evil-visual-state-map (kbd "R") 'end-of-buffer)
  (define-key evil-visual-state-map (kbd "x") 'scroll-down)
  (define-key evil-visual-state-map (kbd "c") 'scroll-up)
  (define-key evil-visual-state-map (kbd "h") 'evil-visual-char)
  (define-key evil-visual-state-map (kbd "H") 'evil-visual-line)
  (define-key evil-visual-state-map (kbd "M-h") 'evil-visual-block)

  (define-key evil-motion-state-map (kbd "C-f") 'evil-normal-state)
  (define-key evil-motion-state-map (kbd "M-x") (lambda () (interactive) (scroll-down 1)))
  (define-key evil-motion-state-map (kbd "M-c") (lambda () (interactive) (scroll-up 1)))
  (define-key evil-motion-state-map (kbd "a") 'evil-backward-char)
  (define-key evil-motion-state-map (kbd "f") 'evil-forward-char)
  (define-key evil-motion-state-map (kbd "s") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "d") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "k") 'evil-insert)
  (define-key evil-motion-state-map (kbd "K") 'evil-insert-line)
  (define-key evil-motion-state-map (kbd "l") 'evil-append)
  (define-key evil-motion-state-map (kbd "L") 'evil-append-line)
  (define-key evil-motion-state-map (kbd "w") 'evil-backward-word-begin)
  (define-key evil-motion-state-map (kbd "e") 'evil-forward-word-end)
  (define-key evil-motion-state-map (kbd "W") 'evil-backward-WORD-begin)
  (define-key evil-motion-state-map (kbd "E") 'evil-forward-WORD-end)
  (define-key evil-motion-state-map (kbd "q") 'evil-beginning-of-visual-line)
  (define-key evil-motion-state-map (kbd "r") 'evil-end-of-visual-line)
  (define-key evil-motion-state-map (kbd "Q") 'beginning-of-buffer)
  (define-key evil-motion-state-map (kbd "R") 'end-of-buffer)
  (define-key evil-motion-state-map (kbd "x") 'scroll-down)
  (define-key evil-motion-state-map (kbd "c") 'scroll-up)
  (define-key evil-motion-state-map (kbd "h") 'evil-visual-char)
  (define-key evil-motion-state-map (kbd "H") 'evil-visual-line)
  (define-key evil-motion-state-map (kbd "M-h") 'evil-visual-block))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

