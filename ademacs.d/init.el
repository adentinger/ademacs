;;; Much of the config taken from
;;; https://github.com/daviwil/emacs-from-scratch/blob/master/init.el

;; Load temporary theme while the real theme is being downloaded
;; or loaded. I don't like to be blinded!
(load-theme 'wombat)

(if (eq system-type 'windows-nt)
	(setq ade/win/usr-dir (replace-regexp-in-string "\\\\" "/" (getenv "userprofile"))))

(if (not (eq system-type 'windows-nt))
	(progn (setq ade/git-prj-dir "~/Git"))
  (progn (setq ade/git-prj-dir (concat ade/win/usr-dir "/Git"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Config variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ade/prj-dirs (list ade/git-prj-dir "~")
  "List of paths that Projectile will search for projects in.")
(defconst ade/terminal-prompt-regexp "^[^#$%>\n]*[#$%>] *"
  "Regular expression that matches a prompt in a terminal.")

;;; KEY PREFIXES
(defconst ade/cmd-pfx-plain "C-SPC"
  "String representation of the prefix key for prefixes of commands (Projectile, LSP, ...).")
(defconst ade/evil-window-mgt-leader-pfx-plain "C-w"
  "String representation of the prefix key for Evil mode window management.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rest of code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create dir specific to flag files that indicate whether some action has been
;; performed already
(setq ade/flag-dir (concat user-emacs-directory "/ade-flags"))
(unless (file-directory-p ade/flag-dir) (mkdir ade/flag-dir t))

(defun ade/make-empty-file (fname)
  "Creates an empty file, or does nothing if the file already exists."
  (interactive
   (let ((fname (read-file-name "Create empty file: ")))))
  (if (not (file-exists-p fname))
	  (write-region "" nil fname)
	(message "Not creating file: File exists: %s" fname)))

(if (eq system-type 'windows-nt)
	(progn
	  (defun ade/win/add-things-to-PATH ()
		;; MSYS2's dir is only a last resort; put it last in the PATH
		(setenv "PATH" (concat (getenv "PATH") ";C:\\msys64\\usr\\bin")))

	  (ade/win/add-things-to-PATH)))

(defun ade/do-basic-ui-setup ()
  ;; Down with the ugly default Emacs startup buffer!!
  (setq inhibit-startup-message t)

  (when (display-graphic-p)
	(scroll-bar-mode -1)
	(tool-bar-mode -1)
	(tooltip-mode -1)
	(set-fringe-mode 0))
  ;; Disable menu bar that appears in both graphic and headless modes
  (menu-bar-mode -1)
  ;; No beeps thank you
  (setq visible-bell t)
  ;; Maximize Emacs once this init script finishes running
  (add-hook 'emacs-startup-hook 'toggle-frame-fullscreen))

;; Call that newly-created function
(ade/do-basic-ui-setup)

(defun ade/set-font ()
  ;; Font && font size
  (if (eq system-type 'windows-nt)
	  (progn (set-face-attribute 'default nil :height 190 :weight 'normal :font "Consolas"))
	(progn (set-face-attribute 'default nil :height 190 :weight 'normal))))

(ade/set-font)

;; Don't add the annoying Custom line things in this file; add
;; them to a separate file
(setq custom-file (concat user-emacs-directory "/custom.el"))

(defun ade/do-basic-text-editing-ui-setup ()
  (column-number-mode)
  (global-display-line-numbers-mode t)
  (set-default-coding-systems 'utf-8)
  (setq-default tab-width 4
				indent-tabs-mode t)
  ;; Don't show line number in certain modes
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  treemacs-mode-hook
                  eshell-mode-hook))
	(add-hook mode (lambda () (display-line-numbers-mode 0)))))

(ade/do-basic-text-editing-ui-setup)

(defun ade/show-trailing-whitespace ()
  (setq-default show-trailing-whitespace t)
  (dolist (hook '(special-mode-hook
                  term-mode-hook
                  comint-mode-hook
                  compilation-mode-hook
                  minibuffer-setup-hook))
    (add-hook hook
			  (lambda () (setq show-trailing-whitespace nil)))))

(ade/show-trailing-whitespace)

(defun ade/setup-pkg-manager ()
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
  (unless (package-installed-p 'use-package) (package-install 'use-package)))

(ade/setup-pkg-manager)

(require 'use-package)
;; Always ensure packages loaded by use-package is downloaded
(setq use-package-always-ensure t)

;; Taken from all-the-icons package
(defun ade/win/all-the-icons-install-fonts ()
  "Helper function to download and install the latests fonts on Windows."
  (interactive)
  (let* ((url-format "https://raw.githubusercontent.com/domtronn/all-the-icons.el/master/fonts/%s")
         (font-dest (cond
		             ((eq system-type 'windows-nt)
					  (concat user-emacs-directory "/fonts-to-install"))))
         (known-dest? (stringp font-dest))
		 (progn (if (not font-dest)
					(error "Running %s on system other than Windows NT"
						   (get-current-function-name)))))

	(unless (file-directory-p font-dest) (mkdir font-dest t))

	(mapc (lambda (font)
			(url-copy-file (format url-format font) (expand-file-name font font-dest) t))
          all-the-icons-font-names)
	(when (yes-or-no-p
		   (format "Please manually install fonts in %s. Did install succeed? " font-dest))
      (message "%s Successfully %s `all-the-icons' fonts to `%s'!"
               (all-the-icons-wicon "stars" :v-adjust 0.0)
               (if known-dest? "installed" "downloaded")
               font-dest))))

(defun ade/all-the-icons-install-fonts ()
  (interactive)
  (setq ade/all-the-icons-install-fonts-flag-path
		(concat ade/flag-dir "/all-the-icons-install-fonts"))

  (unless (file-exists-p ade/all-the-icons-install-fonts-flag-path)
	(progn (if (not (eq system-type 'windows-nt))
			   ;; Don't prompt for fonts install on Linux and MacOS.
			   (all-the-icons-install-fonts t)
			 (ade/win/all-the-icons-install-fonts))
		   (ade/make-empty-file ade/all-the-icons-install-fonts-flag-path))))

;; Needed by doom-modeline
(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (ade/all-the-icons-install-fonts))

(use-package ivy
  :diminish
  :bind (
		 :map ivy-minibuffer-map
		 ("TAB" . ivy-alt-done)
		 ("C-s" . ivy-previous-line)
		 ("C-d" . ivy-next-line)
		 :map ivy-switch-buffer-map
		 ("C-s" . ivy-previous-line)
		 ("C-d" . ivy-switch-buffer-kill)
		 :map ivy-reverse-i-search-map
		 ("C-s" . ivy-previous-line)
		 ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1)
  (dolist (mode '(counsel-mode))
	(add-hook mode (lambda () (setq show-trailing-whitespace nil)))))

(use-package swiper
  :ensure nil
  :init (unbind-key "C-v")
  :custom
  (ivy-extra-directories nil)
  :bind
  (("C-v" . swiper)
   :map swiper-map
   ("C-s" . ivy-previous-line)
   ("C-d" . ivy-next-line)))

;; Counsel package is used by ivy-rich; ivy-rich doesn't seem to work without it
(use-package counsel
  :bind (("M-x" . counsel-M-x)
		 ("C-x b" . counsel-ibuffer)
		 ("C-x C-f" . counsel-find-file)
		 ("C-M-j" . counsel-switch-buffer)
		 :map minibuffer-local-map ("C-r" . 'counsel-minibuffer-history))
  :custom (ivy-initial-inputs-alist nil)) ; Don't start searches with "^"

(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; When some text is selected and we type some other text, the selected text is
;; removed first. Not particularly useful since we use Evil mode, but oh well.
(use-package delsel
  :config (delete-selection-mode 1))

;; https://github.com/seagle0128/doom-modeline/issues/187#issuecomment-507201556
(defun ade/doom-modeline-height ()
  "Calculate the actual char height of the mode-line."
  (- (frame-char-height) 8))

(use-package doom-modeline
  :after all-the-icons
  :init (doom-modeline-mode 1)
  :config
  (advice-add #'doom-modeline--font-height :override #'ade/doom-modeline-height))

(use-package doom-themes
  :init (load-theme 'doom-dark+ t))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; To show available keybindings when using a hierarchical keybinding
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :custom (which-key-idle-delay 1.0))

(use-package helpful
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

(use-package general)

(general-create-definer ade/cmd-pfx
  :prefix ade/cmd-pfx-plain)

(setq ade/generic-pfx-plain "C-c")
(general-create-definer ade/generic-pfx
  :prefix ade/generic-pfx-plain)

(ade/generic-pfx
 "r" 'revert-buffer)

(use-package hydra)

;; Somehow, adding this to the :init of evil still triggers a warning on the
;; first time emacs is run, so just set that before evil and evil-collection
;; are ever mentioned.
(setq evil-want-keybinding nil)

(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                EVIL MODE                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(defhydra ade/evil-window-size-change (:timeout 4)
  "Resize current window"
  ("a" evil-window-decrease-width "- width")
  ("f" evil-window-increase-width "+ width")
  ("s" evil-window-decrease-height "- height")
  ("d" evil-window-increase-height "+ height"))

(general-create-definer ade/evil-window-mgt-leader-def
  :prefix ade/evil-window-mgt-leader-pfx-plain)
;; Remove any existing keybinding conflicting with our window management.
;; Window management is pretty general and important!
(general-unbind ade/evil-window-mgt-leader-pfx-plain)

(use-package evil
  :custom
  ;; Apparently needed with evil-collection
  (evil-want-integration t)
  ;; Apparently also needed with evil-collection
  (evil-want-keybinding nil)
  ;; Hitting "." shouldn't move the cursor (normally repeats last command)
  (evil-repeat-move-cursor nil)
  ;; Exiting insert mode shouldn't move the cursor one char back
  (evil-move-cursor-back nil)
  ;; evil-forward/backward-char can move to the EOL character. Like, seriously.
  (evil-move-beyond-eol t)
  ;; evil-forward/backward-char can move accross lines
  (evil-cross-lines t)

  :config

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Complete rework of the evil keybindings. ;;
  ;;      ~~User discretion is advised~~      ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (defun ade/remove-evil-keybindings ()
	"Removes all Evil keybindings.

I don't really care about Vim compatibility -- I don't use Vim. All I want is modal editing. So I'll make my own afterwards. "
	(interactive)
	(setf (cdr evil-motion-state-map) nil)
	(setf (cdr evil-normal-state-map) nil)
	(setf (cdr evil-visual-state-map) nil)
	(setf (cdr evil-replace-state-map) nil)
	(setf (cdr evil-insert-state-map) nil))

  (defun ade/add-custom-evil-keybindings ()
	"Sets up new Evil keybindings.

These are more about where the buttons are on the keyboard than about the name of the functionality, altough I do insipre myself from the default keybindings too."
	(interactive)

	;; C-g is like "quit"; it makes us go back to normal mode. Easy to hit!
	(general-def 'motion "C-g" 'evil-normal-state)
	(general-def 'insert "C-g" 'evil-normal-state)
	(general-def 'replace "C-g" 'evil-normal-state)
	;; Need access to emacs state in case evil ever betrays me.
	(general-def 'motion "C-z" 'evil-emacs-state)
	(general-def 'insert "C-z" 'evil-emacs-state)
	(general-def 'replace "C-z" 'evil-emacs-state)
	;; Keeping <escape> seems to be a Vim doctrine, so let's keep it.
	(general-def 'motion  "<escape>" 'evil-normal-state)
	(general-def 'insert "<escape>" 'evil-normal-state)
	(general-def 'replace "<escape>" 'evil-normal-state)

	;; Movement and going back to normal state is generally with left hand
	;; whereas changing modes, inserting and special commands (e.g. kill, yank)
	;; usually with right hand.
	(general-def 'motion ":"   'evil-ex) ; Vim execute command thing.
	(general-def 'motion "a"   'evil-backward-char)
	(general-def 'motion "f"   'evil-forward-char)
	(general-def 'motion "s"   'evil-previous-visual-line)
	(general-def 'motion "d"   'evil-next-visual-line)
	(general-def 'motion "k"   'evil-insert)
	(general-def 'motion "K"   'evil-insert-line)
	(general-def 'motion "l"   'evil-append)
	(general-def 'motion "L"   'evil-append-line)
	(general-def 'motion "w"   'evil-backward-word-begin)
	(general-def 'motion "e"   'evil-forward-word-end)
	(general-def 'motion "W"   'evil-backward-WORD-begin)
	(general-def 'motion "E"   'evil-forward-WORD-end)
	(general-def 'motion "q"   'evil-beginning-of-visual-line)
	(general-def 'motion "r"   'evil-end-of-visual-line)
	(general-def 'motion "Q"   'beginning-of-buffer)
	(general-def 'motion "R"   'end-of-buffer)
	(general-def 'motion "x"   'scroll-down)
	(general-def 'motion "c"   'scroll-up)
	(general-def 'motion "X"   (lambda () (interactive) (scroll-down 1)))
	(general-def 'motion "C"   (lambda () (interactive) (scroll-up 1)))
	(general-def 'motion "b"   'evil-jump-backward)
	(general-def 'motion "B"   'evil-jump-forward)
	(general-def 'motion "h"   'evil-visual-char)
	(general-def 'motion "H"   'evil-visual-line)
	(general-def 'motion "M-h" 'evil-visual-block)
	(general-def 'motion "t"   'evil-jump-item)
	(ade/evil-window-mgt-leader-def 'motion "a"   'evil-window-left)
	(ade/evil-window-mgt-leader-def 'motion "f"   'evil-window-right)
	(ade/evil-window-mgt-leader-def 'motion "s"   'evil-window-up)
	(ade/evil-window-mgt-leader-def 'motion "d"   'evil-window-down)
	(ade/evil-window-mgt-leader-def 'motion "q"   'evil-quit)
	(ade/evil-window-mgt-leader-def 'motion "C-q" 'evil-quit)
	(ade/evil-window-mgt-leader-def 'motion "w"   'evil-window-split)
	(ade/evil-window-mgt-leader-def 'motion "e"   'evil-window-vsplit)
	(ade/evil-window-mgt-leader-def 'motion "C-w" 'evil-window-split)
	(ade/evil-window-mgt-leader-def 'motion "C-e" 'evil-window-vsplit)
	(ade/evil-window-mgt-leader-def 'motion "g"   (lambda () (interactive) (kill-buffer nil)))
	;; C-g is for emacs cancelling.
	;; (ade/evil-window-mgt-leader-def 'motion "C-g" (lambda () (interactive) (kill-buffer nil)))
    (ade/evil-window-mgt-leader-def 'motion "b"   'counsel-ibuffer)
    (ade/evil-window-mgt-leader-def 'motion "C-b" 'counsel-ibuffer)
	(ade/evil-window-mgt-leader-def 'motion "r"   'ade/evil-window-size-change/body)
	(ade/evil-window-mgt-leader-def 'motion "C-r" 'ade/evil-window-size-change/body)

	(general-def 'normal "k"   'evil-insert)
	(general-def 'normal "K"   'evil-insert-line)
	(general-def 'normal "l"   'evil-append)
	(general-def 'normal "L"   'evil-append-line)
	(general-def 'normal "j"   'evil-visual-char)
	(general-def 'normal "J"   'evil-visual-line)
	(general-def 'normal "M-j" 'evil-visual-block)
	(general-def 'normal "h"   'evil-replace)
	(general-def 'normal "H"   'evil-enter-replace-state)
	(general-def 'normal "u"   'evil-undo) ; C-z on Linux
	(general-def 'normal "U"   'evil-redo) ; C-Z on Linux, so uppercase makes sense
	(general-def 'normal "p"   'evil-paste-before)
	(general-def 'normal "P"   'evil-paste-after)
	(general-def 'normal "n"   'evil-delete-backward-char)
	(general-def 'normal "N"   'evil-delete-whole-line)
	(general-def 'normal "m"   'evil-delete-char)
	(general-def 'normal "M"   'evil-delete-whole-line)
	(general-def 'normal "y"   'save-buffer)

	(general-def '(insert replace) "C-a" 'evil-backward-char)
	(general-def '(insert replace) "C-f" 'evil-forward-char)
	(general-def '(insert replace) "C-s" 'evil-previous-visual-line)
	(general-def '(insert replace) "C-d" 'evil-next-visual-line)
	(general-def '(insert replace) "C-u" 'evil-undo)
	(general-def '(insert replace) "C-U" 'evil-redo)
	;; Already in insert/replace state so C-k should go to beginning of line
	(general-def '(insert replace) "C-k" 'evil-insert-line)
	(general-def '(insert replace) "C-K" 'evil-insert-line)
	;; Already in insert/replace state so C-l should go to end of line
	(general-def '(insert replace) "C-l" 'evil-append-line)
	(general-def '(insert replace) "C-L" 'evil-append-line)
	;; <return> maps to C-m, so rebinding C-m looks like a can of worms:
	;; http://makble.com/rebind-ctrlm-and-enter-in-emacs
	;;
	;; (general-def '(insert replace) "C-n" 'evil-delete-backward-char)
	;; (general-def '(insert replace) "C-N" 'evil-delete-whole-line)
	;; (general-def '(insert replace) "C-m" 'evil-delete-char)
	;; (general-def '(insert replace) "C-M" 'evil-delete-whole-line)
	;;
	;; C-w is already mapped to window management
	;; (general-def '(insert replace) "C-w" 'evil-backward-word-begin)
	;; (general-def '(insert replace) "C-W" 'evil-backward-WORD-begin)
	;; (general-def '(insert replace) "C-e" 'evil-forward-word-begin)
	;; (general-def '(insert replace) "C-E" 'evil-forward-WORD-begin)
	(general-def '(insert replace) "C-q" 'evil-beginning-of-visual-line)
	(general-def '(insert replace) "C-r" 'evil-end-of-visual-line)
	(general-def '(insert replace) "C-Q" 'beginning-of-buffer)
	(general-def '(insert replace) "C-R" 'end-of-buffer)
	(general-def '(insert replace) "C-p" 'evil-paste-before)
	(general-def '(insert replace) "C-P" 'evil-paste-after)

	(general-def 'visual ";"   'comment-dwim) ; I do that often!
	(general-def 'visual "h"   'evil-visual-char)
	(general-def 'visual "H"   'evil-visual-line)
	(general-def 'visual "M-h" 'evil-visual-block)
	(general-def 'visual "o"   'kill-ring-save)
	(general-def 'visual "O"   'kill-region)
	;; "a" seems to be overridden by its nil keybindings, causing it
	;; to be considered a prefix keybinding, so forcefully set that!
	(general-def 'visual "a"   'evil-backward-char)
	(general-def 'visual "u"   'evil-downcase)
	(general-def 'visual "U"   'evil-upcase))

  (evil-mode 1)
  (ade/remove-evil-keybindings)
  (ade/add-custom-evil-keybindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                TERMINAL                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (and
		 module-file-suffix ; Check for dynamic modules support.

		 ;; VTerm: speedy terminal but requires Emacs dynamic modules support,
		 ;; and build essentials to setup.
		 ;; Check out the ademacs repo's README.md for the list of packages to
		 ;; install on Ubuntu.
		 (use-package vterm
		   ;; Ensure VTerm is loaded right away so we know whether to setup
		   ;; Term.
		   :demand t
		   :init
		   (defun ade/vterm-compile ()
			 "Try compiling VTerm if it hasn't been attempted already. Either
way, return nil if compilation has/had failed, and non-nil otherwise."
			 (let
				 ((compile-attempted-flag-path
				   (concat ade/flag-dir "/vterm-compile-attempted"))
				  (compile-worked-flag-path
				   (concat ade/flag-dir "/vterm-compile-worked"))
				  (compile-worked nil))
			   ;; If vterm compile wasn't attempted yet...
			   (unless (file-exists-p compile-attempted-flag-path)
				 ;; ...then try to compile...
				 (setq compile-worked (ignore-errors (vterm-module-compile) t))
				 ;; ...and if compile worked, then remember so...
				 (when compile-worked
				   (ade/make-empty-file compile-worked-flag-path)))
				 ;; ...and remember that compilation was attempted.
				 (ade/make-empty-file compile-attempted-flag-path)
			   ;; Then evaluate to whether compile had worked when we
			   ;; attempted it.
			   (file-exists-p compile-worked-flag-path)))
		   (add-hook 'emacs-startup-hook 'ade/vterm-compile)
		   :custom
		   (vterm-always-compile-module t)
		   :config
		   (setq-default term-prompt-regexp ade/terminal-prompt-regexp)
		   (setq-default vterm-max-scrollback 10000)))

  ;; Term: terminal emulator written in Elisp.
  (use-package term
	:config
	(setq-default explicit-shell-file-name "bash")
	(setq-default term-prompt-regexp ade/terminal-prompt-regexp))

  ;; For terminal colors
  (use-package eterm-256color
	:hook (term-mode . eterm-256color-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           IDE-LIKE FEATURES                             ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package projectile
  :diminish projectile-mode
  :init
  ;; Otherwise projectile screams because keybinding is not a prefix key.
  (unbind-key ade/cmd-pfx-plain)
  (dolist (dir (-filter 'file-directory-p ade/prj-dirs))
	(setq projectile-project-search-path (list ade/prj-dirs)))

  ;; This does not seem to work unfortunately...
  (setq-default projectile-globally-ignored-files '("~undo-tree~"))

  ;; Does the same as :bind-keymap, but uses a variable's value to compute
  ;; the keybinding string. See:
  ;; https://emacs.stackexchange.com/questions/74322/use-package-bind-keymap-with-car-as-a-form
  (bind-key (concat ade/cmd-pfx-plain " p")
			#'(lambda nil
				(interactive)
				(use-package-autoload-keymap 'projectile-command-map 'projectile nil)))

  ;; Doesn't work at the moment because SpaceStudio doesn't create a project
  ;; file with a fixed name.
  ;; (defun ade/projectile-register-spacestudio ()
  ;; 	(projectile-register-project-type 'spacestudio '("*.spacestudio")))
  ;; (ade/projectile-register-spacestudio)

  :custom
  (projectile-completion-system 'ivy)
  ;; Extra ignore rules don't work without native indexing
  (projectile-indexing-method 'native)
  :config
  (projectile-mode))

(defun ade/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(defun ade/lsp-c-c++-mode-setup ()
  (lsp-mode 1)
  (lsp)
  (setq-default lsp-idle-delay 0.1))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix (concat ade/cmd-pfx-plain " l"))
  :hook
  (lsp-mode . ade/lsp-mode-setup)
  (c-mode   . ade/lsp-c-c++-mode-setup)
  (c++-mode . ade/lsp-c-c++-mode-setup)
  :bind
  (("C-c C-c" . completion-at-point))
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-treemacs)

(use-package lsp-ivy)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind
  (:map lsp-ui-peek-mode-map
		("M-s" . lsp-ui-peek--select-prev)
		("M-d" . lsp-ui-peek--select-next)
		("M-a" . lsp-ui-peek--select-prev-file)
		("M-f" . lsp-ui-peek--select-next-file)
		("C-g" . lsp-ui-peek--abort)))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  ;; Use TAB to complete and setup evil-like cycling through the
  ;; entries.
  :bind (:map company-active-map
			  ("<tab>" . company-complete-selection)
			  ("M-s" . company-select-previous)
			  ("M-d" . company-select-next))
  (:map lsp-mode-map ("<tab>" . indent-for-tab-command))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

(use-package cc-mode
  :ensure nil
  :custom
  (c-tab-always-indent nil)
  :config
  (setq-default c-basic-offset 4)
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

  (defun ade/c-c++-remove-keybindings ()
	(setf (cdr c-mode-base-map) nil)
	(setf (cdr c-mode-map) nil))
  (ade/c-c++-remove-keybindings)

  (defun ade/c-c++-indent-setup ()
	(c-set-offset 'arglist-intro '+)
	(c-set-offset 'innamespace 0)
	(c-set-offset 'arglist-close 0))

  (add-hook 'c-mode-hook   'ade/c-c++-indent-setup)
  (add-hook 'c++-mode-hook 'ade/c-c++-indent-setup)
  :bind (
		 :map c-mode-map
			   ("C-c C-s" . c-show-syntactic-information)
		 :map c++-mode-map
			   ("C-c C-s" . c-show-syntactic-information)))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :hook (cmake-mode . lsp-deferred))

;; Some kind of 'improved' CMake highlighting. Gimme!
(use-package cmake-font-lock
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

;; Config from https://www.reddit.com/r/emacs/comments/audffp/tip_how_to_use_a_stable_and_fast_environment_to/
(defun ade/cmake-ide-setup-build-dir ()
  "Sets the cmake-ide-build-dir variable from projectile's project root."
  (when (projectile-project-p)
	(setq cmake-ide-build-dir
		  (concat (projectile-project-root) "/build"))))

(use-package cmake-ide
  :after projectile
  :hook
  (c-mode . ade/cmake-ide-setup-build-dir)
  (c++-mode . ade/cmake-ide-setup-build-dir)
  :config
  (cmake-ide-setup)
  (ade/cmd-pfx
	"c c" 'cmake-ide-run-cmake
	"c b" 'camke-ide-compile
	"c d" 'cmake-ide-load-db))

