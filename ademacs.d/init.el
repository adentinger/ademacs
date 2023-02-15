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

;;; PROFILE ID CONSTANTS (personal vs work vs...)
(defconst ade/profile-id--generic "generic"
  "Setup ID for profile with no particular extras.")
(defconst ade/profile-id--personal "personal"
  "Setup ID for personal profile.")
(defconst ade/profile-id--work "work"
  "Setup ID for work profile.")

;; PROFILE ID FOR THIS CONFIG
(defconst ade/profile-id ade/profile-id--generic
  "Value of the profile ID.")

;; MISC CONFIGURATION
(defconst ade/prj-dirs (list ade/git-prj-dir "~")
  "List of paths that Projectile will search for projects in.")
(defconst ade/terminal-prompt-regexp "^[^#$%>\n]*[#$%>] *"
  "Regular expression that matches a prompt in a terminal.")

;;; KEY PREFIXES
(defconst ade/cmd-pfx-plain "C-SPC"
  "String representation of the prefix key for prefixes of commands \
(Projectile, LSP, ...).")
(defconst ade/evil-window-mgt-leader-pfx-plain "C-w"
  "String representation of the prefix key for Evil mode window management.")
(defconst ade/projectile-subpfx-plain "p"
  "String representation of Projectile sub-prefix key.")
(defconst ade/lsp-subpfx-plain "l"
  "String representation of `lsp' sub-prefix key.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Rest of code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ade/profile-personal-p ()
  "Whether we are in the personal profile."
  (interactive)
  (equal ade/profile-id ade/profile-id--personal))

(defun ade/profile-work-p ()
  "Whether we are in the work profile."
  (interactive)
  (equal ade/profile-id ade/profile-id--work))

;; Create dir specific to flag files that indicate whether some action has been
;; performed already
(defconst ade/ade-dir (concat user-emacs-directory "/ade")
  "Directory for ademacs internal stuff.")
(unless (file-directory-p ade/ade-dir) (mkdir ade/ade-dir t))
(defconst ade/flag-dir (concat ade/ade-dir "/flags")
  "Directory for ademacs flag files to indicate whether some action is done.")
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

(defun ade/font-exists-p (font-family)
  "Whether a font with a given font family exists."
  (interactive "sFont family: ")
  (find-font (font-spec :name font-family)))

(defun ade/set-font (font-family height)
  "Sets the font family as well as some basic other attributes, or only set \
the attributes if font-family is nil."
  (interactive "sFont family: \nnFont height: ")
  (message "Setting font to %s, height %d." font-family height)
  (if font-family
	  (set-face-attribute 'default nil :height height :weight 'normal
						  :font font-family)
	(set-face-attribute 'default nil :height height :weight 'normal)))

(defun ade/auto-set-font ()
  "Determines my preferred available font family, and sets the default font \
accordingly."
  (interactive)
  ;; Font && font size
  (cond
   ((ade/font-exists-p "Consolas")
	(ade/set-font "Consolas" 190))
   ((ade/font-exists-p "Ubuntu Mono")
	(ade/set-font "Ubuntu Mono" 190))
   ((ade/font-exists-p "Liberation Mono")
	(ade/set-font "Liberation Mono" 160))
   (t
	(ade/set-font nil 190))))

(ade/auto-set-font)

;; Don't add the annoying Custom line things in this file; add
;; them to a separate file
(setq custom-file (concat user-emacs-directory "/custom.el"))

(defun ade/setup-backup-files ()
  "Sets up backup files into a single absolute dir and sets up multiple backups."
  (interactive)
  ;; Please please stop creating the backup "myfile~" files in the same
  ;; directory as the file Emacs backs up. They appear inside the file list of
  ;; Projectile and pollute the directory. Put them all into a single location
  ;; outside everything else.
  (setq backup-directory-alist
		(list (cons "." (concat user-emacs-directory "/file-backups"))))
  ;; https://stackoverflow.com/a/151946/2252948
  (setq backup-by-copying t
		delete-old-versions t
		version-control t
		kept-new-versions 6
		kept-old-versions 2))

(ade/setup-backup-files)

(defun ade/do-basic-text-editing-setup ()
  (column-number-mode)
  ;; Use 1-based column number, not zero-based.
  ;; In practice in this configuration this is not super useful because we use
  ;; doom-modeline, which has an independant setting, but oh well.
  (setq-default mode-line-position-column-format " C%C")
  (global-display-line-numbers-mode)
  (set-default-coding-systems 'utf-8)
  (setq-default tab-width 4
				indent-tabs-mode t)
  ;; I (usually) follow the 80-column rule in C/C++ and the like to have
  ;; code side-by-side, so truncate lines if the smaller of the two windows
  ;; is too small to display all the line.
  (toggle-truncate-lines 1)
  (setq-default view-read-only t)
  ;; Automatically revert buffers if they change on disk and don't have
  ;; any unsaved change.
  (setq-default auto-revert-mode)
  (setq text-scale-mode-step 1.1)
  ;; upcase-region and downcase-region shouldn't ask for confirmation
  ;; (By default it prompts: "You have invoked the disabled command
  ;; downcase-region. It is disabled because new users often find it
  ;; confusing.)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (dolist (mode '(text-mode-hook
                  prog-mode-hook))
	(add-hook mode
			  (lambda ()
				;; The underscore character is also a word constitutent in these modes
				(modify-syntax-entry ?_ "w")
				;; When a region is active/selected, indent-for-tab-command (which is the
				;; command that is called when hitting <tab> in most modes) should not call
				;; indent-region). I don't like to have to hit <tab> on every line just
				;; because selecting multiple lines and hitting <tab> would do something that
				;; I don't like!
				(transient-mark-mode 0))))
  ;; Don't show line number in certain modes.
  ;; (Some more modes will additionally be setup later in the relevant
  ;; use-package)
  (dolist (mode '(org-mode-hook
                  shell-mode-hook
                  eshell-mode-hook
                  treemacs-mode-hook))
	(add-hook mode (lambda () (display-line-numbers-mode 0)))))

(ade/do-basic-text-editing-setup)

(defun ade/show-trailing-whitespace ()
  (setq-default show-trailing-whitespace t)
  ;; (Some more modes will additionally be setup later in the relevant
  ;; use-package)
  (dolist (hook '(special-mode-hook
                  shell-mode-hook
                  eshell-mode-hook
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
		 ("C-d" . ivy-previous-line)
		 ("C-f" . ivy-next-line)
		 :map ivy-switch-buffer-map
		 ("C-d" . ivy-previous-line)
		 ("C-f" . ivy-switch-buffer-kill)
		 :map ivy-reverse-i-search-map
		 ("C-d" . ivy-previous-line)
		 ("C-f" . ivy-reverse-i-search-kill))
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
   ("C-d" . ivy-previous-line)
   ("C-f" . ivy-next-line)))

;; To use PCRE for search-replace
(use-package visual-regexp-steroids
  :bind
  ("C-M-v" . 'vr/query-replace))

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
  (- (frame-char-height) 10))

(use-package doom-modeline
  :after all-the-icons
  :init (doom-modeline-mode 1)
  :config
  (advice-add #'doom-modeline--font-height :override #'ade/doom-modeline-height)
  ;; One-based column number please!
  (setq doom-modeline-column-zero-based nil))

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
  ([remap describe-key] . helpful-key)
  :config
  ;; These mode maps override some of the Evil keybinds below (they usually add
  ;; stuff under 'g').
  ;;
  ;; I don't know how to unbind specific keybinds of specific keymaps
  ;; without leaving a `nil' in the keymap, so instead let's just nuke the
  ;; whole keymap.
  (setf (cdr helpful-mode-map) nil))

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
  (add-to-list 'undo-tree-history-directory-alist
			   (cons "." (concat user-emacs-directory "/undo-tree")))
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                EVIL MODE                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  ;; These mode maps override some of the Evil keybinds below (they usually add
  ;; stuff under 'g').
  ;;
  ;; I don't know how to unbind specific keybinds of specific keymaps
  ;; without leaving a `nil' in the keymap, so instead let's just nuke the
  ;; whole keymap.
  (setf (cdr emacs-lisp-mode-map) nil))

(use-package sh-script
  :hook
  ;; sh-mode-map overrides some of the Evil keybinds below.
  ;; I'd like to remove only the keybinds that are in conflict, but when I do
  ;; so by setting the entry to nil the keybind no longer does anything, and
  ;; I don't know how to completely remove the entry without setting it to
  ;; nil.
  (sh-mode-hook . (lambda () (setf (cdr sh-mode-map) nil))))

(defhydra ade/evil-window-size-change (:timeout 4)
  "Resize current window"
  ("s" evil-window-decrease-width "- width")
  ("g" evil-window-increase-width "+ width")
  ("d" evil-window-decrease-height "- height")
  ("f" evil-window-increase-height "+ height"))

(general-create-definer ade/evil-window-mgt-leader-def
  :prefix ade/evil-window-mgt-leader-pfx-plain)
;; Remove any existing keybinding conflicting with our window management.
;; Window management is pretty general and important!
(general-unbind ade/evil-window-mgt-leader-pfx-plain)

;; From: http://xahlee.info/emacs/emacs/emacs_kill-ring.html
(defun ade/delete-line ()
  "Delete whole current line without adding it to kill-ring."
  (interactive)
  (beginning-of-line 1)
  (setq p-start (point))
  (end-of-line 1)
  (setq p-end (point))
  (delete-region p-start p-end)
  ;; Also delete eol itself
  (delete-char 1))

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
	(general-def 'motion ";"   'comment-dwim) ; I do that often!
	(general-def 'motion "s"   'evil-backward-char)
	(general-def 'motion "g"   'evil-forward-char)
	(general-def 'motion "d"   'evil-previous-visual-line)
	(general-def 'motion "f"   'evil-next-visual-line)
	(general-def 'motion "k"   'evil-insert)
	(general-def 'motion "K"   'evil-insert-line)
	(general-def 'motion "l"   'evil-append)
	(general-def 'motion "L"   'evil-append-line)
	(general-def 'motion "e"   'evil-backward-word-begin)
	(general-def 'motion "r"   'evil-forward-word-end)
	(general-def 'motion "E"   'evil-backward-WORD-begin)
	(general-def 'motion "R"   'evil-forward-WORD-end)
	(general-def 'motion "w"   'evil-beginning-of-visual-line)
	(general-def 'motion "t"   'evil-end-of-visual-line)
	(general-def 'motion "W"   'beginning-of-buffer)
	(general-def 'motion "T"   'end-of-buffer)
	(general-def 'motion "c"   'scroll-down)
	(general-def 'motion "v"   'scroll-up)
	(general-def 'motion "C"   (lambda () (interactive) (scroll-down 1)))
	(general-def 'motion "V"   (lambda () (interactive) (scroll-up 1)))
	(general-def 'motion "x"   'evil-jump-backward)
	(general-def 'motion "X"   'evil-jump-forward)
	(general-def 'motion "h"   'evil-replace)
	(general-def 'motion "H"   'evil-enter-replace-state)
	(general-def 'motion "M-h" 'evil-visual-block)
	(general-def 'motion "q"   'evil-jump-item)
	(ade/evil-window-mgt-leader-def 'motion "s"   'evil-window-left)
	(ade/evil-window-mgt-leader-def 'motion "g"   'evil-window-right)
	(ade/evil-window-mgt-leader-def 'motion "d"   'evil-window-up)
	(ade/evil-window-mgt-leader-def 'motion "f"   'evil-window-down)
	(ade/evil-window-mgt-leader-def 'motion "q"   'evil-quit)
	(ade/evil-window-mgt-leader-def 'motion "C-q" 'evil-quit)
	(ade/evil-window-mgt-leader-def 'motion "w"   'evil-window-split)
	(ade/evil-window-mgt-leader-def 'motion "e"   'evil-window-vsplit)
	(ade/evil-window-mgt-leader-def 'motion "C-w" 'evil-window-split)
	(ade/evil-window-mgt-leader-def 'motion "C-e" 'evil-window-vsplit)
	(ade/evil-window-mgt-leader-def 'motion "a"   (lambda () (interactive) (kill-buffer nil)))
	(ade/evil-window-mgt-leader-def 'motion "C-a" (lambda () (interactive) (kill-buffer nil)))
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
	(general-def 'normal "u"   'evil-undo) ; C-z on Linux
	(general-def 'normal "U"   'evil-redo) ; C-Z on Linux, so uppercase makes sense
	(general-def 'normal "p"   'evil-paste-before)
	(general-def 'normal "P"   'evil-paste-after)
	(general-def 'normal "n"   'delete-backward-char)
	(general-def 'normal "N"   'ade/delete-line)
	(general-def 'normal "m"   'delete-char)
	(general-def 'normal "M"   'ade/delete-line)
	(general-def 'normal "y"   'save-buffer)

	(general-def '(insert replace) "C-s" 'evil-backward-char)
	;; C-g used in Emacs for cancel operations
	;; (general-def '(insert replace) "C-g" 'evil-forward-char)
	(general-def '(insert replace) "C-d" 'evil-previous-visual-line)
	(general-def '(insert replace) "C-f" 'evil-next-visual-line)
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
	(general-def '(insert replace) "C-e" 'evil-backward-word-begin)
	(general-def '(insert replace) "C-E" 'evil-backward-WORD-begin)
	(general-def '(insert replace) "C-r" 'evil-forward-word-begin)
	(general-def '(insert replace) "C-R" 'evil-forward-WORD-begin)
	(general-def '(insert replace) "C-w" 'evil-beginning-of-visual-line)
	(general-def '(insert replace) "C-W" 'beginning-of-buffer)
	(general-def '(insert replace) "C-t" 'evil-end-of-visual-line)
	(general-def '(insert replace) "C-T" 'end-of-buffer)
	(general-def '(insert replace) "C-p" 'evil-paste-before)
	(general-def '(insert replace) "C-P" 'evil-paste-after)

	(general-def 'visual ";"   'comment-dwim) ; I do that often!
	(general-def 'visual "h"   'evil-replace)
	(general-def 'visual "H"   'evil-enter-replace-state)
	(general-def 'visual "o"   'kill-ring-save)
	(general-def 'visual "O"   'kill-region)
	(general-def 'visual "u"   'evil-downcase)
	(general-def 'visual "U"   'evil-upcase))

  (evil-mode 1)
  (ade/remove-evil-keybindings)
  (ade/add-custom-evil-keybindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                 MAGIT                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Don't use it yet, but it adds pretty colors to the git commit file! :)
(use-package magit
  :config
  ;; These mode maps override some of the Evil keybinds below (they usually add
  ;; stuff under 'g').
  ;;
  ;; I don't know how to unbind specific keybinds of specific keymaps
  ;; without leaving a `nil' in the keymap, so instead let's just nuke the
  ;; whole keymap.
  (setf (cdr git-commit-mode-map) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                TERMINAL                                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ade/vterm-compile-attempted-flag-path
  (concat ade/flag-dir "/vterm-compile-attempted")
  "File which, when present, indicates that `vterm' compilation has been \
attempted already.")
(defconst ade/vterm-compile-worked-flag-path
  (concat ade/flag-dir "/vterm-compile-worked")
  "File which, when present, indicated that `vterm' compilcation has been \
successful.")


(defun ade/terminal-general-setup ()
  "Sets the text scale for a terminal buffer."
  (interactive)
  (text-scale-set -3)
  (display-line-numbers-mode 0)
  (setq show-trailing-whitespace nil)
  (setq-default explicit-shell-file-name "bash")
  (setq-default term-prompt-regexp ade/terminal-prompt-regexp))

(defun ade/vterm-compile ()
  "Try compiling `vterm' if it hasn't been attempted already."
  (let
	  ((compile-worked nil))
	;; If vterm compile wasn't attempted yet...
	(unless (file-exists-p ade/vterm-compile-attempted-flag-path)
	  ;; ...then try to compile...
	  (setq compile-worked (ignore-errors (vterm-module-compile) t))
	  ;; ...and if compile worked, then remember so...
	  (when compile-worked
		(ade/make-empty-file ade/vterm-compile-worked-flag-path)))
	;; ...and remember that compilation was attempted.
	(ade/make-empty-file ade/vterm-compile-attempted-flag-path)))

(defun ade/vterm-compile-attempted-p ()
  "Whether `vterm' compilation has been attempted."
  (interactive)
  (file-exists-p ade/vterm-compile-attempted-flag-path))

(defun ade/vterm-compile-worked-p ()
  "Whether `vterm' compilation has been attempted and has worked."
  (interactive)
  (file-exists-p ade/vterm-compile-worked-flag-path))

(defun ade/vterm-config ()
  "`vterm' `use-package''s :config forms"
  (interactive)
  (setq-default term-prompt-regexp ade/terminal-prompt-regexp)
  (setq-default vterm-max-scrollback 10000)
  ;; Couldn't figure out how to remove a keybinding from a specific keymap
  ;; with general-unbind.
  (define-key vterm-mode-map (kbd ade/cmd-pfx-plain) nil)
  (add-hook 'vterm-mode-hook (lambda ()
							   (ade/terminal-general-setup))))

(defun ade/vterm-clear-compile-attempt ()
  "Remove files indicating that `vterm' compilation has been attempted."
  (interactive)
  (delete-file ade/vterm-compile-attempted-flag-path)
  (delete-file ade/vterm-compile-worked-flag-path))

(defun ade/vterm-use-package-p ()
  "Run `use-package' of `vterm', compiles it if that hasn't been attempted \
yet, and returns non-nil if compile has worked, or if it had been attempted \
before but had already worked."
  (interactive)
  (if (not (ade/vterm-compile-attempted-p))
	  (progn
		;; VTerm: speedy terminal but requires Emacs dynamic modules support,
		;; and build essentials to setup.
		;; Check out the ademacs repo's README.md for the list of packages to
		;; install on Ubuntu.
		(use-package vterm
		  ;; Ensure VTerm is loaded right away so that ade/vterm-config
		  ;; has access to the VTerm functions.
		  :demand t
		  :custom
		  (vterm-always-compile-module t)
		  (vterm-min-window-width 80)
		  :config
		  (ade/vterm-config))

		(ade/vterm-compile))

	(use-package vterm
	  :commands vterm
	  :custom
	  (vterm-min-window-width 80)
	  :config
	  (ade/vterm-config)))
  (ade/vterm-compile-worked-p))

(unless (and
		 module-file-suffix ; Check for dynamic modules support.
		 (ade/vterm-use-package-p))

  (message "VTerm compile has/had failed; using Term instead...")

  ;; TODO Setup term keybinds (toggle terminal buffer, ...).
  ;; Term: terminal emulator written in Elisp.
  (use-package term
	:ensure nil
	;; TODO :bind
	))

;; Always set the Term hook up, even when VTerm works correctly, since this
;; package is already part of Emacs anyway.
(add-hook 'term-mode-hook 'ade/terminal-general-setup)
;; For Term colors
(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

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
  (bind-key (concat ade/cmd-pfx-plain " " ade/projectile-subpfx-plain)
			#'(lambda nil
				(interactive)
				(use-package-autoload-keymap 'projectile-command-map 'projectile nil)))

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
  ;; clangd seemed to crash once in a while when this was set to 0.0. Maybe
  ;; setting this to slightly more than zero will help?
  (setq-default lsp-idle-delay 0.1))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (unless (eq system-type 'windows-nt)
	;; Use clangd-12 on non-windows (default clangd package on Ubuntu 20.04 has
	;; errors like stddef.h not found).
	(custom-theme-set-variables 'use-package
								'(lsp-clients-clangd-executable "/usr/bin/clangd-12")))
  (setq lsp-keymap-prefix (concat ade/cmd-pfx-plain " " ade/lsp-subpfx-plain))
  :custom
  (lsp-clients-clangd-args '("--header-insertion-decorators=0" "--log=verbose"))
  :hook
  (lsp-mode . ade/lsp-mode-setup)
  (c-mode   . ade/lsp-c-c++-mode-setup)
  (c++-mode . ade/lsp-c-c++-mode-setup)
  (sh-mode . lsp-deferred)
  (verilog-mode . lsp-deferred)
  :config
  (lsp-enable-which-key-integration t)
  (bind-key (concat ade/generic-pfx-plain " C-c") #'completion-at-point nil nil))

;; Adds code snippets. Never really understood these things, so I guess this is
;; the occasion!
(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package lsp-ivy)

;; Shows LSP stuff on the UI:
;; - Sideline (in which I only show flycheck diagnostics)
;; - (most importantly) Doc boxes with lsp-ui-doc-* vars and functions
;; - "Peek references" window
;; - ...
(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind
  (:map lsp-ui-peek-mode-map
		("M-d" . lsp-ui-peek--select-prev)
		("M-f" . lsp-ui-peek--select-next)
		("M-s" . lsp-ui-peek--select-prev-file)
		("M-g" . lsp-ui-peek--select-next-file)
		("C-g" . lsp-ui-peek--abort))
  :custom
  ;; Show the lsp-ui-doc documentation box when going over symbols with the
  ;; cursor, not just when hovering over them with the mouse.
  (lsp-ui-doc-show-with-cursor t)
  ;; This is the default, but in case the package changes the defaults let's
  ;; set this.
  (lsp-ui-doc-show-with-mouse t)
  (lsp-ui-doc-delay 0.5))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  ;; Use TAB to complete and setup evil-like cycling through the
  ;; entries.
  :bind (:map company-active-map
			  ("<tab>" . company-complete-selection)
			  ("M-d" . company-select-previous)
			  ("M-f" . company-select-next))
  (:map lsp-mode-map ("<tab>" . indent-for-tab-command))
  :custom
  (company-minimum-prefix-length 1)
  ;; NOTE: For C/C++ this setting is overridden elsewhere.
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
  (define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

  (defun ade/c-c++-indent-setup ()
	(setq c-default-style "linux")
	(setq c-basic-offset 4)
	(c-set-offset 'arglist-intro '+)
	(c-set-offset 'innamespace 0)
	(c-set-offset 'arglist-close 0)
	;; I don't want my "if"'s opening curly braces to be indented! Altough I
	;; usually put them on the same line as the "if"...
	(c-set-offset 'substatement-open 0))

  (add-hook 'c-mode-hook   'ade/c-c++-indent-setup)
  (add-hook 'c++-mode-hook 'ade/c-c++-indent-setup)
  :bind (
		 :map c-mode-map
			   ("C-c C-s" . c-show-syntactic-information)
		 :map c++-mode-map
		 ("C-c C-s" . c-show-syntactic-information)))

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'"))

;; Some kind of 'improved' CMake highlighting. Gimme!
(use-package cmake-font-lock
  :after (cmake-mode)
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package yaml-mode
  :hook (yaml-mode . lsp-deferred)
  :mode ("\\.clangd\\'" "\\.clang-tidy\\'" "\\.clang-format\\'"))

(use-package json-mode
  ;; Avro schemas are also JSON files
  :mode ("\\.avsc\\'")
  :hook (json-mode . lsp-deferred))

(use-package lsp-java
  :custom
  (lsp-java-server-install-dir
   (expand-file-name (concat user-emacs-directory "/jdt-lsp/server/")))
  (lsp-java-workspace-dir
   (expand-file-name (concat user-emacs-directory "/jdt-lsp/workspace/")))
  :hook (java-mode . lsp-deferred))

(use-package groovy-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                               WORK STUFF                                ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (ade/profile-work-p)
  (shell-command "echo hello world!"))

