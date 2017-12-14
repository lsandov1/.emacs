;; share from Victor Jaquez

;; the modeline
(line-number-mode t)                      ; show line numbers
(column-number-mode t)                    ; show column numbers
(size-indication-mode t)                  ; show file size (emacs 22+)
(display-time-mode t)                     ; show time
(menu-bar-mode -1)                        ; don't show the menu
(tool-bar-mode -1)                        ; don't show the toolbar
;;(cua-mode t)                              ; enable the cua mode
(transient-mark-mode t)                   ; make the current 'selection' visible
(delete-selection-mode t)                 ; delete the selection area with a keypress
;; (iswitchb-mode t)                         ; buffer switching: easier than icicles


;; loading files from a previous session at startup
;; http://www.emacswiki.org/emacs/DeskTop
;; (setq my-desktop-dir (expand-file-name "~/.emacs.d/desktop"))
;; (setq
;;   desktop-dir my-desktop-dir
;;   desktop-path (list my-desktop-dir))
;; (desktop-save-mode 1)

(setq search-highlight t                  ; highlight wen searching...
  query-replayce-highlight t)         ; ... and replacing

(fset 'yes-or-no-p 'y-or-n-p)             ; enable one letter y/n answers to yes/no

(set-language-environment "UTF-8")        ; prefer utf-8 for language settings

;; http://www.oreillynet.com/lpt/wlg/6162
(setq x-select-enable-clipboard t)        ; copy-paste should work ...

(global-font-lock-mode t)                 ; always do syntax highlighting
(when (require 'jit-lock)                 ; enable JIT to make font-lock faster
  (setq jit-lock-stealth-time 1))

(setq scroll-conservatively 10000)        ; smooth scrolling
;; (fringe-mode '(1 . 1))                    ; don't have too much space left (minmal)

(file-name-shadow-mode t)                 ; be smart about filenames
                                          ; (understand ~/ etc.)
(setq completion-ignore-case t            ; ignore case when completing ...
  read-file-name-completion-ignore-case t) ; ... filenames too

;; don't show startup messages
(setq inhibit-startup-message t
  inhibit-startup-echo-area-message t)

;; set frame  title / icon title using filename or buffername
;; little trick (based on http://www.emacswiki.org/cgi-bin/wiki/ShadyTrees)
;; to replace /home/foo with ~
(defun my-title-format ()
  (if buffer-file-name
    (replace-regexp-in-string "\\\\" "/"
      (replace-regexp-in-string (regexp-quote (getenv "HOME")) "~"
        (convert-standard-filename buffer-file-name)))
    (buffer-name)))

(setq
  frame-title-format '(:eval (my-title-format))
  icon-title-format  '(:eval (concat "emacs: " (my-title-format))))

;; show-paren-mode
;; show a subtle blinking of the matching paren (the defaults are ugly)
;; http://www.emacswiki.org/cgi-bin/wiki/ShowParenMode
(progn
  (show-paren-mode t)
  (setq show-paren-style 'expression)
  (set-face-background 'show-paren-match-face "#444444")
  (set-face-attribute 'show-paren-match-face nil
    :weight 'normal :underline nil :overline nil :slant 'normal))

;; run devhelp and find a token
(defun devhelp-word-at-point ()
  "runs devhelp"
  (interactive)
  (setq w (current-word))
  (start-process-shell-command "devhelp" nil "jhbuild" "run" "devhelp" "-s" w))

(global-set-key (kbd "<f8>") 'devhelp-word-at-point)

;; *fast* linenumbers on the left (unlike setnu.el)
;; http://www.emacsblog.org/2007/03/29/quick-tip-line-numbering/
(when (require 'linum)
  (global-set-key (kbd "<f6>") 'linum))

;; close the current buffer, just like in Win*
(global-set-key (kbd "C-<f4>") 'kill-buffer-and-window)

;; ido seem much less annoying than icicles...
;; makes completing buffers nicer, even nicer than iswitchb
;; http://www.emacswiki.org/cgi-bin/wiki/InteractivelyDoThings
;; http://www.forcix.cx/weblog/2005-08-03.html
(defun my-ido ()
  (interactive)
  (ido-mode t)
  (setq
    ido-ignore-buffers ;; ignore these guys
    '("\\` " "^\*Mess" "^\*Back" "^\*scratch" ".*Completion" "^\*Ido")
    ido-everywhere t             ; use for many file dialogs
    ido-case-fold  t             ; be case-insensitive
;;    ido-use-filename-at-point t  ; try to use filename at point
;;    ido-use-url-at-point t       ; ... or url at point
    ido-enable-flex-matching t   ; be flexible
    ido-max-prospects 5          ; don't spam my minibuffer
    ido-confirm-unique-completion t ; wait for RET. even with unique completion
    ido-decorations    ;; list of strings used to display alternatives
    '("  " "" " " " ..." " [" "]" " (No Match)" " (Matched)")
    ido-save-directory-list-file nil ; no ido state saved
    ido-max-window-height 1))

(when (require 'ido) (my-ido))

;; backups  (emacs will write backups and number them)
(setq make-backup-files t ; do make backups
      backup-by-copying t ; and copy them ...
      backup-directory-alist '(("." . "~/.emacs-backup")) ; ... here
      version-control t
      kept-new-versions 2
      kept-old-versions 5
      delete-old-versions t)

;; make gtags
(defun my-gtags-create-or-update ()
  "create or update the GNU global tag file"
  (interactive)
  (when (not (= 0 (call-process "global" nil nil nil " -p"))) ;; no tagfile
    (let ((oldir default-directory)
           (topdir (read-directory-name "gtags: top of source tree:" default-directory)))
      (cd topdir)
      (shell-command "gtags && echo 'created tagfile'")
      (cd oldir))
    ;; otherwise update the tagfile
    (shell-command "global -u && echo 'updated tagfile'")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LIBRARIES CONFIGURATIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; marmelade!!! this most be first
(when (require 'package)
  (add-to-list 'package-archives
    '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives
    '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (package-initialize))

;; change cursor color based on mode
;; http://www.emacswiki.org/cgi-bin/wiki/download/cursor-chg.el
;; (when (require 'cursor-chg)         ; Load this library
;;   (change-cursor-mode 1)            ; On for overwrite/read-only/input mode
;;   (toggle-cursor-type-when-idle 1)) ; On when idle

;; gtk-look-symbol
;; (when (require 'gtk-look)
;;   (define-key global-map [?\C-h ?\C-j] 'gtk-lookup-symbol))

;; (load-theme 'solarized-dark t)

;; (when (require 'magit)
;;  (setq magit-repo-dirs ("~/checkout/", "~/prog/")))

;; (add-to-list 'load-path "~/src/twittering-mode")
;; (when (require 'twittering-mode)
;;   (setq twittering-use-ssl t
;;     twittering-use-master-password t
;;     twittering-use-native-retweet t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; MODE HOOKS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; RST
(defun my-rst-mode-hook ()
  (interactive)

  (ispell-change-dictionary "castellano")

  (add-to-list 'load-path "~/.emacs.d/misc/pelican-mode")
  (require 'pelican-mode))

(add-hook 'rst-mode-hook 'my-rst-mode-hook)

;; Elisp
(defun my-emacs-lisp-mode-hook ()
  (interactive)

  ;; overrides the global f7 for compilation
  (local-set-key (kbd "<f7>") 'eval-buffer)

  (set-input-method nil)       ; i don't want accented chars, funny "a etc.
  (setq lisp-indent-offset 2)  ; indent with two spaces, enough for lisp
  (set (make-local-variable 'dabbrev-case-fold-search) nil)
  (set (make-local-variable 'dabbrev-case-replace) nil))

;; show some functions as keywords
(font-lock-add-keywords 'emacs-lisp-mode
  '(("\\<\\(quote\\|add-hook\\)" .
      font-lock-keyword-face)))

;; recognize some things as functions
(font-lock-add-keywords 'emacs-lisp-mode
  '(("\\<\\(set\\|setq\\|require-maybe\\|when-available\\|add-hook\\)\\>" .
      font-lock-function-name-face)))

;; recognize some things as constants
(font-lock-add-keywords 'emacs-lisp-mode
  '(("\\<\\(nil\\|\\t\\)\\_>" .
      font-lock-constant-face)))

(font-lock-add-keywords 'emacs-lisp-mode
  '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):"
      1 font-lock-warning-face prepend)))

(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

;; text-mode
(defun my-text-mode-hook ()
  (interactive)
  (set-fill-column 78)                  ; lines are 78 chars long ...
  (auto-fill-mode t)                    ; ... and wrapped around automagically
  (flyspell-mode t)
  (set-input-method "latin-1-prefix") ; make " + e => ë etc.

  (when (require 'filladapt)          ; do the intelligent wrapping of lines...
    (filladapt-mode t)))              ; ... (bullets, numbering) if available

(add-hook 'text-mode-hook 'my-text-mode-hook)

;; turn on autofill for all text-related modes
(toggle-text-mode-auto-fill)

;; c-mode / c++-mode
(defun my-c-mode-common ()
  (interactive)

  ;; start with the linux style
  (c-set-style "linux")

  (subword-mode t)
  (hs-minor-mode t)
  (define-key c-mode-base-map "\C-m" 'newline-and-indent)

  ;; highlight some stuff
  ;; this is for _all_ c modes
  (font-lock-add-keywords nil
    '(("\\<\\(FIXME\\|TODO\\|XXX+\\|BUG\\):"
        1 font-lock-warning-face prepend)))
  ;; highlight some stuff
  ;; this is for _all_ c modes
  (font-lock-add-keywords nil
    '(("\\<\\(__FUNCTION__\\|__PRETTY_FUNCTION__\\|__LINE__\\)"
        1 font-lock-preprocessor-face prepend)))

  (setq
    compilation-scroll-output 'first-error   ; scroll until first error
    compilation-read-command nil             ; don't need enter
    compilation-window-height 16             ; keep it readable
    c-hungry-delete-key t                    ; eat as much as possible
    show-trailing-whitespace t               ; show trailing whitespaces
    c-toggle-hungry-state t
    show-trailing-whitespace t)

  ;; guess the indentation of the current file, and use
  ;; that instead of my own settings; nice for foreign
  ;; files
  ;; https://savannah.nongnu.org/projects/dtrt-indent
  (when (require 'dtrt-indent) (dtrt-indent-mode t))
  (when (require 'gtags) (gtags-mode t))
  (when (require 'doxymacs)
    (doxymacs-mode t)
    (doxymacs-font-lock))

  (add-to-list 'load-path "~/.emacs.d/misc")
  (require 'gtk-doc)

  ;; warn when lines are > 80 characters (in c-mode)
  (font-lock-add-keywords 'c-mode
    '(("^[^\n]\\{80\\}\\(.*\\)$"
        1 font-lock-warning-face prepend))))

(add-hook 'c-mode-common-hook 'my-c-mode-common)

(defun my-c++-mode ()
  ;; warn when lines are > 100 characters (in c++-mode)
  (font-lock-add-keywords 'c++-mode
    '(("^[^\n]\\{100\\}\\(.*\\)$"
        1 font-lock-warning-face prepend))))

(add-hook 'c++-mode-hook 'my-c++-mode)

;; Makefiles
(defun my-makefile-mode-hook ()
  (interactive)
  (setq show-trailing-whitespace t))
(add-hook 'makefile-mode-hook 'my-makefile-mode-hook)

;; http://fsinfo.noone.org/~abe/mutt/
(defun my-mail-mode-hook ()
  (turn-on-auto-fill) ;;; Auto-Fill is necessary for mails
  (turn-on-font-lock) ;;; Font-Lock is always cool *g*
  (flush-lines "^\\(> \n\\)*> -- \n\\(\n?> .*\\)*") ;;; Kills quoted sigs.
  (not-modified) ;;; We haven't changed the buffer, haven't we? *g*
  (mail-text) ;;; Jumps to the beginning of the mail text
  (setq make-backup-files nil) ;;; No backups necessary.
  ;; http://shreevatsa.wordpress.com/2007/01/06/using-emacsclient/
  (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
  (flyspell-mode t)
  )

(or (assoc "mutt-" auto-mode-alist)
(setq auto-mode-alist (cons '("mutt-" . mail-mode) auto-mode-alist)))
(add-hook 'mail-mode-hook 'my-mail-mode-hook)

;; aspell
(setq ispell-program-name "aspell"
  ispell-extra-args '("--sug-mode=ultra"))

(setq ispell-dictionary "english")

;; check spelling in comments in source code
(dolist (hook '(c-mode-hook
                 sh-mode-hook
                 c++-mode-hook
                 ruby-mode-hook
                 cperl-mode-hook
                 python-mode-hook
                 autoconf-mode-hook
                 autotest-mode-hook
                 makefile-mode-hook
                 emacs-lisp-mode-hook
                 vala-mode-hook))
  (add-hook hook 'flyspell-prog-mode))

;; check spelling in comments in documents
(dolist (hook '(org-mode-hook
                 latex-mode-hook
                 mail-mode-hook
                 text-mode-hook
                 html-helper-mode-hook ))
  (add-hook hook 'flyspell-mode))


;; ;;show line number at the left
(global-linum-mode 1)

;; ;;show line and column number
;; (setq line-number-mode t)
;; (setq column-number-mode t)

;; ;; do not backup files
;; (setq make-backup-files nil) 

;; ;; Enable spelling corrector
;; (setq-default flyspell-mode)

;; ;; highline the current line
;; ;;(global-hl-line-mode 1)

;; ;; tab related
;; (setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)
;; (setq indent-line-function 'insert-tab)

;; dired
;; NOT working (define-key dired-mode-map "c" 'find-file) ;; 'creates' a file

;; For the moment, use python-mode on bitbake files
(setq auto-mode-alist (cons '("\\.bb$" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.inc$" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.bbappend$" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.bbclass$" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.conf$" . python-mode) auto-mode-alist))

;; Hide dired details
;; URL: http://www.emacswiki.org/emacs/DiredDetails
;; (require 'autofit-frame)
;; (add-hook 'after-make-frame-functions 'fit-frame)
;; (require 'dired-details+)

;; (show-paren-mode 1)

;;(setq-default scroll-lock-mode 1)
;;(require 'smooth-scrolling)

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

(global-set-key "\M-?" 'help-command)
(global-set-key "\C-h" 'delete-backward-char)

;;
(defun other-window-backward (&optional n)
  "Select the Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value))))
(global-set-key "\C-x\C-p" 'other-window-backward)

;;
(defalias 'scroll-ahead 'scroll-up)
(defalias 'scroll-behind 'scroll-down)

(defun scroll-n-lines-ahead (&optional n)
   "Scroll ahead one line."
   (interactive "P")
   (scroll-ahead (prefix-numeric-value n)))
(global-set-key "\C-z" 'scroll-n-lines-ahead)

(defun scroll-n-lines-behind (&optional n)
   "Scroll ahead one line."
   (interactive "P")
   (scroll-behind (prefix-numeric-value n)))
(global-set-key "\C-q" 'scroll-n-lines-behind)

(defun point-to-top ()
  "Put point on top line of window."
  (interactive)
  (move-to-window-line 0))
(global-set-key "\M-," 'point-to-top)
(global-set-key "\C-x," 'tags-loop-continue)

(defun point-to-bottom ()
  "Put point at the beginning of last visible line."
  (interactive)
  (move-to-window-line -1))
(global-set-key "\M-." 'point-to-bottom)



;; (defun line-to-top ()
;;   "Move current line to top of window."
;;   (interactive)
;;   (recenter 0))
;; (global-set-key "\M-!" 'line-to-top)

;;mnlort78

(add-hook 'find-file-hooks
	  '(lambda ()
	     (if (file-symlink-p buffer-file-name)
		 (progn
		   (setq buffer-read-only t)
		   (message "File is a symlink"))))
	  )

(defadvice switch-to-buffer (before existing-buffer activate compile)
  "When interactive, switch to existing buffers only."
  (interactive
   (list (read-buffer "Switch to buffer: "
		      (other-buffer)
		      (null current-prefix-arg)))))

(defvar unscroll-to nil
  "Text position for the call to 'unscroll'.")
(defvar unscroll-window-start nil
  "Window start for next call to 'unscroll'.")
(defvar unscroll-hscroll nil
  "Hscroll for the next call to 'unscroll'.")

(defadvice scroll-up (before remember-for-unscroll activate compile)
  "Remember where we started from, for 'unscroll'."
  (if (not (eq last-command 'scroll-up))
      (progn
	(setq unscroll-to (point)
	      unscroll-window-start (window-start)
	      unscroll-hscroll (window-hscroll)))))

(defun unscroll ()
  "Jump to location specified by 'unscroll-to'."
  (interactive)
  (if (not unscroll-to)
      (error "Cannot unscroll yet")
    (progn
      (goto-char unscroll-to)
      (set-window-start nil unscroll-window-start)
      (set-window-hscroll nil unscroll-hscroll))))

(global-set-key (kbd "C-x g") 'magit-status)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-added ((((type tty)) (:foreground "green"))))
 '(magit-diff-added-highlight ((((type tty)) (:foreground "LimeGreen"))))
 '(magit-diff-context-highlight ((((type tty)) (:foreground "default"))))
 '(magit-diff-file-heading ((((type tty)) nil)))
 '(magit-diff-removed ((((type tty)) (:foreground "red"))))
 '(magit-diff-removed-highlight ((((type tty)) (:foreground "IndianRed"))))
 '(magit-section-highlight ((((type tty)) nil))))

(setq ido-default-buffer-method 'selected-window)

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
  '(package-selected-packages
     (quote
       (python-mode w3 ssh rust-mode magit jabber gnuplot-mode gnuplot cm-mode adoc-mode))))


(setq tramp-default-method "ssh")

(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)


;;
;; this pice was created automatically and some pieces at end appended by Leonardo Sandoval
;;
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (magit python))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(setq ispell-program-name "/usr/bin/aspell")
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

;; bitbake metadata
(add-to-list 'auto-mode-alist '("\\.bb\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.bbclass\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.inc\\'" . python-mode))

;; bitbake temp files
(add-to-list 'auto-mode-alist '("log.do_" . makefile-mode))
(add-to-list 'auto-mode-alist '("run.do_" . makefile-mode))

;; 
;; Taken from Unix Power Tools, 3rd Edition
;; 

;; Make CTRL-h delete the previous character. Normally, this gets
;; you into the "help" system.
;; (define-key global-map "\C-h" 'backward-delete-char)
;; make sure CTRL-h works in searches, too
;; (setq search-delete-char (string-to-char "\C-h"))

;; bind the "help" facility somewhere else (CTRL-underscore).
;; NOTE:  CTRL-underscore is not defined on some terminals.
;; (define-key global-map "\C-_" 'help-command) ;; replacement
:; (setq help-char (string-to-char "\C-_"))

;; Make ESC-h delete the previous word.
 (define-key global-map "\M-h" 'backward-kill-word)

;; Make CTRL-x CTRL-u the "undo" command; this is better than "CTRL-x u"
;; because you don't have to release the CTRL key.
 ;; (define-key global-map "\C-x\C-u" 'undo)

;; scroll the screen "up" or "down" one line with CTRL-z and ESC z
 (defun scroll-up-one () "Scroll up 1 line." (interactive)
   (scroll-up (prefix-numeric-value current-prefix-arg)))
 (defun scroll-down-one () "Scroll down 1 line." (interactive)
   (scroll-down (prefix-numeric-value current-prefix-arg)))
 (define-key global-map "\C-z" 'scroll-up-one)
 (define-key global-map "\M-z" 'scroll-down-one)
;; Use CTRL-x CTRL-v to "visit" a new file, keeping the current file
;; on the screen
 (define-key global-map "\C-x\C-v" 'find-file-other-window)

;;
(setq erc-echo-notices-in-minibuffer-flag t)


;; have buffers sync with disk
(global-auto-revert-mode 1)

;; look for packaes in melpa
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;;; key-binding for magit
(global-set-key (kbd "C-x g") 'magit-status)

;; better colors for patches
;; https://emacs.stackexchange.com/questions/16840/how-to-change-green-background-in-magit-running-in-xterm
(custom-set-faces
 ;; other faces
 '(magit-diff-added ((((type tty)) (:foreground "green"))))
 '(magit-diff-added-highlight ((((type tty)) (:foreground "LimeGreen"))))
 '(magit-diff-context-highlight ((((type tty)) (:foreground "default"))))
 '(magit-diff-file-heading ((((type tty)) nil)))
 '(magit-diff-removed ((((type tty)) (:foreground "red"))))
 '(magit-diff-removed-highlight ((((type tty)) (:foreground "IndianRed"))))
 '(magit-section-highlight ((((type tty)) nil))))

(global-linum-mode 1)
