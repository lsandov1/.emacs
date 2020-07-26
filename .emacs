;; TODO: simple loop for loading
(load packages.el)
(load book-gnu-emacs-extensions.el)
(load eshell.el)
(load irc.el)
(load magit.el)
(load org.el)
(load themes.el)
(load tramp.el)
(load zoom-frm.el)

;; TODO: place the below commands under their respective emacs-lisp file
(global-linum-mode 1)
(setq x-select-enable-clipboard t)        ; copy-paste should work ...

(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs))

(global-auto-revert-mode 1)

;; provided by Icarus
(global-set-key (kbd "C-c j") 'counsel-git-grep)

(setq grep-highlight-matches t)

;; provided by Icarus
(global-set-key (kbd "M-i") 'imenu)

;; easy way to find-grep and find-dired
(global-set-key (kbd "M-s g") 'find-grep)
(global-set-key (kbd "M-s d") 'find-dired)
