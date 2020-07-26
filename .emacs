;; TODO: simple loop for loading
(add-to-list 'load-path "~/repos/.emacs")
(load "./local-packages.el")
(load "./local-book-gnu-emacs-extensions.el")
(load "./local-irc.el")
(load "./local-magit.el")
(load "./local-org.el")
(load "./local-themes.el")
(load "./local-tramp.el")
(load "./local-zoom-frm.el")
(load "./local-backups.el")
(load "./local-eshell.el")

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
