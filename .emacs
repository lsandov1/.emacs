;; TODO: simple loop for loading
(add-to-list 'load-path "~/repos/.emacs")
(load "./packages.el")
(load "./book-gnu-emacs-extensions.el")
(load "./irc.el")
(load "./magit.el")
(load "./org.el")
(load "./themes.el")
(load "./tramp.el")
(load "./zoom-frm.el")

;; TODO: fix the following emacs initialization issue
;; error: Recursive load, /home/lsandov1/repos/.emacs/eshell.el, /home/lsandov1/repos/.emacs/eshell.el, /home/lsandov1/repos/.emacs/eshell.el, /home/lsandov1/repos/.emacs/eshell.el, /home/lsandov1/repos/.emacs/eshell.el, /home/lsandov1/repos/.emacs/.emacs, /home/lsandov1/.emacs
;; (load "./eshell.el")
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

;;Clear the eshell buffer.
(defun eshell/clear ()      
   (let ((eshell-buffer-maximum-lines 0)) (eshell-truncate-buffer)))


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
