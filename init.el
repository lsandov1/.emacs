;; TODO: simple loop for loading
(add-to-list 'load-path "~/repos/personal/.emacs")
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
(load "./local-makefile.el")

;; (load "./local-gerrit.el")

;; TODO: place the below commands under their respective emacs-lisp file
(global-linum-mode 1)
(setq select-enable-clipboard t)        ; copy-paste should work ...

(defun save-kill-emacs ()
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
(global-set-key (kbd "M-s d") 'find-name-dired)

(ivy-mode t)

(add-hook 'after-init-hook #'global-flycheck-mode)


(line-number-mode t)                      ; show line numbers
(column-number-mode t)                    ; show column numbers
(size-indication-mode t)                  ; show file size (emacs 22+)
(display-time-mode t)                     ; show time
(menu-bar-mode -1)                        ; don't show the menu


(add-hook 'dired-load-hook (function (lambda () (load "dired-x"))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/repos/linaro/tasks/tf.org")))
 '(package-selected-packages
   (quote
    (julia-mode julia-repl julia-shell lua-mode auto-org-md gerrit gerrit-download tea-time tomatinho zerodark-theme zenburn-theme yaml-mode wsd-mode python-mode paredit markdown-mode magit helm groovy-mode geiser flycheck dockerfile-mode docker counsel-gtags))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (require 'tea-time)

;; (add-hook 'tea-time-notification-hook
;;   (lambda ()
;;     (notifications-notify :title "Time is up!"
;;                           :body "I know you're busy, but it's TEA TIME!!"
;;                           :app-name "Tea Time"
;;                           :sound-name "alarm-clock-elapsed")))


(global-eldoc-mode -1)
(put 'downcase-region 'disabled nil)

;; enable flyspell-mode in all buffers

(flyspell-mode t)


(setq-default ediff-forward-word-function 'forward-char)
