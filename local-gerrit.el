(use-package gerrit
  :ensure t
  :custom
  (gerrit-host "https://review.trustedfirmware.org")  ;; is needed for REST API calls
  :config
  (progn
    (add-hook 'magit-status-sections-hook #'gerrit-magit-insert-status t)
    (global-set-key (kbd "C-x i") 'gerrit-upload)
    (global-set-key (kbd "C-x o") 'gerrit-download)))
