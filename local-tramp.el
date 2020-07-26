(setq tramp-default-method "ssh")


(defun sudo ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
      (concat "/sudo:root@localhost:"
	buffer-file-name))))
