(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (erc :server "irc.freenode.net")
  (setq erc-autojoin-channels-alist '(("irc.freenode.net" "#linaro-lava"))))

(setq erc-log-channels-directory "~/.erc/logs/")
