(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

;;Clear the eshell buffer.
(defun eshell/clear ()      
   (let ((eshell-buffer-maximum-lines 0)) (eshell-truncate-buffer)))
