;; Writing GNU Emacs Extensions: Editor Customizations and Creations with Lisp
;; Bob Glickstein
;; all code taken from the book above

;; chapter 01 - customizing emacs
(global-set-key "\M-?" 'help-command)

;; chapter 02 - traversing windows

(global-set-key "\C-x\C-n" 'other-window)

(defun other-window-backward (&optional n)
  "Select the Nth previous window."
  (interactive "P")
  (other-window (- (prefix-numeric-value  n))))
(global-set-key "\C-x\C-p" 'other-window-backward)


;; chapter 02 - line-at-a-time scrolling
(defalias 'scroll-ahead 'scroll-up)
(defalias 'scroll-behind 'scroll-down)

(defun scroll-n-lines-ahead (&optional n)
   "Scroll ahead one line."
   (interactive "P")
   (scroll-ahead (prefix-numeric-value n)))

(defun scroll-n-lines-behind (&optional n)
   "Scroll ahead one line."
   (interactive "P")
   (scroll-behind (prefix-numeric-value n)))

(global-set-key "\C-z" 'scroll-n-lines-ahead)
(global-set-key "\C-q" 'scroll-n-lines-behind)
(global-set-key "\C-x\C-q" 'read-only-mode) ;; moves key binding C-q to C-x C-q

;; chapter 02 - otehr cursor and text motion commands
(defun point-to-top ()
  "Put point on top line of window."
  (interactive)
  (move-to-window-line 0))
(global-set-key "\M-," 'point-to-top)
(global-set-key "\C-x," 'tags-loop-continue) ;; this (re-key)bind M-,

(defun point-to-bottom ()
  "Put point at the beginning of last visible line."
  (interactive)
  (move-to-window-line -1))
(global-set-key "\M-." 'point-to-bottom)

