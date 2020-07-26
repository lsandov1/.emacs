(add-to-list 'load-path "~/.emacs.d/frame-fns/")
(load "frame-fns")

(add-to-list 'load-path "~/.emacs.d/frame-cmds/")
(load "frame-cmds")

(add-to-list 'load-path "~/.emacs.d/zoom/")
(load "zoom-frm")

(define-key ctl-x-map [(control ?+)] 'zoom-in/out)
(define-key ctl-x-map [(control ?-)] 'zoom-in/out)
(define-key ctl-x-map [(control ?=)] 'zoom-in/out)
