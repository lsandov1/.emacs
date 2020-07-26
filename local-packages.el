(when (require 'package)
  (add-to-list 'package-archives
    '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives
    '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (package-initialize))
