;; install org mode if for some reason this version of emacs doesn't
;; have org-mode pre-installed
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-refresh-contents)

(unless (package-installed-p 'org)
	(package-install 'org))
(unless (package-installed-p 'org-plus-contrib)
	(package-install 'org-plus-contrib))

(org-babel-load-file "~/.emacs.d/config.org")
