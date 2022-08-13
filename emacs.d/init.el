;; install org mode if for some reason this version of emacs doesn't
;; have org-mode pre-installed
(require 'package)
;; (package-initialize)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-refresh-contents)

;; (unless (package-installed-p 'org)
;; 	(package-install 'org))
;; (unless (package-installed-p 'org-contrib)
;; 	(package-install 'org-contrib))

(org-babel-load-file "~/.emacs.d/config.org")
