;; Personal information
(defun asim/personal-information ()
  (setq user-full-name "Alvin Sim"
	user-mail-address "sim.alvin@gmail.com"))

;; packages
(defun asim/packages ()
  (setq asim/use-packages '(company
			    beacon
			    leuven
			    projectile
			    flx-ido
			    magit
			    atom-one-dark-theme
			    yasnippet
			    clojure
			    cider
			    solarized-theme
			    dash
			    paredit
			    powerline))

  ;; Init packages and add package archives
  (load "package")
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

  ;; install any packages listed in asim-packages if they are not installed
  (let ((refreshed nil))
    (when (not package-archive-contents)
      (package-refresh-contents)
      (setq refresh t))
    (dolist (pkg asim/use-packages)
      (when (and (not (package-installed-p pkg))
		 (assoc pkg package-archive-contents))
	(unless refreshed
	  (package-refresh-contents)
	  (setq refreshed t))
	(package-install pkg)))))

(defun asim/indicate-empty-lines ()
  (setq-default indicate-empty-lines t)
  (when (not indicate-empty-lines)
    (toggle-indicate-empty-lines)))

;; windows configuration
(defun asim/window-system ()
  (when window-system
    (tooltip-mode -1)
    (tool-bar-mode -1)
    (menu-bar-mode 1)
    (scroll-bar-mode -1)
    ;; theme
    (load-theme 'atom-one-dark t))
  (add-to-list 'write-file-functions 'delete-trailing-whitespace))

;; winner-mode
(defun asim/winner-mode ()
  (when (fboundp 'winner-mode)
    (winner-mode 1)))

;; utf-8 encoding
(defun asim/utf8-encoding ()
  (prefer-coding-system 'utf-8)
  (when (display-graphic-p)
    (setq x-select-request-type '(UTF8-STRING_COMPOUND_TEXT TEXT STRING))))

;; paredit
(defun asim/paredit ()
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojurescript-mode-hook 'paredit-mode))

;; other misc configuration
(defun asim/other ()
  (setq-default tab-width 2)
  (setq inhibit-splash-screen t
	indent-tabs-mode t
	line-number-mode 1
	column-number-mode t
	show-trailing-whitespace t
	sentence-end-double-space nil
	echo-keystrokes 0.1
	use-dialog-box nil
	visible-bell t
	transient-mark-mode 1)
  (show-paren-mode t)
  (defalias 'yes-or-no-p 'y-or-n-p)	;; make all "yes or no" to "y or n"
  (global-visual-line-mode 1)		;; visual mode word wrap
  (add-hook 'after-init-hook 'global-company-mode)
  (global-hl-line-mode)
  (turn-on-eldoc-mode))

;; ido
(defun asim/ido ()
  (setq ido-enable-flex-matching t
	ido-everywhere t
	ido-create-new-buffer 'always
	ido-ignore-extensions t)
  (ido-mode 1))

;; org mode
(defun asim/org ()
  (setq org-agenda-files (list "/path/to/file.org"))
  (setq org-todo-keywords
	'((sequence "TODO(t)" "WAITING(w)" "DOING(g)" "|" "DONE(d)" "CANCEL(c)")))

  ;; keybindings
  (global-set-key "\C-cl" 'org-store-link)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cb" 'org-iswitchb)

  (eval-after-load "org"  '(require 'ox-md nil t))

  (setq org-catch-invisible-edits 1
	org-hide-emphasis-markers t
	org-ellipsis " .."))

  ;; org bullets
  ;; (require 'org-bullets)
  ;; (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))))

;; Beacon
(defun asim/beacon ()
  (beacon-mode 1))

;; Powerline
(defun asim/powerline ()
  (powerline-default-theme))

;; multi-term
;;(defun asim/multi-term ()
;;  (require 'multi-term)
;;  (setq multi-term-program "/usr/bin/bash"
;;	term-unbind-key-list '("C-z" "C-x" "C-h" "C-y")
;;	multi-term-dedicated-select-after-open-p t
;;	multi-term-dedicated-close-back-to-open-buffer-p t)
;;  (define-key global-map (kbd "C-x t") 'multi-term-dedicated-toggle))

;; projectile
(defun asim/projectile ()
  (require 'projectile)
  (projectile-global-mode))

;; flx-ido
(defun asim/flx-ido ()
  (require 'flx-ido)
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)

  ;; disable ido faces to see flx highlights
  (setq ido-enable-flex-matching t
	ido-use-faces nil))

;; org-babel
(defun asim/plantuml ()
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t))))

;; magit
(defun asim/magit ()
  (require 'magit)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
  (global-set-key (kbd "C-c M-g") 'magit-file-popup)
  (global-magit-file-mode))

;; cider
(defun asim/cider ()
  (setq cider-repl-pop-to-buffer-on-connect nil))

;;; key-bindings

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-:") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)

;;; toggle on/off configurations

(asim/personal-information)
(asim/packages)
(asim/indicate-empty-lines)
(asim/window-system)
(asim/winner-mode)
(asim/utf8-encoding)
(asim/other)
(asim/ido)
(asim/org)
(asim/magit)
(asim/plantuml)
(asim/flx-ido)
(asim/projectile)
(asim/powerline)
(asim/beacon)
(asim/paredit)
(asim/cider)
