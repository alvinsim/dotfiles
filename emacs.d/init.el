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
														which-key
														adoc-mode
														twilight-bright-theme
														move-text
														totd
														elpy
														clj-refactor
														flycheck
														py-autopep8
														org-plus-contrib
														powerline))

  ;; Init packages and add package archives
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa"	. "http://melpa.org/packages/") t)
	(add-to-list 'package-archives '("org"	. "http://orgmode.org/elpa/") t)

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
		(setq tooltip-use-echo-area t)
		(tool-bar-mode -1)
		(menu-bar-mode 1)
		(scroll-bar-mode -1))
		;; theme
		;;		(load-theme 'atom-one-dark t))
		;;		(load-theme 'leuven t))

  (require 'twilight-bright-theme)
  (load-theme 'twilight-bright t)
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

;; clojure dev
(defun asim/clojure-dev ()
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojurescript-mode-hook 'paredit-mode))

(defun asim/clojure-mode-hook ()
	(require 'clj-refactor)

	(clj-refactor-mode 1)
	(yas-minor-mode)	;;;; for adding require/use/import statements
	(cljr-add-keybindings-with-prefix "C-c C-m"))

;; other misc configuration
(defun asim/other ()
  (setq-default tab-width 2)
;								with-editor-emacsclient-executable "emacsclient")
  (setq inhibit-splash-screen t
		indent-tabs-mode t
		line-number-mode 1
		column-number-mode t
		show-trailing-whitespace t
		sentence-end-double-space nil
		echo-keystrokes 0.1
		use-dialog-box nil
		visible-bell t
		next-line-add-newlines t
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
	(setq org-emphasis-regexp-components
      '("     ('\"{??"
        "-   .,!?;''??\")}/\\??"
        "    \r\n,"
        "."
        1))
	(require 'org)
	(setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,\"")
	(custom-set-variables `(org-emphasis-alist ',org-emphasis-alist))
	(org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

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
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
  (setq org-plantuml-jar-path (expand-file-name "/path/to/plantuml.jar")))

;; magit
(defun asim/magit ()
  (require 'magit)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
  (global-set-key (kbd "C-c M-g") 'magit-file-popup)
  (global-magit-file-mode))

;; cider
(defun asim/cider ()
  (setq cider-repl-pop-to-buffer-on-connect nil)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode))

;; which-key
(defun asim/which-key ()
  (require 'which-key)
  (which-key-mode))

;; adoc
(defun asim/adoc ()
	(require 'adoc-mode)
	(add-to-list 'auto-mode-alist (cons "\\.txt\\'" 'adoc-mode))
	(add-hook 'adoc-mode-hook (lambda() (buffer-face-mode t))))

;; find-dired
(defun asim/find-dired ()
	(require 'find-dired)
	(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld")))

;; totd
(defun asim/totd ()
	(totd-start))

;; python development
(defun asim/python-dev ()
	(elpy-enable)
	(setq elpy-rpc-backend "jedi")

	; flycheck
	(require 'flycheck)
	(setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
	(add-hook 'elpy-mode-hook 'flycheck-mode)

	; py-autopep8
	(require 'py-autopep8)
	(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)))

;; clojure cookbook
(defun asim/clojure-cookbook ()
	(add-hook 'adoc-mode-hook 'cider-mode)
	(define-key adoc-mode-map (kbd "M-+") 'increment-clojure-cookbook))

;; interacting with the oreilly clojure cookbook
(defun increment-clojure-cookbook ()
	"When reading the Clojure cookbook, find the next section, and
	close the buffer. If the next section is a sub-directory or in
	the next chapter, open Dired so you can find it manually."
	(interactive)
	(let* ((cur (buffer-name))
	       (split-cur (split-string cur "[-_]"))
	       (chap (car split-cur))
	       (rec (car (cdr split-cur)))
	       (rec-num (string-to-number rec))
	       (next-rec-num (1+ rec-num))
	       (next-rec-s (number-to-string next-rec-num))
	       (next-rec (if (< next-rec-num 10)
											 (concat "0" next-rec-s)
										 next-rec-s))
	       (target (file-name-directory (concat chap "-" next-rec))))
	      (progn
		      (if (equal target nil)
							(dired (file-name-directory (buffer-file-name)))
						(find-file target))
		      (kill-buffer cur))))

;; clojure mode
(defun asim/clojure ()
	(autoload 'clojure-mode "clojure-mode" "A mode for Clojure lisp" t)
	(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))
	(autoload 'paredit-mode "paredit" "Parenthesis editing minor mode" t)
	(eval-after-load "clojure-mode"
		'(progn
			 (defun clojure-paredit-hook () (paredit-mode +1))
			 (add-hook 'clojure-mode-hook 'clojure-paredit-hook)
			 (add-hook 'clojurescript-mode 'clojure-paredit-hook)

			 (define-key clojure-mode-map "{" 'paredit-open-brace)
			 (define-key clojure-mode-map "}" 'paredit-close-brace)))

	;; clj-refcctor
	(require 'clj-refactor)
	(clj-refactor-mode 1)
	(yas-minor-mode)	;; for adding require/use/import statements
	(cljr-add-keybindings-with-prefix "C-c C-m"))

;;; key-bindings

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-:") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key shell-mode-map (kbd "SPC") 'comint-magic-space)

;; move-text
(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)

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
;;(asim/clojure-dev)
(asim/cider)
(asim/which-key)
(asim/adoc)
(asim/totd)
(asim/python-dev)
(asim/find-dired)
(asim/clojure)

;; clojure-mode-hooks
(add-hook 'clojure-mode-hook #'asim/clojure-mode-hook)

;;; utilities/helpers

;; interacting with the oreilly clojure cookbook
(defun increment-clojure-cookbook ()
	"When reading the Clojure cookbook, find the next section, and
	close the buffer. If the next section is a sub-directory or in
	the next chapter, open Dired so you can find it manually."
	(interactive)
	(let* ((cur (buffer-name))
	       (split-cur (split-string cur "[-_]"))
	       (chap (car split-cur))
	       (rec (car (cdr split-cur)))
	       (rec-num (string-to-number rec))
	       (next-rec-num (1+ rec-num))
	       (next-rec-s (number-to-string next-rec-num))
	       (next-rec (if (< next-rec-num 10)
			     (concat "0" next-rec-s)
			     next-rec-s))
	       (target (file-name-directory (concat chap "-" next-rec) "")))
	      (progn
		      (if (equal target nil)
			  (dired (file-name-directory (buffer-file-name)))
			  (find-file target))
		      (kill-buffer cur))))
