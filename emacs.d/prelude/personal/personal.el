;; ~/.emacs.d/personal/personal.el

;;;; Themes
;;;;;; ef-themes
(require 'ef-themes)
(ef-themes-load-random)

;;;; Yasnippet
;;;;;; TODO: Not working
(require 'yasnippet)
(setq yas-global-mode 1)
(setq yas-snippet-dirs '("~/.emacs.d/personal/snippets"))
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;;;; Org Mode
;;;;;; Make org COLUMNVIEW dynamic blocks behave normally
(with-eval-after-load 'org
  (advice-add 'org-dblock-update :around
              (lambda (orig-fun &rest args)
                (let ((inhibit-modification-hooks t))
                  (apply orig-fun args)))))
