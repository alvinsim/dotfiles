;; Overwrite prelude configurations

;;;; Using prelude's minimalistic ui settings including removing the line number.
;;;; But bring back the menu bar.
(setq prelude-minimalistic-ui t)
(menu-bar-mode t)

;;;; Remove the scrollbars
(scroll-bar-mode -1)

;;;; When deleting a file, move them to the OS's trash.
(setq delete-by-moving-to-trash t)

;;;; Focus the help window when they are opened.
(setq help-window-select t)

;;;; Org-mode

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "DOING(g)" "|" "DONE(D)" "CANCEL(C)")
        (sequence "PAY(p)" "|" "PAID(P)" "CANCEL(C)")))

;;;;;; Resolve conflicts between windmove navigation and org-mode
(setq windmove-mode nil)

;;;; Key-bindings
(global-set-key (kbd "C-c C-.") 'org-time-stamp-inactive)
