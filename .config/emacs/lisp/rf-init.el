(setq mac-command-modifier 'control)
(setq mac-control-modifier 'super)

(global-set-key (kbd "C-z") 'undo-only)
(global-set-key (kbd "C-S-z") 'undo-redo)

; https://emacs.stackexchange.com/questions/53196/how-to-quickly-revert-the-buffer
(defun rf/reload-file-preserve-point ()
  (interactive)
  (when (or (buffer-modified-p)
            (y-or-n-p "Reverting will discard changes. Proceed?"))
    (save-excursion
      (revert-buffer t t t))
    (setq buffer-undo-list nil)
    (message "Buffer reverted")))

(global-set-key (kbd "C-x x h") 'rf/reload-file-preserve-point)

(global-set-key (kbd "<f9>") (lambda () (interactive) (load user-init-file)))

(pixel-scroll-precision-mode t)

;; (global-display-line-numbers-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; these two does not work to prevent jumping/shifting in absolute line number
;; (setq display-line-numbers-width-start t)
;; (setq display-line-numbers-width-start 5)

;; with this there is only one jump when grows in absolute line number
;; https://www.reddit.com/r/emacs/comments/8pfdlb/comment/e0bh22k/
(setq display-line-numbers-grow-only t)

;(setq display-line-numbers-type 'relative)

;; prevent jumping when showing relative line number
;; https://emacs.stackexchange.com/questions/55165/uneven-line-numbers-with-display-line-numbers/55166#55166
(defun rf/display-line-numbers-equalize ()
  "prevent jumping when showing relative line number"
  (setq display-line-numbers-width (length (number-to-string (line-number-at-pos (point-max))))))
(add-hook 'find-file-hook 'rf/display-line-numbers-equalize)

(tool-bar-mode 0)
(menu-bar-mode 1)
;; (scroll-bar-mode 0)
(column-number-mode 1)
(setq ring-bell-function 'ignore)

(provide 'rf-init)
