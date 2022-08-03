;;; -*- lexical-binding: t; -*-

(when (eq system-type 'darwin)
  (when window-system
    ;; does not work when in early-init.el
    (set-frame-parameter nil 'fullscreen 'maximized)
  )
)

(defun rf/toggle-between-fullscreen-and-maximized ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
    (if (eq 'fullboth (frame-parameter (selected-frame) 'fullscreen)) 'maximized 'fullboth)))
(global-set-key (kbd "<f7>") 'rf/toggle-between-fullscreen-and-maximized)

(provide 'rf-maximized)