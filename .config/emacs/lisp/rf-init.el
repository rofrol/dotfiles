(setq mac-command-modifier 'control)
(setq mac-control-modifier 'super)

(global-set-key (kbd "C-z") 'undo-only)
(global-set-key (kbd "C-S-z") 'undo-redo)
(global-set-key (kbd "<f9>") (lambda () (interactive) (load user-init-file)))

(pixel-scroll-precision-mode t)

(defun roforl/display-line-numbers-equalize ()
  "prevent jumping when showing relative line number"
  (setq display-line-numbers-width (length (number-to-string (line-number-at-pos (point-max))))))
(add-hook 'find-file-hook 'roforl/display-line-numbers-equalize)

(tool-bar-mode 0)
(menu-bar-mode 0)
;; (scroll-bar-mode 0)
(column-number-mode 1)
(setq ring-bell-function 'ignore)

(set-face-attribute 'default nil :font "NotoMono Nerd Font Mono 20")

(provide 'rf-init)