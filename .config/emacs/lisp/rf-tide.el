;;; -*- lexical-binding: t; -*-

(rf/require 'tide)

(defun rf/turn-on-tide ()
  (interactive)
  (tide-setup))

(add-hook 'typescript-mode-hook 'rf/turn-on-tide)

(provide 'rf-tide)