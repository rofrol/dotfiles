;;; -*- lexical-binding: t; -*-

;; https://github.com/susam/emfy/blob/2c92129df8e1c80e06e093eb54cf3b1edee4a434/.emacs#L110
(defun show-current-time ()
  "Show current time."
  (interactive)
  (message (current-time-string)))
(global-set-key (kbd "C-c t") 'show-current-time)

(provide 'rf-current-time)