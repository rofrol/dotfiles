;;; -*- lexical-binding: t; -*-

;;; multiple cursors
(rf/require 'multiple-cursors)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)
(global-set-key (kbd "C-c j") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-S-j") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-x SPC") 'set-rectangular-region-anchor)

(provide 'rf-multiple-cursors)