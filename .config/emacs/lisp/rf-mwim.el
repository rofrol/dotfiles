;;; -*- lexical-binding: t; -*-

;; https://emacs.stackexchange.com/questions/48121/is-there-an-emacs-command-to-go-to-end-of-code-line-before-line-comment-and-whi/48160#48160
;; https://superuser.com/questions/331221/jump-to-first-non-whitespace-character-in-line-in-emacs
;; https://stackoverflow.com/questions/6035872/moving-to-the-start-of-a-code-line-emacs/6037523#6037523
;; https://stackoverflow.com/questions/6035872/moving-to-the-start-of-a-code-line-emacs/35394552#35394552
;; crux-move-beginning-of-line https://github.com/bbatsov/crux/blob/6bfd212a7f7ae32e455802fde1f9e3f4fba932a0/crux.el#L334
(rf/require 'mwim)
(global-set-key (kbd "C-a") 'mwim-beginning)
(global-set-key (kbd "C-e") 'mwim-end)

(provide 'rf-mwim)