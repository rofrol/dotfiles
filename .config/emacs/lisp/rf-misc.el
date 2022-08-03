;;; -*- lexical-binding: t; -*-

;; https://manueluberti.eu/emacs/2021/08/06/fido/
;; http://xahlee.info/emacs/emacs/emacs_icomplete_mode.html
(fido-vertical-mode 1)

;; https://stackoverflow.com/questions/2627289/how-to-replace-a-region-in-emacs-with-yank-buffer-contents
;; When Delete Selection mode is enabled, typed text replaces the selection
;; if the selection is active.  Otherwise, typed text is just inserted at
;; point regardless of any selection.
(delete-selection-mode 1)

;; https://git.sr.ht/~protesilaos/dotfiles/tree/master/item/emacs/.emacs.d/init.el#L34
(setq use-short-answers t)

;; recentf stuff
;; https://stackoverflow.com/questions/50417/how-do-i-get-list-of-recent-files-in-gnu-emacs/50422#50422
;; https://stackoverflow.com/questions/3527150/open-recent-in-emacs/3527488
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-auto-cleanup 'never)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(provide 'rf-misc)