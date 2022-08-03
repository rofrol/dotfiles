;;; -*- lexical-binding: t; -*-

;; https://stackoverflow.com/questions/5052088/what-is-custom-set-variables-and-faces-in-my-emacs/5058752#5058752
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(provide 'rf-custom)