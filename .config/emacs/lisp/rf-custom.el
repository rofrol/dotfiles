;;; -*- lexical-binding: t; -*-

;; https://stackoverflow.com/questions/5052088/what-is-custom-set-variables-and-faces-in-my-emacs/5058752#5058752
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; disabling as it causes problems: Symbol's function definition is void: custom-set-icon
;; (load custom-file)

(provide 'rf-custom)