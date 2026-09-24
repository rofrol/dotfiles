;;; -*- lexical-binding: t; -*-

(require 'which-func)
;; https://alhassy.github.io/emacs.d/#Which-function-are-we-writing
(add-hook 'prog-mode-hook #'which-function-mode)

(provide 'rf-which-func)