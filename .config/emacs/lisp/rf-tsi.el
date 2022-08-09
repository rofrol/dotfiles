;;; -*- lexical-binding: t; -*-

;; https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/

;; indent as you code
(unless (package-installed-p 'tsi)
  (quelpa '(tsi :repo "orzechowskid/tsi.el" :fetcher github)))
(with-eval-after-load 'tsi
  (tsi-typescript-mode)
  (tsi-json-mode)
  (tsi-css-mode))

(provide 'rf-tsi)