;;; -*- lexical-binding: t; -*-

;; https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/

;; If I use tree-sitter-hl-mode for syntax highlighting and tsi.el for indentation, do I even need typescript-mode.el anymore?
;; tree-sitter keeps a major-modes -> languages mapping at tree-sitter-major-mode-language-alist,
;; so I think you still need a major mode of some sort to associate with tree-sitter's TS parser.
;; also, typescript-mode integrates with compilation-mode and also provides some utility functions
;; so I do think it's more than just a highlighter+indenter.
;; https://www.reddit.com/r/emacs/comments/sbxjer/comment/hwox06t/
(rf/require 'typescript-mode)

(rf/require 'tree-sitter)
(rf/require 'tree-sitter-langs)

(with-eval-after-load 'tree-sitter
  (global-tree-sitter-mode))

(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; https://github.com/andersmurphy/.emacs.d/blob/7d19d4c93e8734a58fee0a7791fdfd5ff553689b/init.el#L773
;; https://vxlabs.com/2022/06/12/typescript-development-with-emacs-tree-sitter-and-lsp-in-2022/
;; https://github.com/andersmurphy/andersmurphy-blog/blob/90884606ee01f878d80c8ff757cc7ba42ffdceec/resources/posts/2020-08-20-emacs-setting-up-apheleia-to-use-zprint.md#L27
;; (rf/require 'apheleia)
;; (with-eval-after-load 'apheleia
;;   (apheleia-global-mode +1)
;;   (setf (alist-get 'prettier apheleia-formatters)
;;      '("yarn" "prettier"
;;         ;; "--trailing-comma"  "es5"
;;         ;; "--bracket-spacing" "true"
;;         ;; "--single-quote"    "true"
;;         ;; "--semi"            "false"
;;         ;; "--print-width"     "100"
;;         filepath)))

;; with apheleia there was noticeable delay after save
;; >If you're looking for a decent autosave experience with js/ts/json/yaml/ruby I highly recommend disabling doom format mode and using prettier-mode instead 
;; https://www.reddit.com/r/DoomEmacs/comments/mfr0ed/comment/gsp9yxa/
(rf/require 'prettier)
(add-hook 'after-init-hook #'global-prettier-mode)

(provide 'rf-tree-sitter)